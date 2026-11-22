//! Signature help: the parameters of the call being written, with the one
//! the cursor is at. A call being written rarely parses (`f(a, `), so the call
//! is found from the tokens before the cursor rather than the AST.

use ls_types::{
    Documentation, MarkupContent, MarkupKind, ParameterInformation, ParameterLabel, Position,
    SignatureHelp, SignatureInformation,
};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::{Lexer, SourceId, Token, TokenLocation};
use ndc_parser::{BindingPattern, Expression, ExpressionLocation};

use crate::features::completion::{FunctionInfo, declared_functions, receiver_type};
use crate::state::DocumentState;

/// The call the cursor is in.
#[derive(Debug, PartialEq, Eq)]
struct Call {
    /// The name of the function called.
    name: String,
    /// Where the dot is, for a call written with one (`xs.map(`), whose
    /// receiver is the first argument.
    dot: Option<usize>,
    /// Which argument the cursor is at, counting from 0 after the receiver.
    argument: usize,
}

/// The signatures of the function the cursor is calling, each overload its
/// own, with the parameter the cursor is at.
pub fn signature_help(
    state: &DocumentState,
    functions: &[FunctionInfo],
    position: Position,
) -> Option<SignatureHelp> {
    let offset = state.line_index.offset(&state.source, position)?;
    let call = call_at(state.source.get(..offset)?)?;
    let receiver = call.dot.and_then(|dot| receiver_type(state, dot));

    let declared = declared_functions(state, position);
    let overloads: Vec<&FunctionInfo> = declared
        .iter()
        .chain(functions)
        .filter(|function| function.name == call.name && !function.is_field)
        .filter(
            |function| match (&function.static_type, call.dot, &receiver) {
                // A call with a dot passes the receiver as the first argument.
                (StaticType::Function { parameters, .. }, Some(_), receiver) => {
                    match (parameters, receiver) {
                        (Some(parameters), Some(receiver)) => parameters
                            .first()
                            .is_some_and(|first| receiver.may_fit(first)),
                        (Some(parameters), None) => !parameters.is_empty(),
                        (None, _) => true,
                    }
                }
                (StaticType::Function { .. }, None, _) => true,
                _ => false,
            },
        )
        .collect();
    if overloads.is_empty() {
        return None;
    }

    let skip = usize::from(call.dot.is_some());
    let signatures: Vec<SignatureInformation> = overloads
        .iter()
        .map(|function| signature(state, function, skip))
        .collect();
    // The first overload that has a parameter where the cursor is.
    let active = overloads
        .iter()
        .position(|function| match &function.static_type {
            StaticType::Function {
                parameters: Some(parameters),
                ..
            } => parameters.len() > call.argument + skip,
            _ => true,
        })
        .unwrap_or(0);
    Some(SignatureHelp {
        signatures,
        active_signature: u32::try_from(active).ok(),
        active_parameter: u32::try_from(call.argument).ok(),
    })
}

/// One overload's signature: `name(x: Int, Float) -> Int`, with the names of
/// a function's parameters or a struct's fields where the program gives
/// them, and without the receiver of a call with a dot (`skip`).
fn signature(state: &DocumentState, function: &FunctionInfo, skip: usize) -> SignatureInformation {
    let StaticType::Function {
        parameters,
        return_type,
    } = &function.static_type
    else {
        unreachable!("only functions are offered");
    };
    let names = function
        .declaration
        .and_then(|declaration| parameter_names(&state.ast, declaration));

    let mut label = format!("{}(", function.name);
    let mut infos = Vec::new();
    match parameters {
        Some(parameters) => {
            for (index, typ) in parameters.iter().enumerate().skip(skip) {
                if index > skip {
                    label.push_str(", ");
                }
                let start = utf16_len(&label);
                match names.as_ref().and_then(|names| names.get(index)) {
                    Some(name) => label.push_str(&format!("{name}: {typ}")),
                    None => label.push_str(&typ.to_string()),
                }
                infos.push(ParameterInformation {
                    label: ParameterLabel::LabelOffsets([start, utf16_len(&label)]),
                    documentation: None,
                });
            }
        }
        // A function that takes any arguments.
        None => label.push_str("..."),
    }
    label.push_str(&format!(") -> {return_type}"));

    SignatureInformation {
        label,
        documentation: function.documentation.as_ref().map(|text| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: text.clone(),
            })
        }),
        parameters: Some(infos),
        active_parameter: None,
    }
}

fn utf16_len(text: &str) -> u32 {
    u32::try_from(text.encode_utf16().count()).unwrap_or(u32::MAX)
}

/// The names of the parameters of the function declared at `declaration`,
/// or the fields of the struct, which its constructor takes. A parameter
/// that destructures has no one name.
fn parameter_names(ast: &[ExpressionLocation], declaration: NodeId) -> Option<Vec<String>> {
    let mut stack: Vec<&ExpressionLocation> = ast.iter().collect();
    while let Some(expr) = stack.pop() {
        if expr.id == declaration {
            return match &expr.expression {
                Expression::FunctionDeclaration { parameters, .. } => Some(
                    parameters
                        .iter()
                        .map(|parameter| match &parameter.lvalue.pattern {
                            BindingPattern::Identifier { identifier, .. } => identifier.clone(),
                            BindingPattern::Sequence(_) => "_".to_string(),
                        })
                        .collect(),
                ),
                Expression::StructDeclaration { fields, .. } => Some(
                    fields
                        .iter()
                        .map(|field| field.identifier.clone())
                        .collect(),
                ),
                _ => None,
            };
        }
        stack.extend(expr.children());
    }
    None
}

/// The call that `before`, the text up to the cursor, ends inside: the
/// innermost `(` not closed yet that follows a function's name, and how many
/// of its arguments come before the cursor.
fn call_at(before: &str) -> Option<Call> {
    // A token the cursor is in the middle of, as an unfinished string, ends
    // the lexing; what's before it still counts.
    let tokens: Vec<TokenLocation> = Lexer::new(before, SourceId::SYNTHETIC)
        .map_while(Result::ok)
        .collect();

    // What each bracket still open holds: where it opens, and how many commas
    // it has at its own level.
    struct Open {
        index: usize,
        parenthesis: bool,
        commas: usize,
    }
    let mut open: Vec<Open> = Vec::new();
    for (index, token) in tokens.iter().enumerate() {
        match token.token {
            Token::LeftParentheses => open.push(Open {
                index,
                parenthesis: true,
                commas: 0,
            }),
            Token::LeftSquareBracket | Token::LeftCurlyBracket | Token::MapOpen => {
                open.push(Open {
                    index,
                    parenthesis: false,
                    commas: 0,
                });
            }
            Token::RightParentheses | Token::RightSquareBracket | Token::RightCurlyBracket => {
                open.pop();
            }
            Token::Comma => {
                if let Some(innermost) = open.last_mut() {
                    innermost.commas += 1;
                }
            }
            _ => {}
        }
    }

    // In `f([1, |`, the call is still `f`'s, at its first argument.
    let call = open.iter().rev().find(|open| open.parenthesis)?;
    let callee = tokens.get(call.index.checked_sub(1)?)?;
    let Token::Identifier(name) = &callee.token else {
        return None;
    };
    let dot = call
        .index
        .checked_sub(2)
        .and_then(|index| tokens.get(index))
        .filter(|token| token.token == Token::Dot)
        .map(|token| token.span.offset());
    Some(Call {
        name: name.clone(),
        dot,
        argument: call.commas,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::document;

    fn call(before: &str) -> Option<Call> {
        call_at(before)
    }

    #[test]
    fn the_call_is_the_innermost_open_one_after_a_name() {
        assert_eq!(
            call("f(1, g(2, "),
            Some(Call {
                name: "g".into(),
                dot: None,
                argument: 1
            })
        );
        assert_eq!(
            call("f(1, g(2), "),
            Some(Call {
                name: "f".into(),
                dot: None,
                argument: 2
            })
        );
        // Commas inside a list don't move to the next argument.
        assert_eq!(call("f([1, 2, ").map(|c| c.argument), Some(0));
        // A grouping or a closed call isn't a call being written.
        assert_eq!(call("(1 + "), None);
        assert_eq!(call("f(1)"), None);
        // An unfinished string doesn't hide the call before it.
        assert_eq!(call("f(1, \"ab").map(|c| c.argument), Some(1));
    }

    #[test]
    fn a_call_with_a_dot_knows_where_its_receiver_ends() {
        let found = call("xs.map(").expect("a call");
        assert_eq!(
            (found.name.as_str(), found.dot, found.argument),
            ("map", Some(2), 0)
        );
    }

    /// The labels of the signatures at `$0` in the last of `texts`, the
    /// active one's index, and its active parameter's label.
    fn help(texts: &[&str]) -> (Vec<String>, usize, Option<String>) {
        let (document, position) = document(texts);
        let help = document.signature_help(position).expect("signature help");
        let active = help.active_signature.expect("an active signature") as usize;
        let signature = &help.signatures[active];
        let parameter = help.active_parameter.and_then(|index| {
            let ParameterLabel::LabelOffsets([start, end]) =
                signature.parameters.as_ref()?.get(index as usize)?.label
            else {
                return None;
            };
            let label: Vec<u16> = signature.label.encode_utf16().collect();
            Some(String::from_utf16_lossy(
                &label[start as usize..end as usize],
            ))
        });
        (
            help.signatures.into_iter().map(|s| s.label).collect(),
            active,
            parameter,
        )
    }

    #[test]
    fn a_function_the_program_declares_shows_its_parameter_names() {
        let before = "fn area(width: Int, height: Int) { width * height }\n";
        let (labels, _, parameter) = help(&[before, &format!("{before}area(3, $0")]);
        assert_eq!(labels, ["area(width: Int, height: Int) -> Int"]);
        assert_eq!(parameter.as_deref(), Some("height: Int"));
    }

    #[test]
    fn a_struct_constructor_shows_its_fields() {
        let before = "struct Point { x: Int, y: Int }\n";
        let (labels, _, parameter) = help(&[before, &format!("{before}Point($0")]);
        assert_eq!(labels, ["Point(x: Int, y: Int) -> Point"]);
        assert_eq!(parameter.as_deref(), Some("x: Int"));
    }

    #[test]
    fn overloads_are_each_shown_and_the_first_that_fits_is_active() {
        let before = "fn f(a: Int) { a }\nfn f(a: Int, b: Int) { a + b }\n";
        let (labels, active, parameter) = help(&[before, &format!("{before}f(1, $0")]);
        assert_eq!(labels.len(), 2, "{labels:?}");
        assert_eq!(labels[active], "f(a: Int, b: Int) -> Int");
        assert_eq!(parameter.as_deref(), Some("b: Int"));
    }

    #[test]
    fn a_call_with_a_dot_leaves_out_the_receiver() {
        let before = "fn scale(xs: [Int], by: Int) { xs }\nlet xs = [1];\n";
        let (labels, _, parameter) = help(&[before, &format!("{before}xs.scale($0")]);
        assert_eq!(labels, ["scale(by: Int) -> [Int]"]);
        assert_eq!(parameter.as_deref(), Some("by: Int"));
    }

    #[test]
    fn a_built_in_shows_its_types_and_documentation() {
        let (document, position) = document(&["print($0"]);
        let help = document.signature_help(position).expect("signature help");
        assert!(
            help.signatures
                .iter()
                .all(|s| s.label.starts_with("print("))
        );
        assert!(help.signatures.iter().any(|s| s.documentation.is_some()));
    }

    #[test]
    fn nothing_is_offered_outside_a_call() {
        let (document, position) = document(&["let x = (1 + $0"]);
        assert_eq!(document.signature_help(position), None);
    }
}
