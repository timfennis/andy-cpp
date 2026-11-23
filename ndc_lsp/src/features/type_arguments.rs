//! What a generic function's type parameters are at the calls the program
//! makes of it. Its body is typed once for every call, so inside it a value
//! is an `A`; for a function called with one set of types, what `A` is there
//! is usually what the reader wants to know.

use std::cell::RefCell;

use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation};

use crate::resolved::Definition;
use crate::state::DocumentState;

/// How many calls a hover lists before saying how many more there are.
const SHOWN: usize = 5;

/// What some type parameters are, by name.
type Bound = Vec<(Box<str>, StaticType)>;

/// A call of a generic function, and what its type parameters are there.
#[derive(Debug, Clone, PartialEq)]
struct Call {
    span: Span,
    types: Bound,
}

/// The markdown that says what the type parameters in `typ` are at the
/// calls the program makes of the generic function around `offset`, as
/// "`A` is `[Int]` in `f(xs)`, line 7", or `None` when `typ` has none or
/// nothing calls the function with types of its own.
pub fn describe(state: &DocumentState, typ: &StaticType, offset: usize) -> Option<String> {
    let in_type = generics_in(typ);
    if in_type.is_empty() {
        return None;
    }
    let declaration = enclosing_function(state, offset, &in_type)?;
    // What the type parameters in `typ` are at each call, once per set.
    let mut shown: Vec<(Bound, Span, usize)> = Vec::new();
    for call in calls_of(state, declaration) {
        let types: Vec<_> = call
            .types
            .into_iter()
            .filter(|(name, _)| in_type.contains(name))
            .collect();
        if types.is_empty() {
            continue;
        }
        match shown.iter_mut().find(|(seen, _, _)| *seen == types) {
            Some((_, _, count)) => *count += 1,
            None => shown.push((types, call.span, 1)),
        }
    }
    if shown.is_empty() {
        return None;
    }

    let mut lines: Vec<String> = shown
        .iter()
        .take(SHOWN)
        .map(|(types, span, count)| {
            let names = types
                .iter()
                .map(|(name, typ)| format!("`{name}` is `{typ}`"))
                .collect::<Vec<_>>()
                .join(", ");
            let more = match count {
                1 => String::new(),
                2 => " and 1 more call like it".to_string(),
                n => format!(" and {} more calls like it", n - 1),
            };
            format!("{names} in {}{more}", where_called(state, *span))
        })
        .collect();
    if shown.len() > SHOWN {
        lines.push(format!("and {} more", shown.len() - SHOWN));
    }
    Some(lines.join("  \n"))
}

/// `` `f(xs)`, line 7 ``, cut short if long.
fn where_called(state: &DocumentState, span: Span) -> String {
    let text = state
        .analysed_text()
        .get(span.offset()..span.end())
        .unwrap_or("");
    let mut text: String = text.split_whitespace().collect::<Vec<_>>().join(" ");
    if text.chars().count() > 40 {
        text = text.chars().take(39).collect();
        text.push('…');
    }
    match state.range_of(span) {
        Some(range) => format!("`{text}`, line {}", range.start.line + 1),
        None => format!("`{text}`"),
    }
}

/// The names of the type parameters in `typ`, in the order they come.
fn generics_in(typ: &StaticType) -> Vec<Box<str>> {
    let names = RefCell::new(Vec::<Box<str>>::new());
    // A test that never holds visits every part.
    typ.contains(&|part| {
        if let StaticType::Generic(name) = part {
            let mut names = names.borrow_mut();
            if !names.contains(name) {
                names.push(name.clone());
            }
        }
        false
    });
    names.into_inner()
}

/// The innermost function declaration around `offset` whose type has the
/// type parameters `names`.
fn enclosing_function(state: &DocumentState, offset: usize, names: &[Box<str>]) -> Option<NodeId> {
    let mut found = None;
    let mut level: Vec<&ExpressionLocation> = state.ast.iter().collect();
    while let Some(expr) = level
        .iter()
        .find(|expr| expr.span.offset() <= offset && offset <= expr.span.end())
    {
        if matches!(expr.expression, Expression::FunctionDeclaration { .. }) {
            let declared = signature(state, expr.id)
                .iter()
                .flat_map(generics_in)
                .collect::<Vec<_>>();
            if names.iter().all(|name| declared.contains(name)) {
                found = Some(expr.id);
            }
        }
        level = expr.children();
    }
    found
}

/// The types of the parameters of the function declared at `declaration`,
/// then what it returns.
fn signature(state: &DocumentState, declaration: NodeId) -> Vec<StaticType> {
    let mut types = match state.analysis.function_types.get(&declaration) {
        Some(StaticType::Function {
            parameters: Some(parameters),
            ..
        }) => parameters.clone(),
        _ => Vec::new(),
    };
    if let Some(returns) = state.analysis.inferred_return_types.get(&declaration) {
        types.push(returns.clone());
    }
    types
}

/// Every call of the function declared at `declaration` whose arguments
/// and result say what its type parameters are, outside the function
/// itself: a recursive call passes along its own.
fn calls_of(state: &DocumentState, declaration: NodeId) -> Vec<Call> {
    let parameters: Vec<StaticType> = match state.analysis.function_types.get(&declaration) {
        Some(StaticType::Function {
            parameters: Some(parameters),
            ..
        }) => parameters.clone(),
        _ => return Vec::new(),
    };
    let returns = state.analysis.inferred_return_types.get(&declaration);

    let mut calls = Vec::new();
    let mut stack: Vec<&ExpressionLocation> = state.ast.iter().collect();
    while let Some(expr) = stack.pop() {
        stack.extend(expr.children());
        let (Expression::Call {
            function,
            arguments,
        }
        | Expression::MethodCall {
            function,
            arguments,
        }) = &expr.expression
        else {
            continue;
        };
        let calls_it = state
            .resolved
            .definitions_of(function.id)
            .is_some_and(|definitions| definitions.contains(&Definition::Node(declaration)));
        if !calls_it || arguments.len() != parameters.len() {
            continue;
        }
        let mut types = Vec::new();
        for (parameter, argument) in parameters.iter().zip(arguments) {
            if let Some(typ) = state.analysis.expr_types.get(&argument.id) {
                bind(parameter, typ, &mut types);
            }
        }
        if let (Some(returns), Some(typ)) = (returns, state.analysis.expr_types.get(&expr.id)) {
            bind(returns, typ, &mut types);
        }
        // A call in the function's own body knows no more than it does.
        let settled = |typ: &StaticType| {
            !typ.contains(&|part| {
                matches!(
                    part,
                    StaticType::Generic(_)
                        | StaticType::Var(_)
                        | StaticType::Unknown
                        | StaticType::Error
                )
            })
        };
        types.retain(|(_, typ)| settled(typ));
        if !types.is_empty() {
            types.sort_by(|(a, _), (b, _)| a.cmp(b));
            calls.push(Call {
                span: expr.span,
                types,
            });
        }
    }
    calls.sort_by_key(|call| call.span.offset());
    calls
}

/// Adds to `types` what the type parameters in `generic` are, where
/// `concrete` has the same shape: `[A]` and `[Int]` make `A` an `Int`. The
/// first a type parameter is matched with stays.
fn bind(generic: &StaticType, concrete: &StaticType, types: &mut Bound) {
    use StaticType as T;
    match (generic, concrete) {
        (T::Generic(name), _) => {
            if !types.iter().any(|(bound, _)| bound == name) {
                types.push((name.clone(), concrete.clone()));
            }
        }
        (T::List(g), T::List(c))
        | (T::Option(g), T::Option(c))
        | (T::Lazy(g), T::Lazy(c))
        | (T::Deque(g), T::Deque(c))
        | (T::MinHeap(g), T::MinHeap(c))
        | (T::MaxHeap(g), T::MaxHeap(c))
        // A sequence passes for what it iterates as.
        | (T::Iterable(g), T::Iterable(c) | T::List(c) | T::Lazy(c) | T::Deque(c)) => {
            bind(g, c, types);
        }
        (T::Tuple(g), T::Tuple(c)) if g.len() == c.len() => {
            for (g, c) in g.iter().zip(c) {
                bind(g, c, types);
            }
        }
        (T::Map { key: gk, value: gv }, T::Map { key: ck, value: cv }) => {
            bind(gk, ck, types);
            bind(gv, cv, types);
        }
        (
            T::Function {
                parameters: gp,
                return_type: gr,
            },
            T::Function {
                parameters: cp,
                return_type: cr,
            },
        ) => {
            if let (Some(gp), Some(cp)) = (gp, cp)
                && gp.len() == cp.len()
            {
                for (g, c) in gp.iter().zip(cp) {
                    bind(g, c, types);
                }
            }
            bind(gr, cr, types);
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// What a hover in `text` at `$0` says of the type `typ`'s parameters.
    fn described(text: &str, typ: &StaticType) -> Option<String> {
        let offset = text.find("$0").expect("a cursor");
        let source = text.replacen("$0", "", 1);
        describe(&DocumentState::analysed(&source), typ, offset)
    }

    fn generic(name: &str) -> StaticType {
        StaticType::Generic(name.into())
    }

    #[test]
    fn a_type_parameter_is_what_the_call_passes() {
        let text = "fn merge(ranges) {\n  let r$0c = ranges;\n  rc\n}\nlet rr = [(1, 2)];\nlet m = merge(rr);\n";
        assert_eq!(
            described(text, &generic("A")).as_deref(),
            Some("`A` is `[(Int, Int)]` in `merge(rr)`, line 6")
        );
    }

    #[test]
    fn each_different_set_of_types_is_listed_once() {
        let text = "fn pair(a, b) { (a$0, b) }\nlet p = pair(1, \"s\");\nlet q = pair(2.5, [1]);\nlet r = pair(3, \"t\");\n";
        let found = described(text, &StaticType::Tuple(vec![generic("A"), generic("B")]))
            .expect("a description");
        let lines: Vec<&str> = found.split("  \n").collect();
        assert_eq!(
            lines,
            [
                "`A` is `Int`, `B` is `String` in `pair(1, \"s\")`, line 2 and 1 more call like it",
                "`A` is `Float`, `B` is `[Int]` in `pair(2.5, [1])`, line 3",
            ]
        );
    }

    #[test]
    fn a_type_parameter_inside_a_type_is_found_by_its_shape() {
        let text = "fn heads<A>(xs: [A]) { xs$0[0] }\nprint(heads([[1], [2]]));\n";
        assert_eq!(
            described(text, &generic("A")).as_deref(),
            Some("`A` is `[Int]` in `heads([[1], [2]])`, line 2")
        );
    }

    #[test]
    fn a_call_with_a_dot_passes_its_receiver_first() {
        let text = "fn firsts(xs) { xs$0 }\nlet ys = [1, 2].firsts();\n";
        assert_eq!(
            described(text, &generic("A")).as_deref(),
            Some("`A` is `[Int]` in `[1, 2].firsts()`, line 2")
        );
    }

    #[test]
    fn nothing_is_said_without_a_call_or_a_type_parameter() {
        assert_eq!(described("fn id(x) { x$0 }\n", &generic("A")), None);
        assert_eq!(
            described("fn id(x) { x$0 }\nprint(id(1));\n", &StaticType::Int),
            None
        );
        // A recursive call passes its own type parameters along.
        let text = "fn count(xs) { if len(xs) > 0 { count(xs) } else { 0 } }$0\n";
        assert_eq!(described(text, &generic("A")), None);
    }
}
