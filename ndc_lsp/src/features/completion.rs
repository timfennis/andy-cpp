use ls_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionResponse,
    Documentation, MarkupContent, MarkupKind, Position,
};
use ndc_analysis::Analysis;
use ndc_core::collections::HashMap;
use ndc_core::{Intrinsic, NodeId, StaticType};
use ndc_interpreter::{Builtins, Interpreter};

use crate::scope_resolve::{Decl, DeclKind, collect_declarations, file_scope, is_visible};
use crate::state::DocumentState;

/// A `Send` snapshot of a registered native function, built once at startup so
/// completion (and hover) never have to rebuild the interpreter per request.
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub static_type: StaticType,
    pub documentation: Option<String>,
    /// What the compiler writes in its place, if it's an intrinsic.
    pub intrinsic: Option<Intrinsic>,
    /// A struct field's getter, `x` for `p.x`, which completes after a dot as
    /// a field rather than a call.
    pub is_field: bool,
    /// The declaration of a function or struct the program declares, whose
    /// parameter or field names signature help shows.
    pub declaration: Option<NodeId>,
}

impl FunctionInfo {
    /// Snapshot every function registered in an interpreter, in global
    /// slot order: the natives, then the intrinsics.
    pub fn collect(interpreter: &Interpreter) -> Vec<Self> {
        let natives = interpreter.natives();
        let intrinsics = natives.intrinsics().iter().map(|fun| Self {
            name: fun.name.clone(),
            static_type: fun.static_type.clone(),
            documentation: fun.documentation.clone(),
            intrinsic: Some(fun.intrinsic),
            is_field: false,
            declaration: None,
        });
        let natives = (0..natives.len()).map(|slot| Self {
            name: natives.name(slot).to_string(),
            static_type: natives.static_type(slot).clone(),
            documentation: natives.documentation(slot).map(str::to_string),
            intrinsic: None,
            is_field: false,
            declaration: None,
        });
        natives.chain(intrinsics).collect()
    }

    /// The functions a declaration in the program declares: a named
    /// function, or a struct's constructor and its fields' getters.
    fn declared(decl: &Decl, analysis: &Analysis) -> Vec<Self> {
        let declared = |name: &str, static_type: StaticType, is_field: bool| Self {
            name: name.to_string(),
            static_type,
            documentation: None,
            intrinsic: None,
            is_field,
            declaration: match decl.kind {
                DeclKind::Function(node) | DeclKind::Struct(node) => Some(node),
                DeclKind::Variable(_) => None,
            },
        };
        match decl.kind {
            DeclKind::Variable(_) => Vec::new(),
            DeclKind::Function(node) => {
                let Some(StaticType::Function {
                    parameters,
                    return_type,
                }) = analysis.function_types.get(&node)
                else {
                    return Vec::new();
                };
                // Without an annotation the declared return type is `Any`;
                // what the body returns says more.
                let return_type = analysis
                    .inferred_return_types
                    .get(&node)
                    .map_or_else(|| return_type.clone(), |t| Box::new(t.clone()));
                let static_type = StaticType::Function {
                    parameters: parameters.clone(),
                    return_type,
                };
                vec![declared(&decl.name, static_type, false)]
            }
            DeclKind::Struct(node) => {
                let Some(declared_struct) = analysis.structs.get(&node) else {
                    return Vec::new();
                };
                let info = &declared_struct.info;
                let constructor = declared(&decl.name, info.constructor_type(), false);
                let fields = info.fields.iter().map(|(field, typ)| {
                    let getter = StaticType::Function {
                        parameters: Some(vec![info.static_type()]),
                        return_type: Box::new(typ.clone()),
                    };
                    declared(field, getter, true)
                });
                std::iter::once(constructor).chain(fields).collect()
            }
        }
    }
}

/// Build completion response for the given cursor position and document state.
pub fn complete(
    state: Option<&DocumentState>,
    position: Position,
    functions: &[FunctionInfo],
) -> CompletionResponse {
    let receiver_type = state.and_then(|s| {
        let offset = s.line_index.offset(&s.source, position)?;
        let dot_offset = find_dot_before(s.source.as_bytes(), offset)?;
        receiver_type(s, dot_offset)
    });

    let is_dot = receiver_type.is_some();

    // The program's own functions join the built-ins; one with a built-in's
    // name and number of parameters folds into the same item.
    let declared = state.map_or_else(Vec::new, |state| declared_functions(state, position));
    let groups = group_overloads(declared.iter().chain(functions), receiver_type.as_ref());
    let function_items = groups.iter().map(|group| group.render(is_dot));

    if is_dot {
        return CompletionResponse::Array(function_items.collect());
    }

    // General (non-dot) completion: functions + in-scope locals + keywords.
    let mut items: Vec<CompletionItem> = function_items.collect();
    if let Some(state) = state {
        // Locals come from walking the AST's declaration spans, so they are only
        // trustworthy when the AST matches the current source. Mid-edit (last
        // parse failed) the spans are stale, so skip them — the resilient
        // dot-completion caches above are unaffected.
        if state.analysis_matches_source {
            items.extend(local_completions(state, position));
        }
    }
    items.extend(keyword_completions());
    CompletionResponse::Array(items)
}

/// One completion item per function name and arity. Overloads that differ only
/// in parameter types fold together: `randf(Int, Int)`, `randf(Int, Float)`, …,
/// `randf(Number, Number)` become a single `randf(Int | Float | Number, Int | Float | Number)`,
/// while `randf()` and `randf(max)` stay separate items.
struct OverloadGroup<'a> {
    name: &'a str,
    /// A struct field, completed as `p.x` rather than a call.
    is_field: bool,
    /// Distinct candidate types per parameter position. `None` when the
    /// overloads don't declare their parameters.
    parameters: Option<Vec<Vec<&'a StaticType>>>,
    return_types: Vec<&'a StaticType>,
    documentation: Option<&'a str>,
}

impl OverloadGroup<'_> {
    /// Render this group as one completion item. In dot mode the first
    /// parameter is the receiver and is omitted from the displayed signature.
    fn render(&self, is_dot: bool) -> CompletionItem {
        if self.is_field {
            return self.render_field();
        }
        let param_detail = match &self.parameters {
            Some(positions) => {
                let ps = positions
                    .iter()
                    .skip(usize::from(is_dot))
                    .map(|candidates| merged_type_display(candidates))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({ps})")
            }
            None => "(...)".to_string(),
        };

        // Same-name items otherwise sort in whatever order the client picks;
        // sorting by arity keeps `randf()` above `randf(min, max)`.
        let sort_text = match &self.parameters {
            Some(positions) => format!("{}{:02}", self.name, positions.len()),
            None => format!("{}~~", self.name),
        };

        let return_detail = merged_type_display(&self.return_types);

        CompletionItem {
            label: self.name.to_string(),
            // Clients that don't render label_details in the menu (e.g. Helix)
            // show `detail` in the docs popup instead.
            detail: Some(format!("{}{param_detail} -> {return_detail}", self.name)),
            label_details: Some(CompletionItemLabelDetails {
                detail: Some(param_detail),
                description: Some(return_detail),
            }),
            kind: Some(CompletionItemKind::FUNCTION),
            sort_text: Some(sort_text),
            documentation: self.documentation.map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: d.to_string(),
                })
            }),
            ..Default::default()
        }
    }
}

impl OverloadGroup<'_> {
    /// A field shows the type it holds, as `x: Int`.
    fn render_field(&self) -> CompletionItem {
        let typ = merged_type_display(&self.return_types);
        CompletionItem {
            label: self.name.to_string(),
            detail: Some(format!("{}: {typ}", self.name)),
            label_details: Some(CompletionItemLabelDetails {
                detail: None,
                description: Some(typ),
            }),
            kind: Some(CompletionItemKind::FIELD),
            // Above the functions that take the struct, which are many.
            sort_text: Some(format!("!{}", self.name)),
            ..Default::default()
        }
    }
}

/// The functions the program declares that are visible at `position`. While
/// the text doesn't parse they come from the last analysis of a text that did,
/// so the cursor is first mapped to where it was in that text.
pub(crate) fn declared_functions(state: &DocumentState, position: Position) -> Vec<FunctionInfo> {
    let (Some(offset), Some(first)) = (
        state.line_index.offset(&state.source, position),
        state.ast.first(),
    ) else {
        return Vec::new();
    };
    let offset = state.analysed_offset(offset);
    let scope = file_scope(first.span.source_id(), state.analysed_len());
    collect_declarations(&state.ast, scope)
        .iter()
        .filter(|decl| is_visible(decl, offset))
        .flat_map(|decl| FunctionInfo::declared(decl, &state.analysis))
        .collect()
}

/// Group registered functions by (name, arity), keeping registration order and
/// deduplicating the types seen at each parameter position. When
/// `receiver_type` is given (dot completion), overloads whose first parameter
/// doesn't accept the receiver are dropped before grouping.
fn group_overloads<'a>(
    functions: impl IntoIterator<Item = &'a FunctionInfo>,
    receiver_type: Option<&StaticType>,
) -> Vec<OverloadGroup<'a>> {
    let mut groups: Vec<OverloadGroup<'a>> = Vec::new();
    let mut index: HashMap<(&'a str, Option<usize>, bool), usize> = HashMap::default();

    for fun in functions {
        if !is_normal_ident(&fun.name) {
            continue;
        }
        // `p.x` reads a field; `x(p)` would too, but nobody writes that.
        if fun.is_field && receiver_type.is_none() {
            continue;
        }
        let StaticType::Function {
            parameters,
            return_type,
        } = &fun.static_type
        else {
            continue;
        };
        if let (Some(recv), Some(params)) = (receiver_type, parameters)
            && (params.is_empty() || !recv.may_fit(&params[0]))
        {
            continue;
        }

        let key = (
            fun.name.as_str(),
            parameters.as_ref().map(Vec::len),
            fun.is_field,
        );
        let slot = *index.entry(key).or_insert_with(|| {
            groups.push(OverloadGroup {
                name: &fun.name,
                is_field: fun.is_field,
                parameters: parameters
                    .as_ref()
                    .map(|params| vec![Vec::new(); params.len()]),
                return_types: Vec::new(),
                documentation: None,
            });
            groups.len() - 1
        });
        let group = &mut groups[slot];
        if let (Some(positions), Some(params)) = (&mut group.parameters, parameters) {
            for (candidates, param) in positions.iter_mut().zip(params) {
                if !candidates.contains(&param) {
                    candidates.push(param);
                }
            }
        }
        if !group.return_types.contains(&&**return_type) {
            group.return_types.push(return_type);
        }
        if group.documentation.is_none() {
            group.documentation = fun.documentation.as_deref();
        }
    }

    groups
}

/// `[Int]` -> `Int`; `[[Int], Iterable<Int>]` -> `Iterable<Int>` (a
/// candidate subsuming all others wins); `[Int, Float, Number]` ->
/// `Int | Float | Number` (no candidate subsumes the others).
fn merged_type_display(candidates: &[&StaticType]) -> String {
    if let Some(widest) = candidates
        .iter()
        .find(|t| candidates.iter().all(|c| c.fits(t)))
    {
        return widest.to_string();
    }
    candidates
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" | ")
}

/// Language keywords offered in general (non-dot) completion.
const KEYWORDS: &[&str] = &[
    "let", "fn", "pure", "struct", "if", "else", "while", "for", "in", "as", "as?", "return",
    "break", "continue", "and", "or", "not", "true", "false", "None",
];

fn keyword_completions() -> impl Iterator<Item = CompletionItem> {
    KEYWORDS.iter().map(|kw| CompletionItem {
        label: String::from(*kw),
        kind: Some(CompletionItemKind::KEYWORD),
        ..Default::default()
    })
}

/// Collect in-scope local variables from the last successfully analysed AST.
/// Uses lexical-scope visibility (enclosing scope + declared-before-use), so a
/// local declared in one function is not offered inside another.
fn local_completions(state: &DocumentState, position: Position) -> Vec<CompletionItem> {
    let Some(offset) = state.line_index.offset(&state.source, position) else {
        return Vec::new();
    };
    let Some(source_id) = state.ast.first().map(|e| e.span.source_id()) else {
        return Vec::new();
    };
    let scope = file_scope(source_id, state.source.len());

    let mut names: HashMap<String, Option<StaticType>> = HashMap::default();
    for decl in collect_declarations(&state.ast, scope) {
        // Functions and structs complete as functions, with their signatures.
        if matches!(decl.kind, DeclKind::Variable(_)) && is_visible(&decl, offset) {
            // Type is a best-effort hint from the name-keyed map (a shadowed name
            // may show the wrong type until the analyser resolution is exposed).
            let typ = state.variable_types.get(&decl.name).cloned();
            names.insert(decl.name, typ);
        }
    }

    names
        .into_iter()
        .map(|(name, typ)| CompletionItem {
            label: name,
            label_details: typ.as_ref().map(|t| CompletionItemLabelDetails {
                detail: None,
                description: Some(t.to_string()),
            }),
            kind: Some(CompletionItemKind::VARIABLE),
            ..Default::default()
        })
        .collect()
}

fn is_normal_ident(input: &str) -> bool {
    input
        .chars()
        .all(|c| c.is_alphanumeric() || c == '?' || c == '_')
}

/// The type of what's before the dot at `dot_offset`: of the expression
/// that ends there (`func(args).`, any expression), or else of the variable
/// named right before it (`ident.`), which works while the text doesn't parse.
pub(crate) fn receiver_type(state: &DocumentState, dot_offset: usize) -> Option<StaticType> {
    state
        .expression_types
        .get(&dot_offset)
        .cloned()
        .or_else(|| {
            let ident = identifier_before_dot(&state.source, dot_offset + 1)?;
            state.variable_types.get(ident).cloned()
        })
}

/// Scan backward from `offset` to find the byte position of the `.` trigger.
/// Returns the offset of the dot itself (i.e. the byte offset where the expression
/// before the dot ends, which is the key in `expression_types`).
fn find_dot_before(text: &[u8], offset: usize) -> Option<usize> {
    // Skip any whitespace between cursor and the dot
    let mut i = offset;
    while i > 0 && text[i - 1].is_ascii_whitespace() {
        i -= 1;
    }
    if i > 0 && text[i - 1] == b'.' {
        Some(i - 1)
    } else {
        None
    }
}

/// Given a byte offset (pointing at or just after the `.`), scan backward to find
/// the identifier immediately before the dot.
fn identifier_before_dot(text: &str, offset: usize) -> Option<&str> {
    let before = &text[..offset];
    let before = before.trim_end();
    let before = before.strip_suffix('.')?;
    let before = before.trim_end();

    let end = before.len();
    let start = before
        .char_indices()
        .rev()
        .take_while(|(_, c)| c.is_alphanumeric() || *c == '_' || *c == '?')
        .last()
        .map_or(end, |(i, _)| i);

    if start == end {
        return None;
    }

    Some(&before[start..end])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::LineIndex;

    fn functions() -> Vec<FunctionInfo> {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        FunctionInfo::collect(&interpreter)
    }

    /// The state after a successful analysis of `source`.
    fn analysed(source: &str) -> DocumentState {
        DocumentState::analysed(source)
    }

    /// The state after the user typed `typed` at the end of `source`: the
    /// source is updated but the analysis is still that of `source`.
    fn edited(source: &str, typed: &str) -> DocumentState {
        let mut state = analysed(source);
        state.source = format!("{source}{typed}");
        state.line_index = LineIndex::new(&state.source);
        state.analysis_matches_source = false;
        state
    }

    /// Complete at the end of the document.
    fn complete_at_end(state: &DocumentState, functions: &[FunctionInfo]) -> Vec<CompletionItem> {
        let end = state.line_index.position(&state.source, state.source.len());
        let CompletionResponse::Array(items) = complete(Some(state), end, functions) else {
            panic!("expected Array response");
        };
        items
    }

    fn has_label(items: &[CompletionItem], label: &str) -> bool {
        items.iter().any(|item| item.label == label)
    }

    #[test]
    fn identifier_before_dot_simple() {
        assert_eq!(identifier_before_dot("x.", 2), Some("x"));
    }

    #[test]
    fn identifier_before_dot_with_spaces() {
        assert_eq!(identifier_before_dot("x . ", 4), Some("x"));
    }

    #[test]
    fn identifier_before_dot_multiline() {
        // "let x = 5\nx." is 12 bytes; offset 12 is end of text (after the dot)
        assert_eq!(identifier_before_dot("let x = 5\nx.", 12), Some("x"));
    }

    #[test]
    fn identifier_before_dot_no_dot() {
        assert_eq!(identifier_before_dot("x", 1), None);
    }

    #[test]
    fn identifier_before_dot_nothing_before() {
        assert_eq!(identifier_before_dot(".", 1), None);
    }

    #[test]
    fn identifier_before_dot_underscore_and_question_mark() {
        assert_eq!(identifier_before_dot("is_empty?.", 10), Some("is_empty?"));
    }

    #[test]
    fn operators_are_not_offered_as_completions() {
        let items = complete_at_end(&DocumentState::from_source(String::new()), &functions());
        assert!(has_label(&items, "len"), "expected named functions");
        for operator in ["+", "==", "++"] {
            assert!(
                !has_label(&items, operator),
                "operator {operator} must not be offered"
            );
        }
    }

    #[test]
    fn dot_completion_offers_only_functions_that_accept_the_receiver() {
        let items = complete_at_end(&edited("let x = [1, 2, 3];", "\nx."), &functions());
        assert!(has_label(&items, "len"), "a list has a length");
        assert!(!has_label(&items, "abs"), "a list has no absolute value");
        assert!(!has_label(&items, "true"), "keywords are not members");
    }

    #[test]
    fn dot_completion_uses_the_last_analysis_while_the_edit_does_not_parse() {
        let state = edited("let x = 42;", "\nx.");
        assert!(!state.analysis_matches_source);

        let items = complete_at_end(&state, &functions());
        assert!(
            !has_label(&items, "true"),
            "should be dot completion, not general completion"
        );
        assert!(has_label(&items, "abs"), "an Int has an absolute value");
    }

    #[test]
    fn dot_completion_after_a_call_uses_its_return_type() {
        let items = complete_at_end(
            &edited("fn words() { [\"a\"] }\nwords()", "."),
            &functions(),
        );
        assert!(
            !has_label(&items, "true"),
            "should be dot completion, not general completion"
        );
        assert!(has_label(&items, "len"), "a list has a length");
    }

    /// A synthetic native function overload, so grouping tests don't depend on
    /// which overloads the stdlib happens to register.
    fn native(name: &str, params: &[StaticType], ret: StaticType) -> FunctionInfo {
        FunctionInfo {
            name: name.to_string(),
            static_type: StaticType::Function {
                parameters: Some(params.to_vec()),
                return_type: Box::new(ret),
            },
            documentation: None,
            intrinsic: None,
            is_field: false,
            declaration: None,
        }
    }

    fn parameter_details(items: &[CompletionItem], label: &str) -> Vec<String> {
        items
            .iter()
            .filter(|i| i.label == label)
            .map(|i| {
                i.label_details
                    .as_ref()
                    .and_then(|d| d.detail.clone())
                    .expect("function items carry a parameter detail")
            })
            .collect()
    }

    #[test]
    fn general_completion_groups_overloads_by_arity() {
        use StaticType::{Float, Int, Number};
        let overloads = vec![
            native("randf", &[], Float),
            native("randf", &[Int], Float),
            native("randf", &[Float], Float),
            native("randf", &[Number], Float),
            native("randf", &[Int, Int], Float),
            native("randf", &[Number, Number], Float),
        ];
        let items = complete_at_end(&DocumentState::from_source(String::new()), &overloads);

        // One item per arity; type permutations within an arity fold into a
        // union display (`Int`, `Float` and `Number` are sibling types).
        assert_eq!(
            parameter_details(&items, "randf"),
            vec![
                "()",
                "(Int | Float | Number)",
                "(Int | Number, Int | Number)"
            ]
        );
    }

    #[test]
    fn dot_completion_groups_only_overloads_that_accept_the_receiver() {
        use StaticType::{Float, Int, Number};
        let overloads = vec![
            native("randf", &[], Float),            // no receiver parameter
            native("randf", &[Int], Float),         // survives
            native("randf", &[Float], Float),       // Int is not a Float
            native("randf", &[Int, Number], Float), // survives, folds with next
            native("randf", &[Int, Int], Float),
        ];
        // Receiver is Int, so only overloads whose first parameter accepts Int
        // survive; the receiver is then omitted from the displayed signature.
        let items = complete_at_end(&edited("let x = 42;", "\nx."), &overloads);

        assert_eq!(
            parameter_details(&items, "randf"),
            vec!["()", "(Number | Int)"]
        );
    }

    #[test]
    fn merged_type_display_shows_a_common_supertype_or_a_union() {
        let list = StaticType::List(Box::new(StaticType::Int));
        let seq = StaticType::Iterable(Box::new(StaticType::Int));
        assert_eq!(merged_type_display(&[&list, &seq]), seq.to_string());
        assert_eq!(
            merged_type_display(&[&StaticType::Int, &StaticType::Float]),
            format!("{} | {}", StaticType::Int, StaticType::Float)
        );
    }

    #[test]
    fn general_completion_includes_keywords() {
        let items = complete_at_end(
            &DocumentState::from_source("let x = 42\n".to_string()),
            &functions(),
        );
        assert!(
            has_label(&items, "true"),
            "general completion should include keywords"
        );
        assert!(
            has_label(&items, "fn"),
            "general completion should include the `fn` keyword"
        );
    }

    #[test]
    fn general_completion_includes_in_scope_locals() {
        // The cursor is at the end, on the empty second line, where `greeting`
        // is in scope.
        let items = complete_at_end(&analysed("let greeting = \"hi\";\n"), &functions());
        assert!(
            items
                .iter()
                .any(|i| i.label == "greeting" && i.kind == Some(CompletionItemKind::VARIABLE)),
            "general completion should include the in-scope local `greeting`"
        );
    }

    #[test]
    fn locals_do_not_leak_across_functions() {
        // `foo` is local to `a`; completing inside `b` must not offer it.
        let state = analysed("fn a() { let foo = 1; }\nfn b() {\n\n}\n");

        // The blank line 2 is inside b's body.
        let response = complete(Some(&state), Position::new(2, 0), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            !has_label(&items, "foo"),
            "a local from another function must not be suggested"
        );
    }

    #[test]
    fn stale_analysis_suppresses_locals_but_keeps_keywords() {
        // Simulate a mid-edit buffer whose last parse failed: the AST is stale.
        let state = edited("let greeting = \"hi\";\n", "");

        let items = complete_at_end(&state, &functions());
        assert!(
            !has_label(&items, "greeting"),
            "stale AST must not contribute local suggestions"
        );
        assert!(
            has_label(&items, "fn"),
            "keywords should still be offered while mid-edit"
        );
    }

    mod declared {
        use crate::fixture::{completions, item};
        use ls_types::CompletionItemKind;

        const DOUBLE: &str = "fn double(x: Int) { x * 2 }\nlet n = 5;\nlet s = \"a\";\n";

        fn after_typing(before: &str, typed: &str) -> Vec<ls_types::CompletionItem> {
            completions(&[before, &format!("{before}{typed}$0")])
        }

        #[test]
        fn a_dot_offers_the_programs_functions_that_take_the_receiver() {
            let items = after_typing(DOUBLE, "n.");
            let double = item(&items, "double").expect("`double` after `n.`");
            assert_eq!(double.kind, Some(CompletionItemKind::FUNCTION));
            assert_eq!(double.detail.as_deref(), Some("double() -> Int"));

            assert!(item(&after_typing(DOUBLE, "s."), "double").is_none());
        }

        #[test]
        fn a_dot_offers_a_function_without_annotations_for_any_receiver() {
            let source = "fn twice(x) { [x, x] }\nlet s = \"a\";\n";
            assert!(item(&after_typing(source, "s."), "twice").is_some());
        }

        #[test]
        fn a_dot_after_a_struct_offers_its_fields() {
            let source = "struct Point { x: Int, label: String }\nlet p = Point(1, \"a\");\n";
            let items = after_typing(source, "p.");
            let x = item(&items, "x").expect("field `x` after `p.`");
            assert_eq!(x.kind, Some(CompletionItemKind::FIELD));
            assert_eq!(x.detail.as_deref(), Some("x: Int"));
            assert!(item(&items, "label").is_some());

            // Another struct's fields, or a list's, aren't offered.
            let source = format!("{source}let xs = [1];\n");
            assert!(item(&after_typing(&source, "xs."), "x").is_none());
        }

        #[test]
        fn without_a_dot_the_programs_functions_complete_as_functions() {
            let items = completions(&["fn words() { [\"a\"] }\nstruct Point { x: Int }\n$0"]);
            let words = item(&items, "words").expect("`words`");
            assert_eq!(words.kind, Some(CompletionItemKind::FUNCTION));
            assert_eq!(words.detail.as_deref(), Some("words() -> [String]"));
            let point = item(&items, "Point").expect("the constructor `Point`");
            assert_eq!(point.kind, Some(CompletionItemKind::FUNCTION));
            // A field is read after a dot, not called.
            assert!(item(&items, "x").is_none());
        }

        #[test]
        fn a_function_declared_after_the_cursor_is_not_offered() {
            let items = completions(&["let n = 5;\n$0\nfn later(x: Int) { x }\n"]);
            assert!(item(&items, "later").is_none());
            let items = completions(&["fn f() {\n  fn inner() { 1 }\n}\n$0"]);
            assert!(item(&items, "inner").is_none());
        }

        #[test]
        fn a_comprehensions_expression_offers_the_names_it_binds() {
            let items = completions(&["let xs = [1];\nlet ys = [1$0 for item in xs];\n"]);
            let item_ = item(&items, "item").expect("`item` in the comprehension");
            assert_eq!(item_.kind, Some(CompletionItemKind::VARIABLE));
            // Not in its own sequence.
            let items = completions(&["let xs = [1];\nlet ys = [1 for item in x$0s];\n"]);
            assert!(item(&items, "item").is_none());
            // Nor in a map comprehension's default, which is outside the loop.
            let items = completions(&["let m = %{:0$0, item: item for item in [1]};\n"]);
            assert!(item(&items, "item").is_none());
            let items = completions(&["let m = %{:0, item: item$0 for item in [1]};\n"]);
            assert!(item(&items, "item").is_some());
        }

        #[test]
        fn a_dot_typed_at_the_end_of_a_block_offers_the_blocks_functions() {
            let before = "fn main() {\n  fn double(x: Int) { x * 2 }\n  let n = 5;\n}\n";
            let after = "fn main() {\n  fn double(x: Int) { x * 2 }\n  let n = 5;\n  n.$0}\n";
            assert!(item(&completions(&[before, after]), "double").is_some());
        }

        #[test]
        fn a_dot_after_text_was_deleted_earlier_offers_the_files_functions() {
            let before =
                "let unused = [1, 2, 3, 4, 5, 6];\nfn double(x: Int) { x * 2 }\nlet n = 5;\n";
            // Deleting the list's end breaks the parse and shortens the text
            // by more than was typed, so the cursor lands past its new length.
            let after = "let unused = [1, 2, 3;\nfn double(x: Int) { x * 2 }\nlet n = 5;\nn.$0";
            assert!(item(&completions(&[before, after]), "double").is_some());
        }
    }
}
