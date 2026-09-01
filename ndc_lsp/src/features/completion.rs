use ahash::AHashMap;
use ndc_core::StaticType;
use ndc_interpreter::Interpreter;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionResponse,
    Documentation, MarkupContent, MarkupKind, Position,
};

use crate::scope_resolve::{collect_declarations, file_scope, is_visible};
use crate::state::DocumentState;

/// A `Send` snapshot of a registered native function, built once at startup so
/// completion (and hover) never have to rebuild the interpreter per request.
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub static_type: StaticType,
    pub documentation: Option<String>,
}

impl FunctionInfo {
    /// Snapshot every function registered in an interpreter.
    pub fn collect(interpreter: &Interpreter) -> Vec<Self> {
        interpreter
            .functions()
            .map(|fun| Self {
                name: fun.name.clone(),
                static_type: fun.static_type.clone(),
                documentation: fun.documentation.clone(),
            })
            .collect()
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
        // Try expression type map first (handles `func(args).` and any expression),
        // then fall back to variable name lookup for simple `ident.` cases.
        s.expression_types.get(&dot_offset).cloned().or_else(|| {
            let ident = identifier_before_dot(&s.source, offset)?;
            s.variable_types.get(ident).cloned()
        })
    });

    let is_dot = receiver_type.is_some();

    let groups = group_overloads(functions, receiver_type.as_ref());
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

/// Group registered functions by (name, arity), keeping registration order and
/// deduplicating the types seen at each parameter position. When
/// `receiver_type` is given (dot completion), overloads whose first parameter
/// doesn't accept the receiver are dropped before grouping.
fn group_overloads<'a>(
    functions: &'a [FunctionInfo],
    receiver_type: Option<&StaticType>,
) -> Vec<OverloadGroup<'a>> {
    let mut groups: Vec<OverloadGroup<'a>> = Vec::new();
    let mut index: AHashMap<(&'a str, Option<usize>), usize> = AHashMap::new();

    for fun in functions {
        if !is_normal_ident(&fun.name) {
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
            && (params.is_empty() || !recv.is_subtype(&params[0]))
        {
            continue;
        }

        let key = (fun.name.as_str(), parameters.as_ref().map(Vec::len));
        let slot = *index.entry(key).or_insert_with(|| {
            groups.push(OverloadGroup {
                name: &fun.name,
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

/// `[Int]` -> `Int`; `[List<Int>, Sequence<Int>]` -> `Sequence<Int>` (a
/// candidate subsuming all others wins); `[Int, Float, Number]` ->
/// `Int | Float | Number` (no candidate subsumes the others).
fn merged_type_display(candidates: &[&StaticType]) -> String {
    if let Some(supertype) = candidates
        .iter()
        .find(|t| candidates.iter().all(|c| c.is_subtype(t)))
    {
        return supertype.to_string();
    }
    candidates
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" | ")
}

/// Language keywords offered in general (non-dot) completion.
const KEYWORDS: &[&str] = &[
    "let", "fn", "struct", "if", "else", "while", "for", "in", "return", "break", "continue",
    "true", "false",
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

    let mut names: AHashMap<String, Option<StaticType>> = AHashMap::new();
    for decl in collect_declarations(&state.ast, scope) {
        if is_visible(&decl, offset) {
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

    fn functions() -> Vec<FunctionInfo> {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        FunctionInfo::collect(&interpreter)
    }

    /// Build a document state whose `variable_types` / `expression_types` are set
    /// directly, simulating the cached-after-analysis state used by completion.
    fn state_with(
        source: &str,
        variable_types: AHashMap<String, StaticType>,
        expression_types: AHashMap<usize, StaticType>,
    ) -> DocumentState {
        let mut state = DocumentState::from_source(source.to_string());
        state.variable_types = variable_types;
        state.expression_types = expression_types;
        state
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
    fn is_normal_ident_accepts_alphanumeric() {
        assert!(is_normal_ident("foo"));
        assert!(is_normal_ident("foo_bar"));
        assert!(is_normal_ident("empty?"));
    }

    #[test]
    fn is_normal_ident_rejects_operators() {
        assert!(!is_normal_ident("+"));
        assert!(!is_normal_ident("=="));
    }

    #[test]
    fn dot_completion_filters_by_receiver_type() {
        // Simulate: user typed `let x = [1,2,3]` then `x.`
        let state = state_with(
            "let x = [1,2,3]\nx.",
            AHashMap::from([("x".to_string(), StaticType::List(Box::new(StaticType::Int)))]),
            AHashMap::new(),
        );

        // Cursor is after the dot: line 1, character 2
        let response = complete(Some(&state), Position::new(1, 2), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        // Should not contain keyword completions (true/false) in dot mode
        assert!(
            !items.iter().any(|i| i.label == "true"),
            "dot-completion should not include keywords"
        );

        // Should contain list-compatible functions like `len`
        assert!(
            items.iter().any(|i| i.label == "len"),
            "dot-completion on list should include `len`"
        );
    }

    #[test]
    fn dot_completion_works_with_preserved_types_after_parse_failure() {
        // Source has been updated to contain the dot, but variable_types are
        // preserved from a previous successful analysis.
        let state = state_with(
            "let x = 42\nx.",
            AHashMap::from([("x".to_string(), StaticType::Int)]),
            AHashMap::new(),
        );

        let response = complete(Some(&state), Position::new(1, 2), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            !items.iter().any(|i| i.label == "true"),
            "should be dot-completion, not general"
        );
        assert!(
            items.iter().any(|i| i.label == "abs"),
            "dot-completion on Int should include `abs`"
        );
    }

    #[test]
    fn dot_completion_on_call_expression_via_expression_types() {
        // Simulates `read_file("foo").` where the expression type map knows
        // that the call expression `read_file("foo")` returns String.
        let source = r#"read_file("foo")."#;
        // The call expression spans bytes 0..16, so its end offset is 16.
        let state = state_with(
            source,
            AHashMap::new(),
            AHashMap::from([(16, StaticType::String)]),
        );

        // Cursor is at end: line 0, character 17 (after the dot)
        let response = complete(Some(&state), Position::new(0, 17), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            !items.iter().any(|i| i.label == "true"),
            "should be dot-completion, not general"
        );
        assert!(
            items.iter().any(|i| i.label == "len"),
            "dot-completion on String should include `len`"
        );
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
        let state = state_with("", AHashMap::new(), AHashMap::new());

        let response = complete(Some(&state), Position::new(0, 0), &overloads);
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        // One item per arity; type permutations within an arity fold into the
        // common supertype (`Number` subsumes `Int` and `Float`).
        assert_eq!(
            parameter_details(&items, "randf"),
            vec!["()", "(Number)", "(Number, Number)"]
        );
    }

    #[test]
    fn dot_completion_groups_surviving_overloads() {
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
        let state = state_with(
            "let x = 42\nx.",
            AHashMap::from([("x".to_string(), StaticType::Int)]),
            AHashMap::new(),
        );

        let response = complete(Some(&state), Position::new(1, 2), &overloads);
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert_eq!(parameter_details(&items, "randf"), vec!["()", "(Number)"]);
    }

    #[test]
    fn merged_type_display_prefers_common_supertype() {
        let list = StaticType::List(Box::new(StaticType::Int));
        let seq = StaticType::Sequence(Box::new(StaticType::Int));
        assert_eq!(merged_type_display(&[&list, &seq]), seq.to_string());
        assert_eq!(
            merged_type_display(&[&StaticType::Int, &StaticType::Float]),
            format!("{} | {}", StaticType::Int, StaticType::Float)
        );
    }

    #[test]
    fn general_completion_includes_keywords() {
        let state = state_with(
            "let x = 42\n",
            AHashMap::from([("x".to_string(), StaticType::Int)]),
            AHashMap::new(),
        );

        // No dot — general completion
        let response = complete(Some(&state), Position::new(1, 0), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            items.iter().any(|i| i.label == "true"),
            "general completion should include keywords"
        );
        assert!(
            items.iter().any(|i| i.label == "fn"),
            "general completion should include the `fn` keyword"
        );
    }

    #[test]
    fn general_completion_includes_in_scope_locals() {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let source = "let greeting = \"hi\";\n";
        let (ast, analysis) = interpreter.analyse_str(source).expect("analysis succeeds");
        let state = DocumentState::from_analysis(source.to_string(), ast, analysis);

        // Cursor on the (empty) second line — `greeting` is in scope.
        let response = complete(Some(&state), Position::new(1, 0), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            items
                .iter()
                .any(|i| i.label == "greeting" && i.kind == Some(CompletionItemKind::VARIABLE)),
            "general completion should include the in-scope local `greeting`"
        );
    }

    #[test]
    fn locals_do_not_leak_across_functions() {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        // `foo` is local to `a`; completing inside `b` must not offer it.
        let source = "fn a() { let foo = 1; }\nfn b() {\n\n}\n";
        let (ast, analysis) = interpreter.analyse_str(source).expect("analysis succeeds");
        let state = DocumentState::from_analysis(source.to_string(), ast, analysis);

        // The blank line 2 is inside b's body.
        let response = complete(Some(&state), Position::new(2, 0), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            !items.iter().any(|i| i.label == "foo"),
            "a local from another function must not be suggested"
        );
    }

    #[test]
    fn stale_analysis_suppresses_locals_but_keeps_keywords() {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let source = "let greeting = \"hi\";\n";
        let (ast, analysis) = interpreter.analyse_str(source).expect("analysis succeeds");
        let mut state = DocumentState::from_analysis(source.to_string(), ast, analysis);
        // Simulate a mid-edit buffer whose last parse failed: the AST is stale.
        state.analysis_matches_source = false;

        let response = complete(Some(&state), Position::new(1, 0), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            !items.iter().any(|i| i.label == "greeting"),
            "stale AST must not contribute local suggestions"
        );
        assert!(
            items.iter().any(|i| i.label == "fn"),
            "keywords should still be offered while mid-edit"
        );
    }
}
