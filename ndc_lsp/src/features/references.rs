//! Find references, document highlight and rename: every place a name means
//! the same declaration as the name under the cursor. A program is one file,
//! so references and highlights are the same places shown two ways.

use ls_types::{Position, Range, TextEdit};
use ndc_lexer::{Lexer, SourceId, Span, Token};

use crate::document::{self, Configure};
use crate::features::definition::{definitions_at, meaning};
use crate::names::{Name, Refers};
use crate::resolved::Definition;
use crate::scope_resolve::{DeclKind, collect_declarations, file_scope, is_visible};
use crate::state::DocumentState;

/// One place a name is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Occurrence {
    pub range: Range,
    /// Whether it declares or assigns rather than reads.
    pub write: bool,
}

/// Every place the name under the cursor's declarations are named, in the
/// order they're written, with the declarations themselves only if
/// `include_declaration`.
pub fn references(
    state: &DocumentState,
    position: Position,
    include_declaration: bool,
) -> Vec<Range> {
    let Some(targets) = targets_at(state, position) else {
        return Vec::new();
    };
    occurrences(state, &targets)
        .into_iter()
        .filter(|name| include_declaration || !declares(name, &targets))
        .map(|name| state.line_index.range(&state.source, name.span))
        .collect()
}

/// Every place the name under the cursor's declarations are named,
/// declarations included, each as a read or a write.
pub fn highlights(state: &DocumentState, position: Position) -> Vec<Occurrence> {
    let Some(targets) = targets_at(state, position) else {
        return Vec::new();
    };
    occurrences(state, &targets)
        .into_iter()
        .map(|name| Occurrence {
            range: state.line_index.range(&state.source, name.span),
            write: name.write,
        })
        .collect()
}

/// The range of the name under the cursor, if it can be renamed: one the
/// program declares, in a document whose names all resolved. `Ok(None)` when
/// the cursor isn't on a name.
pub fn prepare_rename(state: &DocumentState, position: Position) -> Result<Option<Range>, String> {
    let Some(offset) = state.line_index.offset(&state.source, position) else {
        return Ok(None);
    };
    let Some((name, targets)) = definitions_at(state, offset) else {
        return Ok(None);
    };
    renameable(state, &targets)?;
    Ok(Some(state.line_index.range(&state.source, name.span)))
}

/// The edits that rename the name under the cursor, and every other name
/// that means the same declarations, to `new_name`. Fails rather than change
/// what a name in the program means: when `new_name` isn't a name, when it
/// would hide another declaration or be hidden by one, and when the renamed
/// program has errors the program doesn't.
pub fn rename(
    state: &DocumentState,
    configure: Configure,
    position: Position,
    new_name: &str,
) -> Result<Vec<TextEdit>, String> {
    let Some(offset) = state.line_index.offset(&state.source, position) else {
        return Ok(Vec::new());
    };
    let Some((_, targets)) = definitions_at(state, offset) else {
        return Ok(Vec::new());
    };
    renameable(state, &targets)?;
    if !is_name(new_name) {
        return Err(format!("`{new_name}` isn't a name"));
    }
    let targets = together(state, targets);
    let occurrences = occurrences(state, &targets);
    check_conflicts(state, &targets, &occurrences, new_name)?;

    let edits: Vec<TextEdit> = occurrences
        .iter()
        .map(|name| TextEdit {
            range: state.line_index.range(&state.source, name.span),
            new_text: new_name.to_string(),
        })
        .collect();

    // A last check that nothing the conflict checks missed broke: the
    // renamed program has no errors the program doesn't.
    let mut renamed = state.source.clone();
    for name in occurrences.iter().rev() {
        renamed.replace_range(name.span.offset()..name.span.end(), new_name);
    }
    let before = document::analyse(configure, &state.source).0.len();
    let after = document::analyse(configure, &renamed).0.len();
    if after > before {
        return Err(format!(
            "renaming to `{new_name}` would change what the program means"
        ));
    }
    Ok(edits)
}

/// What the name under the cursor refers to, if it refers to something the
/// program declares.
fn targets_at(state: &DocumentState, position: Position) -> Option<Vec<Definition>> {
    let offset = state.line_index.offset(&state.source, position)?;
    let (_, targets) = definitions_at(state, offset)?;
    (!targets.is_empty()).then_some(targets)
}

/// Every name that means one of `targets`, in the order they're written.
fn occurrences(state: &DocumentState, targets: &[Definition]) -> Vec<Name> {
    state
        .names
        .iter()
        .filter(|name| meaning(state, name).iter().any(|d| targets.contains(d)))
        .copied()
        .collect()
}

/// Whether `name` is one of `targets`' own names rather than a use.
fn declares(name: &Name, targets: &[Definition]) -> bool {
    match name.refers {
        Refers::Declaration(_) => true,
        // A binding pattern means itself.
        Refers::Node(node) => targets.contains(&Definition::Node(node)),
    }
}

/// Why `targets` can't be renamed, if they can't.
fn renameable(state: &DocumentState, targets: &[Definition]) -> Result<(), String> {
    if targets.is_empty() {
        return Err("only names the program declares can be renamed".to_string());
    }
    // A name the resolver didn't get to may mean the same declaration too.
    let unresolved = state.names.iter().any(|name| match name.refers {
        Refers::Node(node) => state.resolved.definitions_of(node).is_none(),
        Refers::Declaration(_) => false,
    });
    if unresolved {
        return Err("fix the unknown names in the program first".to_string());
    }
    Ok(())
}

/// `targets`, with every declaration a name means along with one of them:
/// a call in a generic function that picks either of two overloads makes
/// renaming one rename both.
fn together(state: &DocumentState, mut targets: Vec<Definition>) -> Vec<Definition> {
    loop {
        let before = targets.len();
        for name in state.names.iter() {
            let means = meaning(state, name);
            if means.iter().any(|d| targets.contains(d)) {
                for definition in means {
                    if !targets.contains(&definition) {
                        targets.push(definition);
                    }
                }
            }
        }
        if targets.len() == before {
            return targets;
        }
    }
}

/// Whether `text` is a name: one identifier and nothing else.
fn is_name(text: &str) -> bool {
    let mut tokens = Lexer::new(text, SourceId::SYNTHETIC);
    matches!(
        (tokens.next(), tokens.next()),
        (Some(Ok(token)), None) if token.token == Token::Identifier(text.to_string())
    )
}

/// Why renaming `targets` to `new_name` would change what a name means.
fn check_conflicts(
    state: &DocumentState,
    targets: &[Definition],
    occurrences: &[Name],
    new_name: &str,
) -> Result<(), String> {
    let conflict = || {
        Err(format!(
            "`{new_name}` is already declared where it would clash"
        ))
    };
    let text = |span: Span| state.source.get(span.offset()..span.end()).unwrap_or("");

    // A struct can't have two fields of one name.
    for target in targets {
        if let Definition::Field { declaration, .. } = *target {
            let mut index = 0;
            while let Some(span) = state
                .names
                .declaration(Definition::Field { declaration, index })
            {
                if text(span) == new_name {
                    return conflict();
                }
                index += 1;
            }
        }
    }

    let Some(first) = state.ast.first() else {
        return Ok(());
    };
    let decls = collect_declarations(
        &state.ast,
        file_scope(first.span.source_id(), state.source.len()),
    );
    // Hidden: another declaration of `new_name` is visible where a renamed
    // name is written.
    for name in occurrences {
        if decls
            .iter()
            .any(|decl| decl.name == new_name && is_visible(decl, name.span.offset()))
        {
            return conflict();
        }
    }
    // Hiding: a renamed declaration is visible where `new_name` already means
    // something else.
    let renamed = decls.iter().filter(|decl| {
        let (DeclKind::Variable(id) | DeclKind::Function(id) | DeclKind::Struct(id)) = decl.kind;
        targets.contains(&Definition::Node(id))
    });
    for decl in renamed {
        let hides = state
            .names
            .iter()
            .any(|name| text(name.span) == new_name && is_visible(decl, name.span.offset()));
        if hides {
            return conflict();
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use ls_types::DocumentHighlightKind;

    use crate::fixture::document;
    use crate::util::LineIndex;

    /// The text each of `ranges` covers in `text`, with the line it's on.
    fn located(text: &str, ranges: &[ls_types::Range]) -> Vec<(u32, String)> {
        let index = LineIndex::new(text);
        ranges
            .iter()
            .map(|range| {
                let start = index.offset(text, range.start).expect("start");
                let end = index.offset(text, range.end).expect("end");
                (range.start.line, text[start..end].to_string())
            })
            .collect()
    }

    /// `text` with `$0` marking the cursor, and that text without it.
    fn at(text: &str) -> (crate::Document, ls_types::Position, String) {
        let (document, position) = document(&[text]);
        (document, position, text.replacen("$0", "", 1))
    }

    fn renamed(text: &str, new_name: &str) -> Result<String, String> {
        let (document, position, text) = at(text);
        let edits = document.rename(position, new_name)?;
        let index = LineIndex::new(&text);
        let mut edits: Vec<_> = edits
            .into_iter()
            .map(|edit| {
                let start = index.offset(&text, edit.range.start).expect("start");
                let end = index.offset(&text, edit.range.end).expect("end");
                (start, end, edit.new_text)
            })
            .collect();
        edits.sort_by_key(|&(start, ..)| std::cmp::Reverse(start));
        let mut text = text;
        for (start, end, new_text) in edits {
            text.replace_range(start..end, &new_text);
        }
        Ok(text)
    }

    #[test]
    fn references_are_every_use_of_the_same_declaration() {
        let (document, position, text) =
            at("let total = 0;\nfor x in [1] { total += x; }\nprint(tot$0al);");
        let with = located(&text, &document.references(position, true));
        assert_eq!(
            with,
            [
                (0, "total".into()),
                (1, "total".into()),
                (2, "total".into())
            ]
        );
        let without = located(&text, &document.references(position, false));
        assert_eq!(without, [(1, "total".into()), (2, "total".into())]);
    }

    #[test]
    fn a_shadowing_declaration_is_a_different_name() {
        let (document, position, text) = at("let x$0 = 1;\n{ let x = 2; print(x); }\nprint(x);");
        let found = located(&text, &document.references(position, true));
        assert_eq!(found, [(0, "x".into()), (2, "x".into())]);
    }

    #[test]
    fn references_of_an_overload_are_the_calls_that_pick_it() {
        let (document, position, text) =
            at("fn g$0(x: Int) { 1 }\nfn g(x: String) { 2 }\ng(1);\ng(\"a\");\ng(2);");
        let found = located(&text, &document.references(position, false));
        assert_eq!(found, [(2, "g".into()), (4, "g".into())]);
    }

    #[test]
    fn a_fields_references_are_its_reads_and_writes() {
        let (document, position, text) =
            at("struct P { x$0: Int }\nlet p = P(1);\np.x;\np.x = 2;\np.x += 1;");
        let found = located(&text, &document.references(position, true));
        assert_eq!(
            found,
            [
                (0, "x".into()),
                (2, "x".into()),
                (3, "x".into()),
                (4, "x".into())
            ]
        );
    }

    #[test]
    fn highlights_say_which_names_write() {
        let (document, position, _) = at("let total = 0;\ntotal += 1;\nprint(total$0);");
        let kinds: Vec<_> = document
            .highlights(position)
            .into_iter()
            .map(|highlight| (highlight.range.start.line, highlight.kind))
            .collect();
        assert_eq!(
            kinds,
            [
                (0, Some(DocumentHighlightKind::WRITE)),
                (1, Some(DocumentHighlightKind::WRITE)),
                (2, Some(DocumentHighlightKind::READ))
            ]
        );
        // A built-in highlights nothing.
        let (document, position, _) = at("pri$0nt(1);");
        assert!(document.highlights(position).is_empty());
    }

    #[test]
    fn renaming_a_variable_renames_its_declaration_and_uses() {
        assert_eq!(
            renamed("let total = 0;\ntotal += 1;\nprint(tot$0al);", "sum").as_deref(),
            Ok("let sum = 0;\nsum += 1;\nprint(sum);")
        );
    }

    #[test]
    fn renaming_a_struct_renames_its_constructor_and_type_names() {
        assert_eq!(
            renamed(
                "struct Po$0int { x: Int }\nlet p: Point = Point(1);\nfn f(q: [Point]) { q }",
                "Spot"
            )
            .as_deref(),
            Ok("struct Spot { x: Int }\nlet p: Spot = Spot(1);\nfn f(q: [Spot]) { q }")
        );
    }

    #[test]
    fn renaming_a_field_renames_its_reads_and_writes() {
        assert_eq!(
            renamed("struct P { x: Int }\nlet p = P(1);\np.x$0 = p.x + 1;", "y").as_deref(),
            Ok("struct P { y: Int }\nlet p = P(1);\np.y = p.y + 1;")
        );
    }

    #[test]
    fn overloads_a_generic_function_calls_alike_are_renamed_together() {
        assert_eq!(
            renamed(
                "fn g$0(x: Int) { 1 }\nfn g(x: String) { 2 }\nfn h(v) { g(v) }\nh(1);\nh(\"a\");",
                "k"
            )
            .as_deref(),
            Ok("fn k(x: Int) { 1 }\nfn k(x: String) { 2 }\nfn h(v) { k(v) }\nh(1);\nh(\"a\");")
        );
    }

    #[test]
    fn a_rename_that_would_change_what_a_name_means_is_refused() {
        // `x` would be hidden by `y`'s declaration where it's used.
        assert!(renamed("let x$0 = 1;\nlet y = 2;\nprint(x + y);", "y").is_err());
        // `x` would hide the outer `y` that the function reads.
        assert!(renamed("let y = 1;\nfn f() { let x$0 = 2; y }", "y").is_err());
        // A struct can't have two fields of one name.
        assert!(renamed("struct P { x$0: Int, y: Int }", "y").is_err());
    }

    #[test]
    fn only_a_name_the_program_declares_can_become_another_name() {
        let (document, position, _) = at("pri$0nt(1);");
        assert!(document.prepare_rename(position).is_err());
        assert!(renamed("let x$0 = 1;", "1abc").is_err());
        assert!(renamed("let x$0 = 1;", "fn").is_err());
        assert!(renamed("let x$0 = 1;", "a b").is_err());
        // A name the resolver didn't get to might be one of the renamed.
        assert!(renamed("let x$0 = 1;\nprint(missing, x);", "y").is_err());
    }

    #[test]
    fn prepare_rename_gives_the_name_under_the_cursor() {
        let (document, position, text) = at("let total = 0;\nprint(to$0tal);");
        let range = document
            .prepare_rename(position)
            .expect("renameable")
            .expect("on a name");
        assert_eq!(located(&text, &[range]), [(1, "total".into())]);
    }
}
