//! Find references, document highlight and rename: every place a name means
//! the same declaration as the name under the cursor. A program is one file,
//! so references and highlights are the same places shown two ways.

use std::collections::BTreeSet;
use std::panic::{self, AssertUnwindSafe};

use ls_types::{Diagnostic, Position, Range, TextEdit};
use ndc_core::collections::HashMap;
use ndc_lexer::{Lexer, SourceId, Span, Token};

use crate::document::{self, Analysed, Configure};
use crate::features::completion::FunctionInfo;
use crate::features::definition::{Meanings, definitions_at};
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

/// Why a rename of a text that isn't analysed can't be done.
pub const NOT_ANALYSED: &str =
    "the program has changed since it was last analysed, or doesn't parse";

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
    occurrences(state, &Meanings::new(state), &targets)
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
    occurrences(state, &Meanings::new(state), &targets)
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
/// that means the same declarations, to `new_name`; `Ok(None)` when the
/// cursor isn't on a name. Fails rather than change what a name in the
/// program means: when `new_name` isn't a name or is a built-in's, when it
/// would hide another declaration or be hidden by one, and in the end
/// whenever analysing the renamed program finds a name that means something
/// else than it did, or an error the program didn't have.
pub fn rename(
    state: &DocumentState,
    configure: Configure,
    builtins: &[FunctionInfo],
    position: Position,
    new_name: &str,
) -> Result<Option<Vec<TextEdit>>, String> {
    let Some(offset) = state.line_index.offset(&state.source, position) else {
        return Ok(None);
    };
    let Some((_, targets)) = definitions_at(state, offset) else {
        return Ok(None);
    };
    renameable(state, &targets)?;
    if !is_name(new_name) {
        return Err(format!("`{new_name}` isn't a name"));
    }
    let meanings = Meanings::new(state);
    let targets = together(state, &meanings, targets);
    // A function, a struct's constructor or a field's getter joins the
    // built-ins of its name as a candidate for a call, which can then pick
    // the built-in, and the analyser has had trouble with programs that do
    // that. A variable can have a built-in's name (`let sum = 0`).
    let declares_function = state
        .names
        .iter()
        .any(|name| matches!(name.refers, Refers::Declaration(d) if targets.contains(&d)));
    if declares_function && builtins.iter().any(|function| function.name == new_name) {
        return Err(format!("`{new_name}` is the name of a built-in"));
    }
    let occurrences = occurrences(state, &meanings, &targets);
    check_conflicts(state, &targets, &occurrences, new_name)?;
    check_renamed(state, &meanings, configure, &occurrences, new_name)?;

    Ok(Some(
        occurrences
            .iter()
            .map(|name| TextEdit {
                range: state.line_index.range(&state.source, name.span),
                new_text: new_name.to_string(),
            })
            .collect(),
    ))
}

/// What the name under the cursor refers to, if it refers to something the
/// program declares.
fn targets_at(state: &DocumentState, position: Position) -> Option<Vec<Definition>> {
    let offset = state.line_index.offset(&state.source, position)?;
    let (_, targets) = definitions_at(state, offset)?;
    (!targets.is_empty()).then_some(targets)
}

/// Every name that means one of `targets`, in the order they're written.
fn occurrences(
    state: &DocumentState,
    meanings: &Meanings<'_>,
    targets: &[Definition],
) -> Vec<Name> {
    state
        .names
        .iter()
        .filter(|name| meanings.of(name).iter().any(|d| targets.contains(d)))
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
fn together(
    state: &DocumentState,
    meanings: &Meanings<'_>,
    mut targets: Vec<Definition>,
) -> Vec<Definition> {
    loop {
        let before = targets.len();
        for name in state.names.iter() {
            let means = meanings.of(name);
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

/// Why renaming `targets` to `new_name` would change what a name means, as
/// far as the scopes of the program's own declarations tell. Quick, and says
/// what clashes; [`check_renamed`] has the last word.
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

/// Analyses the program with `occurrences` renamed to `new_name`, and fails
/// unless every name in it means the same declarations as before, the
/// renamed ones the renamed declarations, and it has no error the program
/// doesn't. That covers what [`check_conflicts`] can't see: overloads, the
/// built-ins and a struct's field getters joining a call's candidates.
fn check_renamed(
    state: &DocumentState,
    meanings: &Meanings<'_>,
    configure: Configure,
    occurrences: &[Name],
    new_name: &str,
) -> Result<(), String> {
    let changes = || {
        Err(format!(
            "renaming to `{new_name}` would change what the program means"
        ))
    };

    let mut renamed = state.source.clone();
    for name in occurrences.iter().rev() {
        renamed.replace_range(name.span.offset()..name.span.end(), new_name);
    }
    // Where an offset in the program is in the renamed program.
    let moved = |offset: usize| -> usize {
        let before = occurrences
            .iter()
            .filter(|name| name.span.offset() < offset);
        before.fold(offset, |offset, name| {
            offset + new_name.len() - (name.span.end() - name.span.offset())
        })
    };

    let (diagnostics, _) = analyse(configure, &state.source)?;
    let (renamed_diagnostics, analysed) = analyse(configure, &renamed)?;
    if !no_new_errors(&diagnostics, &renamed_diagnostics) {
        return changes();
    }
    let Some(analysed) = analysed else {
        return changes();
    };
    let mut after = DocumentState::from_source(renamed);
    after.set_analysis(analysed);
    let after_meanings = Meanings::new(&after);

    let declared_at = |state: &DocumentState, definitions: Vec<Definition>| -> BTreeSet<usize> {
        definitions
            .into_iter()
            .filter_map(|definition| state.names.declaration(definition))
            .map(|span| span.offset())
            .collect()
    };
    for name in state.names.iter() {
        let offset = moved(name.span.offset());
        let Some(renamed_name) = after.names.at(offset).filter(|n| n.span.offset() == offset)
        else {
            return changes();
        };
        let before: BTreeSet<usize> = declared_at(state, meanings.of(name))
            .into_iter()
            .map(&moved)
            .collect();
        let now = declared_at(&after, after_meanings.of(renamed_name));
        if before != now {
            return changes();
        }
    }
    Ok(())
}

/// Analyses `text`, turning a panic of the analyser into a refusal.
fn analyse(
    configure: Configure,
    text: &str,
) -> Result<(Vec<Diagnostic>, Option<Analysed>), String> {
    panic::catch_unwind(AssertUnwindSafe(|| document::analyse(configure, text)))
        .map_err(|_panic| "the analyser failed on the renamed program".to_string())
}

/// Whether every error in `after` is one of `before`'s, by message.
fn no_new_errors(before: &[Diagnostic], after: &[Diagnostic]) -> bool {
    let mut counts: HashMap<&str, usize> = HashMap::default();
    for diagnostic in before {
        *counts.entry(diagnostic.message.as_str()).or_default() += 1;
    }
    after.iter().all(|diagnostic| {
        counts
            .get_mut(diagnostic.message.as_str())
            .is_some_and(|count| match count.checked_sub(1) {
                Some(left) => {
                    *count = left;
                    true
                }
                None => false,
            })
    })
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
        let edits = document.rename(position, new_name)?.expect("on a name");
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

    #[test]
    fn a_field_whose_getter_would_take_a_call_over_is_not_renamed() {
        // The getter `print(P)` would win `print(p)`.
        assert!(renamed("struct P { x$0: Int }\nlet p = P(1);\nprint(p);", "print").is_err());
        // The getter `y(P)` would win `y(p)` over the function `y`.
        let text = "struct P { x$0: Int }\nlet p = P(1);\nfn y(q) { \"fn\" }\nprint(y(p));";
        assert!(renamed(text, "y").is_err());
    }

    #[test]
    fn a_built_ins_name_is_not_taken() {
        let text = "fn count$0(m: %{Int: Int}) => 1;\nprint(count(%{: 1}));";
        assert_eq!(
            renamed(text, "len"),
            Err("`len` is the name of a built-in".to_string())
        );
    }

    #[test]
    fn the_function_an_augmented_assignment_calls_is_one_of_its_names() {
        let text = "fn multiply$0(a, b) { a * b }\nlet x = 5;\nx multiply= 10;\nprint(x);";
        let (document, position, plain) = at(text);
        let found = located(&plain, &document.references(position, false));
        assert_eq!(found, [(2, "multiply".into())]);
        assert_eq!(
            renamed(text, "times").as_deref(),
            Ok("fn times(a, b) { a * b }\nlet x = 5;\nx times= 10;\nprint(x);")
        );
    }

    #[test]
    fn a_rename_that_swaps_one_error_for_another_is_refused() {
        // The program has an error already; the renamed one has another.
        let text = "fn f<T$0>(a: T) { cast::<Int>(2.5) }\nprint(f(1.5));";
        assert!(renamed(text, "Int").is_err());
    }

    #[test]
    fn renaming_where_there_is_no_name_does_nothing() {
        let (document, position, _) = at("let x = 1;$0\n");
        assert_eq!(document.rename(position, "y"), Ok(None));
        assert_eq!(document.prepare_rename(position), Ok(None));
    }
}
