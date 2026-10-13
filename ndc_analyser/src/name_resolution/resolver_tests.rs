use super::{NameResolutionError, NameResolver};
use crate::symbols::{BindingKind, SymbolId};
use itertools::Itertools;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;

fn resolve_source(resolver: &mut NameResolver, source: &str) -> Result<(), NameResolutionError> {
    let tokens = Lexer::new(source, SourceId::new(0))
        .collect::<Result<Vec<_>, _>>()
        .expect("source lexes");
    let expressions = Parser::from_tokens(tokens).parse().expect("source parses");
    for expression in &expressions {
        resolver.resolve(expression)?;
    }
    Ok(())
}

/// Declared symbols in declaration order; the symbol table hands out ids sequentially.
fn declared<const N: usize>(resolver: &NameResolver) -> [SymbolId; N] {
    let symbols = resolver
        .declarations
        .values()
        .copied()
        .sorted()
        .collect_vec();
    symbols.try_into().unwrap_or_else(|symbols: Vec<_>| {
        panic!("expected {N} declarations, got {}", symbols.len())
    })
}

fn uses(resolver: &NameResolver, symbol: SymbolId) -> usize {
    resolver
        .references
        .values()
        .filter(|&&id| id == symbol)
        .count()
}

#[test]
fn initializer_reads_outer_binding_before_shadowing() {
    let mut resolver = NameResolver::new();
    resolve_source(&mut resolver, "let x = 1; { let x = x; x; } x;").unwrap();

    let [outer, inner] = declared(&resolver);
    assert_eq!(resolver.symbols.kind(outer), BindingKind::Plain);
    assert_eq!(resolver.symbols.kind(inner), BindingKind::Plain);
    assert_eq!(uses(&resolver, outer), 2);
    assert_eq!(uses(&resolver, inner), 1);
    assert_eq!(resolver.scopes.lookup("x", &resolver.symbols), Some(outer));
}

#[test]
fn self_reference_reports_the_initializer_location() {
    let mut resolver = NameResolver::new();
    let source = "let missing = missing;";
    let error = resolve_source(&mut resolver, source).unwrap_err();
    assert_eq!(
        error.to_string(),
        "identifier 'missing' has not been declared"
    );
    let NameResolutionError::UnknownName { name, span } = error else {
        panic!("expected an unknown name");
    };
    assert_eq!(name, "missing");
    assert_eq!(span.source_id(), SourceId::new(0));
    assert_eq!(&source[span.range()], "missing");
    assert_eq!(span.offset(), source.rfind("missing").unwrap());
    assert!(resolver.declarations.is_empty());
}

#[test]
fn destructuring_resolves_all_initializers_before_binding_names() {
    let mut resolver = NameResolver::new();
    resolve_source(
        &mut resolver,
        "let x = 1; let y = 2; let (x, (y, z)) = (y, (x, 3)); x; y; z;",
    )
    .unwrap();

    let symbols: [_; 5] = declared(&resolver);
    for symbol in symbols {
        assert_eq!(uses(&resolver, symbol), 1);
    }
    let [_, _, x, y, _] = symbols;
    assert_eq!(resolver.scopes.lookup("x", &resolver.symbols), Some(x));
    assert_eq!(resolver.scopes.lookup("y", &resolver.symbols), Some(y));
}

#[test]
fn assignment_reuses_declared_symbols() {
    let mut resolver = NameResolver::new();
    resolve_source(
        &mut resolver,
        "let x = 1; let y = 2; (x, y) = (y, x); x = 3;",
    )
    .unwrap();

    // Assignment targets refer to the existing symbols instead of declaring new ones.
    let [x, y] = declared(&resolver);
    assert_eq!(uses(&resolver, x), 3);
    assert_eq!(uses(&resolver, y), 2);
}

#[test]
fn member_and_index_targets_resolve_receivers_without_declaring_names() {
    let mut resolver = NameResolver::new();
    // This supplied name stands for a value in the surrounding environment.
    let object = resolver
        .symbols
        .declare("object".to_string(), BindingKind::Plain);
    resolver.scopes.bind(object);
    resolve_source(
        &mut resolver,
        "let i = 0; object.field = i; object[i] = i; object.field;",
    )
    .unwrap();

    let [i] = declared(&resolver);
    assert_eq!(uses(&resolver, i), 3);
    assert_eq!(uses(&resolver, object), 3);
    assert_eq!(resolver.scopes.lookup("field", &resolver.symbols), None);
    assert_eq!(
        resolver.scopes.lookup("object", &resolver.symbols),
        Some(object)
    );
}

#[test]
fn traversal_reports_unknown_names_in_expression_children() {
    let sources = [
        "(missing);",
        "true and missing;",
        "missing or false;",
        "missing as Int;",
        "missing(1);",
        "callee(missing);",
        "1 + missing;",
        "missing.field;",
        "[1, missing];",
        "(1, missing);",
        "%{missing: 1};",
        "%{1: missing};",
        "%{:missing};",
        "%{missing};",
        "if missing { 1; };",
        "if true { missing; };",
        "if true { 1; } else { missing; };",
        "while missing { 1; };",
        "while true { missing; };",
        "return missing;",
        "missing..;",
        "let xs = []; xs[0..missing] = 1;",
        "1..=missing;",
        "missing = 1;",
        "let x = 1; x = missing;",
        "let xs = []; xs[missing] = 1;",
        "missing[0] = 1;",
        "missing.field = 1;",
    ];
    for source in sources {
        let mut resolver = NameResolver::new();
        for name in ["callee", "+"] {
            let symbol = resolver
                .symbols
                .declare(name.to_string(), BindingKind::Function);
            resolver.scopes.bind(symbol);
        }
        let error = resolve_source(&mut resolver, source).expect_err(source);
        let NameResolutionError::UnknownName { name, span } = error else {
            panic!("expected an unknown name in {source:?}, got {error:?}");
        };
        assert_eq!(name, "missing", "{source}");
        assert!(source[span.range()].contains("missing"), "{source}");
    }
}

#[test]
fn errors_restore_visibility_after_nested_blocks() {
    let mut resolver = NameResolver::new();
    resolve_source(
        &mut resolver,
        "let x = 1; { let x = 2; { let local = 3; missing; } }",
    )
    .unwrap_err();
    let [outer, _, _] = declared(&resolver);
    assert_eq!(resolver.scopes.lookup("x", &resolver.symbols), Some(outer));
    assert_eq!(resolver.scopes.lookup("local", &resolver.symbols), None);
    resolve_source(&mut resolver, "x;").unwrap();
    assert_eq!(
        resolver.references.values().copied().collect::<Vec<_>>(),
        vec![outer]
    );
}

#[test]
fn later_comprehension_clauses_see_earlier_binders() {
    let mut resolver = NameResolver::new();
    resolve_source(
        &mut resolver,
        "let xs = [[true]]; [y for x in xs, y in x, if y];",
    )
    .unwrap();

    let [xs, x, y] = declared(&resolver);
    assert_eq!(uses(&resolver, xs), 1);
    assert_eq!(uses(&resolver, x), 1);
    assert_eq!(uses(&resolver, y), 2);
    assert_eq!(resolver.scopes.lookup("x", &resolver.symbols), None);
    assert_eq!(resolver.scopes.lookup("y", &resolver.symbols), None);
}

#[test]
fn iterable_uses_the_outer_binding_of_its_own_name() {
    let mut resolver = NameResolver::new();
    resolve_source(&mut resolver, "let x = [1]; [x for x in x]; x;").unwrap();

    let [outer, inner] = declared(&resolver);
    assert_eq!(uses(&resolver, outer), 2);
    assert_eq!(uses(&resolver, inner), 1);
}

#[test]
fn loop_body_forms_resolve_names_and_restore_scope_on_errors() {
    for source in [
        "for x in [1] { missing; }",
        "[missing for x in [1]];",
        "[x for x in [1], if missing];",
        "%{missing: x for x in [1]};",
        "%{x: missing for x in [1]};",
        "%{:missing, x: x for x in [1]};",
    ] {
        let mut resolver = NameResolver::new();
        let error = resolve_source(&mut resolver, source).unwrap_err();
        assert!(
            matches!(error, NameResolutionError::UnknownName { ref name, .. } if name == "missing"),
            "{source}: {error:?}"
        );
        assert_eq!(
            resolver.scopes.lookup("x", &resolver.symbols),
            None,
            "{source}"
        );
    }
}

#[test]
fn unfinished_language_features_report_errors_instead_of_panicking() {
    for (source, feature) in [
        ("fn identity(x) => x;", "function declarations"),
        ("struct Point { x: Int }", "struct declarations"),
        (
            "let x = 1; x += 2;",
            "augmented assignment operator selection",
        ),
    ] {
        let mut resolver = NameResolver::new();
        let error = resolve_source(&mut resolver, source).unwrap_err();
        assert!(
            matches!(error, NameResolutionError::Unsupported { feature: found, span } if found == feature && span.source_id() == SourceId::new(0)),
            "{error:?}"
        );
    }
}
