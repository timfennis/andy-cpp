use super::{Candidates, NameResolutionError, NameResolver, ScopeKind};
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

/// The symbol a plain value use means: the newest binding in the nearest scope.
fn innermost(candidates: &Candidates) -> SymbolId {
    candidates.0[0][0]
}

/// Reads whose innermost candidate is `symbol`, plus writes to it.
fn uses(resolver: &NameResolver, symbol: SymbolId) -> usize {
    let reads = resolver
        .references
        .values()
        .filter(|candidates| innermost(candidates) == symbol)
        .count();
    let writes = resolver
        .assignments
        .values()
        .filter(|&&id| id == symbol)
        .count();
    reads + writes
}

/// The newest built-in declared under `name`, before any source is resolved.
fn builtin(resolver: &NameResolver, name: &str) -> SymbolId {
    resolver
        .scopes
        .lookup(name, &resolver.symbols)
        .expect("builtin is declared")
}

/// The name of the `UnknownName` error that resolving `source` reports.
fn unknown_name(source: &str) -> String {
    let mut resolver = NameResolver::new([]);
    match resolve_source(&mut resolver, source) {
        Err(NameResolutionError::UnknownName { name, .. }) => name,
        other => panic!("expected an unknown name in {source:?}, got {other:?}"),
    }
}

fn only_reference(resolver: &NameResolver) -> &Candidates {
    resolver
        .references
        .values()
        .exactly_one()
        .expect("exactly one reference")
}

#[test]
fn initializer_reads_outer_binding_before_shadowing() {
    let mut resolver = NameResolver::new([]);
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
    let mut resolver = NameResolver::new([]);
    let source = "let missing = missing;";
    let error = resolve_source(&mut resolver, source).unwrap_err();
    assert_eq!(
        error.to_string(),
        "Identifier missing has not previously been declared"
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
    let mut resolver = NameResolver::new([]);
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
    let mut resolver = NameResolver::new([]);
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
    let mut resolver = NameResolver::new([]);
    resolve_source(
        &mut resolver,
        "let object = (); let i = 0; object.field = i; object[i] = i; object.field;",
    )
    .unwrap();

    let [object, i] = declared(&resolver);
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
        let mut resolver = NameResolver::new(["callee", "+"]);
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
    let mut resolver = NameResolver::new([]);
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
        resolver.references.values().map(innermost).collect_vec(),
        vec![outer]
    );
}

#[test]
fn later_comprehension_clauses_see_earlier_binders() {
    let mut resolver = NameResolver::new([]);
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
    let mut resolver = NameResolver::new([]);
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
        let mut resolver = NameResolver::new([]);
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
        ("struct Point { x: Int }", "struct declarations"),
        (
            "let x = 1; x += 2;",
            "augmented assignment operator selection",
        ),
    ] {
        let mut resolver = NameResolver::new([]);
        let error = resolve_source(&mut resolver, source).unwrap_err();
        assert!(
            matches!(error, NameResolutionError::Unsupported { feature: found, span } if found == feature && span.source_id() == SourceId::new(0)),
            "{error:?}"
        );
    }
}

#[test]
fn candidates_in_one_scope_are_newest_first() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let x = 1; let x = 2; x;").unwrap();

    let [first, second] = declared(&resolver);
    assert_eq!(only_reference(&resolver).0, vec![vec![second, first]]);
}

#[test]
fn candidates_are_grouped_by_scope_without_empty_scopes() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let x = 1; { let x = 2; let x = 3; { x; } }").unwrap();

    let [x1, x2, x3] = declared(&resolver);
    assert_eq!(only_reference(&resolver).0, vec![vec![x3, x2], vec![x1]]);
}

#[test]
fn assignment_targets_resolve_to_the_innermost_symbol() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let x = 1; { let x = 2; x = 3; }").unwrap();

    let [_, inner] = declared(&resolver);
    assert_eq!(
        resolver.assignments.values().copied().collect_vec(),
        vec![inner]
    );
}

#[test]
fn candidates_include_functions_and_plain_bindings() {
    let mut resolver = NameResolver::new(["len"]);
    let native = builtin(&resolver, "len");
    resolve_source(&mut resolver, "let len = 99; len;").unwrap();

    // The resolver keeps both; choosing between them is the type pass's job.
    // The native lives in the built-in scope, outside the program's top level.
    let [local] = declared(&resolver);
    assert_eq!(only_reference(&resolver).0, vec![vec![local], vec![native]]);
}

#[test]
fn builtins_resolve_as_functions() {
    let mut resolver = NameResolver::new(["len"]);
    let native = builtin(&resolver, "len");
    resolve_source(&mut resolver, "len;").unwrap();

    assert_eq!(only_reference(&resolver).0, vec![vec![native]]);
    assert_eq!(resolver.symbols.kind(native), BindingKind::Function);
}

#[test]
fn overloaded_builtins_share_a_group_newest_first() {
    let mut resolver = NameResolver::new(["max", "max"]);
    resolve_source(&mut resolver, "max;").unwrap();

    let [group] = only_reference(&resolver).0.as_slice() else {
        panic!("expected one scope of candidates");
    };
    let [newest, oldest] = group[..] else {
        panic!("expected both overloads, got {group:?}");
    };
    assert!(newest > oldest);
    assert!(
        group
            .iter()
            .all(|&id| resolver.symbols.kind(id) == BindingKind::Function)
    );
}

#[test]
fn function_names_are_visible_in_their_own_body() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "fn fact(n) { fact(n); }").unwrap();

    let [fact, n] = declared(&resolver);
    assert_eq!(resolver.symbols.kind(fact), BindingKind::Function);
    assert_eq!(resolver.symbols.kind(n), BindingKind::Plain);
    assert_eq!(uses(&resolver, fact), 1);
    assert_eq!(uses(&resolver, n), 1);
}

#[test]
fn parameters_are_only_visible_inside_the_function() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "fn f(x) { x; }").unwrap();

    assert_eq!(unknown_name("fn f(x) { 1; } x;"), "x");
}

#[test]
fn function_overloads_share_a_group_newest_first() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "fn f(x) { 1; } fn f(x, y) { 1; } f;").unwrap();

    let [f1, _, f2, _, _] = declared(&resolver);
    assert_eq!(only_reference(&resolver).0, vec![vec![f2, f1]]);
    assert_eq!(resolver.symbols.kind(f1), BindingKind::Function);
    assert_eq!(resolver.symbols.kind(f2), BindingKind::Function);
}

#[test]
fn lambdas_do_not_bind_a_name() {
    // The initializer is resolved before `g` is declared, and the lambda has
    // no name of its own.
    assert_eq!(unknown_name("let g = fn(x) { g; };"), "g");
}

#[test]
fn named_function_expressions_bind_in_the_enclosing_scope() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let g = fn h(n) { n; }; h;").unwrap();

    let [h, n, g] = declared(&resolver);
    assert_eq!(uses(&resolver, h), 1);
    assert_eq!(uses(&resolver, n), 1);
    assert_eq!(uses(&resolver, g), 0);
}

#[test]
fn function_bodies_see_enclosing_bindings() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let y = 1; fn f(x) { y; }").unwrap();

    let [y, _, _] = declared(&resolver);
    assert_eq!(uses(&resolver, y), 1);
}

#[test]
fn duplicate_parameters_are_rejected() {
    let mut resolver = NameResolver::new([]);
    let source = "fn f(x, x) { 1; }";
    let error = resolve_source(&mut resolver, source).unwrap_err();
    assert_eq!(error.to_string(), "Illegal redefinition of parameter x");

    let NameResolutionError::DuplicateParameter { name, span } = error else {
        panic!("expected a duplicate parameter, got {error:?}");
    };
    assert_eq!(name, "x");
    assert_eq!(span.offset(), source.rfind('x').unwrap());
    assert_eq!(resolver.scopes.lookup("x", &resolver.symbols), None);
}

#[test]
fn errors_in_function_bodies_restore_scope() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "fn f(x) { missing; }").unwrap_err();

    assert_eq!(resolver.scopes.lookup("x", &resolver.symbols), None);
}

#[test]
fn functions_cannot_redefine_variables_in_the_same_scope() {
    let cases: [(&[&str], &str); 4] = [
        (&[], "let a = 1; fn a(x) { 1; }"),
        (&[], "let a = fn(x) { 1; }; fn a(x) { 1; }"),
        // The program's top level is one scope, separate from the builtins.
        (&["a"], "let a = 99; fn a(x, y) { 1; }"),
        // An arrow body shares the parameter scope; a block body would not.
        (&[], "fn f(a) => fn a() { 1; };"),
    ];
    for (builtins, source) in cases {
        let mut resolver = NameResolver::new(builtins.iter().copied());
        let error = resolve_source(&mut resolver, source).expect_err(source);
        assert_eq!(
            error.to_string(),
            "Illegal redefinition of variable a as a function in the same scope",
            "{source}"
        );
        let NameResolutionError::IllegalFunctionRedefinition { span, .. } = error else {
            unreachable!("the message identifies the variant");
        };
        assert!(source[span.range()].starts_with("fn a"), "{source}");

        // The rejected function binds neither its name nor its parameters.
        assert!(
            resolver
                .scopes
                .lookup("a", &resolver.symbols)
                .is_none_or(|id| resolver.symbols.kind(id) == BindingKind::Plain),
            "{source}"
        );
        assert_eq!(
            resolver.scopes.lookup("x", &resolver.symbols),
            None,
            "{source}"
        );
    }
}

#[test]
fn functions_may_share_names_with_variables_in_other_scopes() {
    let cases: [(&[&str], &str); 6] = [
        (&[], "fn a(x) { 1; } let a = 1;"),
        (&[], "let a = 1; { fn a(x) { 1; } }"),
        (&[], "let a = 1; fn b() { fn a(x) { 1; } }"),
        (&["len"], "fn len(x) { 1; }"),
        // Rejecting identical overloads needs parameter types: the type pass's job.
        (&[], "fn a(x) { 1; } fn a(x) { 1; }"),
        // A block body is its own scope, separate from the parameters.
        (&[], "fn f(a) { fn a() { 1; } }"),
    ];
    for (builtins, source) in cases {
        let mut resolver = NameResolver::new(builtins.iter().copied());
        resolve_source(&mut resolver, source).unwrap_or_else(|error| panic!("{source}: {error:?}"));
    }
}

#[test]
fn scopes_record_their_kind_in_creation_order() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "fn f(x) { 1; } for i in [1] { 1; }").unwrap();

    let [f, _, _] = declared(&resolver);
    let f_node = resolver
        .declarations
        .iter()
        .find_map(|(&node, &symbol)| (symbol == f).then_some(node))
        .expect("f is declared");
    assert_eq!(
        resolver
            .scopes
            .scopes
            .iter()
            .map(|scope| scope.kind)
            .collect_vec(),
        vec![
            ScopeKind::Builtins,
            ScopeKind::Program,
            ScopeKind::Function(f_node),
            // The function body.
            ScopeKind::Block,
            // The `for` iteration, then the loop body.
            ScopeKind::Block,
            ScopeKind::Block,
        ]
    );
}
