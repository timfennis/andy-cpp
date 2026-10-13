use super::{
    AugmentedCandidates, Candidates, NameResolutionError, NameResolver, ScopeKind, StructSymbols,
};
use crate::symbols::{BindingKind, SymbolId};
use itertools::Itertools;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::{NodeId, Parser};

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

/// Function scope nodes in creation order. Lambdas have no declaration entry,
/// so the scope arena is the one place every function's node appears.
fn functions(resolver: &NameResolver) -> Vec<NodeId> {
    resolver
        .scopes
        .scopes
        .iter()
        .filter_map(|scope| match scope.kind {
            ScopeKind::Function(node) => Some(node),
            _ => None,
        })
        .collect()
}

fn captures_of(resolver: &NameResolver, function: NodeId) -> &[SymbolId] {
    resolver.captures.get(&function).map_or(&[], Vec::as_slice)
}

fn only_augmented(resolver: &NameResolver) -> &AugmentedCandidates {
    resolver
        .augmented
        .values()
        .exactly_one()
        .expect("exactly one augmented assignment")
}

fn only_getter(resolver: &NameResolver) -> &Candidates {
    resolver
        .getters
        .values()
        .exactly_one()
        .expect("exactly one getter lookup")
}

fn only_setter(resolver: &NameResolver) -> &Candidates {
    resolver
        .setters
        .values()
        .exactly_one()
        .expect("exactly one setter lookup")
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
    let mut resolver = NameResolver::new(["field", "field=", "[]="]);
    let field = builtin(&resolver, "field");
    resolve_source(
        &mut resolver,
        "let object = (); let i = 0; object.field = i; object[i] = i; object.field;",
    )
    .unwrap();

    let [object, i] = declared(&resolver);
    assert_eq!(uses(&resolver, i), 3);
    assert_eq!(uses(&resolver, object), 3);
    // `field` still means the native getter: the targets declared nothing new.
    assert_eq!(
        resolver.scopes.lookup("field", &resolver.symbols),
        Some(field)
    );
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

#[test]
fn functions_capture_outer_bindings() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let x = 1; fn f() { x; }").unwrap();

    let [x, _] = declared(&resolver);
    let [f] = functions(&resolver)[..] else {
        panic!("expected one function");
    };
    assert_eq!(captures_of(&resolver, f), [x]);
    assert_eq!(resolver.captured, [x].into_iter().collect());
}

#[test]
fn blocks_are_not_function_boundaries() {
    for source in ["let x = 1; { { x; } }", "fn f(a) { { a; } }"] {
        let mut resolver = NameResolver::new([]);
        resolve_source(&mut resolver, source).unwrap();

        assert!(resolver.captures.is_empty(), "{source}");
        assert!(resolver.captured.is_empty(), "{source}");
    }
}

#[test]
fn captures_pass_through_intermediate_functions() {
    let mut resolver = NameResolver::new([]);
    resolve_source(
        &mut resolver,
        "fn outer() { let x = 1; fn middle() { fn inner() { x; } } }",
    )
    .unwrap();

    let [_, x, _, _] = declared(&resolver);
    let [outer, middle, inner] = functions(&resolver)[..] else {
        panic!("expected three functions");
    };
    assert_eq!(captures_of(&resolver, outer), []);
    assert_eq!(captures_of(&resolver, middle), [x]);
    assert_eq!(captures_of(&resolver, inner), [x]);
}

#[test]
fn lambdas_capture_under_their_own_node() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let x = 1; let g = fn() { x; };").unwrap();

    let [x, _] = declared(&resolver);
    let [lambda] = functions(&resolver)[..] else {
        panic!("expected one function");
    };
    assert_eq!(captures_of(&resolver, lambda), [x]);
}

#[test]
fn builtins_are_never_captured() {
    let mut resolver = NameResolver::new(["len"]);
    resolve_source(&mut resolver, "fn f() { len; }").unwrap();

    assert!(resolver.captures.is_empty());
    assert!(resolver.captured.is_empty());
}

#[test]
fn assignments_capture_their_target() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let x = 1; fn f() { x = 2; }").unwrap();

    let [x, _] = declared(&resolver);
    let [f] = functions(&resolver)[..] else {
        panic!("expected one function");
    };
    assert_eq!(captures_of(&resolver, f), [x]);
    assert!(resolver.captured.contains(&x));
}

#[test]
fn every_candidate_is_captured() {
    let mut resolver = NameResolver::new([]);
    resolve_source(
        &mut resolver,
        "fn outer() { fn h(a) { 1; } fn h(a, b) { 1; } fn inner() { h; } }",
    )
    .unwrap();

    // The type pass picks the overload later, so the closure captures both.
    let [_, h1, _, h2, _, _, _] = declared(&resolver);
    let [_, _, _, inner] = functions(&resolver)[..] else {
        panic!("expected four functions");
    };
    assert_eq!(captures_of(&resolver, inner), [h2, h1]);
}

#[test]
fn recursive_functions_capture_themselves() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "fn fact(n) { fact(n); }").unwrap();

    let [fact, _] = declared(&resolver);
    let [fact_node] = functions(&resolver)[..] else {
        panic!("expected one function");
    };
    assert_eq!(captures_of(&resolver, fact_node), [fact]);
}

#[test]
fn captures_are_listed_once_in_first_use_order() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let a = 1; let b = 2; fn f() { b; a; b; }").unwrap();

    let [a, b, _] = declared(&resolver);
    let [f] = functions(&resolver)[..] else {
        panic!("expected one function");
    };
    assert_eq!(captures_of(&resolver, f), [b, a]);
}

#[test]
fn loop_variables_can_be_captured() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "for i in [1] { let g = fn() { i; }; }").unwrap();

    let [i, _] = declared(&resolver);
    assert!(resolver.captured.contains(&i));
}

#[test]
fn block_scoped_variables_can_be_captured() {
    // The shape from #227: `y` must outlive its block for `g` to read it.
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let g = 0; { let y = 1; g = fn() { y; }; }").unwrap();

    let [_, y] = declared(&resolver);
    assert!(resolver.captured.contains(&y));
}

#[test]
fn augmented_assignment_finds_the_plain_operator() {
    let mut resolver = NameResolver::new(["+"]);
    let plus = builtin(&resolver, "+");
    resolve_source(&mut resolver, "let x = 1; x += 2;").unwrap();

    let [x] = declared(&resolver);
    let augmented = only_augmented(&resolver);
    assert!(augmented.in_place.0.is_empty());
    assert_eq!(augmented.operator.0, vec![vec![plus]]);
    assert_eq!(
        resolver.assignments.values().copied().collect_vec(),
        vec![x]
    );
}

#[test]
fn augmented_assignment_finds_both_operations() {
    let mut resolver = NameResolver::new(["++", "++="]);
    let concat = builtin(&resolver, "++");
    let append = builtin(&resolver, "++=");
    resolve_source(&mut resolver, "let xs = []; xs ++= [1];").unwrap();

    let augmented = only_augmented(&resolver);
    assert_eq!(augmented.in_place.0, vec![vec![append]]);
    assert_eq!(augmented.operator.0, vec![vec![concat]]);
}

#[test]
fn augmented_assignment_resolves_index_and_member_targets() {
    let mut resolver = NameResolver::new(["+", "[]", "[]="]);
    resolve_source(&mut resolver, "let xs = [1]; let i = 0; xs[i] += 1;").unwrap();
    let [xs, i] = declared(&resolver);
    assert_eq!(uses(&resolver, xs), 1);
    assert_eq!(uses(&resolver, i), 1);

    let mut resolver = NameResolver::new(["+", "x", "x="]);
    resolve_source(&mut resolver, "let p = (); p.x += 1;").unwrap();
    let [p] = declared(&resolver);
    assert_eq!(uses(&resolver, p), 1);
}

#[test]
fn augmented_assignment_rejects_sequence_targets() {
    let mut resolver = NameResolver::new(["+"]);
    let error = resolve_source(&mut resolver, "let a = 1; let b = 2; (a, b) += 1;").unwrap_err();

    assert_eq!(
        error.to_string(),
        "This lvalue is required to be a single identifier"
    );
    assert!(
        matches!(error, NameResolutionError::SequenceTargetNotAllowed { .. }),
        "{error:?}"
    );
    assert!(resolver.augmented.is_empty());
}

#[test]
fn augmented_assignment_without_any_operator_is_unknown() {
    assert_eq!(unknown_name("let x = 1; x += 2;"), "+");
}

#[test]
fn augmented_assignment_captures_its_target() {
    let mut resolver = NameResolver::new(["+"]);
    resolve_source(&mut resolver, "let x = 1; fn f() { x += 1; }").unwrap();

    let [x, _] = declared(&resolver);
    let [f] = functions(&resolver)[..] else {
        panic!("expected one function");
    };
    // The native `+` is a global, so only `x` is captured.
    assert_eq!(captures_of(&resolver, f), [x]);
}

#[test]
fn natives_resolve_as_properties() {
    let mut resolver = NameResolver::new(["len"]);
    let len = builtin(&resolver, "len");
    resolve_source(&mut resolver, "[1].len;").unwrap();

    assert_eq!(only_getter(&resolver).0, vec![vec![len]]);
}

#[test]
fn user_functions_resolve_as_properties() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "fn double(x) { x; } 5.double;").unwrap();

    let [double, _] = declared(&resolver);
    assert_eq!(only_getter(&resolver).0, vec![vec![double]]);
}

#[test]
fn getters_keep_same_named_variables_as_candidates() {
    // The type pass skips the variable because it can't be called.
    let mut resolver = NameResolver::new(["x"]);
    let native = builtin(&resolver, "x");
    resolve_source(&mut resolver, "let p = 1; let x = 99; p.x;").unwrap();

    let [_, local] = declared(&resolver);
    assert_eq!(only_getter(&resolver).0, vec![vec![local], vec![native]]);
}

#[test]
fn getters_are_keyed_by_the_member_access_node() {
    let mut resolver = NameResolver::new(["b", "c"]);
    resolve_source(&mut resolver, "let a = 1; a.b.c;").unwrap();

    // One lookup per member access; none of them under the receiver `a`.
    assert_eq!(resolver.getters.len(), 2);
    assert!(
        resolver
            .getters
            .keys()
            .all(|node| !resolver.references.contains_key(node))
    );
}

#[test]
fn member_writes_resolve_the_setter_only() {
    let mut resolver = NameResolver::new(["x="]);
    let setter = builtin(&resolver, "x=");
    resolve_source(&mut resolver, "let p = 1; p.x = 5;").unwrap();

    assert_eq!(only_setter(&resolver).0, vec![vec![setter]]);
    assert!(resolver.getters.is_empty());
}

#[test]
fn index_writes_resolve_the_setter_only() {
    let mut resolver = NameResolver::new(["[]="]);
    let setter = builtin(&resolver, "[]=");
    resolve_source(&mut resolver, "let xs = [1]; xs[0] = 2;").unwrap();

    assert_eq!(only_setter(&resolver).0, vec![vec![setter]]);
    assert!(resolver.getters.is_empty());
}

#[test]
fn augmented_member_assignment_resolves_getter_and_setter() {
    let mut resolver = NameResolver::new(["x", "x=", "+"]);
    let getter = builtin(&resolver, "x");
    let setter = builtin(&resolver, "x=");
    resolve_source(&mut resolver, "let p = 1; p.x += 1;").unwrap();

    assert_eq!(only_getter(&resolver).0, vec![vec![getter]]);
    assert_eq!(only_setter(&resolver).0, vec![vec![setter]]);
    // Both belong to the same target node.
    assert_eq!(
        resolver.getters.keys().collect_vec(),
        resolver.setters.keys().collect_vec()
    );
}

#[test]
fn augmented_index_assignment_resolves_getter_and_setter() {
    let mut resolver = NameResolver::new(["[]", "[]=", "+"]);
    let getter = builtin(&resolver, "[]");
    let setter = builtin(&resolver, "[]=");
    resolve_source(&mut resolver, "let xs = [1]; xs[0] += 1;").unwrap();

    assert_eq!(only_getter(&resolver).0, vec![vec![getter]]);
    assert_eq!(only_setter(&resolver).0, vec![vec![setter]]);
}

#[test]
fn unknown_accessors_are_unknown_names() {
    assert_eq!(unknown_name("let p = 1; p.nope;"), "nope");
    assert_eq!(unknown_name("let p = 1; p.x = 5;"), "x=");
}

/// The names of every resolved type, sorted for stable comparisons.
fn type_names(resolver: &NameResolver) -> Vec<&str> {
    resolver
        .type_names
        .values()
        .map(|&id| resolver.symbols.name(id))
        .sorted()
        .collect()
}

#[test]
fn builtin_type_names_resolve_including_nested_ones() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let m: Map<String, (Int, List<Bool>)> = 1;").unwrap();

    assert_eq!(
        type_names(&resolver),
        ["Bool", "Int", "List", "Map", "String"]
    );
    assert!(
        resolver
            .type_names
            .values()
            .all(|&id| resolver.symbols.kind(id) == BindingKind::Type)
    );
}

#[test]
fn unknown_types_are_reported_in_every_annotation_position() {
    for source in [
        "let x: Nope = 1;",
        "let x: List<Nope> = 1;",
        "fn f(x: Nope) { 1; }",
        "fn f() -> Nope { 1; }",
        "1 as Nope;",
    ] {
        let mut resolver = NameResolver::new([]);
        let error = resolve_source(&mut resolver, source).expect_err(source);
        assert_eq!(error.to_string(), "unknown type `Nope`", "{source}");
        let NameResolutionError::UnknownType { span, .. } = error else {
            unreachable!("the message identifies the variant");
        };
        assert_eq!(&source[span.range()], "Nope", "{source}");
        // A failed parameter annotation leaves the function's scope.
        assert_eq!(
            resolver.scopes.lookup("x", &resolver.symbols),
            None,
            "{source}"
        );
    }
}

#[test]
fn types_and_values_are_separate_namespaces() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let Int = 5; let y: Int = Int;").unwrap();

    // The annotation still means the built-in type; the value is the `let`.
    let [value, _] = declared(&resolver);
    assert_eq!(type_names(&resolver), ["Int"]);
    let type_int = *resolver.type_names.values().next().unwrap();
    assert_ne!(type_int, value);
    assert_eq!(resolver.symbols.kind(type_int), BindingKind::Type);
    assert_eq!(uses(&resolver, value), 1);
}

#[test]
fn annotations_resolve_before_values() {
    for source in ["let x: Nope = missing;", "missing as Nope;"] {
        let mut resolver = NameResolver::new([]);
        let error = resolve_source(&mut resolver, source).expect_err(source);
        assert!(
            matches!(error, NameResolutionError::UnknownType { .. }),
            "{source}: {error:?}"
        );
    }
}

#[test]
fn type_names_are_never_captured() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "fn f() { let x: Int = 1; 1 as Int; }").unwrap();

    assert_eq!(type_names(&resolver), ["Int", "Int"]);
    assert!(resolver.captures.is_empty());
    assert!(resolver.captured.is_empty());
}

#[test]
fn unannotated_code_records_no_type_names() {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, "let x = 1; fn f(a) { a; }").unwrap();

    assert!(resolver.type_names.is_empty());
}

fn only_struct(resolver: &NameResolver) -> &StructSymbols {
    resolver
        .structs
        .values()
        .exactly_one()
        .expect("exactly one struct declaration")
}

/// The message of the error that resolving `source` reports.
fn error_message(source: &str) -> String {
    let mut resolver = NameResolver::new([]);
    resolve_source(&mut resolver, source)
        .expect_err(source)
        .to_string()
}

#[test]
fn structs_bind_a_type_a_constructor_and_accessors() {
    let mut resolver = NameResolver::new([]);
    resolve_source(
        &mut resolver,
        "struct Point { x: Int, y: Int } let p: Point = Point(1, 2); p.x; p.y = 3;",
    )
    .unwrap();

    let point = only_struct(&resolver);
    let [(x, x_set), (y, y_set)] = point.accessors[..] else {
        panic!("expected two fields");
    };
    let symbols = &resolver.symbols;
    assert_eq!(symbols.name(point.type_symbol), "Point");
    assert_eq!(symbols.kind(point.type_symbol), BindingKind::Type);
    assert_eq!(symbols.name(point.constructor), "Point");
    assert_eq!(symbols.kind(point.constructor), BindingKind::Function);
    assert_eq!(
        [x, x_set, y, y_set].map(|id| (symbols.name(id), symbols.kind(id))),
        [
            ("x", BindingKind::Function),
            ("x=", BindingKind::Function),
            ("y", BindingKind::Function),
            ("y=", BindingKind::Function),
        ]
    );

    // Each binding is what its use resolves to.
    assert!(
        resolver
            .type_names
            .values()
            .any(|&id| id == point.type_symbol)
    );
    assert_eq!(uses(&resolver, point.constructor), 1);
    assert_eq!(only_getter(&resolver).0, vec![vec![x]]);
    assert_eq!(only_setter(&resolver).0, vec![vec![y_set]]);
}

#[test]
fn structs_cannot_refer_to_themselves_or_later_structs() {
    assert_eq!(
        error_message("struct Node { value: Int, next: Option<Node> }"),
        "unknown type `Node`"
    );
    assert_eq!(
        error_message("struct A { b: B } struct B { x: Int }"),
        "unknown type `B`"
    );
}

#[test]
fn structs_are_scoped_like_other_declarations() {
    assert_eq!(
        error_message("fn make() { struct S { v: Int } S(1); } fn check(x: S) { x.v; }"),
        "unknown type `S`"
    );

    // An inner struct shadows an outer one of the same name.
    let mut resolver = NameResolver::new([]);
    resolve_source(
        &mut resolver,
        "struct P { x: Int } { struct P { y: Int } let p: P = P(2); p.y; }",
    )
    .unwrap();
    let inner = resolver
        .structs
        .values()
        .map(|symbols| symbols.type_symbol)
        .max()
        .unwrap();
    // Field annotations add `Int`s; the `let p: P` annotation is the only `P`.
    let annotated = resolver
        .type_names
        .values()
        .filter(|&&id| resolver.symbols.name(id) == "P")
        .exactly_one()
        .unwrap();
    assert_eq!(*annotated, inner);
}

#[test]
fn struct_redefinition_errors_keep_the_current_messages() {
    assert_eq!(
        error_message("struct Int { x: Int }"),
        "Struct 'Int' is not allowed to shadow the built-in type 'Int'"
    );
    assert_eq!(
        error_message("struct P { x: Int } struct P { y: Int }"),
        "Illegal redefinition of struct 'P'"
    );

    let source = "struct Dup { a: Int, a: Int }";
    let mut resolver = NameResolver::new([]);
    let error = resolve_source(&mut resolver, source).unwrap_err();
    assert_eq!(
        error.to_string(),
        "Illegal redefinition of field 'a' in struct 'Dup'"
    );
    let NameResolutionError::DuplicateField { span, .. } = error else {
        unreachable!("the message identifies the variant");
    };
    assert_eq!(span.offset(), source.rfind("a: Int").unwrap());
}

#[test]
fn constructors_cannot_redefine_variables_in_the_same_scope() {
    assert_eq!(
        error_message("let P = 5; struct P { x: Int }"),
        "Illegal redefinition of variable P as a struct in the same scope"
    );
    for source in [
        "struct P { x: Int } let P = 5;",
        "let P = 5; { struct P { x: Int } }",
    ] {
        let mut resolver = NameResolver::new([]);
        resolve_source(&mut resolver, source).unwrap_or_else(|error| panic!("{source}: {error:?}"));
    }
}

#[test]
fn field_accessors_may_share_names_with_variables() {
    let mut resolver = NameResolver::new([]);
    resolve_source(
        &mut resolver,
        "let x = 5; struct Point { x: Int, y: Int } Point(1, 2).x;",
    )
    .unwrap();

    // The getter is a candidate beside the variable; the type pass skips the variable.
    let [local] = declared(&resolver);
    let (getter, _) = only_struct(&resolver).accessors[0];
    assert_eq!(only_getter(&resolver).0, vec![vec![getter, local]]);
}

#[test]
fn structs_sharing_a_field_name_overload_its_accessors() {
    let mut resolver = NameResolver::new([]);
    resolve_source(
        &mut resolver,
        "struct A { x: Int } struct B { x: String } A(1).x;",
    )
    .unwrap();

    let [a_x, b_x] = resolver
        .structs
        .values()
        .map(|symbols| symbols.accessors[0].0)
        .sorted()
        .collect_vec()[..]
    else {
        panic!("expected two structs");
    };
    assert_eq!(only_getter(&resolver).0, vec![vec![b_x, a_x]]);
}

#[test]
fn struct_bindings_are_captured_like_functions() {
    let mut resolver = NameResolver::new([]);
    resolve_source(
        &mut resolver,
        "fn outer() { struct P { x: Int } fn inner() { P(1).x; } }",
    )
    .unwrap();

    let p = only_struct(&resolver);
    let inner = functions(&resolver)[1];
    assert_eq!(
        captures_of(&resolver, inner),
        [p.constructor, p.accessors[0].0]
    );
}
