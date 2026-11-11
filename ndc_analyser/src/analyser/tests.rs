use super::*;
use itertools::Itertools;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;

fn analyse_with_globals(
    source: &str,
    globals: Vec<(String, StaticType)>,
) -> (StaticType, AnalysisResult) {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("lex failed");
    let expressions = Parser::from_tokens(tokens).parse().expect("parse failed");
    let mut analyser = Analyser::new(globals);
    for expression in &expressions {
        analyser.analyse(expression);
    }
    let result = analyser.take_result();
    // The last statement's type as the whole program settled it.
    let last_type = expressions.last().map_or_else(StaticType::unit, |last| {
        result.analysis.expr_types[&last.id].clone()
    });
    (last_type, result)
}

fn analyse_last_type_with_globals(source: &str, globals: Vec<(String, StaticType)>) -> StaticType {
    let (last_type, result) = analyse_with_globals(source, globals);
    assert!(
        result.errors.is_empty(),
        "analysis errors: {:?}",
        result.errors
    );
    last_type
}

fn analyse_last_type(source: &str) -> StaticType {
    analyse_last_type_with_globals(source, vec![])
}

/// `[]` and `[]=`, which index reads and writes go through. Name resolution
/// rejects index access without them, as it would any other unknown function.
fn index_natives() -> Vec<(String, StaticType)> {
    // The stdlib's list and map overloads.
    [
        ("[]", "<T>(List<T>, Int) -> T"),
        ("[]", "<T>(List<T>, Iterator<Any>) -> List<T>"),
        ("[]", "<K, V>(Map<K, V>, K) -> V"),
        ("[]=", "<T>(List<T>, Int, T)"),
        ("[]=", "<T>(List<T>, Iterator<Any>, Sequence<T>)"),
        ("[]=", "<K, V>(Map<K, V>, K, V)"),
    ]
    .into_iter()
    .map(|(name, signature)| {
        let typ = StaticType::signature(signature).expect("valid signature");
        (name.to_string(), typ)
    })
    .collect()
}

/// Every error analysing `source` reports, in order.
fn errors(source: &str, globals: Vec<(String, StaticType)>) -> Vec<String> {
    let (_, result) = analyse_with_globals(source, globals);
    result.errors.iter().map(ToString::to_string).collect()
}

fn assert_analysis_error(source: &str, globals: Vec<(String, StaticType)>, expected: &str) {
    let (_, result) = analyse_with_globals(source, globals);
    assert!(
        result
            .errors
            .iter()
            .any(|error| error.to_string().contains(expected)),
        "expected an error containing {expected:?}, got {:?}",
        result.errors,
    );
}

// `Q`'s getter `y` doesn't accept a `P`, so `p.y` names no field of `p`,
// whether the write is plain or augmented.
#[test]
fn writing_a_field_the_receiver_lacks_is_rejected() {
    let plus = StaticType::Function {
        parameters: None,
        return_type: Box::new(StaticType::Any),
    };
    for assignment in ["p.y = 1;", "p.y += 1;"] {
        let source =
            format!("struct P {{ x: Int }} struct Q {{ y: Int }} let p = P(1); {assignment}");
        assert_eq!(
            errors(&source, vec![("+".to_string(), plus.clone())]),
            vec!["No function called 'y' found that matches the arguments 'P'"],
            "{source}",
        );
    }
}

#[test]
fn destructuring_assignment_checks_each_target() {
    assert_analysis_error(
        "let a: Int = 1; let b = 2; (a, b) = (\"s\", 2);",
        vec![],
        "mismatched types",
    );
    assert_analysis_error(
        "let a = 1; let b = 2; (a, b) = (1, 2, 3);",
        vec![],
        "mismatched tuple arity",
    );
}

#[test]
fn index_write_into_a_string_must_be_a_string() {
    for source in [
        "let s: String = \"abc\"; s[0] = 42;",
        "let s = \"abc\"; s[0] = 42;",
    ] {
        assert_analysis_error(source, index_natives(), "mismatched types");
    }
}

#[test]
fn break_and_continue_need_a_loop_in_their_own_function() {
    for source in [
        "break;",
        "continue;",
        "while true { fn() { break; }; }",
        // A sequence is evaluated before its loop starts.
        "for x in [break] { }",
        // A map comprehension's default is evaluated before any loop.
        "%{:break, x: 1 for x in [1]};",
    ] {
        assert_analysis_error(source, vec![], "outside of loop");
    }
}

#[test]
fn break_and_continue_inside_a_loop_are_accepted() {
    for source in [
        "while true { break; }",
        "while break { }",
        "for x in [1] { continue; }",
        "for x in [1], if break { }",
        "[break for x in [1]];",
        "while true { %{:break, x: 1 for x in [1]}; }",
        "for x in [1] { for y in [break] { } }",
    ] {
        analyse_last_type(source);
    }
}

#[test]
fn return_needs_a_function() {
    assert_analysis_error("return 1;", vec![], "outside of function body");
    analyse_last_type("fn f() { while true { return 1; } }");
}

// The body and each `return` make a different element `String` and the others
// `Int`, so leaving any of them out of the join leaves an `Int` in the tuple.
// Each position of the tuple gets its `String` from a different source: the
// first `return`, the second `return`, or the body. A position is only `Any`
// if that source joined in, so dropping any one of them leaves an `Int`.
#[test]
fn a_function_returns_the_join_of_its_body_and_every_return() {
    let (_, result) = analyse_with_globals(
        "fn f(x) {
             if x { return (\"a\", 1, 1); }
             if x { return (1, \"b\", 1); }
             let last = (1, 1, \"c\");
             last
         }",
        vec![],
    );
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert_eq!(
        result
            .analysis
            .inferred_return_types
            .values()
            .exactly_one()
            .ok(),
        Some(&StaticType::Tuple(vec![StaticType::Any; 3])),
    );
}

#[test]
fn annotated_identifier_augmented_assignment_rejects_widening() {
    let add = StaticType::Function {
        parameters: Some(vec![StaticType::Int, StaticType::Float]),
        return_type: Box::new(StaticType::Float),
    };
    assert_analysis_error(
        "let x: Int = 3; x += 0.5;",
        vec![("+".to_string(), add)],
        "mismatched types: found Float but expected Int",
    );
}

#[test]
fn an_in_place_operator_assignment_keeps_the_target_type() {
    let list_any = StaticType::List(Box::new(StaticType::Any));
    let append = StaticType::Function {
        parameters: Some(vec![list_any.clone(), list_any.clone()]),
        return_type: Box::new(list_any),
    };

    assert_eq!(
        analyse_last_type_with_globals(
            "let values = [1]; values ++= [2]; values",
            vec![("++=".to_string(), append)],
        ),
        StaticType::List(Box::new(StaticType::Int)),
    );
}

#[test]
fn an_in_place_operator_assignment_cannot_change_the_target_type() {
    let list_any = StaticType::List(Box::new(StaticType::Any));
    let concat = StaticType::Function {
        parameters: Some(vec![list_any.clone(), list_any.clone()]),
        return_type: Box::new(list_any),
    };

    assert_analysis_error(
        "let values = [1]; values ++= [\"two\"];",
        vec![
            ("++=".to_string(), concat.clone()),
            ("++".to_string(), concat.clone()),
        ],
        "mismatched types: found List<String> but expected List<Int>",
    );
    assert_analysis_error(
        "let values: List<Int> = [1]; values ++= [\"two\"];",
        vec![
            ("++=".to_string(), concat.clone()),
            ("++".to_string(), concat),
        ],
        "mismatched types: found List<String> but expected List<Int>",
    );
}

#[test]
fn annotated_declaration_types_empty_container_literals() {
    assert_eq!(
        analyse_last_type("let x: Map<Int, Int> = %{}; x"),
        StaticType::Map {
            key: Box::new(StaticType::Int),
            value: Box::new(StaticType::Int),
        },
    );
    assert_eq!(
        analyse_last_type("let x: List<Int> = []; x"),
        StaticType::List(Box::new(StaticType::Int)),
    );
    assert_eq!(
        analyse_last_type("let x: List<List<Int>> = [[]]; x"),
        StaticType::List(Box::new(StaticType::List(Box::new(StaticType::Int)))),
    );
}

#[test]
fn annotated_declaration_rejects_a_mismatched_container_literal() {
    assert_analysis_error(
        "let x: Map<Int, Int> = %{\"a\": 1};",
        vec![],
        "mismatched types: found Map<String, Int> but expected Map<Int, Int>",
    );
}

// `f` might hold any function, so which one runs is only known at runtime.
#[test]
fn a_call_of_a_value_of_unknown_type_dispatches_at_runtime() {
    let (_, result) = analyse_with_globals("fn call(f) { f(\"x\") }", vec![]);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let callee = result
        .analysis
        .bindings
        .values()
        .exactly_one()
        .expect("`f` is the only identifier");
    assert!(
        matches!(callee, Binding::Dynamic(candidates) if candidates.len() == 1),
        "{callee:?}"
    );
}

#[test]
fn map_default_value_contributes_to_value_type() {
    assert_eq!(
        analyse_last_type("let m = %{:0}; m"),
        StaticType::Map {
            key: Box::new(StaticType::Any),
            value: Box::new(StaticType::Int),
        },
    );
}

/// `source` reports exactly one error, containing `expected`, and its last
/// expression is `Any`. For code after a failed declaration: the names it
/// declared are bound as `Any`, so using them adds no second error.
fn assert_one_error_then_any(source: &str, expected: &str) {
    let (last_type, result) = analyse_with_globals(source, vec![]);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    assert!(
        result.errors[0].to_string().contains(expected),
        "expected an error containing {expected:?}, got {:?}",
        result.errors,
    );
    assert_eq!(last_type, StaticType::Any);
}

#[test]
fn a_pattern_that_fails_to_type_check_binds_every_name_as_any() {
    for (source, expected) in [
        (
            "let (a, b, c): (Int, Int, Int) = (1, 2); c",
            "Invalid unpacking",
        ),
        (
            "let (a, b, c): (Int, Int) = (1, 2); c",
            "mismatched tuple arity",
        ),
        ("let (a, (b, c)) = (1, 2); c", "Invalid unpacking"),
    ] {
        assert_one_error_then_any(source, expected);
    }
}

#[test]
fn a_statement_with_an_unknown_name_binds_its_declarations_as_any() {
    for source in [
        "fn f() { missing; } let g = f; g",
        "fn f() { missing; } f()",
        // Read from inside another function.
        "fn f() { missing; } fn g() { f } g()",
        "let a = missing; a",
        "let (a, b) = missing; b",
    ] {
        assert_one_error_then_any(
            source,
            "Identifier missing has not previously been declared",
        );
    }
}

#[test]
fn a_named_function_declaration_evaluates_to_unit() {
    let (last_type, result) = analyse_with_globals("fn g() { fn h() { 1 } } g()", vec![]);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert_eq!(last_type, StaticType::unit());
}

#[test]
fn a_redefined_function_is_reported_once_and_stays_usable() {
    for source in [
        "fn f(x) { x } fn f(y) { y } let g = f;",
        "fn f(x) { x } fn f(y) { y } f(1)",
    ] {
        assert_eq!(
            errors(source, vec![]),
            vec!["Illegal redefinition of function 'f' with parameters (Any) in the same scope"],
            "{source}",
        );
    }
}

// An annotation names the struct its scope sees: the inner `T` inside
// `inner`, the outer `T` after it. Mixing them up would make `t.b` or
// `u.a` an unknown field.
#[test]
fn annotations_name_the_struct_in_scope() {
    assert_eq!(
        analyse_last_type(
            "struct T { a: Int }
             fn inner() { struct T { b: String } let t: T = T(\"x\"); t.b }
             let u: T = T(1);
             u.a;
             inner()"
        ),
        StaticType::String,
    );
}

// Each native is typed by its own registry entry: mixing them up would
// give `second()` the type of `first()`.
#[test]
fn natives_are_typed_by_their_registry_entry() {
    let native = |name: &str, return_type| {
        (
            name.to_string(),
            StaticType::Function {
                parameters: Some(vec![]),
                return_type: Box::new(return_type),
            },
        )
    };
    assert_eq!(
        analyse_last_type_with_globals(
            "second()",
            vec![
                native("first", StaticType::Int),
                native("second", StaticType::String)
            ],
        ),
        StaticType::String,
    );
}

#[test]
fn same_arity_overloads_with_different_parameter_types_are_allowed() {
    assert_eq!(
        analyse_last_type(
            "fn f(x: Int) { 1 } fn f(x: String) { \"one\" } fn f(x: List<Int>) { [1] } f(\"a\")"
        ),
        StaticType::String,
    );
}

#[test]
fn a_variable_keeps_its_inferred_type() {
    let add = StaticType::Function {
        parameters: Some(vec![StaticType::Int, StaticType::Float]),
        return_type: Box::new(StaticType::Float),
    };
    let globals = [index_natives(), vec![("+".to_string(), add)]].concat();
    let scalar = "mismatched types: found Float but expected Int";
    let list = "mismatched types: found List<Float> but expected List<Int>";
    for (source, expected) in [
        ("let x = 3; x = 0.5;", scalar),
        ("let x = 3; x += 0.5;", scalar),
        ("let a = 1; let b = 2; (a, b) = (0.5, 2);", scalar),
        ("for x in [1] { x = 0.5; }", scalar),
        ("let values = [1]; values[0] = 0.5;", scalar),
        ("let values = [1]; values[0..1] = [0.5];", list),
        ("let values = [1]; values[0] += 0.5;", scalar),
        ("let values = %{\"one\": 1}; values[\"two\"] = 0.5;", scalar),
    ] {
        assert_eq!(errors(source, globals.clone()), vec![expected], "{source}");
    }
}

#[test]
fn an_any_value_can_be_stored_anywhere() {
    let any_value = vec![(
        "anything".to_string(),
        StaticType::Function {
            parameters: Some(vec![]),
            return_type: Box::new(StaticType::Any),
        },
    )];
    assert_eq!(
        analyse_last_type_with_globals("let x = 3; x = anything(); x", any_value),
        StaticType::Int
    );
}

#[test]
fn the_first_write_settles_what_an_empty_literal_holds() {
    let list_of = |element| StaticType::List(Box::new(element));
    assert_eq!(
        analyse_last_type_with_globals("let xs = []; xs[0] = 1; xs", index_natives()),
        list_of(StaticType::Int)
    );
    assert_eq!(
        analyse_last_type_with_globals("let xs = []; xs[0] = [1.5]; xs", index_natives()),
        list_of(list_of(StaticType::Float))
    );
    assert_eq!(
        analyse_last_type_with_globals("let m = %{}; m[\"a\"] = 1; m", index_natives()),
        StaticType::Map {
            key: Box::new(StaticType::String),
            value: Box::new(StaticType::Int),
        }
    );
    // Reads before the write see the settled type too, once the batch ends.
    assert_eq!(
        analyse_last_type_with_globals("let xs = []; let ys = xs; xs[0] = 1; ys", index_natives()),
        list_of(StaticType::Int)
    );
}

#[test]
fn an_annotation_settles_what_its_value_holds() {
    let option_of = |inner| StaticType::Option(Box::new(inner));
    assert_eq!(
        analyse_last_type("let o: Option<Int> = None; o"),
        option_of(StaticType::Int)
    );
    assert_eq!(
        analyse_last_type("let p: (Int, Option<Int>) = (1, None); p"),
        StaticType::Tuple(vec![StaticType::Int, option_of(StaticType::Int)])
    );
}

/// Natives with these signatures, as a stdlib would declare them.
fn natives(signatures: &[(&str, &str)]) -> Vec<(String, StaticType)> {
    signatures
        .iter()
        .map(|(name, signature)| {
            let typ = StaticType::signature(signature).expect("valid signature");
            ((*name).to_string(), typ)
        })
        .collect()
}

#[test]
fn a_call_settles_its_arguments_through_the_picked_signature() {
    let globals = natives(&[("push", "<T>(List<T>, T)")]);
    assert_eq!(
        analyse_last_type_with_globals("let xs = []; push(xs, 1); xs", globals.clone()),
        StaticType::List(Box::new(StaticType::Int))
    );
    assert_eq!(
        errors("let xs = [1]; push(xs, \"a\");", globals),
        vec!["No function called 'push' found that matches the arguments 'List<Int>, String'"]
    );
}

#[test]
fn each_call_instantiates_the_signature_afresh() {
    let globals = natives(&[("first", "<T>(List<T>) -> T")]);
    assert_eq!(
        analyse_last_type_with_globals("first([1.5])", globals.clone()),
        StaticType::Float
    );
    assert_eq!(
        analyse_last_type_with_globals("first([1]); first([\"a\"])", globals),
        StaticType::String
    );
}

#[test]
fn a_call_picked_at_runtime_has_no_type_parameters_in_its_type() {
    // `xs` could be anything, so `reversed` is picked when the program runs
    // and its `T` is never known.
    let globals = natives(&[("reversed", "<T>(List<T>) -> List<T>")]);
    assert_eq!(
        analyse_last_type_with_globals("fn f(xs) { reversed(xs) } f", globals),
        StaticType::Function {
            parameters: Some(vec![StaticType::Any]),
            return_type: Box::new(StaticType::List(Box::new(StaticType::Any))),
        }
    );
}

#[test]
fn a_generic_built_in_used_as_a_value_has_one_type() {
    let globals = natives(&[("push", "<T>(List<T>, T)")]);
    assert_eq!(
        analyse_last_type_with_globals("let p = push; let xs = [1]; p(xs, 2); p", globals),
        StaticType::Function {
            parameters: Some(vec![
                StaticType::List(Box::new(StaticType::Int)),
                StaticType::Int
            ]),
            return_type: Box::new(StaticType::unit()),
        }
    );
}

#[test]
fn calling_a_function_value_checks_its_arguments() {
    assert_eq!(
        errors(
            "let f = fn(x: Int) { x }; let g = [f]; g[0](\"a\");",
            index_natives()
        ),
        vec!["A function of type Fn(Int) -> Int doesn't accept the arguments 'String'"]
    );
}

#[test]
fn an_index_write_checks_the_index_against_the_container() {
    let plus = natives(&[("+", "(Int, Int) -> Int")]);
    let globals = [index_natives(), plus].concat();
    for (source, expected) in [
        (
            "let m = %{1: 2}; m[\"a\"] = 3;",
            "No function called '[]=' found that matches the arguments 'Map<Int, Int>, String, Int'",
        ),
        // Reading the old value fails first, and is the one error reported.
        (
            "let m = %{\"a\": 1}; m[0] += 3;",
            "No function called '[]' found that matches the arguments 'Map<String, Int>, Int'",
        ),
        ("let a = 5; a[0] = 1;", "Unable to index into Int"),
    ] {
        assert_eq!(errors(source, globals.clone()), vec![expected], "{source}");
    }
}

#[test]
fn a_function_annotation_types_a_variable() {
    let int_to_int = StaticType::Function {
        parameters: Some(vec![StaticType::Int]),
        return_type: Box::new(StaticType::Int),
    };
    assert_eq!(
        analyse_last_type("let f: Fn(Int) -> Int = fn(x: Int) => x; f"),
        int_to_int
    );
    assert_eq!(
        errors("let f: Fn(Int) -> Int = fn(x: String) => x;", vec![]),
        vec!["mismatched types: found Fn(String) -> String but expected Fn(Int) -> Int"]
    );
}

#[test]
fn a_function_argument_gets_what_the_function_parameter_passes() {
    let globals = natives(&[("map", "<T, U>(Sequence<T>, Fn(T) -> U) -> List<U>")]);
    // A typed lambda fits a type parameter, and settles it.
    assert_eq!(
        analyse_last_type_with_globals(
            "let xs = []; map(xs, fn(x: Int) => x); xs",
            globals.clone()
        ),
        StaticType::List(Box::new(StaticType::Int))
    );
    assert_eq!(
        analyse_last_type_with_globals("map([1], fn(x: Int) => \"a\")", globals.clone()),
        StaticType::List(Box::new(StaticType::String))
    );
    // It has to take what the list holds.
    assert_eq!(
        errors("map([1], fn(x: String) => x);", globals),
        vec![
            "No function called 'map' found that matches the arguments 'List<Int>, Fn(String) -> String'"
        ]
    );
}

#[test]
fn a_function_argument_fits_a_function_parameter_where_their_known_parts_agree() {
    let apply = "fn apply(f: Fn(Int) -> Int) { f(1) } ";
    // An untyped lambda is `Fn(Any) -> Any`, which is checked as the program runs.
    assert_eq!(
        errors(&format!("{apply}apply(fn(x) => x);"), vec![]),
        Vec::<String>::new()
    );
    for (lambda, shown) in [
        ("fn(x: String) => 1", "Fn(String) -> Int"),
        ("fn(x) => \"a\"", "Fn(Any) -> String"),
        ("fn(a, b) => 1", "Fn(Any, Any) -> Int"),
    ] {
        assert_eq!(
            errors(&format!("{apply}apply({lambda});"), vec![]),
            vec![format!(
                "No function called 'apply' found that matches the arguments '{shown}'"
            )],
            "{lambda}"
        );
    }
}

#[test]
fn an_any_parameter_settles_what_its_argument_leaves_unknown_as_any() {
    // `keep` may store anything in the list, so it can't become a List<Int>.
    let globals = [natives(&[("keep", "(List<Any>)")]), index_natives()].concat();
    assert_eq!(
        analyse_last_type_with_globals("let xs = []; keep(xs); xs[0] = 1; xs", globals),
        StaticType::List(Box::new(StaticType::Any))
    );
}

#[test]
fn a_default_function_gives_a_map_the_values_it_returns() {
    assert_eq!(
        analyse_last_type("%{:fn() => 1}"),
        StaticType::Map {
            key: Box::new(StaticType::Any),
            value: Box::new(StaticType::Int),
        }
    );
}

#[test]
fn what_nothing_settles_falls_back_to_any() {
    assert_eq!(
        analyse_last_type("let xs = []; xs"),
        StaticType::List(Box::new(StaticType::Any))
    );
    assert_eq!(
        analyse_last_type("None"),
        StaticType::Option(Box::new(StaticType::Any))
    );
    // An empty map with nothing written to it is an empty set.
    assert_eq!(
        analyse_last_type("%{}"),
        StaticType::Map {
            key: Box::new(StaticType::Any),
            value: Box::new(StaticType::unit()),
        }
    );
}

#[test]
fn a_function_settles_its_own_type_variables_when_its_body_ends() {
    // `f` returns a list of unknown elements: they fall back to `Any` when
    // `f` ends, so a caller's write can't settle them for every caller.
    assert_eq!(
        analyse_last_type_with_globals(
            "fn f() { [] } let xs = f(); xs[0] = \"a\"; f()",
            index_natives()
        ),
        StaticType::List(Box::new(StaticType::Any))
    );
}

/// `len`, a native taking a `String`, for the test below.
fn len_native() -> Vec<(String, StaticType)> {
    vec![(
        "len".to_string(),
        StaticType::Function {
            parameters: Some(vec![StaticType::String]),
            return_type: Box::new(StaticType::Int),
        },
    )]
}

#[test]
fn a_call_uses_a_variable_standing_nearer_than_a_function() {
    assert_eq!(
        errors("fn foo(x) { 1 } fn test(foo) { foo(2) }", vec![]),
        Vec::<String>::new(),
    );
    assert_eq!(
        errors("fn test(len) { len(\"abc\") }", len_native()),
        Vec::<String>::new(),
    );
}

#[test]
fn a_call_of_a_variable_that_cant_be_called_does_not_reach_the_function() {
    assert_eq!(
        errors("fn test(len: Int) { len(\"abc\") }", len_native()),
        vec!["The variable 'len' is Int, which can't be called"],
    );
}

/// `+`, which augmented assignment needs, for the tests below.
fn plus_native() -> Vec<(String, StaticType)> {
    vec![(
        "+".to_string(),
        StaticType::Function {
            parameters: Some(vec![StaticType::Int, StaticType::Int]),
            return_type: Box::new(StaticType::Int),
        },
    )]
}

#[test]
fn field_reads_and_writes_skip_a_value_named_like_the_field() {
    for source in [
        "struct P { x: Int } fn f(p, x) { p.x }",
        "struct P { x: Int } fn f(p, x) { p.x += 1 }",
    ] {
        assert_eq!(
            errors(source, plus_native()),
            Vec::<String>::new(),
            "{source}"
        );
    }
}

#[test]
fn plain_assignment_to_a_field_is_not_ambiguous_with_a_value_named_like_the_field() {
    assert_eq!(
        errors("struct P { x: Int } fn f(p, x) { p.x = 1 }", vec![]),
        Vec::<String>::new()
    );
}
