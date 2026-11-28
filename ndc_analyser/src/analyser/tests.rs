use super::*;
use itertools::Itertools;
use ndc_analysis::{Binding, Callee, Candidate, ResolvedVar};
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
        ("[]", "<T>([T], Int) -> T"),
        ("[]", "<T>([T], Range) -> [T]"),
        ("[]", "<K, V>(%{K: V}, K) -> V"),
        ("[]=", "<T>([T], Int, T)"),
        ("[]=", "<T>([T], Range, Iterable<T>)"),
        ("[]=", "<K, V>(%{K: V}, K, V)"),
        ("[]=", "(String, Any, String)"),
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
// `Int`, so the tuples they return share no type once they're joined.
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
    // Tuples of different types have no type in common, and `Any` has to
    // be asked for.
    let error = result.errors.iter().exactly_one().expect("one error");
    assert_eq!(
        error.to_string(),
        "values of different types: (String, Int, Int) and (Int, String, Int) share no type"
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
    let append = StaticType::signature("<T>([T], [T]) -> [T]").expect("valid signature");

    assert_eq!(
        analyse_last_type_with_globals(
            "let values = [1]; values ++= [2]; values",
            vec![
                ("++=".to_string(), append.clone()),
                ("++".to_string(), append)
            ],
        ),
        StaticType::List(Box::new(StaticType::Int)),
    );
}

#[test]
fn an_in_place_operator_assignment_cannot_change_the_target_type() {
    let concat = StaticType::signature("<T>([T], [T]) -> [T]").expect("valid signature");

    assert_analysis_error(
        "let values = [1]; values ++= [\"two\"];",
        vec![
            ("++=".to_string(), concat.clone()),
            ("++".to_string(), concat.clone()),
        ],
        "No function called '++' found that matches the arguments '[Int], [String]'",
    );
    assert_analysis_error(
        "let values: [Int] = [1]; values ++= [\"two\"];",
        vec![
            ("++=".to_string(), concat.clone()),
            ("++".to_string(), concat),
        ],
        "No function called '++' found that matches the arguments '[Int], [String]'",
    );
}

#[test]
fn annotated_declaration_types_empty_container_literals() {
    assert_eq!(
        analyse_last_type("let x: %{Int: Int} = %{}; x"),
        StaticType::Map {
            key: Box::new(StaticType::Int),
            value: Box::new(StaticType::Int),
        },
    );
    assert_eq!(
        analyse_last_type("let x: [Int] = []; x"),
        StaticType::List(Box::new(StaticType::Int)),
    );
    assert_eq!(
        analyse_last_type("let x: [[Int]] = [[]]; x"),
        StaticType::List(Box::new(StaticType::List(Box::new(StaticType::Int)))),
    );
}

#[test]
fn annotated_declaration_rejects_a_mismatched_container_literal() {
    assert_analysis_error(
        "let x: %{Int: Int} = %{\"a\": 1};",
        vec![],
        "mismatched types: found %{String: Int} but expected %{Int: Int}",
    );
}

// An `Any` takes a cast to be called, which says what function it holds.
#[test]
fn calling_an_any_takes_a_cast() {
    assert_eq!(
        errors("fn call(f: Any) { f(\"x\") }", vec![]),
        vec!["The variable 'f' is Any, which can't be called"]
    );
    assert_eq!(
        analyse_last_type("fn call(f: Any) { cast::<fn(String) -> Int>(f)(\"x\") } call"),
        function(vec![StaticType::Any], StaticType::Int)
    );
}

#[test]
fn map_default_value_contributes_to_value_type() {
    assert_eq!(
        analyse_last_type("let m = %{:0}; m"),
        StaticType::Map {
            key: Box::new(StaticType::Unknown),
            value: Box::new(StaticType::Int),
        },
    );
}

/// `source` reports exactly one error, containing `expected`, and its last
/// expression's type has the failed check in it. For code after a failed
/// declaration: the names it declared are bound as failed, so using them adds
/// no second error.
fn assert_one_error_then_failed(source: &str, expected: &str) {
    let (last_type, result) = analyse_with_globals(source, vec![]);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    assert!(
        result.errors[0].to_string().contains(expected),
        "expected an error containing {expected:?}, got {:?}",
        result.errors,
    );
    assert!(last_type.has_errors(), "{last_type:?}");
}

#[test]
fn a_pattern_that_fails_to_type_check_binds_every_name_as_failed() {
    for (source, expected) in [
        ("let (a, b, c) = (1, 2); c", "mismatched tuple arity"),
        (
            "let (a, b, c): (Int, Int) = (1, 2); c",
            "mismatched tuple arity",
        ),
        ("let (a, (b, c)) = (1, 2); c", "Invalid unpacking"),
    ] {
        assert_one_error_then_failed(source, expected);
    }
}

#[test]
fn an_annotated_name_the_value_has_no_part_for_keeps_its_annotation() {
    let (last_type, result) =
        analyse_with_globals("let (a, b, c): (Int, Int, Int) = (1, 2); c", vec![]);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    assert_eq!(last_type, StaticType::Int);
}

#[test]
fn a_statement_with_an_unknown_name_binds_its_declarations_as_failed() {
    for source in [
        "fn f() { missing; } let g = f; g",
        "fn f() { missing; } f()",
        // Read from inside another function.
        "fn f() { missing; } fn g() { f } g()",
        "let a = missing; a",
        "let (a, b) = missing; b",
    ] {
        assert_one_error_then_failed(
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
            "fn f(x: Int) { 1 } fn f(x: String) { \"one\" } fn f(x: [Int]) { [1] } f(\"a\")"
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
    // `one()` is an Int that isn't a literal, which would take the type
    // its uses need.
    let globals = [
        index_natives(),
        vec![("+".to_string(), add)],
        natives(&[("one", "() -> Int")]),
    ]
    .concat();
    let scalar = "mismatched types: found Float but expected Int";
    let list = "mismatched types: found [Float] but expected [Int]";
    for (source, expected) in [
        ("let x = one(); x = 0.5;", scalar),
        ("let x = one(); x += 0.5;", scalar),
        (
            "let a = one(); let b = one(); (a, b) = (0.5, one());",
            scalar,
        ),
        ("for x in [one()] { x = 0.5; }", scalar),
        ("let values = [one()]; values[0] = 0.5;", scalar),
        ("let values = [one()]; values[0..1] = [0.5];", list),
        ("let values = [one()]; values[0] += 0.5;", scalar),
        (
            "let values = %{\"one\": one()}; values[\"two\"] = 0.5;",
            scalar,
        ),
    ] {
        assert_eq!(errors(source, globals.clone()), vec![expected], "{source}");
    }
}

#[test]
fn an_any_value_takes_a_cast_to_be_stored_in_a_typed_variable() {
    let any_value = vec![(
        "anything".to_string(),
        StaticType::Function {
            parameters: Some(vec![]),
            return_type: Box::new(StaticType::Any),
        },
    )];
    assert_eq!(
        errors("let x = 3; x = anything(); x", any_value.clone()),
        vec!["mismatched types: found Any but expected Int"]
    );
    assert_eq!(
        analyse_last_type_with_globals(
            "let x = 3; x = cast::<Int>(anything()); x",
            any_value.clone()
        ),
        StaticType::Int
    );
    // Anything goes into an `Any` without one.
    assert_eq!(
        analyse_last_type_with_globals("let x: Any = anything(); x = 3; x", any_value),
        StaticType::Any
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
    let globals = natives(&[("push", "<T>([T], T)")]);
    assert_eq!(
        analyse_last_type_with_globals("let xs = []; push(xs, 1); xs", globals.clone()),
        StaticType::List(Box::new(StaticType::Int))
    );
    assert_eq!(
        errors("let xs = [1]; push(xs, \"a\");", globals),
        vec!["No function called 'push' found that matches the arguments '[Int], String'"]
    );
}

#[test]
fn each_call_instantiates_the_signature_afresh() {
    let globals = natives(&[("first", "<T>([T]) -> T")]);
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
fn an_any_passed_for_a_list_takes_a_cast_that_says_its_elements() {
    // `xs` could hold anything, so `reversed` doesn't take it, but a cast
    // says what it holds, which settles `T`.
    let globals = natives(&[("reversed", "<T>([T]) -> [T]")]);
    assert_eq!(
        errors("fn f(xs: Any) { reversed(xs) }", globals.clone()),
        vec!["No function called 'reversed' found that matches the arguments 'Any'"]
    );
    assert_eq!(
        analyse_last_type_with_globals("fn f(xs: Any) { reversed(cast::<[Int]>(xs)) } f", globals),
        function(
            vec![StaticType::Any],
            StaticType::List(Box::new(StaticType::Int))
        )
    );
}

#[test]
fn a_generic_built_in_used_as_a_value_has_one_type() {
    let globals = natives(&[("push", "<T>([T], T)")]);
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
        vec!["A function of type fn(Int) -> Int doesn't accept the arguments 'String'"]
    );
}

#[test]
fn an_index_write_checks_the_index_against_the_container() {
    let plus = natives(&[("+", "(Int, Int) -> Int")]);
    let globals = [index_natives(), plus].concat();
    for (source, expected) in [
        (
            "let m = %{1: 2}; m[\"a\"] = 3;",
            "No function called '[]=' found that matches the arguments '%{Int: Int}, String, Int'",
        ),
        // Reading the old value fails first, and is the one error reported.
        (
            "let m = %{\"a\": 1}; m[0] += 3;",
            "No function called '[]' found that matches the arguments '%{String: Int}, Int'",
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
        analyse_last_type("let f: fn(Int) -> Int = fn(x: Int) => x; f"),
        int_to_int
    );
    assert_eq!(
        errors("let f: fn(Int) -> Int = fn(x: String) => x;", vec![]),
        vec!["mismatched types: found fn(String) -> String but expected fn(Int) -> Int"]
    );
}

#[test]
fn a_function_argument_gets_what_the_function_parameter_passes() {
    let globals = natives(&[("map", "<T, U>(Iterable<T>, fn(T) -> U) -> [U]")]);
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
            "No function called 'map' found that matches the arguments '[Int], fn(String) -> String'"
        ]
    );
}

#[test]
fn a_function_argument_fits_a_function_parameter_where_their_known_parts_agree() {
    let apply = "fn apply(f: fn(Int) -> Int) { f(1) } ";
    // An untyped lambda's parameters take the types `apply` passes.
    assert_eq!(
        errors(&format!("{apply}apply(fn(x) => x);"), vec![]),
        Vec::<String>::new()
    );
    for (lambda, shown) in [
        ("fn(x: String) => 1", "fn(String) -> Int"),
        ("fn(x) => \"a\"", "fn(Int) -> String"),
        ("fn(a, b) => 1", "fn(Unknown, Unknown) -> Int"),
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
fn a_function_value_rejects_a_callback_taking_another_number_of_arguments() {
    // Called through an expression, so no overload is matched first.
    assert_eq!(
        errors(
            "fn apply(f: fn(Int) -> Int) { f(1) } let fs = [apply]; fs[0](fn(a, b) => 1);",
            index_natives()
        ),
        vec![
            "A function of type fn(fn(Int) -> Int) -> Int doesn't accept the arguments 'fn(Unknown, Unknown) -> Int'"
        ]
    );
}

#[test]
fn a_lambdas_type_picks_between_overloads_taking_functions() {
    let choose = "fn choose(f: fn() -> Int) { 1 } fn choose(f: fn() -> String) { \"s\" } ";
    assert_eq!(
        analyse_last_type(&format!("{choose}choose(fn() => 1)")),
        StaticType::Int
    );
    assert_eq!(
        analyse_last_type(&format!("{choose}choose(fn() => \"a\")")),
        StaticType::String
    );
}

#[test]
fn a_lambda_takes_the_function_type_expected_where_it_is_written() {
    let int_to_int = StaticType::Function {
        parameters: Some(vec![StaticType::Int]),
        return_type: Box::new(StaticType::Int),
    };
    // An annotated variable.
    assert_eq!(
        analyse_last_type("let f: fn(Int) -> Int = fn(x) => x; f"),
        int_to_int
    );
    // A function value's parameter. Either `+` could take `x` until the
    // call says it's a `String`.
    let globals = natives(&[("+", "(Int, Int) -> Int"), ("+", "(Float, Int) -> Float")]);
    assert_eq!(
        errors(
            "let apply = fn(g: fn(String) -> Int) { g(\"a\") }; let fs = [apply]; fs[0](fn(x) => x + 1);",
            [index_natives(), globals].concat()
        ),
        vec!["No function called '+' found that matches the arguments 'String, Int'"]
    );
    // A map's default function returns the map's values.
    assert_eq!(
        analyse_last_type("let m: %{String: [Int]} = %{:fn() => []}; m"),
        StaticType::Map {
            key: Box::new(StaticType::String),
            value: Box::new(StaticType::List(Box::new(StaticType::Int))),
        }
    );
}

#[test]
fn an_any_parameter_settles_what_its_argument_leaves_unknown_as_any() {
    // `keep` may store anything in the list, so it can't become a [Int].
    let globals = [natives(&[("keep", "([Any])")]), index_natives()].concat();
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
            key: Box::new(StaticType::Unknown),
            value: Box::new(StaticType::Int),
        }
    );
}

#[test]
fn what_an_empty_literal_holds_is_unknown_when_nothing_says() {
    assert_eq!(
        analyse_last_type("let xs = []; xs"),
        StaticType::List(Box::new(StaticType::Unknown))
    );
    assert_eq!(
        analyse_last_type("None"),
        StaticType::Option(Box::new(StaticType::Unknown))
    );
    assert_eq!(
        analyse_last_type("%{}"),
        StaticType::Map {
            key: Box::new(StaticType::Unknown),
            value: Box::new(StaticType::Unknown),
        }
    );
}

#[test]
fn a_function_settles_its_own_type_variables_when_its_body_ends() {
    // `f` returns a list of whatever each call's caller stores in it, so
    // one caller's write doesn't settle it for another: the last call's
    // list holds nothing known.
    assert_eq!(
        analyse_last_type_with_globals(
            "fn f() { [] } let xs = f(); xs[0] = \"a\"; f()",
            index_natives()
        ),
        StaticType::List(Box::new(StaticType::Unknown))
    );
}

/// `+` with two overloads that both take an `Int` on the right, so `x + 1`
/// can't be picked until `x`'s type is known.
fn ambiguous_plus() -> Vec<(String, StaticType)> {
    natives(&[("+", "(Int, Int) -> Int"), ("+", "(Float, Int) -> Float")])
}

fn function(parameters: Vec<StaticType>, return_type: StaticType) -> StaticType {
    StaticType::Function {
        parameters: Some(parameters),
        return_type: Box::new(return_type),
    }
}

#[test]
fn a_call_waits_until_its_arguments_pick_an_overload() {
    // Either `+` could take `x` until `f` is called with a Float.
    assert_eq!(
        analyse_last_type_with_globals("let f = fn(x) => x + 1; f(2.5); f", ambiguous_plus()),
        function(vec![StaticType::Float], StaticType::Float)
    );
}

#[test]
fn a_call_commits_to_the_only_overload_that_can_take_its_arguments() {
    let globals = natives(&[
        ("plus", "(Int, Int) -> Int"),
        ("plus", "(String, String) -> String"),
    ]);
    assert_eq!(
        analyse_last_type_with_globals("let f = fn(x) => plus(x, 1); f", globals),
        function(vec![StaticType::Int], StaticType::Int)
    );
}

#[test]
fn a_lambda_passed_to_a_function_takes_its_parameter_types_from_the_call() {
    let globals = [
        natives(&[("map", "<T, U>([T], fn(T) -> U) -> [U]")]),
        ambiguous_plus(),
    ]
    .concat();
    assert_eq!(
        analyse_last_type_with_globals("[2.5].map(fn(x) => x + 1)", globals),
        StaticType::List(Box::new(StaticType::Float))
    );
}

#[test]
fn a_lambda_in_a_variable_has_one_type() {
    assert_eq!(
        errors(
            "let f = fn(x) => x + 1; f(one()); f(2.5);",
            [ambiguous_plus(), natives(&[("one", "() -> Int")])].concat()
        ),
        vec!["The variable 'f' holds a function that doesn't accept the arguments 'Float'"]
    );
}

#[test]
fn a_variable_holding_an_unknown_function_takes_what_it_is_called_with() {
    assert_eq!(
        analyse_last_type_with_globals(
            "let apply = fn(g, v) => g(v); apply(fn(z) => z + 1, 2.5)",
            ambiguous_plus()
        ),
        StaticType::Float
    );
    // Each later call goes through the type the first one settled.
    assert_eq!(
        analyse_last_type_with_globals(
            "let apply = fn(g, v) { g(v); g(v) }; apply(fn(z) => z + 1, 2.5)",
            ambiguous_plus()
        ),
        StaticType::Float
    );
    assert_eq!(
        errors("let apply = fn(g) { g(1); g(\"s\") };", ambiguous_plus()),
        vec!["The variable 'g' holds a function that doesn't accept the arguments 'String'"]
    );
    // A function that would have to take itself stays unknown, and the
    // call is checked as the program runs.
    assert_eq!(
        errors("let f = fn(g) => g(g);", vec![]),
        Vec::<String>::new()
    );
}

#[test]
fn iterating_waits_until_the_kind_of_sequence_is_known() {
    let globals = [natives(&[("push", "<T>([T], T)")]), ambiguous_plus()].concat();
    assert_eq!(
        analyse_last_type_with_globals(
            "let g = fn(rows) { let out = []; for r in rows { out.push(r + 1) }; out }; g([2.5])",
            globals
        ),
        StaticType::List(Box::new(StaticType::Float))
    );
}

#[test]
fn unpacking_waits_until_the_kind_of_value_is_known() {
    assert_eq!(
        analyse_last_type("let swap = fn(p) { let (a, b) = p; (b, a) }; swap((1, \"s\"))"),
        StaticType::Tuple(vec![StaticType::String, StaticType::Int])
    );
    assert_eq!(
        errors("let f = fn(p) { let (a, b) = p; a }; f((1, 2, 3));", vec![]),
        vec!["mismatched tuple arity: 2 names can't take apart a tuple of 3 values"]
    );
}

#[test]
fn what_iterating_or_unpacking_gives_must_fit_how_it_was_used_meanwhile() {
    // `plus(r, 1)` and `plus(a, 1)` make `r` and `a` Ints before the call
    // settles what they are.
    let globals = natives(&[
        ("plus", "(Int, Int) -> Int"),
        ("plus", "(String, String) -> String"),
    ]);
    assert_eq!(
        errors(
            "let g = fn(rows) { for r in rows { plus(r, 1); } }; g([\"s\"]);",
            globals.clone()
        ),
        vec!["mismatched types: found String but expected Int"]
    );
    assert_eq!(
        errors(
            "let f = fn(p) { let (a, b) = p; plus(a, 1) }; f((\"s\", 2));",
            globals
        ),
        vec!["mismatched types: found String but expected Int"]
    );
}

#[test]
fn an_op_assignment_waits_for_its_operands_like_any_call() {
    // `t += 1` doesn't fix `t` as an `Int` before the call says what `x` is.
    assert_eq!(
        analyse_last_type_with_globals(
            "let f = fn(x) { let t = x; t += 1; t }; f(2.5)",
            ambiguous_plus()
        ),
        StaticType::Float
    );
    // The in-place overload is picked along with the operator once they're
    // known, as `++=` is here: the natives' global slots are in this order.
    let globals = natives(&[
        ("++", "<T>([T], [T]) -> [T]"),
        ("++", "(String, String) -> String"),
        ("++=", "<T>([T], [T]) -> [T]"),
    ]);
    let (_, result) = analyse_with_globals("let f = fn(xs) { xs ++= [1]; }; f([2]);", globals);
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert_eq!(
        result
            .analysis
            .augmented_assignments
            .values()
            .collect::<Vec<_>>(),
        vec![&Binding::Resolved(Candidate::Scalar(Callee::Var(
            ResolvedVar::Global { slot: 2 }
        )))]
    );
}

#[test]
fn a_call_only_one_overload_could_take_commits_to_it() {
    // Only the map's `[]` takes a map, so `m["k"]` is its, which makes the
    // keys Strings, though nothing is known about them before.
    assert_eq!(
        analyse_last_type_with_globals("let m = %{}; m[\"k\"]; m", index_natives()),
        StaticType::Map {
            key: Box::new(StaticType::String),
            value: Box::new(StaticType::Unknown),
        }
    );
}

#[test]
fn a_call_nothing_else_settles_takes_no_narrowest_overload_when_unsure() {
    let function = |parameter, returns| StaticType::Function {
        parameters: Some(vec![parameter]),
        return_type: Box::new(returns),
    };
    // Nothing is known about `x`, so it's `Unknown`, which only `pick(Any)`
    // takes.
    assert_eq!(
        analyse_last_type(
            "fn pick(x: Int) => 1; fn pick(x: Any) => \"any\"; let f = fn(x) => pick(x); f"
        ),
        function(StaticType::Unknown, StaticType::String)
    );
    // Overloads in two scopes fit, so none settles `x`: it's `Unknown`, and
    // `[x]` is a `[Unknown]`, which neither takes.
    assert_eq!(
        errors(
            "fn pick(xs: [String]) => \"outer\"; { fn pick(xs: [Int]) => 1; let f = fn(x) => pick([x]); f }",
            vec![]
        ),
        vec!["No function called 'pick' found that matches the arguments '[Unknown]'"]
    );
}

#[test]
fn a_waiting_call_does_not_commit_to_a_narrower_overload_too_early() {
    // `pick(x)` waits: `x` may still turn out to be what only `pick(Any)` takes.
    assert_eq!(
        analyse_last_type(
            "fn pick(x: Int) => 1; fn pick(x: Any) => \"any\"; let f = fn(x) => pick(x); f(\"s\")"
        ),
        StaticType::String
    );
    // The nearest scope's `pick` takes the call, however narrow the outer one is.
    assert_eq!(
        analyse_last_type(
            "fn pick(x: Int) => 1; { fn pick(x: Any) => \"inner\"; let f = fn(x) => pick(x); f(1) }"
        ),
        StaticType::String
    );
}

#[test]
fn a_failed_op_assignment_is_reported_once() {
    let globals = [
        natives(&[("++", "(String, String) -> String")]),
        index_natives(),
    ]
    .concat();
    assert_eq!(
        errors("let g = fn(xs) { xs[0] ++= \"a\"; }; g([1]);", globals),
        vec!["No function called '++' found that matches the arguments 'Int, String'"]
    );
}

#[test]
fn a_field_write_settles_the_receiver_like_a_field_read() {
    let structs = "struct P { score: Int } struct Q { score: String } ";
    assert_eq!(
        errors(
            &format!("{structs}let f = fn(p) {{ p.score = 3; p }}; f(Q(\"a\"));"),
            vec![]
        ),
        vec!["The variable 'f' holds a function that doesn't accept the arguments 'Q'"]
    );
    // A value that doesn't fit the field is reported once, against the field.
    assert_eq!(
        errors(
            &format!("{structs}let f = fn(p) {{ p.score = 1.5; }}; f(P(1));"),
            vec![]
        ),
        vec!["mismatched types: found Float but expected Int"]
    );
}

#[test]
fn an_error_shows_what_isnt_known_yet_as_unknown() {
    assert_eq!(
        errors("let f = fn(x) -> Int { [x] };", vec![]),
        vec!["mismatched types: found [Unknown] but expected Int"]
    );
}

#[test]
fn a_cast_settles_what_isnt_known_about_its_value() {
    let (_, result) = analyse_with_globals("cast::<fn(Int) -> Int>(fn(x) => x)", vec![]);
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    // `x` is an Int, so the cast holds without a check when the program runs.
    assert_eq!(
        result.analysis.cast_checks.values().collect::<Vec<_>>(),
        vec![&None]
    );
}

#[test]
fn a_waiting_call_does_not_commit_to_an_overload_taking_any_instead() {
    // While the list's elements aren't known, both `[]=` might take the
    // write, so it waits. Once they're Ints, only the catch-all takes a list
    // as an element: the element-wise `[]=` would need its `T` to be both an
    // Int and a list.
    let globals = [natives(&[("[]=", "<T>([T], Any, Any)")]), index_natives()].concat();
    assert_eq!(
        errors("let xs = [1]; xs[0] = [];", globals.clone()),
        Vec::<String>::new()
    );
    // Without the catch-all, nothing takes it.
    assert_eq!(
        errors("let xs = [1]; xs[0] = [];", index_natives()),
        vec!["mismatched types: found [Unknown] but expected Int"]
    );
}

#[test]
fn an_operator_waits_while_an_operand_could_still_be_a_tuple() {
    // `x ++ "!"` could also apply to each string of a tuple.
    let globals = natives(&[("++", "(String, String) -> String")]);
    assert_eq!(
        analyse_last_type_with_globals(
            "let suffix = fn(x) => x ++ \"!\"; suffix((\"a\", \"b\"))",
            globals
        ),
        StaticType::Tuple(vec![StaticType::String, StaticType::String])
    );
}

#[test]
fn an_operator_whose_overload_takes_tuples_whole_does_not_wait() {
    // Whatever `a` and `b` are, tuples too, `==` compares them whole.
    let globals = natives(&[("==", "<A, B>(A, B) -> Bool")]);
    let (_, result) = analyse_with_globals("fn same(a, b) => a == b;", globals.clone());
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    let returns = result
        .analysis
        .inferred_return_types
        .values()
        .map(ToString::to_string)
        .collect::<Vec<_>>();
    assert_eq!(returns, ["Bool"]);
    assert_eq!(
        analyse_last_type_with_globals("let one = fn(x) => x == 1; one((1, 2))", globals),
        StaticType::Bool
    );
}

#[test]
fn a_call_whose_overloads_all_return_one_type_returns_it_before_the_pick() {
    // Whichever `to_int` `x` picks, it returns an Int; `echo`'s depend on it.
    let globals = natives(&[
        ("to_int", "(Float) -> Int"),
        ("to_int", "(String) -> Int"),
        ("echo", "(Float) -> Float"),
        ("echo", "(String) -> String"),
    ]);
    let returns = |source: &str| {
        let (_, result) = analyse_with_globals(source, globals.clone());
        assert!(result.errors.is_empty(), "{:?}", result.errors);
        let returns = result.analysis.inferred_return_types.values();
        returns.map(ToString::to_string).collect::<Vec<_>>()
    };
    assert_eq!(returns("fn f(x) { let n = to_int(x); n }"), ["Int"]);
    assert_eq!(returns("fn f(x) { let n = x.to_int; n }"), ["Int"]);
    assert_ne!(returns("fn f(x) { let s = echo(x); s }"), ["Float"]);
    assert_eq!(
        analyse_last_type_with_globals("fn f(x) => to_int(x); f(\"1\")", globals.clone()),
        StaticType::Int
    );
}

#[test]
fn an_operator_waits_while_a_tuple_operand_would_not_fit_its_overload() {
    // `x` must be a String for `in` to take it whole, so a tuple of them
    // is looked up one element at a time.
    let globals = natives(&[("in", "<T>(T, Iterable<T>) -> Bool")]);
    assert_eq!(
        analyse_last_type_with_globals(
            "let vowel = fn(x) => x in \"aeiou\"; vowel((\"a\", \"z\"))",
            globals
        ),
        StaticType::Tuple(vec![StaticType::Bool, StaticType::Bool])
    );
}

#[test]
fn an_operator_takes_no_any_operand_that_no_overload_takes() {
    // `a` could be a tuple that `++` applies to element-wise, or a list,
    // but it takes a cast to be either.
    let globals = natives(&[("++", "<T>([T], [T]) -> [T]")]);
    assert_eq!(
        errors("let g = fn(a: Any) => a ++ [];", globals.clone()),
        vec!["No function called '++' found that matches the arguments 'Any, [Unknown]'"]
    );
    let (_, result) =
        analyse_with_globals("let g = fn(a: Any) => cast::<[Int]>(a) ++ [];", globals);
    assert!(result.errors.is_empty(), "{:?}", result.errors);
}

#[test]
fn a_write_passes_its_value_like_an_argument() {
    // Any sequence goes where an `Iterable<Int>` is expected, and settles its
    // elements as Ints.
    assert_eq!(
        analyse_last_type_with_globals(
            "let ys = []; total(ys); ys",
            natives(&[("total", "(Iterable<Int>) -> Int")])
        ),
        StaticType::List(Box::new(StaticType::Int))
    );
}

#[test]
fn a_map_default_waits_until_it_is_known_whether_it_makes_the_values() {
    let globals = [natives(&[("+", "(Int, Int) -> Int")]), index_natives()].concat();
    assert_eq!(
        analyse_last_type_with_globals(
            "let make = fn(factory) => %{:factory}; let m = make(fn() => 1); m[\"x\"] + 1",
            globals
        ),
        StaticType::Int
    );
}

#[test]
fn a_write_that_does_not_fit_a_number_default_is_the_mismatch() {
    let source = "let m = %{:0}; m[\"a\"] = \"many\";";
    let (_, result) = analyse_with_globals(source, index_natives());
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    let error = &result.errors[0];
    assert_eq!(&source[error.span().range()], "m[\"a\"] = \"many\"");
}

#[test]
fn a_number_default_is_settled_where_the_map_is_stored() {
    // `m` is a map of Ints from its `let`, so a Float written later is an
    // error rather than what makes the `0` a Float.
    assert_eq!(
        analyse_last_type_with_globals("let m = %{:0}; m[\"a\"] = 1; m", index_natives()),
        StaticType::Map {
            key: Box::new(StaticType::String),
            value: Box::new(StaticType::Int),
        },
    );
}

/// `split` as the stdlib has it: one overload splits on whitespace, the
/// other on a pattern.
fn split_natives() -> Vec<(String, StaticType)> {
    natives(&[
        ("split", "(String) -> [String]"),
        ("split", "(String, String) -> [String]"),
        ("map", "<T, U>([T], fn(T) -> U) -> [U]"),
    ])
}

#[test]
fn an_overloaded_function_value_is_the_overload_its_use_expects() {
    let list_of_lists = StaticType::List(Box::new(StaticType::List(Box::new(StaticType::String))));
    assert_eq!(
        analyse_last_type_with_globals("[\"a b\"].map(split)", split_natives()),
        list_of_lists
    );
    // A variable's uses say what it is.
    assert_eq!(
        analyse_last_type_with_globals("let f = split; f(\"a b\"); f", split_natives()),
        StaticType::Function {
            parameters: Some(vec![StaticType::String]),
            return_type: Box::new(StaticType::List(Box::new(StaticType::String))),
        }
    );
}

#[test]
fn a_return_annotation_picks_the_overloaded_function_value_returned() {
    assert_eq!(
        errors(
            "fn by_pattern() -> fn(String, String) -> [String] { split }",
            split_natives()
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_return_annotation_settles_what_the_body_left_unknown() {
    assert_eq!(
        errors("fn f() -> [Int] { let xs = []; xs }", vec![]),
        Vec::<String>::new()
    );
    // Each value returned has to fit the annotation.
    assert_eq!(
        errors("fn f(x) -> Int { if x { return \"s\"; } 1 }", vec![]),
        vec!["mismatched types: found String but expected Int"]
    );
}

#[test]
fn the_help_for_an_ambiguous_function_value_names_an_overload_that_fits() {
    let help = |source: &str, globals| {
        let (_, result) = analyse_with_globals(source, globals);
        result.errors[0].help_text().map(str::to_string)
    };
    // A type parameter can't be written in an annotation.
    assert_eq!(
        help(
            "let f = push;",
            natives(&[("push", "<T>([T], T)"), ("push", "<T>(MinHeap<T>, T)")])
        ),
        Some(
            "Say which one with a function type, as in `let f: fn(MinHeap<Any>, Any) -> () = push`"
                .to_string()
        )
    );
}

#[test]
fn a_function_value_is_not_picked_by_its_return_type_before_its_parameters_are_known() {
    // Only `pick(Any)` returns an Int, but once `xs` holds Ints a call with
    // an Int means `pick(Int)`, so that's the one meant, and it doesn't fit.
    let globals = natives(&[
        ("map", "<T, U>([T], fn(T) -> U) -> [U]"),
        ("push", "<T>([T], T)"),
    ]);
    assert_eq!(
        errors(
            "fn pick(x) => 0; fn pick(x: Int) => \"s\"; let xs = []; let ys: [Int] = xs.map(pick); xs.push(1);",
            globals
        ),
        vec!["No overload of 'pick' is a fn(Int) -> Int"]
    );
}

#[test]
fn a_function_value_commits_when_only_one_overload_can_take_the_arguments() {
    // `map` calls `split` with one argument, which only `split(String)`
    // takes, so it's meant even though nothing says what `xs` holds.
    assert_eq!(
        errors("let xs = []; xs.map(split);", split_natives()),
        Vec::<String>::new()
    );
}

#[test]
fn an_overloaded_function_value_nothing_picks_is_an_error() {
    assert_eq!(
        errors("let f = split;", split_natives()),
        vec!["'split' has several overloads, and nothing here says which one is meant"]
    );
    assert_eq!(
        errors("let f: fn(Int) -> Int = split;", split_natives()),
        vec!["No overload of 'split' is a fn(Int) -> Int"]
    );
    // `split(String)` takes the String, but doesn't return an Int.
    assert_eq!(
        errors("let f: fn(String) -> Int = split;", split_natives()),
        vec!["No overload of 'split' is a fn(String) -> Int"]
    );
}

#[test]
fn an_overloaded_function_value_uses_the_nearest_scope_with_an_overload_that_fits() {
    // The inner `pick` doesn't take an Int, so the outer one is meant.
    assert_eq!(
        analyse_last_type(
            "fn pick(x: Int) => 1; { fn pick(x: String) => \"s\"; let f: fn(Int) -> Int = pick; f(1) }"
        ),
        StaticType::Int
    );
    // When both fit, the nearer one is meant, which
    // 005_functions/067 shows by what the program prints.
}

#[test]
fn a_join_waits_until_both_branches_are_known() {
    // `x` isn't fixed as an `Int` by the branch next to it: the branches join
    // once the call says what `x` is.
    assert_eq!(
        analyse_last_type("let f = fn(x) => if true { x } else { 1 }; f(2)"),
        StaticType::Int
    );
    // The literal `1` becomes a Float like `x`, but a String and an Int
    // share no type.
    assert_eq!(
        analyse_last_type("let f = fn(x) => if true { x } else { 1 }; f(2.5)"),
        StaticType::Float
    );
    let (_, result) = analyse_with_globals(
        "let f = fn(x) => if true { x } else { 1 }; f(\"a\")",
        vec![],
    );
    let error = result.errors.iter().exactly_one().expect("one error");
    assert!(
        error.to_string().starts_with("values of different types"),
        "{error}"
    );
    // What both branches share is known meanwhile: two lists join as a list
    // that `push` can take.
    assert_eq!(
        analyse_last_type_with_globals(
            "let xs = if true { [] } else { [] }; xs.push(1); xs",
            natives(&[("push", "<T>([T], T)"), ("push", "<T>(MinHeap<T>, T)")])
        ),
        StaticType::List(Box::new(StaticType::Int))
    );
}

#[test]
fn a_parameter_nothing_settles_is_unknown_which_no_overload_takes() {
    // Nothing says what `x` is, so it's `Unknown`, which neither `+` takes.
    assert_eq!(
        errors("let f = fn(x) => x + 1; f", ambiguous_plus()),
        vec!["No function called '+' found that matches the arguments 'Unknown, Int'"]
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

#[test]
fn a_generic_function_has_the_type_its_body_makes_of_each_call() {
    for (call, typ) in [("inc(1)", StaticType::Int), ("inc(2.5)", StaticType::Float)] {
        assert_eq!(
            analyse_last_type_with_globals(
                &format!("fn inc(x) => x + 1; inc(1); inc(2.5); {call}"),
                ambiguous_plus()
            ),
            typ,
            "{call}"
        );
    }
}

#[test]
fn a_generic_function_whose_calls_depend_on_its_types_runs_only_as_instances() {
    // `inc`'s `+` depends on what `x` is, so it has instances, none yet,
    // and its own body picks nothing.
    let (_, result) = analyse_with_globals("fn inc(x) => x + 1;", ambiguous_plus());
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert_eq!(
        result
            .analysis
            .instances
            .values()
            .map(Vec::len)
            .collect::<Vec<_>>(),
        [0]
    );
}

#[test]
fn a_generic_function_calling_one_has_its_constraints_too() {
    assert_eq!(
        analyse_last_type_with_globals(
            "fn inc(x) => x + 1; fn twice(x) => inc(inc(x)); twice(2.5)",
            ambiguous_plus()
        ),
        StaticType::Float
    );
}

#[test]
fn an_error_in_a_generic_body_is_the_outermost_calls() {
    let source = "fn inc(x) => x + 1; fn twice(x) => inc(inc(x)); twice(\"a\");";
    let (_, result) = analyse_with_globals(source, ambiguous_plus());
    let [error] = &result.errors[..] else {
        panic!("one error expected: {:?}", result.errors);
    };
    let text = |span: Span| &source[span.range()];
    assert_eq!(text(error.span()), "twice(\"a\")");
    assert_eq!(
        error
            .notes()
            .iter()
            .map(|(span, note)| (text(*span), note.as_str()))
            .collect::<Vec<_>>(),
        vec![
            ("inc(x)", "`twice` calls `inc` here"),
            ("x + 1", "`inc` needs this"),
        ]
    );
}

#[test]
fn a_generic_function_keeps_one_copy_of_each_call_it_copies() {
    // Each function calls the one before twice. With a copy of each call's
    // constraints, the last would have 2^40.
    let mut source = "fn f0(x) => x + 1;".to_string();
    for i in 1..40 {
        source.push_str(&format!("fn f{i}(x) => f{}(x) + f{}(x);", i - 1, i - 1));
    }
    source.push_str("f39(2.5)");
    assert_eq!(
        analyse_last_type_with_globals(
            &source,
            natives(&[
                ("+", "(Int, Int) -> Int"),
                ("+", "(Float, Int) -> Float"),
                ("+", "(Float, Float) -> Float"),
            ])
        ),
        StaticType::Float
    );
}

#[test]
fn a_variable_from_outside_has_one_type_in_a_generic_function() {
    // `n` is stored in `xs`, which the first call settles.
    let globals = natives(&[("push", "<T>([T], T)")]);
    assert_eq!(
        analyse_last_type_with_globals(
            "let xs = []; fn f(n) { xs.push(n) } f(1); xs",
            globals.clone()
        ),
        StaticType::List(Box::new(StaticType::Int))
    );
    assert_eq!(
        errors(
            "let xs = []; fn f(n) { xs.push(n) } f(1); f(\"a\");",
            globals
        ),
        vec!["No function called 'f' found that matches the arguments 'String'"]
    );
}

#[test]
fn what_no_call_can_decide_is_decided_in_the_body() {
    // The map's types can't depend on how `count` is called, so they're
    // settled where it's declared, as at the end of the program.
    let mut globals = index_natives();
    globals.extend(natives(&[("+", "(Int, Int) -> Int")]));
    assert_eq!(
        analyse_last_type_with_globals(
            "fn count() { let m = %{}; m[\"k\"] = 1; m } count()",
            globals
        ),
        StaticType::Map {
            key: Box::new(StaticType::String),
            value: Box::new(StaticType::Int),
        }
    );
}

#[test]
fn a_recursive_generic_function_returns_what_its_branches_join_to() {
    let globals = natives(&[
        ("<", "(Int, Int) -> Bool"),
        ("<", "(Float, Int) -> Bool"),
        ("-", "(Int, Int) -> Int"),
        ("-", "(Float, Int) -> Float"),
        ("+", "(Int, Int) -> Int"),
        ("+", "(Float, Float) -> Float"),
    ]);
    for (argument, typ) in [("10", StaticType::Int), ("10.0", StaticType::Float)] {
        assert_eq!(
            analyse_last_type_with_globals(
                &format!(
                    "fn fib(n) => if n < 2 {{ n }} else {{ fib(n - 1) + fib(n - 2) }}; fib({argument})"
                ),
                globals.clone()
            ),
            typ,
        );
    }
}

#[test]
fn overloads_tie_when_a_call_could_fit_both_and_neither_is_more_specific() {
    for (source, ties) in [
        ("fn f(x: Int, y) {} fn f(x, y: Int) {}", true),
        ("fn f(x: Int, y: Any) {} fn f(x: Any, y: Int) {}", false),
        ("fn f(x: Int) {} fn f(x) {}", false),
        ("fn f(x: [Int]) {} fn f(x) {}", false),
        ("fn f(x: Int) {} fn f(x: Any) {}", false),
        ("fn f(x: [Int]) {} fn f(x: [String]) {}", false),
        ("fn f(x) {} fn f(x, y) {}", false),
    ] {
        let errors = errors(source, vec![]);
        assert_eq!(
            errors
                .iter()
                .any(|error| error.contains("neither is more specific")),
            ties,
            "{source}: {errors:?}"
        );
    }
    // Two unannotated parameters are a redefinition, as two `Any`s are.
    assert_eq!(
        errors("fn f(x) {} fn f(y) {}", vec![]),
        vec!["Illegal redefinition of function 'f' with parameters (Any) in the same scope"]
    );
}

#[test]
fn a_lambda_a_generic_function_passes_on_takes_what_each_call_gives_it() {
    // `map` waits for what `xs` is, so its lambda's types are only known
    // once `incs` is called.
    let mut globals = natives(&[
        ("map", "<T, U>([T], fn(T) -> U) -> [U]"),
        ("map", "<T, U>(Deque<T>, fn(T) -> U) -> [U]"),
    ]);
    globals.extend(ambiguous_plus());
    assert_eq!(
        analyse_last_type_with_globals(
            "fn incs(xs) => xs.map(fn(x) => x + 1); incs([1]); incs([2.5])",
            globals
        ),
        StaticType::List(Box::new(StaticType::Float))
    );
}

#[test]
fn an_overloaded_function_value_in_a_generic_function_is_picked_for_each_call() {
    // Each instance of `f` picks the `abs` its argument's type calls for.
    let globals = natives(&[("abs", "(Int) -> Int"), ("abs", "(Float) -> Float")]);
    let source = "fn f(x) { let g = abs; g(x) }";
    assert_eq!(
        analyse_last_type_with_globals(&format!("{source} f(1)"), globals.clone()),
        StaticType::Int
    );
    assert_eq!(
        analyse_last_type_with_globals(&format!("{source} f(2.5)"), globals.clone()),
        StaticType::Float
    );
    assert_eq!(
        errors(
            "fn f(x) { let g: fn(Int) -> Int = abs; g(x) } f(1);",
            globals
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_generic_function_stored_in_a_variable_keeps_its_constraints() {
    assert_eq!(
        analyse_last_type_with_globals("fn inc(x) => x + 1; let h = inc; h(2.5)", ambiguous_plus()),
        StaticType::Float
    );
    assert_eq!(
        errors(
            "fn inc(x) => x + 1; let h = inc; h(\"a\");",
            ambiguous_plus()
        ),
        vec!["No function called '+' found that matches the arguments 'String, Int'"]
    );
}

/// How many instances of generic functions analysing `source` makes.
fn instance_count(source: &str, globals: Vec<(String, StaticType)>) -> usize {
    let (_, result) = analyse_with_globals(source, globals);
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    result.analysis.instances.values().map(Vec::len).sum()
}

#[test]
fn each_set_of_key_types_gets_one_instance() {
    let (_, result) = analyse_with_globals(
        "fn inc(x) => x + 1; inc(1); inc(2.5); inc(3);",
        ambiguous_plus(),
    );
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    let instances = result
        .analysis
        .instances
        .values()
        .flatten()
        .collect::<Vec<_>>();
    assert_eq!(instances.len(), 2);
    // Each picks its `+` from its types.
    for instance in instances {
        assert!(
            instance
                .bindings
                .values()
                .any(|binding| matches!(binding, Binding::Resolved(_))),
            "{instance:?}"
        );
    }
}

#[test]
fn a_function_whose_calls_dont_depend_on_its_types_has_no_instances() {
    assert_eq!(
        instance_count("fn wrap(x) => [x]; wrap(1); wrap(\"a\");", vec![]),
        0
    );
}

#[test]
fn a_call_with_an_any_argument_checks_the_body_with_any() {
    // `x` is `Any` in this call, which no `+` takes.
    assert_eq!(
        errors(
            "fn inc(x) => x + 1; let a: Any = 1; inc(a);",
            ambiguous_plus()
        ),
        vec!["No function called '+' found that matches the arguments 'Any, Int'"]
    );
    // A body that works with `Any` takes it.
    assert_eq!(
        instance_count(
            "fn wrap(x) => [x]; let a: Any = 1; wrap(a);",
            ambiguous_plus()
        ),
        0
    );
}

#[test]
fn an_instance_calls_the_instances_its_types_need() {
    // `twice(2.5)` needs `twice` for Floats, which needs `inc` for Floats.
    let (_, result) = analyse_with_globals(
        "fn inc(x) => x + 1; fn twice(x) => inc(inc(x)); twice(2.5);",
        ambiguous_plus(),
    );
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert_eq!(
        result.analysis.instances.len(),
        2,
        "{:?}",
        result.analysis.instances
    );
    let twice = result
        .analysis
        .instances
        .values()
        .flatten()
        .find(|instance| instance.bindings.len() == 2)
        .expect("twice's instance picks its two calls of inc");
    let inc = result
        .analysis
        .instances
        .values()
        .flatten()
        .find(|instance| instance.bindings.len() == 1)
        .expect("inc's instance picks its +");
    assert!(
        twice
            .bindings
            .values()
            .all(|binding| *binding
                == Binding::Resolved(Candidate::Scalar(Callee::Var(inc.variable)))),
        "{twice:?}"
    );
}

#[test]
fn an_instance_calls_itself_where_its_body_recurses() {
    // `n < 2` compares with an Int whatever `n` is, so `fib` is generic
    // only if `<` compares an Int with more than an Int, as it does.
    let globals = natives(&[
        ("<", "(Int, Int) -> Bool"),
        ("<", "(Float, Int) -> Bool"),
        ("-", "(Int, Int) -> Int"),
        ("+", "(Int, Int) -> Int"),
    ]);
    let (_, result) = analyse_with_globals(
        "fn fib(n) => if n < 2 { n } else { fib(n - 1) + fib(n - 2) }; fib(10);",
        globals,
    );
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    let [instance] = &result
        .analysis
        .instances
        .values()
        .flatten()
        .collect::<Vec<_>>()[..]
    else {
        panic!("one instance expected: {:?}", result.analysis.instances);
    };
    let calls_itself = instance
        .bindings
        .values()
        .filter(|binding| {
            **binding == Binding::Resolved(Candidate::Scalar(Callee::Var(instance.variable)))
        })
        .count();
    assert_eq!(calls_itself, 2, "{instance:?}");
}

#[test]
fn a_list_literal_holding_the_list_it_is_pushed_to_is_analysed() {
    // `[xs]` holds a list of what it is itself, which settling its type
    // mustn't recurse into forever.
    for source in [
        "let xs = []; xs.push([xs]);",
        "let xs = [\"s\"]; xs = [xs];",
    ] {
        let (_, result) = analyse_with_globals(source, natives(&[("push", "<T>([T], T)")]));
        assert!(result.errors.len() <= 1, "{source}: {:?}", result.errors);
    }
}

#[test]
fn a_call_no_overload_takes_says_what_the_overloads_take() {
    // `put`'s body puts `x` in `r`, which the first call settles as holding
    // Ints, so `put` takes an Int from then on.
    let (_, result) = analyse_with_globals(
        "let r = []; fn put(x) { r.push(x); 0 } put(1); put(\"a\");",
        natives(&[("push", "<T>([T], T)")]),
    );
    let help = result
        .errors
        .iter()
        .find_map(|error| error.help_text().map(str::to_string));
    assert_eq!(
        help.as_deref(),
        Some("`put` takes (Int)"),
        "{:?}",
        result.errors
    );
}

/// Natives for the tests of what's built from a failed check.
fn failed_check_natives() -> Vec<(String, StaticType)> {
    let mut globals = natives(&[
        ("len", "<T>(Iterable<T>) -> Int"),
        ("map", "<T, U>(Iterable<T>, fn(T) -> U) -> [U]"),
        ("sum", "(Iterable<Int>) -> Int"),
        ("sum", "(Iterable<Float>) -> Float"),
        ("abs", "(Int) -> Int"),
        ("abs", "(Float) -> Float"),
        ("++", "<T>([T], [T]) -> [T]"),
        ("++", "(String, String) -> String"),
    ]);
    globals.extend(index_natives());
    globals
}

#[test]
fn what_is_built_from_a_failed_call_is_not_reported_again() {
    let globals = failed_check_natives();
    assert_eq!(
        errors(
            "struct Player { name: String, score: Int }
            let players = [Player(\"ann\", 3), Player(\"bob\", 5)];
            players.map(fn(p) => p.len).sum();",
            globals,
        ),
        vec!["No function called 'len' found that matches the arguments 'Player'"]
    );
}

#[test]
fn a_failed_call_is_reported_once_wherever_its_value_goes() {
    let globals = failed_check_natives();
    let failed = "No function called 'len' found that matches the arguments 'Bool'";
    for source in [
        // Indexed.
        "let x = len(true)[0]; [x].sum();",
        // Iterated.
        "for x in len(true) { [x].sum(); }",
        "let xs: [String] = [x for x in len(true)];",
        // Passed on.
        "[len(true)].sum();",
        "len(true).map(abs);",
        "len(true).map(fn(x) => [x]).sum();",
        "abs(len(true)) ++ \"a\";",
        // Assigned to a typed variable.
        "let xs: [String] = [len(true)];",
        "let s: String = len(true);",
        // Called.
        "let f = len(true); [f(1)].sum();",
        // Unpacked.
        "let (a, b) = len(true); [a, b].sum();",
    ] {
        assert_eq!(errors(source, globals.clone()), vec![failed], "{source}");
    }
}

#[test]
fn an_error_after_a_failed_call_is_still_reported() {
    let globals = failed_check_natives();
    assert_eq!(
        errors(
            "let n = len(true); let s: String = 1; [\"a\"].sum();",
            globals,
        ),
        vec![
            "No function called 'len' found that matches the arguments 'Bool'",
            "mismatched types: found Int but expected String",
            "No function called 'sum' found that matches the arguments '[String]'",
        ]
    );
}

#[test]
fn a_failed_annotation_is_not_reported_again() {
    let globals = failed_check_natives();
    assert_eq!(
        errors("let x: Int<String> = 1; [x].sum();", globals),
        vec!["type `Int` does not take generic arguments"]
    );
}

#[test]
fn a_tie_at_a_tuple_position_is_ambiguous() {
    let globals = natives(&[("+", "(Int, Any) -> Int"), ("+", "(Any, Int) -> Int")]);
    let errors = errors("(1, 2) + (3, 4)", globals);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("is ambiguous"), "{errors:?}");
}

#[test]
fn type_parameters_are_named_in_the_order_they_are_written() {
    let (_, result) = analyse_with_globals("fn swap(a, b) => (b, a);", vec![]);
    let shown = |types: &HashMap<_, StaticType>| {
        types.values().map(ToString::to_string).collect::<Vec<_>>()
    };
    assert_eq!(shown(&result.analysis.function_types), ["fn(A, B) -> Any"]);
    assert_eq!(shown(&result.analysis.inferred_return_types), ["(B, A)"]);
}

/// The help of the first error analysing `source` with numeric `+` reports.
fn plus_help(source: &str) -> Option<String> {
    let (_, result) = analyse_with_globals(
        source,
        natives(&[("+", "(Int, Int) -> Int"), ("+", "(Float, Float) -> Float")]),
    );
    let error = result.errors.first().expect("an error");
    error.help_text().map(str::to_string)
}

#[test]
fn adding_two_strings_points_to_concatenation() {
    assert_eq!(
        plus_help("\"ab\" + \"cd\";").as_deref(),
        Some("`+` adds numbers. Join two strings with `++`, as in `\"ab\" ++ \"cd\"`"),
    );
}

#[test]
fn adding_a_string_and_a_number_points_to_joining_and_converting() {
    let help = "`+` adds numbers. `<>` joins both sides as strings, as in `\"total: \" <> 12`. To add a number written as a string, convert it first, as in `int(\"12\") + 1`";
    assert_eq!(plus_help("\"total: \" + 12;").as_deref(), Some(help));
    assert_eq!(plus_help("1.5 + \"x\";").as_deref(), Some(help));
    assert_eq!(
        plus_help("let total = 0; let line = \"12\"; total += line;").as_deref(),
        Some(help)
    );
}

#[test]
fn adding_other_types_still_says_what_plus_takes() {
    assert_eq!(
        plus_help("[1] + 2;").as_deref(),
        Some("The overloads of `+` take (Float, Float), (Int, Int)"),
    );
}

#[test]
fn an_if_whose_other_branch_is_what_it_returns_is_its_first_branch() {
    // `-` takes an Int and a Float, so `f` stays generic in `n`, but what
    // it returns is the `1`: each branch that recurses returns what the
    // `if` does, directly, through a `&` that returns what it's passed, or
    // through an `if` of its own that a `return` joins.
    let globals = [
        natives(&[
            ("==", "<A, B>(A, B) -> Bool"),
            ("-", "(Int, Int) -> Int"),
            ("-", "(Float, Int) -> Float"),
            ("&", "(Int, Int) -> Int"),
            ("&", "(Bool, Bool) -> Bool"),
        ]),
        index_natives(),
    ]
    .concat();
    for (source, returns) in [
        ("fn f(n) { if n == 0 { 1 } else { f(n - 1) } }", vec!["Int"]),
        (
            "fn f(n) { if n == 0 { 1 } else if n == 1 { f(n - 1) & f(n - 2) } else { f(n - 1) } }",
            vec!["Int"],
        ),
        (
            "fn f(n) { if n == 0 { return 1; } if n == 1 { f(n - 1) & f(n - 2) } else { f(n - 1) } }",
            vec!["Int"],
        ),
        // A function declared in another has one type, which the one it's
        // in settles, as with a graph of lambdas that call back into it.
        (
            "fn graph() {
                 let nodes = %{};
                 fn solve(node) { if node == 0 { 1 } else { nodes[node]() } }
                 nodes[1] = if true { fn() => solve(0) & solve(0) } else { fn() => solve(0) };
                 nodes
             }",
            vec!["%{Int: fn() -> Int}", "Int", "Int", "Int"],
        ),
    ] {
        let (_, result) = analyse_with_globals(source, globals.clone());
        assert!(result.errors.is_empty(), "{source}: {:?}", result.errors);
        let mut shown = result
            .analysis
            .inferred_return_types
            .values()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        shown.sort();
        assert_eq!(shown, returns, "{source}");
    }
}

#[test]
fn a_function_lists_its_instances_in_the_order_they_were_made() {
    // The compiler compiles the instances in the order they're listed, which
    // mustn't depend on how a hash map orders them: it's their slots' order.
    let source = (0..12)
        .map(|n| format!("fn f{n}(x) => x + 1; f{n}(1); f{n}(2.5);"))
        .join(" ");
    let (_, result) = analyse_with_globals(&source, ambiguous_plus());
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    for instances in result.analysis.instances.values() {
        let slots = instances
            .iter()
            .map(|instance| match instance.variable {
                ResolvedVar::Global { slot } => slot,
                ref other => panic!("an instance is a global, not {other:?}"),
            })
            .collect::<Vec<_>>();
        assert!(slots.is_sorted(), "{slots:?}");
    }
}

/// Places an expression goes, with `{}` where it goes.
const EXPRESSION_POSITIONS: [&str; 18] = [
    "false and {}",
    "true or {}",
    "fn g(x) { x } g({})",
    "({}, 2)",
    "[{}]",
    "%{{}: 1}",
    "%{1: {}}",
    "if {} { 1 }",
    "if false { } else if {} { }",
    "while {} { }",
    "for x in [1], if {} { }",
    "[x for x in [1], if {}]",
    "[1][{}]",
    "let y = {}",
    "let y = 1; y = {}",
    "fn g() => {}",
    "fn g() { return {}; }",
    "({})()",
];

#[test]
fn a_named_function_is_declared_only_as_a_statement() {
    let rejected =
        ["only a statement can declare a named function, and `f` is declared inside an expression"];
    for position in EXPRESSION_POSITIONS {
        let at_the_top_level = position.replace("{}", "fn f() { true }");
        let in_a_function = format!("fn outer() {{ {at_the_top_level}; 0 }}");
        // Once, though each instance has the declaration.
        let in_a_generic_function =
            format!("fn outer(z) {{ {at_the_top_level}; z }} outer(1); outer(\"a\");");
        for source in [at_the_top_level, in_a_function, in_a_generic_function] {
            assert_eq!(errors(&source, index_natives()), rejected, "{source}");
        }
    }
}

#[test]
fn an_anonymous_function_goes_anywhere_an_expression_does() {
    for position in EXPRESSION_POSITIONS {
        let source = position.replace("{}", "fn() { true }");
        let rejected = errors(&source, index_natives())
            .into_iter()
            .filter(|error| error.contains("only a statement"))
            .collect::<Vec<_>>();
        assert!(rejected.is_empty(), "{source}: {rejected:?}");
    }
}

#[test]
fn a_named_function_declared_as_a_statement_is_accepted() {
    for source in [
        "fn f() { 1 } f()",
        "fn f() { 1 }; f()",
        "fn g() { fn f() { 1 } f() } g()",
        "fn g() { fn f() { 1 } } g()",
        "{ fn apply(h) { h() } apply }(fn() { 3 })",
        "for x in [1] { fn f() { x } f(); }",
        "if true { fn f() { 1 } f() } else { 2 }",
        "while false { fn f() { 1 } }",
    ] {
        assert_eq!(errors(source, vec![]), Vec::<String>::new(), "{source}");
    }
}
