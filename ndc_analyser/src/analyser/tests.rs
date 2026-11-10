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
    let mut last_type = StaticType::unit();
    for expression in &expressions {
        last_type = analyser.analyse(expression);
    }
    (last_type, analyser.take_result())
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
    let any_function = StaticType::Function {
        parameters: None,
        return_type: Box::new(StaticType::Any),
    };
    vec![
        ("[]".to_string(), any_function.clone()),
        ("[]=".to_string(), any_function),
    ]
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

// An accessor that doesn't take the receiver is an error like any other:
// analysis carries on, and later errors are reported too.
#[test]
fn an_augmented_member_target_without_a_getter_is_reported_with_later_errors() {
    let plus = StaticType::Function {
        parameters: None,
        return_type: Box::new(StaticType::Any),
    };
    let (_, result) = analyse_with_globals(
        "struct P { x: Int } struct Q { y: Int } let p = P(1); p.y += 1; let b: Int = \"t\";",
        vec![("+".to_string(), plus)],
    );
    let messages: Vec<String> = result.errors.iter().map(ToString::to_string).collect();
    assert!(
        messages
            .iter()
            .any(|m| m.contains("No function called 'y'")),
        "{messages:?}"
    );
    assert!(
        messages.iter().any(|m| m.contains("mismatched types")),
        "{messages:?}"
    );
}

#[test]
fn destructuring_assignment_checks_each_target() {
    assert_analysis_error(
        "let a: Int = 1; let b = 2; (a, b) = (\"s\", 2);",
        vec![],
        "mismatched types",
    );
    assert_analysis_error("let a = 1; let b = 2; (a, b) = (1, 2, 3);", vec![], "tuple");
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
fn destructuring_assignment_widens_each_target() {
    let typ = analyse_last_type("let a = 1; let b = 2; (a, b) = (\"s\", 2); a");
    assert_eq!(typ, StaticType::Int.lub(&StaticType::String));
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

#[test]
fn inferred_list_index_assignment_widens_element_type() {
    assert_eq!(
        analyse_last_type_with_globals(
            "let values = [1]; values[0] = \"two\"; values",
            index_natives()
        ),
        StaticType::List(Box::new(StaticType::Any)),
    );
}

#[test]
fn inferred_list_slice_assignment_widens_element_type() {
    assert_eq!(
        analyse_last_type_with_globals(
            "let values = [1]; values[0..1] = [\"two\"]; values",
            index_natives()
        ),
        StaticType::List(Box::new(StaticType::Any)),
    );
}

#[test]
fn inferred_map_index_assignment_widens_value_and_preserves_key_type() {
    assert_eq!(
        analyse_last_type_with_globals(
            "let values = %{\"one\": 1}; values[\"two\"] = \"two\"; values",
            index_natives()
        ),
        StaticType::Map {
            key: Box::new(StaticType::String),
            value: Box::new(StaticType::Any),
        },
    );
}

#[test]
fn inferred_index_augmented_assignment_widens_element_type() {
    let add = StaticType::Function {
        parameters: Some(vec![StaticType::Int, StaticType::Float]),
        return_type: Box::new(StaticType::Number),
    };
    assert_eq!(
        analyse_last_type_with_globals(
            "let values = [1]; values[0] += 0.5; values",
            [index_natives(), vec![("+".to_string(), add)]].concat(),
        ),
        StaticType::List(Box::new(StaticType::Any)),
    );
}

#[test]
fn inferred_identifier_assignments_widen_subsequent_reads() {
    assert_eq!(analyse_last_type("let x = 3; x = 0.5; x"), StaticType::Any,);

    let add = StaticType::Function {
        parameters: Some(vec![StaticType::Int, StaticType::Float]),
        return_type: Box::new(StaticType::Float),
    };
    assert_eq!(
        analyse_last_type_with_globals("let x = 3; x += 0.5; x", vec![("+".to_string(), add)],),
        StaticType::Any,
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
fn compatible_specialized_assignment_preserves_left_type() {
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
fn incompatible_specialized_assignment_is_rejected() {
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
fn annotated_declaration_still_rejects_mismatched_literals() {
    assert_analysis_error(
        "let x: Map<Int, Int> = %{\"a\": 1};",
        vec![],
        "mismatched types: found Map<String, Int> but expected Map<Int, Int>",
    );
}

#[test]
fn constructor_arity_mismatch_is_rejected() {
    assert_analysis_error(
        "struct Point { x: Int, y: Int }\nPoint(1)",
        vec![],
        "No function called 'Point' found that matches the arguments 'Int'",
    );
}

#[test]
fn constructor_argument_type_mismatch_is_rejected() {
    assert_analysis_error(
        "struct Point { x: Int, y: Int }\nPoint(\"x\", 2)",
        vec![],
        "No function called 'Point' found that matches the arguments 'String, Int'",
    );
}

#[test]
fn any_typed_callee_still_dispatches_dynamically() {
    let (_, result) = analyse_with_globals("f(\"x\")", vec![("f".to_string(), StaticType::Any)]);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
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

#[test]
fn duplicate_struct_field_is_rejected() {
    assert_analysis_error(
        "struct Dup { a: Int, a: String, a: Bool }",
        vec![],
        "Illegal redefinition of field 'a' in struct 'Dup'",
    );
}

#[test]
fn struct_with_duplicate_fields_does_not_claim_its_name() {
    let (_, result) = analyse_with_globals(
        "struct Dup { a: Int, a: String }\nstruct Dup { a: Int }",
        vec![],
    );
    assert_eq!(
        result.errors.iter().map(ToString::to_string).collect_vec(),
        vec!["Illegal redefinition of field 'a' in struct 'Dup'"],
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
fn short_tuple_still_binds_every_pattern_name() {
    assert_one_error_then_any(
        "let (a, b, c): (Int, Int, Int) = (1, 2); c",
        "Invalid unpacking",
    );
}

#[test]
fn tuple_arity_mismatch_still_binds_every_pattern_name() {
    assert_one_error_then_any(
        "let (a, b, c): (Int, Int) = (1, 2); c",
        "mismatched tuple arity",
    );
}

#[test]
fn unpacking_a_scalar_still_binds_every_pattern_name() {
    assert_one_error_then_any("let (a, (b, c)) = (1, 2); c", "Invalid unpacking");
}

#[test]
fn function_with_an_unknown_name_still_binds_its_name() {
    assert_one_error_then_any(
        "fn f() { missing; } let g = f; g",
        "Identifier missing has not previously been declared",
    );
}

#[test]
fn function_with_an_unknown_name_can_still_be_called() {
    assert_one_error_then_any(
        "fn f() { missing; } f()",
        "Identifier missing has not previously been declared",
    );
}

#[test]
fn function_with_an_unknown_name_can_still_be_captured() {
    assert_one_error_then_any(
        "fn f() { missing; } fn g() { f } g()",
        "Identifier missing has not previously been declared",
    );
}

/// `source` reports exactly one error, the redefinition of `f`.
fn assert_only_redefinition_error(source: &str) {
    let (_, result) = analyse_with_globals(source, vec![]);
    assert_eq!(
        result.errors.iter().map(ToString::to_string).collect_vec(),
        vec!["Illegal redefinition of function 'f' with parameters (Any) in the same scope"],
    );
}

#[test]
fn redefined_function_can_still_be_used_as_a_value() {
    assert_only_redefinition_error("fn f(x) { x } fn f(y) { y } let g = f;");
}

#[test]
fn redefined_function_can_still_be_called() {
    assert_only_redefinition_error("fn f(x) { x } fn f(y) { y } f(1)");
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
fn same_parameter_types_under_other_names_are_a_redefinition() {
    let (_, result) = analyse_with_globals("fn f(a: Int) { a } fn f(b: Int) { b }", vec![]);
    assert_eq!(
        result.errors.iter().map(ToString::to_string).collect_vec(),
        vec!["Illegal redefinition of function 'f' with parameters (Int) in the same scope"],
    );
}

/// `len`, a native taking a `String`, for the tests below.
fn len_native() -> Vec<(String, StaticType)> {
    vec![(
        "len".to_string(),
        StaticType::Function {
            parameters: Some(vec![StaticType::String]),
            return_type: Box::new(StaticType::Int),
        },
    )]
}

fn errors(source: &str, globals: Vec<(String, StaticType)>) -> Vec<String> {
    let (_, result) = analyse_with_globals(source, globals);
    result.errors.iter().map(ToString::to_string).collect()
}

#[test]
fn unknown_value_nearer_than_an_accepting_function_is_ambiguous() {
    assert_eq!(
        errors("fn foo(x) { 1 } fn test(foo) { foo(2) }", vec![]),
        vec![
            "Ambiguous call to 'foo': it could mean the function 'foo' or a value named 'foo' whose type isn't known"
        ],
    );
}

#[test]
fn natives_compete_with_unknown_values_too() {
    assert_eq!(
        errors("fn test(len) { len(\"abc\") }", len_native()),
        vec![
            "Ambiguous call to 'len': it could mean the function 'len' or a value named 'len' whose type isn't known"
        ],
    );
}

#[test]
fn unknown_value_is_called_when_no_function_accepts_the_call() {
    assert_eq!(
        errors("fn foo(a, b) { 1 } fn test(foo) { foo(2) }", vec![]),
        Vec::<String>::new()
    );
}

#[test]
fn method_calls_are_checked_like_plain_calls() {
    assert_eq!(
        errors("fn test(len) { \"abc\".len() }", len_native()),
        vec![
            "Ambiguous call to 'len': it could mean the function 'len' or a value named 'len' whose type isn't known"
        ],
    );
}

#[test]
fn annotating_a_value_that_cant_be_called_leaves_the_name_to_the_function() {
    assert_eq!(
        errors("fn test(len: Int) { len(\"abc\") }", len_native()),
        Vec::<String>::new()
    );
}

#[test]
fn a_nearer_function_shadows_an_unknown_value() {
    assert_eq!(
        errors("fn test(foo) { fn foo(x) { 1 } foo(2) }", vec![]),
        Vec::<String>::new()
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
fn property_reads_are_checked_like_calls() {
    assert_eq!(
        errors("fn test(len) { \"abc\".len }", len_native()),
        vec![
            "Ambiguous call to 'len': it could mean the function 'len' or a value named 'len' whose type isn't known"
        ],
    );
}

#[test]
fn a_field_read_is_ambiguous_with_an_unknown_value_named_like_the_field() {
    assert_eq!(
        errors("struct P { x: Int } fn f(p, x) { p.x }", vec![]),
        vec![
            "Ambiguous call to 'x': it could mean the function 'x' or a value named 'x' whose type isn't known"
        ],
    );
}

#[test]
fn augmented_assignment_to_a_field_reads_it_first() {
    assert_eq!(
        errors("struct P { x: Int } fn f(p, x) { p.x += 1 }", plus_native()),
        vec![
            "Ambiguous call to 'x': it could mean the function 'x' or a value named 'x' whose type isn't known"
        ],
    );
}

#[test]
fn plain_assignment_to_a_field_reads_nothing() {
    assert_eq!(
        errors("struct P { x: Int } fn f(p, x) { p.x = 1 }", vec![]),
        Vec::<String>::new()
    );
}
