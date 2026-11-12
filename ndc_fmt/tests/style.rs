//! How the formatter lays out each kind of code.

use ndc_lexer::SourceId;
use ndc_parser as _;
use thiserror as _;

/// `source` formats to `expected`, which formats to itself
#[track_caller]
fn assert_formats(source: &str, expected: &str) {
    let formatted = ndc_fmt::format(source, SourceId::SYNTHETIC).expect("source formats");
    assert_eq!(
        formatted, expected,
        "\n--- formatted\n{formatted}\n--- expected\n{expected}"
    );
    let again = ndc_fmt::format(expected, SourceId::SYNTHETIC).expect("expected formats");
    assert_eq!(again, expected, "the expected output isn't stable");
}

#[test]
fn spaces_go_around_operators_and_after_commas() {
    assert_formats(
        "let   x =[1,2 ,3];x[0]+=-1*2;x max=4;",
        "let x = [1, 2, 3];\nx[0] += -1 * 2;\nx max= 4;\n",
    );
}

#[test]
fn statements_are_indented_by_four_spaces() {
    assert_formats(
        "fn f(x) {\n  if x > 0 {\n x\n  } else {\n  0\n }\n}\n",
        "fn f(x) {\n    if x > 0 {\n        x\n    } else {\n        0\n    }\n}\n",
    );
}

#[test]
fn one_empty_line_between_statements_is_kept() {
    assert_formats(
        "\n\nlet a = 1;\n\n\n\nlet b = 2;\nlet c = 3;\n\n",
        "let a = 1;\n\nlet b = 2;\nlet c = 3;\n",
    );
}

#[test]
fn a_block_stays_on_one_line_only_if_it_is_on_one() {
    assert_formats(
        "if x { break; }\nlet f = fn() { 1 };\nif y {\n    z\n}\n",
        "if x { break; }\nlet f = fn() { 1 };\nif y {\n    z\n}\n",
    );
}

#[test]
fn the_branches_of_an_if_break_together() {
    assert_formats(
        "if x { a } else {\n    b\n}\n",
        "if x {\n    a\n} else {\n    b\n}\n",
    );
}

#[test]
fn a_function_body_in_braces_needs_no_arrow() {
    assert_formats(
        "fn f() => { 1 }\nlet g = fn(x) => x + 1;\n",
        "fn f() { 1 }\nlet g = fn(x) => x + 1;\n",
    );
}

#[test]
fn parentheses_are_kept_but_not_doubled() {
    assert_formats(
        "let a = (1 + 2) * ((3));\n(s.n) = 7;\nlet t = (1,);\n",
        "let a = (1 + 2) * (3);\n(s.n) = 7;\nlet t = (1,);\n",
    );
}

#[test]
fn semicolons_are_kept_where_they_are() {
    assert_formats(
        "fn f() { return; }\nlet x = { 1; 2 };\nif x { }\n",
        "fn f() { return; }\nlet x = { 1; 2 };\nif x {}\n",
    );
}

#[test]
fn not_in_keeps_its_place() {
    assert_formats(
        "let a = x not in xs;\nlet b = not x in xs;\n",
        "let a = x not in xs;\nlet b = not x in xs;\n",
    );
}

#[test]
fn literals_are_written_as_in_the_source() {
    assert_formats(
        "let a = 0xff_ff;\nlet b = r#\"raw \"quoted\"\"#;\nlet c = 1_000n;\n",
        "let a = 0xff_ff;\nlet b = r#\"raw \"quoted\"\"#;\nlet c = 1_000n;\n",
    );
}

#[test]
fn a_list_that_does_not_fit_has_one_item_per_line_and_a_trailing_comma() {
    let items: Vec<String> = (0..30).map(|i| format!("item_{i:02}")).collect();
    let source = format!("let xs = [{}];\n", items.join(", "));
    let expected = format!(
        "let xs = [\n{}];\n",
        items
            .iter()
            .map(|item| format!("    {item},\n"))
            .collect::<String>()
    );
    assert_formats(&source, &expected);
}

#[test]
fn a_list_that_fits_goes_on_one_line_without_a_trailing_comma() {
    assert_formats("let xs = [\n    1,\n    2,\n];\n", "let xs = [1, 2];\n");
}

#[test]
fn a_map_written_one_entry_per_line_stays_that_way() {
    assert_formats(
        "let m = %{\n    \"a\": 1, \"b\": 2};\nlet n = %{\"a\": 1,\n\"b\": 2};\n",
        "let m = %{\n    \"a\": 1,\n    \"b\": 2,\n};\nlet n = %{\"a\": 1, \"b\": 2};\n",
    );
}

#[test]
fn a_function_passed_last_stays_on_the_line_of_the_call() {
    assert_formats(
        "let ys = xs.map(fn(x) {\n    x * 2\n});\nlet t = xs.fold(0, fn(a, x) {\n    a + x\n}) + 1;\n",
        "let ys = xs.map(fn(x) {\n    x * 2\n});\nlet t = xs.fold(0, fn(a, x) {\n    a + x\n}) + 1;\n",
    );
}

#[test]
fn a_function_passed_last_after_a_comment_is_not_on_the_line_of_the_call() {
    assert_formats(
        "f(a,\n    // why\n    pure fn() {\n        1\n    });\n",
        "f(\n    a,\n    // why\n    pure fn() {\n        1\n    },\n);\n",
    );
}

#[test]
fn a_block_over_several_lines_breaks_only_itself() {
    assert_formats(
        "let x = {\n    let a = 3;\n    a * a\n} + 1;\nlet xs = [{\n    1\n}, 2];\n",
        "let x = {\n    let a = 3;\n    a * a\n} + 1;\nlet xs = [{\n    1\n}, 2];\n",
    );
}

#[test]
fn a_comment_breaks_everything_around_it() {
    assert_formats(
        "let xs = [f(1, // one\n    2), 3];\n",
        "let xs = [\n    f(\n        1, // one\n        2,\n    ),\n    3,\n];\n",
    );
}

#[test]
fn a_comment_after_an_operand_stays_on_its_line() {
    assert_formats(
        "let x = 1 // first\n    + 2; // second\n",
        "let x = 1 // first\n    + 2; // second\n",
    );
}

#[test]
fn empty_destructuring_keeps_its_brackets() {
    assert_formats(
        "let () = ();\nlet [] = [];\n() = ();\n",
        "let () = ();\nlet [] = [];\n() = ();\n",
    );
}

#[test]
fn a_long_operator_chain_breaks_before_each_operator() {
    let operands: Vec<String> = (0..12).map(|i| format!("operand_{i:02}")).collect();
    let source = format!("let total = {};\n", operands.join(" + "));
    let expected = format!(
        "let total = {}\n{};\n",
        operands[0],
        operands[1..]
            .iter()
            .map(|operand| format!("    + {operand}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    assert_formats(&source, &expected);
}

#[test]
fn a_struct_has_spaces_inside_its_braces() {
    assert_formats(
        "struct Point{x:Int,y:Int}\nstruct Wide {\n    x: Int\n}\n",
        "struct Point { x: Int, y: Int }\nstruct Wide {\n    x: Int,\n}\n",
    );
}

#[test]
fn comprehensions_and_ranges() {
    assert_formats(
        "let xs = [ x * x for x in 1 .. 10, if x % 2 == 0 ];\nlet m = %{:0, k: v for k, v in pairs};\n",
        "let xs = [x * x for x in 1..10, if x % 2 == 0];\nlet m = %{:0, k: v for k, v in pairs};\n",
    );
}

#[test]
fn comments_on_their_own_line_stay_on_their_own_line() {
    assert_formats(
        "// first\nlet a = 1;\n\n  // second\nlet b = 2;\n// last\n",
        "// first\nlet a = 1;\n\n// second\nlet b = 2;\n// last\n",
    );
}

#[test]
fn a_comment_after_code_stays_after_it() {
    assert_formats(
        "let a = 1;    // one\nf(a);// call\nlet xs = [\n    1, // first\n    2,\n];\n",
        "let a = 1; // one\nf(a); // call\nlet xs = [\n    1, // first\n    2,\n];\n",
    );
}

#[test]
fn comments_in_blocks_and_lists() {
    assert_formats(
        "fn f() { // why\n    // how\n    1\n    // after\n}\nlet xs = [\n    // only\n];\n",
        "fn f() { // why\n    // how\n    1\n    // after\n}\nlet xs = [\n    // only\n];\n",
    );
}

#[test]
fn an_empty_program_stays_empty() {
    assert_formats("\n\n", "");
}
