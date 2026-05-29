use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use ndc_stdlib as _;
use ndc_vm::chunk::OpCode;
use ndc_vm::chunk::OpCode::*;
use ndc_vm::compiler::Compiler;

fn parse(input: &str) -> Vec<ndc_parser::ExpressionLocation> {
    let tokens = Lexer::new(input, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("lex failed");
    Parser::from_tokens(tokens).parse().expect("parse failed")
}

fn unoptimized(input: &str) -> Vec<OpCode> {
    Compiler::compile_unoptimized(parse(input).into_iter())
        .expect("compile failed")
        .opcodes()
        .to_vec()
}

fn optimized(input: &str) -> Vec<OpCode> {
    Compiler::compile(parse(input).into_iter())
        .expect("compile failed")
        .opcodes()
        .to_vec()
}

// `5;` — a discarded constant. The peephole eliminates the `Constant; Pop`
// pair entirely, leaving just `Halt`.
#[test]
fn elides_constant_pop_statement() {
    assert_eq!(unoptimized("5;"), [Constant(0), Pop, Halt]);
    assert_eq!(optimized("5;"), [Halt]);
}

// A `Pop` that's the target of a conditional jump must not be elided even
// when the preceding instruction is a `Load` — the jumper relies on it to
// drop the condition value off the stack. Here, `JumpIfFalse` targets the
// false-branch `Pop`, and that `Pop` survives optimization.
#[test]
fn preserves_pop_as_jump_target() {
    let raw = unoptimized("if true { 3 } else { 3; }");
    let opt = optimized("if true { 3 } else { 3; }");

    // Optimization removes the `3; → Constant; Pop` in the else branch.
    assert!(
        opt.len() < raw.len(),
        "optimizer should shrink the chunk; raw={raw:?} opt={opt:?}"
    );

    // Both branch-condition Pops survive — one per arm. Raw has a third Pop
    // (the discarded statement in the else branch); optimized does not.
    let pop_count = |ops: &[OpCode]| ops.iter().filter(|op| matches!(op, Pop)).count();
    assert_eq!(pop_count(&raw), 3, "raw should have 3 Pops");
    assert_eq!(pop_count(&opt), 2, "optimized should have 2 Pops");

    // The optimized chunk still has the conditional jump and the
    // unconditional skip — control flow is intact.
    assert!(opt.iter().any(|op| matches!(op, JumpIfFalse(_))));
    assert!(opt.iter().any(|op| matches!(op, Jump(_))));
}

// When the caller asks for unoptimized output, nested function bodies must
// also be raw — otherwise debug/inspection callers would silently see
// optimized bytecode inside the constants table.
fn first_nested_function_opcodes(compiled: &ndc_vm::value::CompiledFunction) -> Vec<OpCode> {
    for (_, _, val) in compiled.body().iter() {
        let Some(ndc_vm::Value::Object(obj)) = val else {
            continue;
        };
        if let ndc_vm::Object::Function(ndc_vm::value::Function::Compiled(f)) = obj.as_ref() {
            return f.opcodes().to_vec();
        }
    }
    panic!("expected a compiled function constant in the top-level chunk");
}

#[test]
fn nested_function_bodies_respect_unoptimized_flag() {
    let mut interp = ndc_interpreter::Interpreter::capturing();
    let raw = interp
        .compile_str_unoptimized("fn f() { 1; }")
        .expect("compile failed");

    let mut interp = ndc_interpreter::Interpreter::capturing();
    let opt = interp.compile_str("fn f() { 1; }").expect("compile failed");

    // Raw body for `1;` followed by an implicit unit Return:
    // `Constant; Pop; Constant; Return`.
    assert_eq!(
        first_nested_function_opcodes(&raw),
        [Constant(0), Pop, Constant(1), Return],
        "nested function body should be raw under compile_str_unoptimized",
    );

    // Optimized body: peephole drops the leading `Constant; Pop`.
    assert_eq!(
        first_nested_function_opcodes(&opt),
        [Constant(1), Return],
        "nested function body should be optimized under compile_str",
    );
}

// `{ 1; 2; 3; }` is three discarded statements followed by the block's
// unit result. Every `Load; Pop` pair is elided; only the trailing block
// result remains. Exercises the peephole loop catching multiple pairs.
#[test]
fn elides_multiple_load_pop_pairs() {
    let raw = unoptimized("{ 1; 2; 3; }");
    let opt = optimized("{ 1; 2; 3; }");

    // Raw: three `Constant; Pop` pairs plus the final unit `Constant` plus Halt.
    assert_eq!(
        raw,
        [
            Constant(0),
            Pop,
            Constant(1),
            Pop,
            Constant(2),
            Pop,
            Constant(3),
            Halt,
        ]
    );

    // Optimized: just the final unit `Constant` plus Halt.
    assert_eq!(opt, [Constant(3), Halt]);
}
