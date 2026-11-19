use ndc_analysis::Analysis;
use ndc_core as _;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use ndc_stdlib as _;
use ndc_vm::chunk::JumpTarget;
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
    Compiler::compile_unoptimized(
        parse(input).into_iter(),
        Analysis::default(),
        &ndc_vm::Natives::default(),
    )
    .expect("compile failed")
    .opcodes()
    .to_vec()
}

fn optimized(input: &str) -> Vec<OpCode> {
    Compiler::compile(
        parse(input).into_iter(),
        Analysis::default(),
        &ndc_vm::Natives::default(),
    )
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

// `true and false;` — when the left operand is false, `JumpIfFalse` jumps
// straight to the statement's `Pop`, which then discards that operand. The
// `Pop` follows the load of the right operand, `false`, but it's a jump target,
// so the pair must stay: removing it would leave the jump nowhere to land.
//
// 0: Constant(0)      push `true`
// 1: JumpIfFalse(2)   if false, jump to the statement's Pop (index 4)
// 2: Pop              pop left (it was true)
// 3: Constant(1)      push `false`
// 4: Pop              discard the statement's value (jump target)
// 5: Halt
#[test]
fn keeps_a_load_pop_pair_whose_pop_is_a_jump_target() {
    let raw = unoptimized("true and false;");
    assert_eq!(
        raw,
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(2)),
            Pop,
            Constant(1),
            Pop,
            Halt
        ]
    );
    assert_eq!(optimized("true and false;"), raw);
}

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

// When the caller asks for unoptimized output, nested function bodies must
// also be raw — otherwise debug/inspection callers would silently see
// optimized bytecode inside the constants table.
#[test]
fn nested_functions_follow_the_top_levels_optimize_setting() {
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
fn elides_every_load_pop_pair() {
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

    // Loads of globals, locals and cells are elided like constants: a
    // discarded read of `a`, `b` or captured `c` leaves nothing behind.
    let source = "let a = 1; a; { let b = 2; b; let c = 3; c; fn() { c }; }";
    let mut interp = ndc_interpreter::Interpreter::capturing();
    let raw = interp
        .compile_str_unoptimized(source)
        .expect("compile failed");
    let mut interp = ndc_interpreter::Interpreter::capturing();
    let opt = interp.compile_str(source).expect("compile failed");
    let discarded_load = |ops: &[OpCode]| {
        ops.windows(2)
            .any(|pair| matches!(pair, [GetGlobal(_) | GetLocal(_) | GetCell(_), Pop]))
    };
    // `a` is global 3, after the intrinsics `cast`, `cast?` and the tuple
    // index.
    for op in [GetGlobal(3), GetLocal(0), GetCell(1)] {
        assert!(
            raw.opcodes()
                .windows(2)
                .any(|pair| pair == [op.clone(), Pop]),
            "the raw chunk should discard a {op:?}: {:?}",
            raw.opcodes()
        );
    }
    assert!(
        !discarded_load(opt.opcodes()),
        "optimized chunk still discards a load: {:?}",
        opt.opcodes()
    );
}

/// The raw and the optimized bytecode of the first function in `source`,
/// compiled with the standard library.
fn nested_raw_and_optimized(source: &str) -> (Vec<OpCode>, Vec<OpCode>) {
    let compile = |optimize: bool| {
        let mut interp = ndc_interpreter::Interpreter::capturing();
        interp.configure(ndc_stdlib::register);
        let compiled = if optimize {
            interp.compile_str(source)
        } else {
            interp.compile_str_unoptimized(source)
        };
        first_nested_function_opcodes(&compiled.expect("compile failed"))
    };
    (compile(false), compile(true))
}

// `let a = (x, y);` stores the tuple in slot 2 and, once the declaration's
// discarded unit value is gone, reads it straight back. As nothing else reads
// `a`, the optimizer leaves the tuple on the stack instead.
#[test]
fn elides_a_local_stored_and_read_once_straight_away() {
    let (raw, opt) = nested_raw_and_optimized("fn f(x, y) { let a = (x, y); (a, x) }");
    assert!(raw.contains(&SetLocal(2)), "{raw:?}");
    assert_eq!(
        opt,
        [
            GetLocal(0),
            GetLocal(1),
            MakeTuple(2),
            GetLocal(0),
            MakeTuple(2),
            Return
        ]
    );
}

// A chain of lets, each read once by the next, leaves no locals behind.
#[test]
fn elides_a_chain_of_lets_read_once() {
    let (_, opt) = nested_raw_and_optimized(
        "fn f(x, y) { let a = (x, y); let b = (a, x); let c = (b, y); c }",
    );
    assert_eq!(
        opt,
        [
            GetLocal(0),
            GetLocal(1),
            MakeTuple(2),
            GetLocal(0),
            MakeTuple(2),
            GetLocal(1),
            MakeTuple(2),
            Return
        ]
    );
}

// `a` is read twice, so its slot is needed after the first read.
#[test]
fn keeps_a_local_read_more_than_once() {
    let (_, opt) = nested_raw_and_optimized("fn f(x) { let a = (x, x); (a, a) }");
    assert_eq!(
        opt,
        [
            GetLocal(0),
            GetLocal(0),
            MakeTuple(2),
            SetLocal(1),
            GetLocal(1),
            GetLocal(1),
            MakeTuple(2),
            Return
        ]
    );
}

// `a` is read once, but not straight after it's stored: `x` is read in
// between, so the tuple can't simply stay on the stack.
#[test]
fn keeps_a_local_read_once_after_another_value() {
    let (_, opt) = nested_raw_and_optimized("fn f(x) { let a = (x, x); (x, a) }");
    assert!(opt.contains(&SetLocal(1)), "{opt:?}");
}

// `t[0]` on a local tuple read only there takes the element of the tuple
// left on the stack, with `TupleIndex`, instead of storing the tuple first.
#[test]
fn reads_the_element_of_a_tuple_read_once_from_the_stack() {
    let (raw, opt) = nested_raw_and_optimized("fn f(x) { let t = (x, x); t[0] }");
    assert!(
        raw.contains(&GetLocalTupleIndex { slot: 1, index: 0 }),
        "{raw:?}"
    );
    assert_eq!(
        opt,
        [
            GetLocal(0),
            GetLocal(0),
            MakeTuple(2),
            TupleIndex(0),
            Return
        ]
    );
}
