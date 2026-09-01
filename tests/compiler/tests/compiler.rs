use ndc_core::r#struct::StructRegistry;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use ndc_vm::chunk::JumpTarget;
use ndc_vm::chunk::OpCode;
use ndc_vm::chunk::OpCode::*;
use ndc_vm::compiler::Compiler;
use std::cell::RefCell;
use std::rc::Rc;

// These helpers compile without the peephole optimizer so the tests
// document the raw compiler output as a specification. Optimizer behaviour
// is exercised in `tests/optimizer.rs`.

fn compile(input: &str) -> Vec<OpCode> {
    let tokens = Lexer::new(input, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("lex failed");
    let expressions = Parser::from_tokens(tokens).parse().expect("parse failed");
    Compiler::compile_unoptimized(
        expressions.into_iter(),
        Rc::new(RefCell::new(StructRegistry::default())),
    )
    .expect("compile failed")
    .opcodes()
    .to_vec()
}

fn compile_with_analysis(input: &str) -> Vec<OpCode> {
    let mut interp = ndc_interpreter::Interpreter::capturing();
    interp
        .compile_str_unoptimized(input)
        .expect("compile failed")
        .opcodes()
        .to_vec()
}

/// Like [`compile_with_analysis`] but also loads the standard library so
/// tests can exercise operator overloads (`+`, `*`, `++`, …) by name. Cheap
/// enough to set up fresh per test.
fn compile_with_stdlib(input: &str) -> Vec<OpCode> {
    let mut interp = ndc_interpreter::Interpreter::capturing();
    interp.configure(ndc_stdlib::register);
    interp
        .compile_str(input)
        .expect("compile failed")
        .opcodes()
        .to_vec()
}

fn compile_with_stdlib_unoptimized(input: &str) -> Vec<OpCode> {
    let mut interp = ndc_interpreter::Interpreter::capturing();
    interp.configure(ndc_stdlib::register);
    interp
        .compile_str_unoptimized(input)
        .expect("compile failed")
        .opcodes()
        .to_vec()
}

// if true { 1 }
//
// 0: Constant(0)      push `true`
// 1: JumpIfFalse(3)   if false, jump to else path (index 5)
// 2: Pop              pop condition (true path)
// 3: Constant(1)      push `1`
// 4: Jump(2)          skip else path (jump to Halt at index 7)
// 5: Pop              pop condition (false path, jumped here)
// 6: Constant(2)      push `None` (unit, no else branch)
// 7: Halt
#[test]
fn test_if_without_else() {
    assert_eq!(
        compile("if true { 1 }"),
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(3)),
            Pop,
            Constant(1),
            Jump(JumpTarget::Offset(2)),
            Pop,
            Constant(2),
            Halt
        ]
    );
}

// if true { 1 } else { 2 }
//
// 0: Constant(0)      push `true`
// 1: JumpIfFalse(3)   if false, jump to else (index 5)
// 2: Pop              pop condition (true path)
// 3: Constant(1)      push `1`
// 4: Jump(2)          skip else, jump to Halt (index 7)
// 5: Pop              pop condition (false path)
// 6: Constant(2)      push `2`
// 7: Halt
#[test]
fn test_if_with_else() {
    assert_eq!(
        compile("if true { 1 } else { 2 }"),
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(3)),
            Pop,
            Constant(1),
            Jump(JumpTarget::Offset(2)),
            Pop,
            Constant(2),
            Halt
        ]
    );
}

// true and false
//
// Short-circuits: if left is false, leave it on stack and jump past right.
//
// 0: Constant(0)      push `true`
// 1: JumpIfFalse(2)   if false, skip Pop+right and leave false on stack
// 2: Pop              pop left (it was true, discard it)
// 3: Constant(1)      push `false` (result)
// 4: Halt
#[test]
fn test_and() {
    assert_eq!(
        compile("true and false"),
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(2)),
            Pop,
            Constant(1),
            Halt
        ]
    );
}

// true or false
//
// Short-circuits: if left is true, leave it on stack and jump past right.
//
// 0: Constant(0)      push `true`
// 1: JumpIfTrue(2)    if true, skip Pop+right and leave true on stack
// 2: Pop              pop left (it was false, discard it)
// 3: Constant(1)      push `false` (result)
// 4: Halt
#[test]
fn test_or() {
    assert_eq!(
        compile("true or false"),
        [
            Constant(0),
            JumpIfTrue(JumpTarget::Offset(2)),
            Pop,
            Constant(1),
            Halt
        ]
    );
}

// 5;
//
// 0: Constant(0)      push `5`
// 1: Pop              discard value (it's a statement)
// 2: Halt
#[test]
fn test_statement() {
    assert_eq!(compile("5;"), [Constant(0), Pop, Halt]);
}

// { 5 }
//
// 0: Constant(0)      push `5` (block result)
// 1: Halt
#[test]
fn test_block_with_expression() {
    assert_eq!(compile("{ 5 }"), [Constant(0), Halt]);
}

// { 5; }
//
// 0: Constant(0)      push `5`
// 1: Pop              discard (trailing semicolon)
// 2: Constant(1)      push `()` (block result is unit)
// 3: Halt
#[test]
fn test_block_with_trailing_statement() {
    assert_eq!(compile("{ 5; }"), [Constant(0), Pop, Constant(1), Halt]);
}

// { 5; 6 }
//
// 0: Constant(0)      push `5`
// 1: Pop              discard intermediate statement
// 2: Constant(1)      push `6` (block result)
// 3: Halt
#[test]
fn test_block_multiple_statements() {
    assert_eq!(compile("{ 5; 6 }"), [Constant(0), Pop, Constant(1), Halt]);
}

// if true { 3 } else { 3; }
//
// true branch returns 3, false branch returns ()
//
// 0: Constant(0)      push `true`
// 1: JumpIfFalse(3)   jump to false branch (index 5)
// 2: Pop              pop condition (true path)
// 3: Constant(1)      push `3`
// 4: Jump(4)          jump to Halt (index 9)
// 5: Pop              pop condition (false path)
// 6: Constant(2)      push `3` (inner of `3;`)
// 7: Pop              discard (trailing semicolon)
// 8: Constant(3)      push `()` (block result)
// 9: Halt
#[test]
fn test_if_with_statement_else() {
    assert_eq!(
        compile("if true { 3 } else { 3; }"),
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(3)),
            Pop,
            Constant(1),
            Jump(JumpTarget::Offset(4)),
            Pop,
            Constant(2),
            Pop,
            Constant(3),
            Halt
        ]
    );
}

// if true { 3; } else { 3; }
//
// Both branches return () — result is unit regardless of condition
//
// 0: Constant(0)      push `true`
// 1: JumpIfFalse(5)   jump to false branch (index 7)
// 2: Pop              pop condition (true path)
// 3: Constant(1)      push `3`
// 4: Pop              discard
// 5: Constant(2)      push `()`
// 6: Jump(4)          jump to Halt (index 11)
// 7: Pop              pop condition (false path)
// 8: Constant(3)      push `3`
// 9: Pop              discard
// 10: Constant(4)     push `()`
// 11: Halt
#[test]
fn test_if_with_statement_branches() {
    assert_eq!(
        compile("if true { 3; } else { 3; }"),
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(5)),
            Pop,
            Constant(1),
            Pop,
            Constant(2),
            Jump(JumpTarget::Offset(4)),
            Pop,
            Constant(3),
            Pop,
            Constant(4),
            Halt
        ]
    );
}

// while true { 1 }
//
// 0: Constant(0)      push `true`  ← loop_start
// 1: JumpIfFalse(4)   if false, jump past body to exit Pop (index 6)
// 2: Pop              pop condition (true path)
// 3: Constant(1)      body: push `1`
// 4: Pop              discard body value (each iteration's result is unobservable)
// 5: Jump(-6)         jump back to loop_start (index 0)
// 6: Pop              pop condition (false path, loop exit)
// 7: Constant(2)      push `()` (the loop's result)
// 8: Halt
#[test]
fn test_while() {
    assert_eq!(
        compile("while true { 1 }"),
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(4)),
            Pop,
            Constant(1),
            Pop,
            Jump(JumpTarget::Offset(-6)),
            Pop,
            Constant(2),
            Halt
        ]
    );
}

// let a = 1;
//
// Value is compiled, then SetLocal stores it in pre-allocated slot 0.
// The declaration's unit result is pushed and popped by the statement;
// the optimizer elides the pair.
//
// 0: Constant(0)   push `1`
// 1: SetLocal(0)   store in slot 0
// 2: Constant(1)   push `()` (declaration result)
// 3: Pop           discard (statement)
// 4: Halt
#[test]
fn test_declaration() {
    assert_eq!(
        compile_with_analysis("let a = 1;"),
        [Constant(0), SetLocal(0), Constant(1), Pop, Halt]
    );
}

// let a = 1;
// a = 5;
//
// Declaration stores 1 into pre-allocated slot 0.
// Assignment pushes new value, SetLocal overwrites,
// push unit as the expression result, Pop discards it.
//
// 0: Constant(0)   push `1`
// 1: SetLocal(0)   store in slot 0 (declaration)
// 2: Constant(1)   push `()` (declaration result)
// 3: Pop           discard (statement)
// 4: Constant(2)   push `5`
// 5: SetLocal(0)   overwrite slot 0 (assignment)
// 6: Constant(3)   push `()` (assignment result)
// 7: Pop           discard (statement)
// 8: Halt
#[test]
fn test_assignment() {
    assert_eq!(
        compile_with_analysis("let a = 1;\na = 5;"),
        [
            Constant(0),
            SetLocal(0),
            Constant(1),
            Pop,
            Constant(2),
            SetLocal(0),
            Constant(3),
            Pop,
            Halt
        ]
    );
}

// let value = [1];
// value ++= [2];
//
// A specialized `++=` mutates and returns the left value. Even though the
// mutation is visible through aliases, SetLocal must write the returned value
// back before the compiler pushes unit for the assignment expression:
//
// Call(2), SetLocal(0), Constant(_), Pop
//
// The indexed form prepares the container and index in temporary locals,
// calls `++=`, then passes its result to `[]=`. The setter's unit result is
// discarded before the assignment's own unit is pushed:
//
// Call(3), Pop, Constant(_)
#[test]
fn test_augmented_assignment_always_writes_back() {
    let variable = compile_with_stdlib_unoptimized("let value = [1]; value ++= [2];");
    assert!(
        variable
            .windows(4)
            .any(|ops| matches!(ops, [Call(2), SetLocal(0), Constant(_), Pop])),
        "specialized variable augmentation must write back its result and push unit: {variable:?}",
    );

    let index = compile_with_stdlib_unoptimized(
        "let value = [1]; let values = [value]; values[0] ++= [2];",
    );
    assert!(
        index
            .windows(3)
            .any(|ops| matches!(ops, [Call(3), Pop, Constant(_)])),
        "specialized indexed augmentation must write back through []= and push unit: {index:?}",
    );
}

// let value = 1;
// value += 2;
//
// Ordinary `+` produces a replacement value, so variable augmentation stores
// the result and then produces unit:
//
// Call(2), SetLocal(0), Constant(_), Pop
//
// let values = [1];
// values[0] += 2;
//
// Indexed augmentation instead sends the replacement through `[]=`. Its unit
// result is popped before the assignment expression's unit is produced:
//
// Call(3), Pop, Constant(_)
#[test]
fn test_augmented_assignment_writeback_uses_target_store_shape() {
    let variable = compile_with_stdlib_unoptimized("let value = 1; value += 2;");
    assert!(
        variable
            .windows(4)
            .any(|ops| matches!(ops, [Call(2), SetLocal(0), Constant(_), Pop])),
        "writeback variable augmentation must store the operation result and push unit: {variable:?}",
    );

    let index = compile_with_stdlib_unoptimized("let values = [1]; values[0] += 2;");
    assert!(
        index
            .windows(3)
            .any(|ops| matches!(ops, [Call(3), Pop, Constant(_)])),
        "writeback index augmentation must discard the setter result before pushing unit: {index:?}",
    );
}

// let values = [1];
// values[0] += { let delta = 2; delta };
//
// Source locals are assigned first:
//
// slot 0: values
// slot 1: delta
//
// Preparing an indexed assignment then reserves non-overlapping compiler
// temporaries after the source-local high-water mark:
//
// slot 2: cached container
// slot 3: cached index
// slot 4: operation result passed to `[]=`
#[test]
fn test_augmented_assignment_temporaries_follow_source_locals() {
    let ops =
        compile_with_stdlib_unoptimized("let values = [1]; values[0] += { let delta = 2; delta };");

    assert!(
        ops.iter().any(|op| matches!(op, SetLocal(1))),
        "the rhs block local should retain analyser-assigned slot 1: {ops:?}",
    );
    assert!(
        ops.iter().any(|op| matches!(op, SetLocal(2)))
            && ops.iter().any(|op| matches!(op, SetLocal(3)))
            && ops.iter().any(|op| matches!(op, SetLocal(4))),
        "prepared-target temporaries must be allocated after both source locals: {ops:?}",
    );
}

// { let a = 3; a }
//
// Declaration stores 3 into pre-allocated slot 0.
// Block result is `a`, read via GetLocal.
// No cleanup needed — locals are pre-allocated.
//
// 0: Constant(0)   push `3`
// 1: SetLocal(0)   store in slot 0 (declaration)
// 2: Constant(1)   push `()` (declaration result)
// 3: Pop           discard (statement)
// 4: GetLocal(0)   push `a` (block result)
// 5: Halt
#[test]
fn test_block_scope_cleanup() {
    assert_eq!(
        compile_with_analysis("{ let a = 3; a }"),
        [
            Constant(0),
            SetLocal(0),
            Constant(1),
            Pop,
            GetLocal(0),
            Halt
        ]
    );
}

// { let a = 1; let b = 2; a }
//
// Both locals stored via SetLocal into pre-allocated slots.
// Block result is `a`, read via GetLocal. No cleanup needed.
//
// 0: Constant(0)   push `1`
// 1: SetLocal(0)   store in slot 0
// 2: Constant(1)   push `()` / 3: Pop
// 4: Constant(2)   push `2`
// 5: SetLocal(1)   store in slot 1
// 6: Constant(3)   push `()` / 7: Pop
// 8: GetLocal(0)   push `a` (block result)
// 9: Halt
#[test]
fn test_block_scope_cleanup_multiple_locals() {
    assert_eq!(
        compile_with_analysis("{ let a = 1; let b = 2; a }"),
        [
            Constant(0),
            SetLocal(0),
            Constant(1),
            Pop,
            Constant(2),
            SetLocal(1),
            Constant(3),
            Pop,
            GetLocal(0),
            Halt
        ]
    );
}

// (1, 2) + (3, 4)
//
// The analyser pins this to a single scalar `+(Int, Int)` overload broadcast
// across two positions. The compiler emits a direct GetGlobal load of the
// scalar function — no OverloadSet allocation — and a CallVec opcode that
// vec-dispatches at runtime.
#[test]
fn test_vec_call_homogeneous_resolved() {
    let ops = compile_with_stdlib("(1, 2) + (3, 4)");
    let call_op = ops
        .iter()
        .rev()
        .find(|op| matches!(op, CallVec(_) | Call(_)));
    assert_eq!(
        call_op,
        Some(&CallVec(2)),
        "Resolved(Vec) call should compile to CallVec(2), got: {ops:?}",
    );
}

// (1, "a") + (2, "b")
//
// Mixed-element tuple over `+`: position 0 resolves to `+(Int, Int)`, but
// position 1 has `(String, String)` and there's no `+(String, String)`
// overload. The analyser surfaces this as Binding::None, which the
// compiler refuses to lower — `compile` must fail.
#[test]
fn test_vec_call_per_position_failure_errors() {
    let mut interp = ndc_interpreter::Interpreter::capturing();
    interp.configure(ndc_stdlib::register);
    assert!(
        interp.compile_str("(1, \"a\") + (2, \"b\")").is_err(),
        "mixed-element vec call should fail compilation",
    );
}

// fn id(x) { x }; id((1, 2, 3))
//
// Regular function call (not operator syntax) must NEVER compile to CallVec
// even when the argument is a tuple — vec dispatch is gated to operator
// syntax via the `OperatorCall` AST variant.
#[test]
fn test_regular_call_with_tuple_arg_does_not_vec() {
    let ops = compile_with_stdlib("fn id(x) { x }; id((1, 2, 3))");
    assert!(
        ops.iter().all(|op| !matches!(op, CallVec(_))),
        "regular call must not lower to CallVec, got: {ops:?}",
    );
    assert!(
        ops.iter().any(|op| matches!(op, Call(1))),
        "expected a Call(1) for id((1, 2, 3)), got: {ops:?}",
    );
}

#[test]
fn test_member_assignment_calls_setter_with_receiver_and_value() {
    let ops = compile_with_analysis("struct Point { x: Int }\nlet point = Point(1);\npoint.x = 2;");

    assert!(
        ops.windows(4)
            .any(|window| matches!(window, [Constant(_), GetLocal(_), Constant(_), Call(2)])),
        "expected setter, receiver, value, Call(2); got: {ops:?}",
    );
}

// while true { print(1 + break); }
//
// The break sits inside a partially compiled call (`print` and `+` callees
// and the constant `1` are already on the stack), so it pops those three
// pending operands before jumping out of the loop.
#[test]
fn test_break_pops_pending_operands() {
    let ops = compile_with_stdlib_unoptimized("while true { print(1 + break); }");
    assert!(
        ops.windows(4)
            .any(|w| matches!(w, [Pop, Pop, Pop, Jump(_)])),
        "expected three cleanup Pops before the break jump, got: {ops:?}",
    );
}
