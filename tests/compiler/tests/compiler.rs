use ndc_analysis::{Analysis, CaptureSource};
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use ndc_vm::chunk::JumpTarget;
use ndc_vm::chunk::OpCode;
use ndc_vm::chunk::OpCode::*;
use ndc_vm::compiler::Compiler;
use ndc_vm::value::CompiledFunction;
use std::rc::Rc;

// These helpers compile without the peephole optimizer so the tests
// document the raw compiler output as a specification. Optimizer behaviour
// is exercised in `tests/optimizer.rs`.

fn compile(input: &str) -> Vec<OpCode> {
    compile_function(input).opcodes().to_vec()
}

fn compile_function(input: &str) -> CompiledFunction {
    let tokens = Lexer::new(input, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("lex failed");
    let expressions = Parser::from_tokens(tokens).parse().expect("parse failed");
    Compiler::compile_unoptimized(
        expressions.into_iter(),
        Analysis::default(),
        &ndc_vm::Natives::default(),
    )
    .expect("compile failed")
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

/// The raw bytecode of a function nested in `input`: `path[0]` is the
/// constant of the top-level code that holds it, `path[1]` the constant of
/// that function holding the next one, and so on.
fn compile_nested_function(input: &str, path: &[usize]) -> Vec<OpCode> {
    let mut interp = ndc_interpreter::Interpreter::capturing();
    nested_function_opcodes(&mut interp, input, path)
}

/// Like [`compile_nested_function`] but with the standard library loaded.
fn compile_nested_function_with_stdlib(input: &str, path: &[usize]) -> Vec<OpCode> {
    let mut interp = ndc_interpreter::Interpreter::capturing();
    interp.configure(ndc_stdlib::register);
    nested_function_opcodes(&mut interp, input, path)
}

fn nested_function_opcodes(
    interp: &mut ndc_interpreter::Interpreter,
    input: &str,
    path: &[usize],
) -> Vec<OpCode> {
    let compiled = interp
        .compile_str_unoptimized(input)
        .expect("compile failed");
    let mut function = function_constant(&compiled, path[0]);
    for &index in &path[1..] {
        function = function_constant(&function, index);
    }
    function.opcodes().to_vec()
}

fn function_constant(function: &CompiledFunction, index: usize) -> Rc<CompiledFunction> {
    let ndc_vm::Value::Object(object) = function.body().constant(index) else {
        panic!("constant {index} is not a function");
    };
    let ndc_vm::Object::Function(ndc_vm::Function::Compiled(function)) = object.as_ref() else {
        panic!("constant {index} is not a compiled function");
    };
    Rc::clone(function)
}

/// Whether `op` is the instruction of a native's op, as `AddInt` is.
fn is_native_op(op: &OpCode) -> bool {
    matches!(
        op,
        AddInt
            | SubInt
            | MulInt
            | AddFloat
            | SubFloat
            | MulFloat
            | DivFloat
            | NegInt
            | NegFloat
            | Not
            | Less
            | LessEqual
            | Greater
            | GreaterEqual
            | Equal
            | NotEqual
    )
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
// 6: Constant(2)      push `()` (no else branch)
// 7: Halt
#[test]
fn an_if_without_else_yields_unit_when_false() {
    let compiled = compile_function("if true { 1 }");
    assert_eq!(
        compiled.opcodes(),
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
    assert!(
        compiled.body().constant(2).is_unit(),
        "the false path must push unit"
    );
}

// if true { 1 }, analysed: an `if` without an `else` is `()`, so the branch's
// own value is dropped for one.
//
// 0: Constant(0)      push `true`
// 1: JumpIfFalse(5)   if false, jump to the else path (index 7)
// 2: Pop              pop condition (true path)
// 3: Constant(1)      push `1`
// 4: Pop              drop the branch's value
// 5: Constant(2)      push `()`
// 6: Jump(2)          skip the else path
// 7: Pop              pop condition (false path)
// 8: Constant(3)      push `()`
// 9: Halt
#[test]
fn an_if_without_else_drops_its_branch_s_value() {
    assert_eq!(
        compile_with_analysis("if true { 1 }"),
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(5)),
            Pop,
            Constant(1),
            Pop,
            Constant(2),
            Jump(JumpTarget::Offset(2)),
            Pop,
            Constant(3),
            Halt
        ]
    );
}

// A branch that's already `()`, or never finishes, needs no extra ops.
#[test]
fn an_if_without_else_keeps_a_unit_or_never_branch_as_it_is() {
    assert_eq!(
        compile_with_analysis("if true { () }"),
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(3)),
            Pop,
            MakeTuple(0),
            Jump(JumpTarget::Offset(2)),
            Pop,
            Constant(1),
            Halt
        ]
    );
    // The inner `if`'s branch is a `break`, which is `Never`.
    assert_eq!(
        compile_with_analysis("while true { if true { break } }"),
        [
            Constant(0),
            JumpIfFalse(JumpTarget::Offset(10)),
            Pop,
            Constant(1),
            JumpIfFalse(JumpTarget::Offset(3)),
            Pop,
            Jump(JumpTarget::Offset(6)),
            Jump(JumpTarget::Offset(2)),
            Pop,
            Constant(2),
            Pop,
            Jump(JumpTarget::Offset(-12)),
            Pop,
            Constant(3),
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
fn an_if_pops_its_condition_on_both_paths() {
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
fn and_short_circuits_on_a_false_left_operand() {
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
fn or_short_circuits_on_a_true_left_operand() {
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

// { 5 }
//
// 0: Constant(0)      push `5` (block result)
// 1: Halt
// "abc"
//
// 0: MakeString(0)    push a new string with the text of constant 0, as a
//                     string can be changed in place and a literal is
//                     evaluated again in a loop or a second call
// 1: Halt
#[test]
fn a_string_literal_makes_a_new_string() {
    assert_eq!(compile("\"abc\""), [MakeString(0), Halt]);
}

#[test]
fn a_block_yields_its_last_expression() {
    assert_eq!(compile("{ 5 }"), [Constant(0), Halt]);
}

// { 5; }
//
// 0: Constant(0)      push `5`
// 1: Pop              discard (trailing semicolon)
// 2: Constant(1)      push `()` (block result is unit)
// 3: Halt
#[test]
fn a_block_ending_in_a_statement_yields_unit() {
    assert_eq!(compile("{ 5; }"), [Constant(0), Pop, Constant(1), Halt]);
}

// { 5; 6 }
//
// 0: Constant(0)      push `5`
// 1: Pop              discard intermediate statement
// 2: Constant(1)      push `6` (block result)
// 3: Halt
#[test]
fn a_block_discards_all_but_its_last_value() {
    assert_eq!(compile("{ 5; 6 }"), [Constant(0), Pop, Constant(1), Halt]);
}

// if true { 3 } else { 3; }
//
// Each branch leaves exactly one value on the stack: the true branch its `3`,
// the false branch, which ends in a statement, unit.
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
fn each_if_branch_leaves_one_value() {
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
fn a_while_loop_discards_its_body_value_and_yields_unit() {
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
// a = 5;
//
// A top-level declaration is a global, so the declaration stores 1 into
// global slot 3 (there are no natives here, and the language's intrinsics
// `cast`, `cast?` and the tuple index take slots 0 to 2).
// Assignment pushes new value, SetGlobal overwrites,
// push unit as the expression result, Pop discards it.
//
// 0: Constant(0)    push `1`
// 1: SetGlobal(3)   store in global slot 3 (declaration)
// 2: Constant(1)    push `()` (declaration result)
// 3: Pop            discard (statement)
// 4: Constant(2)    push `5`
// 5: SetGlobal(3)   overwrite global slot 3 (assignment)
// 6: Constant(3)    push `()` (assignment result)
// 7: Pop            discard (statement)
// 8: Halt
#[test]
fn a_top_level_variable_is_declared_and_assigned_through_its_global() {
    assert_eq!(
        compile_with_analysis("let a = 1;\na = 5;"),
        [
            Constant(0),
            SetGlobal(3),
            Constant(1),
            Pop,
            Constant(2),
            SetGlobal(3),
            Constant(3),
            Pop,
            Halt
        ]
    );
}

// let value = 1;
// value += 2;
//
// An augmented assignment stores the operation's result back into its target,
// then pushes unit for the assignment expression. That holds for a `++=` that
// mutates the list in place and returns it as much as for a `+` that makes a
// new value, which `+(Int, Int)` writes as an instruction and `++` calls by
// its slot. A variable target stores with its Set opcode:
//
// AddInt (or another op) or CallNative, SetGlobal(_), Constant(_), Pop
//
// An indexed target passes the result to `[]=`, and pops that call's unit
// before pushing its own:
//
// Call(3), Pop, Constant(_)
#[test]
fn an_augmented_assignment_stores_its_result_back_even_when_it_mutates_in_place() {
    for source in [
        "let value = 1; value += 2;",
        "let value = [1]; value ++= [2];",
    ] {
        let ops = compile_with_stdlib_unoptimized(source);
        assert!(
            ops.windows(4).any(|ops| match ops {
                [operation, SetGlobal(_), Constant(_), Pop] => {
                    is_native_op(operation) || matches!(operation, CallNative { args: 2, .. })
                }
                _ => false,
            }),
            "`{source}` must store the operation result and push unit: {ops:?}",
        );
    }

    for source in [
        "let values = [1]; values[0] += 2;",
        "let value = [1]; let values = [value]; values[0] ++= [2];",
    ] {
        let ops = compile_with_stdlib_unoptimized(source);
        assert!(
            ops.windows(3)
                .any(|ops| matches!(ops, [Call(3), Pop, Constant(_)])),
            "`{source}` must store through []= and push unit: {ops:?}",
        );
    }
}

// let values = [1];
// values[0] += { let delta = 2; delta };
//
// `values` is a global. The top-level code's own locals are assigned first:
//
// slot 0: delta
//
// Preparing an indexed assignment then reserves non-overlapping compiler
// temporaries after the source-local high-water mark:
//
// slot 1: cached container
// slot 2: cached index
// slot 3: operation result passed to `[]=`
#[test]
fn temporaries_come_after_the_frames_own_locals() {
    let ops =
        compile_with_stdlib_unoptimized("let values = [1]; values[0] += { let delta = 2; delta };");

    assert!(
        ops.iter().any(|op| matches!(op, SetLocal(0))),
        "the rhs block local should retain analyser-assigned slot 0: {ops:?}",
    );
    assert!(
        ops.iter().any(|op| matches!(op, SetLocal(1)))
            && ops.iter().any(|op| matches!(op, SetLocal(2)))
            && ops.iter().any(|op| matches!(op, SetLocal(3))),
        "prepared-target temporaries must be allocated after the source local: {ops:?}",
    );
}

// { let a = 1; let b = 2; a }
//
// Block locals live in slots of the frame, allocated up front, so the block
// stores and reads them with SetLocal and GetLocal and cleans nothing up when
// it ends.
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
fn block_locals_live_in_preallocated_slots() {
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
// across two positions, so the call compiles to a CallVec that applies it
// per position at runtime.
#[test]
fn an_operator_on_numeric_tuples_compiles_to_call_vec() {
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

// fn id(x) { x }; id((1, 2, 3))
//
// Regular function call (not operator syntax) must NEVER compile to CallVec
// even when the argument is a tuple — vec dispatch is gated to operator
// syntax via the `OperatorCall` AST variant.
#[test]
fn a_named_call_with_a_tuple_argument_is_a_plain_call() {
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

// struct Point { x: Int }
// let point = Point(1);
// point.x = 2;
//
// A member assignment calls the field's setter `x=` with the receiver and
// then the value. The intrinsics `cast`, `cast?` and the tuple index take
// global slots 0 to 2, and `Point`, `x` and `x=` 3 to 5, so `point` is
// global 6. However the setter itself is loaded, the call is:
//
// <setter>, GetGlobal(6), Constant(_), Call(2)
#[test]
fn member_assignment_calls_the_setter_with_the_receiver_and_the_value() {
    let ops = compile_with_analysis("struct Point { x: Int }\nlet point = Point(1);\npoint.x = 2;");

    assert!(
        ops.windows(4)
            .any(|window| matches!(window, [_, GetGlobal(6), Constant(_), Call(2)])),
        "expected setter, receiver, value, Call(2); got: {ops:?}",
    );
}

// fn add(a: Int, b: Int, c: Int) -> Int { a + b + c } while true { add(1, 2, break); }
//
// The break sits inside a partially compiled call (the `add` callee and the
// constants `1` and `2` are already on the stack), so it pops those three
// pending operands before jumping out of the loop.
#[test]
fn break_pops_the_operands_pending_in_its_expression() {
    let ops = compile_with_stdlib_unoptimized(
        "fn add(a: Int, b: Int, c: Int) -> Int { a + b + c } while true { add(1, 2, break); }",
    );
    assert!(
        ops.windows(4)
            .any(|w| matches!(w, [Pop, Pop, Pop, Jump(_)])),
        "expected three cleanup Pops before the break jump, got: {ops:?}",
    );
}

// let x: Any = 1; cast::<Int>(x)
//
// `Any` doesn't prove the target type, so the cast compiles to a CheckType
// guard after the value is loaded.
#[test]
fn an_unproven_cast_checks_the_type_at_runtime() {
    let ops = compile_with_analysis("let x: Any = 1; cast::<Int>(x)");
    assert!(
        ops.iter()
            .any(|op| matches!(op, CheckType(t) if **t == ndc_core::StaticType::Int)),
        "expected a CheckType(Int) guard, got: {ops:?}",
    );
}

// let x: Any = [1]; cast::<[Int]>(x); cast::<Int>(x)
//
// A cast to a container type checks that nothing else holds the container,
// ahead of the type check. A cast to a type without a container in it has
// nothing to check.
#[test]
fn a_cast_to_a_container_checks_it_is_unshared() {
    let ops = compile_with_analysis("let x: Any = [1]; cast::<[Int]>(x); cast::<Int>(x)");
    let checks: Vec<usize> = ops
        .iter()
        .enumerate()
        .filter(|(_, op)| matches!(op, CheckUnshared(_)))
        .map(|(at, _)| at)
        .collect();
    assert_eq!(checks.len(), 1, "got: {ops:?}");
    assert!(
        matches!(&ops[checks[0] + 1], CheckType(t) if t.holds_a_mutable_container()),
        "expected the type check right after, got: {ops:?}",
    );
}

// cast::<Int>(1); cast::<Any>([1])
//
// Both casts are proven by the value's static type, so no runtime guard is
// emitted.
#[test]
fn a_proven_cast_needs_no_runtime_check() {
    let ops = compile_with_analysis("cast::<Int>(1); cast::<Any>([1])");
    assert!(
        ops.iter().all(|op| !matches!(op, CheckType(_))),
        "provable casts must not emit CheckType, got: {ops:?}",
    );
}

// let x: Any = 1; cast?::<Int>(x)
//
// An optional cast tests the value with a TryCast, which gives an option
// instead of a CheckType guard that fails.
#[test]
fn an_unproven_optional_cast_tries_the_type_at_runtime() {
    let ops = compile_with_analysis("let x: Any = 1; cast?::<Int>(x)");
    assert!(
        ops.iter()
            .any(|op| matches!(op, TryCast(t) if **t == ndc_core::StaticType::Int)),
        "expected a TryCast(Int), got: {ops:?}",
    );
    assert!(
        ops.iter().all(|op| !matches!(op, CheckType(_))),
        "an optional cast must not emit CheckType, got: {ops:?}",
    );
}

// cast?::<Int>(1)
//
// A proven optional cast is always `Some`, which a TryCast to `Any` makes.
#[test]
fn a_proven_optional_cast_is_always_some() {
    let ops = compile_with_analysis("cast?::<Int>(1)");
    assert!(
        ops.iter()
            .any(|op| matches!(op, TryCast(t) if **t == ndc_core::StaticType::Any)),
        "expected a TryCast(Any), got: {ops:?}",
    );
}

// let a: Any = 1; let n: Int = cast::<Int>(a);
//
// An `Any` stored where an Int is expected takes a cast, which is the only
// check: storing what the cast gives needs none.
#[test]
fn an_any_cast_for_a_typed_variable_is_checked_once() {
    let ops = compile_with_analysis("let a: Any = 1; let n: Int = cast::<Int>(a);");
    assert_eq!(
        ops.iter().filter(|op| matches!(op, CheckType(_))).count(),
        1,
        "expected a single CheckType(Int) guard, got: {ops:?}",
    );
}

// let n: Int = 1; let m = n;
//
// A value whose type is known needs no check.
#[test]
fn a_typed_value_stored_in_a_typed_variable_is_not_checked() {
    let ops = compile_with_analysis("let n: Int = 1; let m: Int = n;");
    assert!(
        ops.iter().all(|op| !matches!(op, CheckType(_))),
        "a value of a known type must not be checked, got: {ops:?}",
    );
}

// fn outer() {
//     let x = 1;
//     let f = fn() { x };
//     x = 2;
//     x
// }
//
// `x` is captured, so its reads and writes go through the cell the closure
// shares. Its declaration writes the slot directly: the first capture is what
// moves the value into a cell. (At the top level `x` would be a
// global, which closures reach directly.)
#[test]
fn a_captured_local_is_read_and_written_through_its_cell() {
    assert_eq!(
        compile_nested_function(
            "fn outer() {\nlet x = 1;\nlet f = fn() { x };\nx = 2;\nx\n}",
            &[0]
        ),
        [
            Constant(0),
            SetLocal(0),
            Constant(1),
            Pop,
            Closure {
                constant_idx: 2,
                values: Rc::from([CaptureSource::Local(0)]),
            },
            SetLocal(1),
            Constant(3),
            Pop,
            Constant(4),
            SetCell(0),
            Constant(5),
            Pop,
            GetCell(0),
            Return
        ]
    );
}

// fn outer() { fn f() -> Any { f } }
//
// `f` captures itself before its value exists, so its slot gets a fresh cell
// before the closure is made, and the closure is written into that cell.
// (At the top level `f` would be a global, which needs no cell.) What it returns
// contains itself, which only an `Any` holds.
#[test]
fn a_self_capturing_function_gets_its_cell_before_its_closure() {
    assert_eq!(
        compile_nested_function("fn outer() { fn f() -> Any { f } }", &[0]),
        [
            NewCell(0),
            Closure {
                constant_idx: 0,
                values: Rc::from([CaptureSource::Local(0)]),
            },
            SetCell(0),
            Constant(1),
            Return
        ]
    );
}

// fn outer() {
//     let x = 1;
//     fn inner() { x = 2; x }
// }
//
// A closure reaches a captured local through its upvalues: `x` is upvalue 0
// of `inner`, the function that is constant 2 of `outer`.
#[test]
fn a_closure_reaches_captured_variables_through_its_upvalues() {
    assert_eq!(
        compile_nested_function(
            "fn outer() {\nlet x = 1;\nfn inner() { x = 2; x }\n}",
            &[0, 2]
        ),
        [
            Constant(0),
            SetUpvalue(0),
            Constant(1),
            Pop,
            GetUpvalue(0),
            Return
        ]
    );
}

// let g = 1;
// fn f() { g = 2; g }
//
// A function reaches a top-level declaration as a global: `g` is global 3,
// after the intrinsics,
// which `f` writes and reads with SetGlobal and GetGlobal. `f` captures
// nothing, so its declaration loads it as a plain constant, with no Closure.
#[test]
fn a_function_reaches_top_level_declarations_as_globals_without_capturing_them() {
    let source = "let g = 1;\nfn f() { g = 2; g }";
    assert!(
        compile_with_analysis(source)
            .iter()
            .all(|op| !matches!(op, Closure { .. })),
        "a function that only uses globals needs no closure",
    );
    assert_eq!(
        compile_nested_function(source, &[2]),
        [
            Constant(0),
            SetGlobal(3),
            Constant(1),
            Pop,
            GetGlobal(3),
            Return
        ]
    );
}

// fn f() {
//     let values = [1];
//     values[0] += { let delta = 2; delta };
//     values
// }
//
// `values` and `delta` take slots 0 and 1 of `f`'s frame. The indexed
// assignment's temporaries (cached container, cached index and the operation
// result) come after them, in slots 2 to 4.
#[test]
fn temporaries_in_a_function_come_after_its_frames_own_locals() {
    let ops = compile_nested_function_with_stdlib(
        "fn f() {\nlet values = [1];\nvalues[0] += { let delta = 2; delta };\nvalues\n}",
        &[0],
    );
    let mut written = ops
        .iter()
        .filter_map(|op| match op {
            SetLocal(slot) => Some(*slot),
            _ => None,
        })
        .collect::<Vec<_>>();
    written.sort_unstable();
    written.dedup();
    assert_eq!(written, [0, 1, 2, 3, 4], "{ops:?}");
}

/// The functions `function`'s code puts on the stack, in order.
fn nested_functions(function: &CompiledFunction) -> Vec<Rc<CompiledFunction>> {
    function
        .opcodes()
        .iter()
        .filter_map(|opcode| match opcode {
            Constant(index)
            | Closure {
                constant_idx: index,
                ..
            } => Some(*index),
            _ => None,
        })
        .filter_map(|index| match function.body().constant(index) {
            ndc_vm::Value::Object(object) => match object.as_ref() {
                ndc_vm::Object::Function(ndc_vm::Function::Compiled(function)) => {
                    Some(Rc::clone(function))
                }
                _ => None,
            },
            _ => None,
        })
        .collect()
}

/// The compiled top-level code of `input`, with the standard library.
fn compile_program_with_stdlib(input: &str) -> CompiledFunction {
    let mut interp = ndc_interpreter::Interpreter::capturing();
    interp.configure(ndc_stdlib::register);
    interp
        .compile_str_unoptimized(input)
        .expect("compile failed")
}

// A generic function's declaration compiles each of its instances: constant
// 0 for `adder` with Ints and 1 with Floats, in some order. A lambda in the
// body is compiled with the instance, whose `+` is picked for its types, so
// the two differ.
#[test]
fn a_function_declared_in_an_instance_uses_the_instances_picks() {
    let program = compile_program_with_stdlib(
        "fn adder(x) { let g = fn(y) => y + 1; g(x) } adder(1); adder(1.5);",
    );
    let first = nested_functions(&function_constant(&program, 0))[0].clone();
    let second = nested_functions(&function_constant(&program, 1))[0].clone();
    assert!(!Rc::ptr_eq(&first, &second));
    assert_ne!(first.opcodes(), second.opcodes());
}

// A function declared in an instance that compiles the same in every
// instance is the same function, so it keeps its identity.
#[test]
fn a_function_declared_in_an_instance_that_compiles_the_same_is_the_same() {
    let program =
        compile_program_with_stdlib("fn make(x) { x + 1; fn f() => 42; f } make(1); make(1.5);");
    let first = nested_functions(&function_constant(&program, 0))[0].clone();
    let second = nested_functions(&function_constant(&program, 1))[0].clone();
    assert!(Rc::ptr_eq(&first, &second));
}

/// The global slots of the natives called `name`.
fn native_slots(name: &str) -> Vec<usize> {
    use ndc_interpreter::Builtins;
    let mut interp = ndc_interpreter::Interpreter::capturing();
    interp.configure(ndc_stdlib::register);
    let natives = interp.natives();
    (0..natives.len())
        .filter(|&slot| natives.name(slot) == name)
        .collect()
}

// The program lists every native its code reads, in the functions it
// declares too, whether or not they're called, so the VM's globals hold them.
#[test]
fn a_program_lists_the_natives_its_functions_read() {
    let program = compile_program_with_stdlib("fn f() { fn g() => len([1]); g } let h = print;");
    let print = native_slots("print");
    let len = native_slots("len");
    assert!(program.natives().iter().any(|slot| print.contains(slot)));
    assert!(program.natives().iter().any(|slot| len.contains(slot)));
    assert!(
        program
            .natives()
            .iter()
            .all(|slot| print.contains(slot) || len.contains(slot)),
        "only print and len are read: {:?}",
        program.natives()
    );
}

// fn fib(n) { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }
// fib(30);
//
// In the Int instance of `fib`, `<`, `-` and `+` resolve to the natives for
// two Ints, which the VM has instructions for. The compiler writes those in
// place of the calls, after the operands; only `fib` itself is called.
#[test]
fn a_call_resolved_to_a_native_with_an_instruction_is_the_instruction() {
    let ops = compile_nested_function_with_stdlib(
        "fn fib(n) { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }\nfib(30);",
        &[0],
    );
    assert!(
        matches!(
            ops[..],
            [
                GetLocal(0),
                Constant(0),
                Less,
                JumpIfFalse(_),
                Pop,
                GetLocal(0),
                Jump(_),
                Pop,
                GetGlobal(_),
                GetLocal(0),
                Constant(1),
                SubInt,
                Call(1),
                GetGlobal(_),
                GetLocal(0),
                Constant(2),
                SubInt,
                Call(1),
                AddInt,
                Return,
            ]
        ),
        "{ops:?}"
    );
}

// let total = 1.5; total += 2.0; total -= 0.5; total *= 3.0;
//
// An augmented assignment that resolves to such a native is its instruction
// too, between reading the target and storing to it.
#[test]
fn an_augmented_assignment_resolved_to_a_native_with_an_instruction_is_the_instruction() {
    let ops = compile_with_stdlib_unoptimized(
        "let total = 1.5; total += 2.0; total -= 0.5; total *= 3.0;",
    );
    let operations = ops
        .iter()
        .filter(|op| is_native_op(op) || matches!(op, Call(_)))
        .collect::<Vec<_>>();
    assert_eq!(operations, [&AddFloat, &SubFloat, &MulFloat,], "{ops:?}");
}

// 7 / 2; "a" ++ "b"; 7 % 2;
//
// A native the VM has no instruction for is called by its global slot, with
// no callee under its arguments.
#[test]
fn a_call_resolved_to_a_native_without_an_instruction_calls_it_by_its_slot() {
    for source in ["7 / 2;", "\"a\" ++ \"b\";", "7 % 2;"] {
        let ops = compile_with_stdlib_unoptimized(source);
        assert!(
            ops.iter()
                .any(|op| matches!(op, CallNative { args: 2, .. }))
                && !ops
                    .iter()
                    .any(|op| is_native_op(op) || matches!(op, Call(_) | GetGlobal(_))),
            "`{source}` must call the native by its slot: {ops:?}"
        );
    }
}

// "a" < "b"; [1] == [1]; (1, 2) != (2, 1);
//
// The comparisons and equality do the same for every overload, so each
// overload is written as the instruction.
#[test]
fn comparisons_of_any_values_are_instructions() {
    let ops = compile_with_stdlib_unoptimized("\"a\" < \"b\"; [1] == [1]; (1, 2) != (2, 1);");
    let operations = ops
        .iter()
        .filter(|op| is_native_op(op) || matches!(op, Call(_)))
        .collect::<Vec<_>>();
    assert_eq!(operations, [&Less, &Equal, &NotEqual,], "{ops:?}");
}

// let xs = [1, 2]; len(xs)
//
// A call the analyser resolves to one native names it by its global slot:
// the arguments go on the stack with no callee under them, and `CallNative`
// calls the native there.
#[test]
fn a_call_resolved_to_a_native_calls_it_by_its_slot() {
    let ops = compile_with_stdlib("let xs = [1, 2]; len(xs)");
    let len = native_slots("len");
    assert!(
        ops.windows(2).any(|pair| matches!(
            pair,
            [GetGlobal(_), CallNative { slot, args: 1 }] if len.contains(slot)
        )),
        "expected xs then CallNative(len, 1), got: {ops:?}",
    );
}

// fn f(x: Float, y: Float, a: Int, p: Bool) { (x / y, -a, -x, !p) }
//
// Float division and the unary operators the VM has instructions for are
// instructions too, with one operand for the unary ones.
#[test]
fn float_division_and_unary_operators_are_instructions() {
    let ops = compile_nested_function_with_stdlib(
        "fn f(x: Float, y: Float, a: Int, p: Bool) { (x / y, -a, -x, !p) }",
        &[0],
    );
    let operations = ops
        .iter()
        .filter(|op| is_native_op(op) || matches!(op, Call(_) | CallNative { .. }))
        .collect::<Vec<_>>();
    assert_eq!(
        operations,
        [&DivFloat, &NegInt, &NegFloat, &Not,],
        "{ops:?}"
    );
}

// let t = (1, "a"); t[1]
//
// A tuple indexed by a number written in the code reads the element in
// place: the tuple, then `TupleIndex` with the index as its operand.
#[test]
fn a_literal_index_of_a_tuple_is_read_in_place() {
    let ops = compile_with_stdlib("let t = (1, \"a\"); t[1]");
    assert!(
        ops.windows(2)
            .any(|ops| matches!(ops, [GetGlobal(_), TupleIndex(1)])),
        "expected t, TupleIndex(1), got: {ops:?}",
    );
    assert!(
        ops.iter()
            .all(|op| !matches!(op, Call(_) | CallNative { .. })),
        "the index should be no call: {ops:?}",
    );
}

// fn f(t: (Int, Int)) { t[1] }
//
// A local tuple's element is read straight from its slot, in one
// instruction.
#[test]
fn a_literal_index_of_a_local_tuple_is_one_instruction() {
    assert_eq!(
        compile_nested_function_with_stdlib("fn f(t: (Int, Int)) { t[-1] }", &[0]),
        [GetLocalTupleIndex { slot: 0, index: -1 }, Return],
    );
}

// let t = (1, "a"); t[-1]
//
// A negative index is the operand too, counted from the end when it runs.
#[test]
fn a_negative_literal_index_of_a_tuple_is_the_operand() {
    let ops = compile_with_stdlib("let t = (1, \"a\"); t[-1]");
    assert!(
        ops.windows(2)
            .any(|ops| matches!(ops, [GetGlobal(_), TupleIndex(-1)])),
        "expected t, TupleIndex(-1), got: {ops:?}",
    );
    assert!(
        ops.iter().all(|op| !matches!(op, NegInt)),
        "the index should be no negation: {ops:?}",
    );
}
