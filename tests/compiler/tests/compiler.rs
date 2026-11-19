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
    Compiler::compile_unoptimized(expressions.into_iter(), Analysis::default())
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
// An `if` without an `else` is `()`, so its branch is compiled for what it
// does, and both paths meet to push the `()`.
//
// 0: Constant(0)        push `true`
// 1: PopJumpIfFalse(2)  pop the condition; if false, skip the branch (index 4)
// 2: Constant(1)        push `1`
// 3: Pop                drop the branch's value
// 4: Constant(2)        push `()`
// 5: Halt
#[test]
fn an_if_without_else_yields_unit_on_both_paths() {
    let expected = [
        Constant(0),
        PopJumpIfFalse(JumpTarget::Offset(2)),
        Constant(1),
        Pop,
        Constant(2),
        Halt,
    ];
    let compiled = compile_function("if true { 1 }");
    assert_eq!(compiled.opcodes(), expected);
    assert!(
        compiled.body().constant(2).is_unit(),
        "both paths push unit"
    );
    assert_eq!(compile_with_analysis("if true { 1 }"), expected);
}

// A branch that's already `()`, or never finishes, is compiled for what it
// does like any other.
#[test]
fn an_if_without_else_compiles_a_unit_or_never_branch_for_what_it_does() {
    assert_eq!(
        compile_with_analysis("if true { () }"),
        [
            Constant(0),
            PopJumpIfFalse(JumpTarget::Offset(2)),
            MakeTuple(0),
            Pop,
            Constant(1),
            Halt
        ]
    );
    // The inner `if`'s branch is a `break`, which is `Never`. The loop's body
    // is compiled for what it does, so the `if` pushes no `()`, and the
    // `break` lands where the loop's condition exits it.
    //
    // 0: Constant(0)        push `true`  ← loop start
    // 1: PopJumpIfFalse(4)  exit the loop (index 6)
    // 2: Constant(1)        push `true`
    // 3: PopJumpIfFalse(1)  skip the branch (index 5)
    // 4: Jump(1)            `break` (index 6)
    // 5: Jump(-6)           back to the loop start
    // 6: Constant(2)        push `()`, the loop's value
    // 7: Halt
    assert_eq!(
        compile_with_analysis("while true { if true { break } }"),
        [
            Constant(0),
            PopJumpIfFalse(JumpTarget::Offset(4)),
            Constant(1),
            PopJumpIfFalse(JumpTarget::Offset(1)),
            Jump(JumpTarget::Offset(1)),
            Jump(JumpTarget::Offset(-6)),
            Constant(2),
            Halt
        ]
    );
}

// if true { 1 } else { 2 }
//
// 0: Constant(0)        push `true`
// 1: PopJumpIfFalse(2)  pop the condition; if false, jump to else (index 4)
// 2: Constant(1)        push `1`
// 3: Jump(1)            skip else, jump to Halt (index 5)
// 4: Constant(2)        push `2`
// 5: Halt
#[test]
fn an_if_pops_its_condition_as_it_branches() {
    assert_eq!(
        compile("if true { 1 } else { 2 }"),
        [
            Constant(0),
            PopJumpIfFalse(JumpTarget::Offset(2)),
            Constant(1),
            Jump(JumpTarget::Offset(1)),
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
// 0: Constant(0)        push `true`
// 1: PopJumpIfFalse(2)  pop the condition; if false, jump to the false branch (index 4)
// 2: Constant(1)        push `3`
// 3: Jump(3)            jump to Halt (index 7)
// 4: Constant(2)        push `3` (inner of `3;`)
// 5: Pop                discard (trailing semicolon)
// 6: Constant(3)        push `()` (block result)
// 7: Halt
#[test]
fn each_if_branch_leaves_one_value() {
    assert_eq!(
        compile("if true { 3 } else { 3; }"),
        [
            Constant(0),
            PopJumpIfFalse(JumpTarget::Offset(2)),
            Constant(1),
            Jump(JumpTarget::Offset(3)),
            Constant(2),
            Pop,
            Constant(3),
            Halt
        ]
    );
}

// while true { 1 }
//
// 0: Constant(0)        push `true`  ← loop_start
// 1: PopJumpIfFalse(3)  pop the condition; if false, exit the loop (index 5)
// 2: Constant(1)        body: push `1`
// 3: Pop                discard body value (each iteration's result is unobservable)
// 4: Jump(-5)           jump back to loop_start (index 0)
// 5: Constant(2)        push `()` (the loop's result)
// 6: Halt
#[test]
fn a_while_loop_discards_its_body_value_and_yields_unit() {
    assert_eq!(
        compile("while true { 1 }"),
        [
            Constant(0),
            PopJumpIfFalse(JumpTarget::Offset(3)),
            Constant(1),
            Pop,
            Jump(JumpTarget::Offset(-5)),
            Constant(2),
            Halt
        ]
    );
}

// let a = 1;
// a = 5;
//
// A top-level declaration is a global, so the declaration stores 1 into
// global slot 2 (there are no natives here, and the language's intrinsics
// `cast` and `cast?` take slots 0 and 1).
// Assignment pushes new value, SetGlobal overwrites. Both are statements,
// whose `()` nothing wants, so neither pushes one.
//
// 0: Constant(0)    push `1`
// 1: SetGlobal(2)   store in global slot 2 (declaration)
// 2: Constant(1)    push `5`
// 3: SetGlobal(2)   overwrite global slot 2 (assignment)
// 4: Halt
#[test]
fn a_top_level_variable_is_declared_and_assigned_through_its_global() {
    assert_eq!(
        compile_with_analysis("let a = 1;\na = 5;"),
        [Constant(0), SetGlobal(2), Constant(1), SetGlobal(2), Halt]
    );
}

// let value = 1;
// value += 2;
//
// An augmented assignment stores the operation's result back into its target.
// That holds for a `++=` that mutates the list in place and returns it as much
// as for a `+` that makes a new value. A variable target stores with its Set
// opcode, and as a statement pushes no unit:
//
// Call(2), SetGlobal(_), Halt
//
// An indexed target passes the result to `[]=`, and pops that call's unit:
//
// Call(3), Pop, Halt
#[test]
fn an_augmented_assignment_stores_its_result_back_even_when_it_mutates_in_place() {
    for source in [
        "let value = 1; value += 2;",
        "let value = [1]; value ++= [2];",
    ] {
        let ops = compile_with_stdlib_unoptimized(source);
        assert!(
            ops.windows(3)
                .any(|ops| matches!(ops, [Call(2), SetGlobal(_), Halt])),
            "`{source}` must store the operation result: {ops:?}",
        );
    }

    for source in [
        "let values = [1]; values[0] += 2;",
        "let value = [1]; let values = [value]; values[0] ++= [2];",
    ] {
        let ops = compile_with_stdlib_unoptimized(source);
        assert!(
            ops.windows(3)
                .any(|ops| matches!(ops, [Call(3), Pop, Halt])),
            "`{source}` must store through []=: {ops:?}",
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
// 2: Constant(1)   push `2`
// 3: SetLocal(1)   store in slot 1
// 4: GetLocal(0)   push `a` (block result)
// 5: Halt
#[test]
fn block_locals_live_in_preallocated_slots() {
    assert_eq!(
        compile_with_analysis("{ let a = 1; let b = 2; a }"),
        [
            Constant(0),
            SetLocal(0),
            Constant(1),
            SetLocal(1),
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
// then the value. The intrinsics `cast` and `cast?` take global slots 0 and
// 1, and `Point`, `x` and `x=` 2 to 4, so `point` is global 5. However the
// setter itself is loaded, the call is:
//
// <setter>, GetGlobal(5), Constant(_), Call(2)
#[test]
fn member_assignment_calls_the_setter_with_the_receiver_and_the_value() {
    let ops = compile_with_analysis("struct Point { x: Int }\nlet point = Point(1);\npoint.x = 2;");

    assert!(
        ops.windows(4)
            .any(|window| matches!(window, [_, GetGlobal(5), Constant(_), Call(2)])),
        "expected setter, receiver, value, Call(2); got: {ops:?}",
    );
}

// fn add(a: Int, b: Int) -> Int { a + b } while true { print(add(1, break)); }
//
// The break sits inside a partially compiled call (`print` and `add` callees
// and the constant `1` are already on the stack), so it pops those three
// pending operands before jumping out of the loop.
#[test]
fn break_pops_the_operands_pending_in_its_expression() {
    let ops = compile_with_stdlib_unoptimized(
        "fn add(a: Int, b: Int) -> Int { a + b } while true { print(add(1, break)); }",
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
            Closure {
                constant_idx: 1,
                values: Rc::from([CaptureSource::Local(0)]),
            },
            SetLocal(1),
            Constant(2),
            SetCell(0),
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
// of `inner`, the function that is constant 1 of `outer`.
#[test]
fn a_closure_reaches_captured_variables_through_its_upvalues() {
    assert_eq!(
        compile_nested_function(
            "fn outer() {\nlet x = 1;\nfn inner() { x = 2; x }\n}",
            &[0, 1]
        ),
        [Constant(0), SetUpvalue(0), GetUpvalue(0), Return]
    );
}

// let g = 1;
// fn f() { g = 2; g }
//
// A function reaches a top-level declaration as a global: `g` is global 2,
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
        compile_nested_function(source, &[1]),
        [Constant(0), SetGlobal(2), GetGlobal(2), Return]
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
