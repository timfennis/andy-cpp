use ndc_lexer::Lexer;
use ndc_parser::Parser;
use ndc_vm::chunk::OpCode;
use ndc_vm::chunk::OpCode::*;
use ndc_vm::compiler::Compiler;

fn compile(input: &str) -> Vec<OpCode> {
    let tokens = Lexer::new(input)
        .collect::<Result<Vec<_>, _>>()
        .expect("lex failed");
    let expressions = Parser::from_tokens(tokens).parse().expect("parse failed");
    Compiler::compile(expressions.into_iter())
        .expect("compile failed")
        .opcodes()
        .to_vec()
}

fn compile_with_analysis(input: &str) -> Vec<OpCode> {
    let mut interp = ndc_interpreter::Interpreter::new(Vec::new());
    interp
        .compile_str(input)
        .expect("compile failed")
        .opcodes()
        .to_vec()
}

// if true { 1 }
//
// 0: Constant(0)      push `true`
// 1: JumpIfFalse(2)   if false, skip true-branch and land on final Pop
// 2: Pop              pop condition (true path)
// 3: Constant(1)      push `1`
// 4: Pop              pop condition (false path, jumped here)
// 5: Halt
#[test]
fn test_if_without_else() {
    assert_eq!(
        compile("if true { 1 }"),
        [Constant(0), JumpIfFalse(2), Pop, Constant(1), Pop, Halt]
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
            JumpIfFalse(3),
            Pop,
            Constant(1),
            Jump(2),
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
        [Constant(0), JumpIfFalse(2), Pop, Constant(1), Halt]
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
        [Constant(0), JumpIfTrue(2), Pop, Constant(1), Halt]
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
            JumpIfFalse(3),
            Pop,
            Constant(1),
            Jump(4),
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
            JumpIfFalse(5),
            Pop,
            Constant(1),
            Pop,
            Constant(2),
            Jump(4),
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
// 1: JumpIfFalse(3)   if false, jump past body to exit Pop (index 5)
// 2: Pop              pop condition (true path)
// 3: Constant(1)      body: push `1`
// 4: Jump(-5)         jump back to loop_start (index 0)
// 5: Pop              pop condition (false path, loop exit)
// 6: Halt
#[test]
fn test_while() {
    assert_eq!(
        compile("while true { 1 }"),
        [
            Constant(0),
            JumpIfFalse(3),
            Pop,
            Constant(1),
            Jump(-5),
            Pop,
            Halt
        ]
    );
}

// let a = 1;
//
// Value is compiled, then SetLocal stores it in pre-allocated slot 0.
//
// 0: Constant(0)   push `1`
// 1: SetLocal(0)   store in slot 0
// 2: Halt
#[test]
fn test_declaration() {
    assert_eq!(
        compile_with_analysis("let a = 1;"),
        [Constant(0), SetLocal(0), Halt]
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
// 2: Constant(1)   push `5`
// 3: SetLocal(0)   overwrite slot 0 (assignment)
// 4: Constant(2)   push `()` (assignment result)
// 5: Pop           discard (statement)
// 6: Halt
#[test]
fn test_assignment() {
    assert_eq!(
        compile_with_analysis("let a = 1;\na = 5;"),
        [
            Constant(0),
            SetLocal(0),
            Constant(1),
            SetLocal(0),
            Constant(2),
            Pop,
            Halt
        ]
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
// 2: GetLocal(0)   push `a` (block result)
// 3: Halt
#[test]
fn test_block_scope_cleanup() {
    assert_eq!(
        compile_with_analysis("{ let a = 3; a }"),
        [Constant(0), SetLocal(0), GetLocal(0), Halt]
    );
}

// { let a = 1; let b = 2; a }
//
// Both locals stored via SetLocal into pre-allocated slots.
// Block result is `a`, read via GetLocal. No cleanup needed.
//
// 0: Constant(0)   push `1`
// 1: SetLocal(0)   store in slot 0
// 2: Constant(1)   push `2`
// 3: SetLocal(1)   store in slot 1
// 4: GetLocal(0)   push `a` (block result)
// 5: Halt
#[test]
fn test_block_scope_cleanup_multiple_locals() {
    assert_eq!(
        compile_with_analysis("{ let a = 1; let b = 2; a }"),
        [Constant(0), SetLocal(0), Constant(1), SetLocal(1), GetLocal(0), Halt]
    );
}
