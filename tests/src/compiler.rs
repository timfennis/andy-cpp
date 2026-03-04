use ndc_lexer::Lexer;
use ndc_parser::Parser;
use ndc_vm::chunk::OpCode;
use ndc_vm::chunk::OpCode::*;
use ndc_vm::compiler::Compiler;

fn compile(input: &str) -> Vec<OpCode> {
    let tokens = Lexer::new(input)
        .collect::<Result<Vec<_>, _>>()
        .expect("lex failed");
    let expressions = Parser::from_tokens(tokens)
        .parse()
        .expect("parse failed");
    Compiler::compile(expressions.into_iter())
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
