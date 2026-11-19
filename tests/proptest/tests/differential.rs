//! Differential test of the two VMs: a generated program must do the same on
//! the stack VM and on the register VM, printing the same, giving the same
//! value, and failing with the same error at the same place.
//!
//! The programs are made from what the language has, so most of them get
//! past the parser and the analyser and run: arithmetic on ints and floats,
//! variables, `if`, `while` with a counter, lists and tuples and indexing,
//! functions and closures, `for` over ranges and lists, assignment with an
//! operator, `break` and `continue`, `return`. Every loop ends after a few
//! rounds and no list grows by more than a few elements a round, so a
//! program can't run or allocate without bound.
//!
//! A program is generated from a list of numbers, each of which picks one of
//! the choices the generator has at that point; when the list runs out,
//! every choice is the first, the simplest one. Proptest shrinks a failing
//! list, which shrinks the program, and prints the program as source.
//!
//! * `generated_programs_do_the_same_on_both_vms` runs in every `cargo test`:
//!   a few hundred programs, and the seeds in `differential.regressions`.
//! * `fuzz_generated_programs_on_both_vms` is `#[ignore]`d, for a long run:
//!
//!   ```sh
//!   cargo test -p proptest_tests --test differential -- --ignored
//!   PROPTEST_CASES=100000 cargo test -p proptest_tests --test differential -- --ignored
//!   ```

use ndc_interpreter::{Backend, Interpreter, InterpreterError};
use proptest::prelude::*;
use proptest::strategy::ValueTree;
use proptest::test_runner::{FileFailurePersistence, TestCaseError, TestRunner};
use std::fmt::{self, Write};
use std::panic::{self, AssertUnwindSafe};
use std::sync::Once;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
// The other harnesses use these; this one goes through the interpreter.
use ndc_analyser as _;
use ndc_core as _;
use ndc_lexer as _;
use ndc_parser as _;
use ndc_vm as _;
use num as _;

/// How long both VMs together may take on one program.
const PER_CASE_TIMEOUT: Duration = Duration::from_secs(5);

/// How many choices a program is generated from, at least and at most.
const MIN_CHOICES: usize = 50;
const MAX_CHOICES: usize = 400;

/// How many rounds a loop goes, at most.
const MAX_ROUNDS: usize = 4;

/// How deeply loops nest, at most.
const MAX_LOOP_DEPTH: usize = 2;

/// How deeply expressions nest, at most.
const MAX_EXPRESSION_DEPTH: usize = 3;

/// How deeply blocks of statements nest, at most.
const MAX_BLOCK_DEPTH: usize = 3;

/// A generated program, which a failure shows as its source.
#[derive(Clone)]
struct Program(String);

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\n{}\n", self.0)
    }
}

/// The type of a variable the generator made, which says what it can do
/// with it.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Type {
    Int,
    Float,
    Bool,
    /// `[Int]`
    List,
    /// `(Int, Int)`
    Pair,
    /// `%{Int: Int}`
    Map,
    /// `fn(Int) -> Int`
    Closure,
}

/// A variable in scope where the generator is.
#[derive(Clone)]
struct Var {
    name: String,
    typ: Type,
    /// Whether the program may assign it: a loop's counter and the variable
    /// of a `for` loop may not be.
    assignable: bool,
}

/// Where the statement being generated is.
#[derive(Clone, Copy)]
struct Context {
    /// Whether it's in a loop, so `break` and `continue` can go here.
    in_loop: bool,
    /// How many more loops may nest here: fewer in a function, which may be
    /// called in a loop.
    loop_budget: usize,
    /// Whether it's in the body of a function that returns an `Int`, so
    /// `return` can go here.
    returns_int: bool,
    /// Whether it's in the body of a `for` over a list, where no list may
    /// grow: the loop would go on over what it adds.
    lists_frozen: bool,
    /// How many blocks deep it is.
    blocks: usize,
}

/// Writes a program as it reads its choices.
struct Generator<'a> {
    choices: &'a [u16],
    next: usize,
    vars: Vec<Var>,
    /// The functions declared so far: each takes two `Int`s and returns one.
    functions: Vec<String>,
    /// The generic functions declared so far: each takes an `Int` or a
    /// `Float` and returns one of the same.
    generics: Vec<String>,
    names: usize,
}

impl Generator<'_> {
    /// One of `0..n`, picked by the next choice: 0 once the choices run out.
    fn pick(&mut self, n: usize) -> usize {
        let choice = self.choices.get(self.next).copied().unwrap_or(0);
        self.next += 1;
        usize::from(choice) % n
    }

    /// Whether the next choice says yes, 0 being no.
    fn chance(&mut self, one_in: usize) -> bool {
        self.pick(one_in) == one_in - 1
    }

    fn exhausted(&self) -> bool {
        self.next >= self.choices.len()
    }

    /// A name no variable or function has yet: `v3`.
    fn fresh(&mut self, prefix: &str) -> String {
        self.names += 1;
        format!("{prefix}{}", self.names)
    }

    /// A variable of type `typ`, one that may be assigned if `assignable`.
    fn var(&mut self, typ: Type, assignable: bool) -> Option<String> {
        let candidates = self
            .vars
            .iter()
            .filter(|var| var.typ == typ && (var.assignable || !assignable))
            .map(|var| var.name.clone())
            .collect::<Vec<_>>();
        if candidates.is_empty() {
            return None;
        }
        let index = self.pick(candidates.len());
        Some(candidates[index].clone())
    }

    /// The whole program: some functions, then statements, then the value,
    /// either at the top level or in the body of a function `main` that the
    /// program calls, so its variables are locals instead of globals.
    fn program(&mut self) -> String {
        let mut out = String::new();
        for _ in 0..self.pick(3) {
            let function = self.function();
            out.push_str(&function);
        }
        if self.chance(2) {
            // A function without types, which each call with an `Int` or a
            // `Float` makes an instance of.
            let name = self.fresh("g");
            let body = ["x + x", "x * x - x", "max(x, x + x)"][self.pick(3)];
            writeln!(out, "fn {name}(x) {{ {body} }};").unwrap();
            self.generics.push(name);
        }
        let in_main = self.chance(2);
        let context = Context {
            in_loop: false,
            loop_budget: MAX_LOOP_DEPTH,
            returns_int: false,
            lists_frozen: false,
            blocks: 0,
        };
        let mut body = String::new();
        let statements = 1 + self.pick(12);
        for _ in 0..statements {
            let statement = self.statement(context);
            body.push_str(&statement);
            if self.exhausted() {
                break;
            }
        }
        let value = self.value();
        if in_main {
            writeln!(out, "fn main() {{\n{body}{value}\n}}\nmain()").unwrap();
        } else {
            writeln!(out, "{body}{value}").unwrap();
        }
        out
    }

    /// `fn f1(a: Int, b: Int) -> Int { ... }`, whose body sees only its
    /// parameters and the functions before it.
    fn function(&mut self) -> String {
        let name = self.fresh("f");
        let outer = std::mem::take(&mut self.vars);
        let (a, b) = (self.fresh("a"), self.fresh("b"));
        for parameter in [&a, &b] {
            self.vars.push(Var {
                name: parameter.clone(),
                typ: Type::Int,
                assignable: true,
            });
        }
        let context = Context {
            in_loop: false,
            loop_budget: 1,
            returns_int: true,
            lists_frozen: false,
            blocks: 1,
        };
        let body = self.statements(context, 3);
        let result = self.expression(Type::Int, 0);
        self.vars = outer;
        self.functions.push(name.clone());
        // A `pure fn` remembers what it returned for each argument.
        let keyword = if self.chance(4) { "pure fn" } else { "fn" };
        format!("{keyword} {name}({a}: Int, {b}: Int) -> Int {{\n{body}{result}\n}};\n")
    }

    /// Up to `most` statements, in a scope of their own.
    fn statements(&mut self, context: Context, most: usize) -> String {
        let scope = self.vars.len();
        let mut out = String::new();
        if context.blocks < MAX_BLOCK_DEPTH {
            for _ in 0..self.pick(most + 1) {
                let statement = self.statement(context);
                out.push_str(&statement);
            }
        }
        self.vars.truncate(scope);
        out
    }

    /// A block's statements, `{ ... }`.
    fn block(&mut self, context: Context) -> String {
        let context = Context {
            blocks: context.blocks + 1,
            ..context
        };
        format!("{{\n{}}}", self.statements(context, 3))
    }

    fn statement(&mut self, context: Context) -> String {
        match self.pick(17) {
            0 => {
                let typ = self.any_type();
                format!("print({});\n", self.expression(typ, 0))
            }
            3 => {
                let typ = self.any_type();
                match self.var(typ, true) {
                    Some(var) => format!("{var} = {};\n", self.expression(typ, 0)),
                    None => self.declaration(context),
                }
            }
            4 => self.assignment_with_operator(context),
            5 => match self.var(Type::List, false) {
                Some(list) if !context.lists_frozen => {
                    format!("{list}.push({});\n", self.expression(Type::Int, 1))
                }
                _ => self.declaration(context),
            },
            6 => match self.var(Type::List, false) {
                Some(list) if !context.lists_frozen => format!(
                    "{list}[{}] = {};\n",
                    self.index(2),
                    self.expression(Type::Int, 1)
                ),
                _ => self.declaration(context),
            },
            7 => {
                let condition = self.expression(Type::Bool, 1);
                let then = self.block(context);
                if self.chance(2) {
                    let otherwise = self.block(context);
                    format!("if {condition} {then} else {otherwise};\n")
                } else {
                    format!("if {condition} {then};\n")
                }
            }
            8 if context.loop_budget > 0 => self.while_loop(context),
            9 if context.loop_budget > 0 => self.for_range(context),
            10 if context.loop_budget > 0 => self.for_list(context),
            11 => match self.var(Type::Pair, false) {
                Some(pair) => {
                    let (first, second) = (self.fresh("v"), self.fresh("v"));
                    for name in [&first, &second] {
                        self.vars.push(Var {
                            name: name.clone(),
                            typ: Type::Int,
                            assignable: true,
                        });
                    }
                    format!("let ({first}, {second}) = {pair};\n")
                }
                None => self.declaration(context),
            },
            12 if context.in_loop => {
                let condition = self.expression(Type::Bool, 1);
                let jump = if self.chance(2) { "continue" } else { "break" };
                format!("if {condition} {{ {jump}; }};\n")
            }
            13 if context.returns_int => {
                let condition = self.expression(Type::Bool, 1);
                let value = self.expression(Type::Int, 1);
                format!("if {condition} {{ return {value}; }};\n")
            }
            14 => format!("{};\n", self.expression(Type::Int, 0)),
            15 => match self.var(Type::Map, false) {
                // A map grows by one key at most, of the few there are.
                Some(map) => format!(
                    "{map}[{}] = {};\n",
                    self.key(),
                    self.expression(Type::Int, 1)
                ),
                None => self.declaration(context),
            },
            _ => self.declaration(context),
        }
    }

    /// `let v1 = ...;` of any type, a closure among them.
    fn declaration(&mut self, context: Context) -> String {
        let typ = match self.pick(8) {
            0 | 1 => Type::Int,
            2 => Type::Float,
            3 => Type::Bool,
            4 => Type::List,
            5 => Type::Pair,
            6 => Type::Map,
            _ => Type::Closure,
        };
        let name = self.fresh("v");
        let value = if typ == Type::Closure {
            self.closure(context)
        } else {
            self.expression(typ, 0)
        };
        self.vars.push(Var {
            name: name.clone(),
            typ,
            assignable: true,
        });
        format!("let {name} = {value};\n")
    }

    /// `fn(p1: Int) -> Int { ... }`, which may read and assign the
    /// variables around it.
    fn closure(&mut self, context: Context) -> String {
        let parameter = self.fresh("p");
        let scope = self.vars.len();
        self.vars.push(Var {
            name: parameter.clone(),
            typ: Type::Int,
            assignable: true,
        });
        // Not in a loop or a `for` over a list as far as its body goes, but
        // it may be called in one, so it grows no list and has at most one
        // loop.
        let context = Context {
            in_loop: false,
            loop_budget: 1,
            returns_int: true,
            lists_frozen: true,
            blocks: context.blocks + 1,
        };
        let body = self.statements(context, 2);
        let result = self.expression(Type::Int, 1);
        self.vars.truncate(scope);
        format!("fn({parameter}: Int) -> Int {{ {body}{result} }}")
    }

    /// `v1 += ...;` and the like, for a variable of a type that has them.
    fn assignment_with_operator(&mut self, context: Context) -> String {
        match self.pick(5) {
            3 => match self.var(Type::List, false) {
                Some(list) => {
                    let operator = ["+=", "-=", "*="][self.pick(3)];
                    format!(
                        "{list}[{}] {operator} {};\n",
                        self.index(2),
                        self.expression(Type::Int, 1)
                    )
                }
                None => self.declaration(context),
            },
            4 => match self.var(Type::Map, false) {
                Some(map) => {
                    let operator = ["+=", "-=", "*="][self.pick(3)];
                    format!(
                        "{map}[{}] {operator} {};\n",
                        self.key(),
                        self.expression(Type::Int, 1)
                    )
                }
                None => self.declaration(context),
            },
            0 => match self.var(Type::Int, true) {
                Some(var) => {
                    let operator = ["+=", "-=", "*="][self.pick(3)];
                    format!("{var} {operator} {};\n", self.expression(Type::Int, 1))
                }
                None => self.declaration(context),
            },
            1 => match self.var(Type::Float, true) {
                Some(var) => {
                    let operator = ["+=", "-=", "*="][self.pick(3)];
                    format!("{var} {operator} {};\n", self.expression(Type::Float, 1))
                }
                None => self.declaration(context),
            },
            _ => match self.var(Type::List, true) {
                // A literal on the right, so a list grows by a few elements
                // a round, never by itself.
                Some(var) if !context.lists_frozen => {
                    format!("{var} ++= {};\n", self.list_literal(1))
                }
                _ => self.declaration(context),
            },
        }
    }

    /// `{ let c1 = 0; while c1 < 3 { c1 += 1; ... } }`: the counter goes up
    /// first, so a `continue` can't skip it.
    fn while_loop(&mut self, context: Context) -> String {
        let counter = self.fresh("c");
        let rounds = self.pick(MAX_ROUNDS + 1);
        let scope = self.vars.len();
        self.vars.push(Var {
            name: counter.clone(),
            typ: Type::Int,
            assignable: false,
        });
        let condition = if self.chance(3) {
            // A condition that isn't a plain comparison, as a loop's might be.
            format!(
                "{counter} < {rounds} and {}",
                self.expression(Type::Bool, 2)
            )
        } else {
            format!("{counter} < {rounds}")
        };
        let body = self.statements(Self::loop_context(context), 3);
        self.vars.truncate(scope);
        format!("{{\nlet {counter} = 0;\nwhile {condition} {{\n{counter} += 1;\n{body}}}\n}};\n")
    }

    /// `for k1 in 0..3 { ... }`
    fn for_range(&mut self, context: Context) -> String {
        let variable = self.fresh("k");
        let start = self.pick(3);
        let end = start + self.pick(MAX_ROUNDS);
        let range = if self.chance(2) {
            format!("{start}..={end}")
        } else {
            format!("{start}..{end}")
        };
        let scope = self.vars.len();
        self.vars.push(Var {
            name: variable.clone(),
            typ: Type::Int,
            assignable: false,
        });
        let body = self.statements(Self::loop_context(context), 3);
        self.vars.truncate(scope);
        format!("for {variable} in {range} {{\n{body}}};\n")
    }

    /// `for k1 in xs { ... }`, in whose body no list grows, as the loop
    /// would go on over what was added.
    fn for_list(&mut self, context: Context) -> String {
        let list = self.expression(Type::List, 1);
        let variable = self.fresh("k");
        let scope = self.vars.len();
        self.vars.push(Var {
            name: variable.clone(),
            typ: Type::Int,
            assignable: false,
        });
        let context = Context {
            lists_frozen: true,
            ..Self::loop_context(context)
        };
        let body = self.statements(context, 3);
        self.vars.truncate(scope);
        format!("for {variable} in {list} {{\n{body}}};\n")
    }

    fn loop_context(context: Context) -> Context {
        Context {
            in_loop: true,
            loop_budget: context.loop_budget - 1,
            blocks: context.blocks + 1,
            ..context
        }
    }

    /// The program's value: a variable, a tuple of a few, or an expression.
    fn value(&mut self) -> String {
        match self.pick(3) {
            0 => {
                let typ = self.any_type();
                self.expression(typ, 0)
            }
            1 => {
                let names = self
                    .vars
                    .iter()
                    .filter(|var| var.typ != Type::Closure)
                    .map(|var| var.name.clone())
                    .collect::<Vec<_>>();
                match names.as_slice() {
                    [] => "()".to_string(),
                    [one] => one.clone(),
                    more => format!("({})", more[more.len().saturating_sub(3)..].join(", ")),
                }
            }
            _ => self.expression(Type::Int, 0),
        }
    }

    fn any_type(&mut self) -> Type {
        [
            Type::Int,
            Type::Float,
            Type::Bool,
            Type::List,
            Type::Pair,
            Type::Map,
        ][self.pick(6)]
    }

    /// An expression of type `typ`, `depth` deep in another.
    fn expression(&mut self, typ: Type, depth: usize) -> String {
        let deep = depth >= MAX_EXPRESSION_DEPTH;
        let depth = depth + 1;
        match typ {
            Type::Int => self.int(depth, deep),
            Type::Float => self.float(depth, deep),
            Type::Bool => self.bool(depth, deep),
            Type::List => self.list(depth, deep),
            Type::Pair => self.pair(depth, deep),
            Type::Map => self.map(depth),
            Type::Closure => unreachable!("a closure is only declared"),
        }
    }

    /// An index of a list: mostly one every list has, as a program stops
    /// at the first index that's out of bounds.
    fn index(&mut self, depth: usize) -> String {
        match self.pick(5) {
            0 => "0".to_string(),
            1 => "(-1)".to_string(),
            2 => "1".to_string(),
            _ => self.expression(Type::Int, depth),
        }
    }

    /// An `Int` from -5 to 15, 0 for the first choice.
    fn int_literal(&mut self) -> String {
        let n = i64::try_from(self.pick(21)).unwrap();
        let n = if n > 15 { 15 - n } else { n };
        if n < 0 {
            format!("({n})")
        } else {
            n.to_string()
        }
    }

    fn int(&mut self, depth: usize, deep: bool) -> String {
        let choices = if deep { 2 } else { 19 };
        match self.pick(choices) {
            0 => self.int_literal(),
            1 => self
                .var(Type::Int, false)
                .unwrap_or_else(|| self.int_literal()),
            2 | 3 => {
                let operator = ["+", "-", "*", "\\", "%"][self.pick(5)];
                format!(
                    "({} {operator} {})",
                    self.expression(Type::Int, depth),
                    self.expression(Type::Int, depth)
                )
            }
            4 => format!(
                "(if {} {{ {} }} else {{ {} }})",
                self.expression(Type::Bool, depth),
                self.expression(Type::Int, depth),
                self.expression(Type::Int, depth)
            ),
            5 => format!("len({})", self.expression(Type::List, depth)),
            6 => format!("{}[{}]", self.expression(Type::Pair, depth), self.pick(2)),
            7 => format!(
                "{}[{}]",
                self.expression(Type::List, depth),
                self.index(depth)
            ),
            8 if !self.functions.is_empty() => {
                let index = self.pick(self.functions.len());
                let function = self.functions[index].clone();
                format!(
                    "{function}({}, {})",
                    self.expression(Type::Int, depth),
                    self.expression(Type::Int, depth)
                )
            }
            9 => match self.var(Type::Closure, false) {
                Some(closure) => format!("{closure}({})", self.expression(Type::Int, depth)),
                None => self.int_literal(),
            },
            10 => {
                let name = self.fresh("b");
                let value = self.expression(Type::Int, depth);
                self.vars.push(Var {
                    name: name.clone(),
                    typ: Type::Int,
                    assignable: true,
                });
                let result = self.expression(Type::Int, depth);
                self.vars.pop();
                format!("{{ let {name} = {value}; {result} }}")
            }
            11 => format!("-{}", self.expression(Type::Int, depth)),
            12 => {
                let function = ["max", "min"][self.pick(2)];
                format!(
                    "{function}({}, {})",
                    self.expression(Type::Int, depth),
                    self.expression(Type::Int, depth)
                )
            }
            13 => format!("int({})", self.expression(Type::Float, depth)),
            16 => format!("{}[{}]", self.expression(Type::Map, depth), self.key()),
            17 if !self.generics.is_empty() => {
                let index = self.pick(self.generics.len());
                let generic = self.generics[index].clone();
                format!("{generic}({})", self.expression(Type::Int, depth))
            }
            18 => format!("len({})", self.expression(Type::Map, depth)),
            14 => {
                // An operand that assigns a variable another operand reads.
                match self.var(Type::Int, true) {
                    Some(var) => format!(
                        "({var} + {{ {var} = {}; {var} }})",
                        self.expression(Type::Int, depth)
                    ),
                    None => self.int_literal(),
                }
            }
            _ => format!("abs({})", self.expression(Type::Int, depth)),
        }
    }

    fn float(&mut self, depth: usize, deep: bool) -> String {
        let choices = if deep { 2 } else { 7 };
        match self.pick(choices) {
            0 => format!("{}.5", self.pick(4)),
            1 => self
                .var(Type::Float, false)
                .unwrap_or_else(|| "0.25".to_string()),
            2 | 3 => {
                let operator = ["+", "-", "*", "/"][self.pick(4)];
                format!(
                    "({} {operator} {})",
                    self.expression(Type::Float, depth),
                    self.expression(Type::Float, depth)
                )
            }
            4 => format!("float({})", self.expression(Type::Int, depth)),
            6 if !self.generics.is_empty() => {
                let index = self.pick(self.generics.len());
                let generic = self.generics[index].clone();
                format!("{generic}({})", self.expression(Type::Float, depth))
            }
            _ => format!(
                "(if {} {{ {} }} else {{ {} }})",
                self.expression(Type::Bool, depth),
                self.expression(Type::Float, depth),
                self.expression(Type::Float, depth)
            ),
        }
    }

    fn bool(&mut self, depth: usize, deep: bool) -> String {
        let choices = if deep { 2 } else { 8 };
        match self.pick(choices) {
            0 => ["true", "false"][self.pick(2)].to_string(),
            1 => self
                .var(Type::Bool, false)
                .unwrap_or_else(|| "true".to_string()),
            2 | 3 => {
                let operator = ["<", "<=", ">", ">=", "==", "!="][self.pick(6)];
                format!(
                    "({} {operator} {})",
                    self.expression(Type::Int, depth),
                    self.expression(Type::Int, depth)
                )
            }
            4 => {
                let operator = ["<", ">", "=="][self.pick(3)];
                format!(
                    "({} {operator} {})",
                    self.expression(Type::Float, depth),
                    self.expression(Type::Float, depth)
                )
            }
            5 => format!("!{}", self.expression(Type::Bool, depth)),
            6 => {
                let operator = ["and", "or"][self.pick(2)];
                format!(
                    "({} {operator} {})",
                    self.expression(Type::Bool, depth),
                    self.expression(Type::Bool, depth)
                )
            }
            _ => format!(
                "({} == {})",
                self.expression(Type::List, depth),
                self.expression(Type::List, depth)
            ),
        }
    }

    /// `[1, v2, 3]`, never empty, so its type is known.
    fn list_literal(&mut self, depth: usize) -> String {
        let elements = (0..1 + self.pick(3))
            .map(|_| self.expression(Type::Int, depth.max(MAX_EXPRESSION_DEPTH - 1)))
            .collect::<Vec<_>>();
        format!("[{}]", elements.join(", "))
    }

    fn list(&mut self, depth: usize, deep: bool) -> String {
        let choices = if deep { 2 } else { 6 };
        match self.pick(choices) {
            0 => self.list_literal(depth),
            1 => self
                .var(Type::List, false)
                .unwrap_or_else(|| self.list_literal(depth)),
            2 => {
                let variable = self.fresh("x");
                let source = self.expression(Type::List, depth);
                self.vars.push(Var {
                    name: variable.clone(),
                    typ: Type::Int,
                    assignable: false,
                });
                let element = self.expression(Type::Int, depth);
                self.vars.pop();
                format!("[{element} for {variable} in {source}]")
            }
            3 => {
                let variable = self.fresh("x");
                let end = self.pick(MAX_ROUNDS + 1);
                self.vars.push(Var {
                    name: variable.clone(),
                    typ: Type::Int,
                    assignable: false,
                });
                let condition = self.expression(Type::Bool, depth);
                self.vars.pop();
                format!("[{variable} for {variable} in 0..{end}, if {condition}]")
            }
            4 => format!(
                "({} ++ {})",
                self.expression(Type::List, depth),
                self.list_literal(depth)
            ),
            _ => format!(
                "(if {} {{ {} }} else {{ {} }})",
                self.expression(Type::Bool, depth),
                self.expression(Type::List, depth),
                self.expression(Type::List, depth)
            ),
        }
    }

    /// A key of a map: one of the few a map has, as reading one it
    /// doesn't have stops the program.
    fn key(&mut self) -> String {
        self.pick(3).to_string()
    }

    /// `%{0: 1, 2: v3}`, or a map variable.
    fn map(&mut self, depth: usize) -> String {
        if self.chance(2)
            && let Some(map) = self.var(Type::Map, false)
        {
            return map;
        }
        let pairs = (0..1 + self.pick(3))
            .map(|key| {
                format!(
                    "{key}: {}",
                    self.expression(Type::Int, depth.max(MAX_EXPRESSION_DEPTH - 1))
                )
            })
            .collect::<Vec<_>>();
        format!("%{{{}}}", pairs.join(", "))
    }

    fn pair(&mut self, depth: usize, deep: bool) -> String {
        let choice = if deep { 0 } else { self.pick(2) };
        match self.var(Type::Pair, false) {
            Some(pair) if choice == 1 => pair,
            _ => format!(
                "({}, {})",
                self.expression(Type::Int, depth),
                self.expression(Type::Int, depth)
            ),
        }
    }
}

/// The program `choices` make.
fn generate(choices: &[u16]) -> Program {
    let mut generator = Generator {
        choices,
        next: 0,
        vars: Vec::new(),
        functions: Vec::new(),
        generics: Vec::new(),
        names: 0,
    };
    Program(generator.program())
}

/// What running a program did, in a form two runs can be compared by.
#[derive(Debug, PartialEq, Eq)]
struct Outcome {
    output: String,
    /// The program's value, or its error with the span of the error.
    result: Result<String, String>,
}

fn run(backend: Backend, program: &str) -> Outcome {
    let mut interpreter = Interpreter::capturing();
    interpreter.configure(ndc_stdlib::register);
    interpreter.set_backend(backend);
    let result = match interpreter.eval(program) {
        Ok(value) => Ok(value.to_string()),
        Err(InterpreterError::Vm(error)) => Err(format!("{} at {:?}", error.message, error.span)),
        Err(other) => Err(format!("{other:?}")),
    };
    let output = String::from_utf8(interpreter.get_output().expect("captured output"))
        .expect("output is UTF-8");
    Outcome { output, result }
}

/// How many programs got past the analyser, of how many were run: a
/// generator whose programs mostly fail to analyse tests little.
static RAN: AtomicUsize = AtomicUsize::new(0);
static GENERATED: AtomicUsize = AtomicUsize::new(0);

const WORKER_THREAD_NAME: &str = "differential-worker";

/// Keeps the worker thread's panics quiet, as the test thread reports them.
fn install_quiet_panic_hook() {
    static HOOK: Once = Once::new();
    HOOK.call_once(|| {
        let default_hook = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            if thread::current().name() != Some(WORKER_THREAD_NAME) {
                default_hook(info);
            }
        }));
    });
}

/// Runs `program` on both VMs in a worker thread, and fails if they do
/// something different, panic, or take longer than [`PER_CASE_TIMEOUT`].
fn same_on_both(program: &Program) -> Result<(), TestCaseError> {
    install_quiet_panic_hook();
    let source = program.0.clone();
    let (tx, rx) = mpsc::channel();
    thread::Builder::new()
        .name(WORKER_THREAD_NAME.into())
        .spawn(move || {
            let outcomes = panic::catch_unwind(AssertUnwindSafe(|| {
                (
                    run(Backend::Stack, &source),
                    run(Backend::Register, &source),
                )
            }));
            let _ = tx.send(outcomes);
        })
        .expect("spawn worker thread");
    let (stack, register) = match rx.recv_timeout(PER_CASE_TIMEOUT) {
        Ok(Ok(outcomes)) => outcomes,
        Ok(Err(payload)) => {
            let message = payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(ToString::to_string))
                .unwrap_or_default();
            return Err(TestCaseError::fail(format!("a VM panicked: {message}")));
        }
        Err(_) => {
            return Err(TestCaseError::fail(format!(
                "the VMs took longer than {} s",
                PER_CASE_TIMEOUT.as_secs()
            )));
        }
    };
    GENERATED.fetch_add(1, Ordering::Relaxed);
    if !matches!(&stack.result, Err(error) if error.starts_with("Analysis")) {
        RAN.fetch_add(1, Ordering::Relaxed);
    }
    if stack != register {
        return Err(TestCaseError::fail(format!(
            "the VMs differ:\n  stack:    {stack:?}\n  register: {register:?}"
        )));
    }
    Ok(())
}

/// Runs `cases` generated programs, and the seeds of earlier failures, on
/// both VMs.
fn check(cases: u32) {
    let config = ProptestConfig {
        cases,
        max_shrink_iters: 4096,
        failure_persistence: Some(Box::new(FileFailurePersistence::WithSource("regressions"))),
        source_file: Some(file!()),
        ..ProptestConfig::default()
    };
    let programs = prop::collection::vec(any::<u16>(), MIN_CHOICES..=MAX_CHOICES)
        .prop_map(|choices| generate(&choices));
    let mut runner = TestRunner::new(config);
    let result = runner.run(&programs, |program| same_on_both(&program));
    #[allow(clippy::print_stderr, reason = "says how many programs ran")]
    {
        eprintln!(
            "{} of {} generated programs got past the analyser",
            RAN.load(Ordering::Relaxed),
            GENERATED.load(Ordering::Relaxed)
        );
    }
    if let Err(error) = result {
        panic!("{error}");
    }
}

#[test]
fn generated_programs_do_the_same_on_both_vms() {
    check(300);
}

#[test]
#[ignore = "long fuzz run; opt in with `cargo test ... -- --ignored`"]
fn fuzz_generated_programs_on_both_vms() {
    let cases = std::env::var("PROPTEST_CASES")
        .ok()
        .and_then(|cases| cases.parse().ok())
        .unwrap_or(10_000);
    check(cases);
}

/// The generator writes programs that mostly analyse and run.
#[test]
fn most_generated_programs_run() {
    let mut runner = TestRunner::deterministic();
    let choices = prop::collection::vec(any::<u16>(), MIN_CHOICES..=MAX_CHOICES);
    let mut ran = 0;
    let total = 100;
    for _ in 0..total {
        let program = generate(&choices.new_tree(&mut runner).unwrap().current());
        if !matches!(run(Backend::Register, &program.0).result, Err(error) if error.starts_with("Analysis"))
        {
            ran += 1;
        }
    }
    assert!(ran * 2 > total, "only {ran} of {total} programs analysed");
}
