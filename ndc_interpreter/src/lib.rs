pub use ndc_core::{compare, hash_map, int, num};

pub mod environment;
pub mod evaluate;
pub mod function;
pub mod heap;
pub mod iterator;
pub mod semantic;
pub mod sequence;
pub mod value;
pub mod vm_bridge;

use crate::environment::{Environment, InterpreterOutput};
use crate::evaluate::EvaluationError;
use crate::semantic::{Analyser, ScopeTree};
use crate::value::Value;
use ndc_lexer::{Lexer, TokenLocation};
use ndc_parser::ExpressionLocation;
use ndc_vm::compiler::Compiler;
use ndc_vm::value::CompiledFunction;
use ndc_vm::vm::Vm;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    analyser: Analyser,
    /// Persistent REPL VM and the compiler checkpoint from the last run.
    /// `None` until the first `run_str` call; kept alive afterwards so that
    /// variables declared on one line are visible on subsequent lines.
    repl_state: Option<(Vm, Compiler)>,
}

impl Interpreter {
    #[must_use]
    pub fn new<T>(dest: T) -> Self
    where
        T: InterpreterOutput + 'static,
    {
        Self::from_env(Environment::new(Box::new(dest)))
    }

    #[must_use]
    pub fn from_env(environment: Environment) -> Self {
        let global_identifiers = environment.get_global_identifiers();
        Self {
            environment: Rc::new(RefCell::new(environment)),
            analyser: Analyser::from_scope_tree(ScopeTree::from_global_scope(global_identifiers)),
            repl_state: None,
        }
    }

    pub fn configure<F: FnOnce(&mut Environment)>(&mut self, f: F) {
        f(&mut self.environment.borrow_mut());
        let global_identifiers = self.environment.borrow().get_global_identifiers();
        self.analyser = Analyser::from_scope_tree(ScopeTree::from_global_scope(global_identifiers));
    }

    #[must_use]
    pub fn environment(self) -> Rc<RefCell<Environment>> {
        self.environment
    }

    pub fn analyse_str(
        &mut self,
        input: &str,
    ) -> Result<Vec<ExpressionLocation>, InterpreterError> {
        self.parse_and_analyse(input)
    }

    pub fn compile_str(&mut self, input: &str) -> Result<CompiledFunction, InterpreterError> {
        let expressions = self.parse_and_analyse(input)?;
        Ok(Compiler::compile(expressions.into_iter())?)
    }

    pub fn disassemble_str(&mut self, input: &str) -> Result<String, InterpreterError> {
        let compiled = self.compile_str(input)?;
        let mut out = String::new();
        out.push_str(&ndc_vm::disassemble::disassemble(&compiled, Some(input)));
        Ok(out)
    }

    pub fn run_str(&mut self, input: &str) -> Result<String, InterpreterError> {
        let expressions = self.parse_and_analyse(input)?;
        let final_value = self.interpret_vm(input, expressions.into_iter())?;
        Ok(format!("{final_value}"))
    }

    fn parse_and_analyse(
        &mut self,
        input: &str,
    ) -> Result<Vec<ExpressionLocation>, InterpreterError> {
        let tokens = Lexer::new(input).collect::<Result<Vec<TokenLocation>, _>>()?;
        let mut expressions = ndc_parser::Parser::from_tokens(tokens).parse()?;

        let checkpoint = self.analyser.checkpoint();
        for e in &mut expressions {
            if let Err(e) = self.analyser.analyse(e) {
                self.analyser.restore(checkpoint);
                return Err(e.into());
            }
        }

        Ok(expressions)
    }
    fn interpret_vm(
        &mut self,
        #[cfg(feature = "vm-trace")] input: &str,
        #[cfg(not(feature = "vm-trace"))] _input: &str,
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<Value, InterpreterError> {
        let globals = vm_bridge::make_vm_globals(&self.environment);

        match self.repl_state.take() {
            None => {
                let (code, checkpoint) = Compiler::compile_resumable(expressions)?;
                let sink = vm_bridge::WriteSink(self.environment.borrow().output_rc());
                let mut vm = Vm::new(code, globals).with_output(Box::new(sink));
                #[cfg(feature = "vm-trace")]
                {
                    vm = vm.with_source(input);
                }
                vm.run()?;
                self.repl_state = Some((vm, checkpoint));
            }
            Some((mut vm, checkpoint)) => {
                let resume_ip = checkpoint.halt_ip();
                let prev_num_locals = checkpoint.num_locals();
                let (code, new_checkpoint) = checkpoint.resume(expressions)?;
                vm.resume_from_halt(code, globals, resume_ip, prev_num_locals);
                #[cfg(feature = "vm-trace")]
                {
                    vm.set_source(input);
                }
                vm.run()?;
                self.repl_state = Some((vm, new_checkpoint));
            }
        }

        Ok(Value::unit())
    }
}

#[derive(thiserror::Error, Debug)]
pub enum InterpreterError {
    #[error("Error while lexing source")]
    Lexer {
        #[from]
        cause: ndc_lexer::Error,
    },
    #[error("Error while parsing source")]
    Parser {
        #[from]
        cause: ndc_parser::Error,
    },
    #[error("Error during static analysis")]
    Resolver {
        #[from]
        cause: semantic::AnalysisError,
    },
    #[error("Compilation error")]
    Compiler {
        #[from]
        cause: ndc_vm::CompileError,
    },
    #[error("Error while executing code")]
    Evaluation {
        #[from]
        cause: EvaluationError,
    },
    #[error("{0}")]
    Vm(#[from] ndc_vm::VmError),
}
