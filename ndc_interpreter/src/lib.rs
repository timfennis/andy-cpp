use ndc_analyser::{Analyser, ScopeTree};
use ndc_core::FunctionRegistry;
use ndc_lexer::{Lexer, TokenLocation};
use ndc_parser::ExpressionLocation;
use ndc_vm::compiler::Compiler;
use ndc_vm::value::CompiledFunction;
use ndc_vm::{NativeFunction, Value, Vm};
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

#[derive(Clone)]
enum OutputSink {
    Stdout,
    Buffer(Rc<RefCell<Vec<u8>>>),
}

impl Write for OutputSink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            OutputSink::Stdout => std::io::stdout().write(buf),
            OutputSink::Buffer(vec) => vec.borrow_mut().write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            OutputSink::Stdout => std::io::stdout().flush(),
            OutputSink::Buffer(vec) => vec.borrow_mut().flush(),
        }
    }
}

pub struct Interpreter {
    registry: FunctionRegistry<Rc<NativeFunction>>,
    output: OutputSink,
    analyser: Analyser,
    /// Persistent REPL VM and the compiler checkpoint from the last run.
    /// `None` until the first `run_str` call; kept alive afterwards so that
    /// variables declared on one line are visible on subsequent lines.
    repl_state: Option<(Vm, Compiler)>,
}

impl Interpreter {
    /// Create an interpreter that writes output to stdout.
    #[must_use]
    pub fn new() -> Self {
        Self::from_sink(OutputSink::Stdout)
    }

    /// Create an interpreter that captures output into an internal buffer,
    /// retrievable via [`get_output`].
    #[must_use]
    pub fn capturing() -> Self {
        Self::from_sink(OutputSink::Buffer(Rc::new(RefCell::new(Vec::new()))))
    }

    fn from_sink(output: OutputSink) -> Self {
        Self {
            registry: FunctionRegistry::default(),
            output,
            analyser: Analyser::from_scope_tree(ScopeTree::from_global_scope(vec![])),
            repl_state: None,
        }
    }

    pub fn configure<F: FnOnce(&mut FunctionRegistry<Rc<NativeFunction>>)>(&mut self, f: F) {
        f(&mut self.registry);
        let functions = self
            .registry
            .iter()
            .map(|fun| (fun.name.clone(), fun.static_type.clone()))
            .collect();

        self.analyser = Analyser::from_scope_tree(ScopeTree::from_global_scope(functions));
    }

    pub fn functions(&self) -> impl Iterator<Item = &Rc<NativeFunction>> {
        self.registry.iter()
    }

    /// Returns the captured output, or `None` if this interpreter writes to stdout.
    pub fn get_output(&self) -> Option<Vec<u8>> {
        match &self.output {
            OutputSink::Stdout => None,
            OutputSink::Buffer(buf) => Some(buf.borrow().clone()),
        }
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
        use ndc_vm::{Function as VmFunction, Object as VmObject, Value as VmValue};

        let globals: Vec<VmValue> = self
            .registry
            .iter()
            .map(|native| {
                VmValue::Object(Rc::new(VmObject::Function(VmFunction::Native(Rc::clone(
                    native,
                )))))
            })
            .collect();

        let result = match self.repl_state.take() {
            None => {
                let (code, checkpoint) = Compiler::compile_resumable(expressions)?;
                let mut vm = Vm::new(code, globals).with_output(Box::new(self.output.clone()));
                #[cfg(feature = "vm-trace")]
                {
                    vm = vm.with_source(input);
                }
                vm.run()?;
                let result = vm.last_value(checkpoint.num_locals());
                self.repl_state = Some((vm, checkpoint));
                result
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
                let result = vm.last_value(new_checkpoint.num_locals());
                self.repl_state = Some((vm, new_checkpoint));
                result
            }
        };

        Ok(result)
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
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
        cause: ndc_analyser::AnalysisError,
    },
    #[error("Compilation error")]
    Compiler {
        #[from]
        cause: ndc_vm::CompileError,
    },
    #[error("{0}")]
    Vm(#[from] ndc_vm::VmError),
}
