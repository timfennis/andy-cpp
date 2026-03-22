use ndc_analyser::{Analyser, ScopeTree};
use ndc_core::FunctionRegistry;
use ndc_lexer::{Lexer, SourceDb, SourceId, TokenLocation};
use ndc_parser::ExpressionLocation;
use ndc_vm::compiler::Compiler;
use ndc_vm::value::CompiledFunction;
use ndc_vm::{OutputSink, Vm};
use std::rc::Rc;

pub use ndc_analyser::AnalysisResult;
#[cfg(feature = "trace")]
pub use ndc_vm::tracer;
pub use ndc_vm::{NativeFunction, Value};

pub struct Interpreter {
    registry: FunctionRegistry<Rc<NativeFunction>>,
    capturing: bool,
    analyser: Analyser,
    source_db: SourceDb,
    /// Persistent REPL VM and the compiler checkpoint from the last run.
    /// `None` until the first `eval` call; kept alive afterwards so that
    /// variables declared on one line are visible on subsequent lines.
    repl_state: Option<(Vm, Compiler)>,
    #[cfg(feature = "trace")]
    tracer: Option<Box<dyn tracer::VmTracer>>,
}

impl Interpreter {
    /// Create an interpreter that writes output to stdout.
    #[must_use]
    pub fn new() -> Self {
        Self::from_capturing(false)
    }

    /// Create an interpreter that captures output into an internal buffer,
    /// retrievable via [`get_output`].
    #[must_use]
    pub fn capturing() -> Self {
        Self::from_capturing(true)
    }

    fn from_capturing(capturing: bool) -> Self {
        Self {
            registry: FunctionRegistry::default(),
            capturing,
            analyser: Analyser::from_scope_tree(ScopeTree::from_global_scope(vec![])),
            source_db: SourceDb::new(),
            repl_state: None,
            #[cfg(feature = "trace")]
            tracer: None,
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

    #[cfg(feature = "trace")]
    pub fn set_tracer(&mut self, tracer: Box<dyn tracer::VmTracer>) {
        self.tracer = Some(tracer);
    }

    pub fn functions(&self) -> impl Iterator<Item = &Rc<NativeFunction>> {
        self.registry.iter()
    }

    /// Returns the captured output, or `None` if this interpreter writes to stdout.
    /// Returns `Some(vec![])` for a capturing interpreter that hasn't run yet.
    pub fn get_output(&self) -> Option<Vec<u8>> {
        if self.capturing {
            Some(
                self.repl_state
                    .as_ref()
                    .and_then(|(vm, _)| vm.get_output().map(<[u8]>::to_vec))
                    .unwrap_or_default(),
            )
        } else {
            None
        }
    }

    pub fn source_db(&self) -> &SourceDb {
        &self.source_db
    }

    pub fn analyse_str(
        &mut self,
        input: &str,
    ) -> Result<(Vec<ExpressionLocation>, AnalysisResult), InterpreterError> {
        let source_id = self.source_db.add("<input>", input);
        let expressions = self.parse_and_analyse(input, source_id)?;
        let result = self.analyser.take_result();
        Ok((expressions, result))
    }

    pub fn compile_str(&mut self, input: &str) -> Result<CompiledFunction, InterpreterError> {
        let source_id = self.source_db.add("<input>", input);
        let expressions = self.parse_and_analyse(input, source_id)?;
        Ok(Compiler::compile(expressions.into_iter())?)
    }

    pub fn disassemble_str(&mut self, input: &str) -> Result<String, InterpreterError> {
        let compiled = self.compile_str(input)?;
        let mut out = String::new();
        out.push_str(&ndc_vm::disassemble::disassemble(&compiled, Some(input)));
        Ok(out)
    }

    /// Execute source code and return the resulting [`Value`].
    ///
    /// Statements (semicolon-terminated) produce [`Value::unit()`].
    pub fn eval(&mut self, input: &str) -> Result<Value, InterpreterError> {
        self.eval_named("<input>", input)
    }

    /// Execute source code with a custom source name for diagnostics.
    pub fn eval_named(
        &mut self,
        name: impl Into<String>,
        input: &str,
    ) -> Result<Value, InterpreterError> {
        let source_id = self.source_db.add(name, input);
        let expressions = self.parse_and_analyse(input, source_id)?;
        self.interpret_vm(input, expressions.into_iter())
    }

    fn parse_and_analyse(
        &mut self,
        input: &str,
        source_id: SourceId,
    ) -> Result<Vec<ExpressionLocation>, InterpreterError> {
        let tokens = Lexer::new(input, source_id).collect::<Result<Vec<TokenLocation>, _>>()?;
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
        #[cfg(feature = "trace")] input: &str,
        #[cfg(not(feature = "trace"))] _input: &str,
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
                let output = if self.capturing {
                    OutputSink::Buffer(Vec::new())
                } else {
                    OutputSink::Stdout
                };
                let (code, checkpoint) = Compiler::compile_resumable(expressions)?;
                let mut vm = Vm::new(code, globals).with_output(output);
                #[cfg(feature = "trace")]
                {
                    vm = vm.with_source(input);
                    if let Some(tracer) = self.tracer.take() {
                        vm = vm.with_tracer(tracer);
                    }
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
                #[cfg(feature = "trace")]
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
