use ndc_analyser::Analyser;
use ndc_analysis::Analysis;
use ndc_core::FunctionRegistry;
use ndc_lexer::{Lexer, SourceDb, SourceId, TokenLocation};
use ndc_parser::ExpressionLocation;
use ndc_vm::compiler::Compiler;
use ndc_vm::value::CompiledFunction;
use ndc_vm::{OutputSink, Vm};
use std::rc::Rc;
use timing::{Phase, measure};

pub use ndc_analyser::AnalysisResult;
#[cfg(feature = "trace")]
pub use ndc_vm::tracer;
pub use ndc_vm::{NativeFunction, Value};
pub use timing::ExecutionTimings;

mod timing;

pub struct Interpreter {
    function_registry: FunctionRegistry<Rc<NativeFunction>>,
    capturing: bool,
    analyser: Analyser,
    source_db: SourceDb,
    /// Persistent REPL VM and the compiler checkpoint from the last run.
    /// `None` until the first `eval` call; kept alive afterwards so that
    /// variables declared on one line are visible on subsequent lines.
    repl_state: Option<(Vm, Compiler)>,
    resumable: bool,
    /// Output captured from one-shot runs, whose VM doesn't outlive the run.
    one_shot_output: Vec<u8>,
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
            function_registry: FunctionRegistry::default(),
            capturing,
            analyser: Analyser::new(vec![]),
            source_db: SourceDb::new(),
            repl_state: None,
            resumable: true,
            one_shot_output: Vec::new(),
            #[cfg(feature = "trace")]
            tracer: None,
        }
    }

    /// Create an interpreter for executing a complete program once.
    ///
    /// Unlike the default resumable interpreter used by the REPL, this uses
    /// the optimizing compiler and does not retain state after execution.
    #[must_use]
    pub fn one_shot() -> Self {
        Self {
            resumable: false,
            ..Self::from_capturing(false)
        }
    }

    /// Like [`Self::one_shot`], but captures output like [`Self::capturing`],
    /// so tests can run a program the way `ndc script.ndc` does.
    #[must_use]
    pub fn capturing_one_shot() -> Self {
        Self {
            resumable: false,
            ..Self::from_capturing(true)
        }
    }

    pub fn configure<F: FnOnce(&mut FunctionRegistry<Rc<NativeFunction>>)>(&mut self, f: F) {
        f(&mut self.function_registry);
        let functions = self
            .function_registry
            .iter()
            .map(|fun| (fun.name.clone(), fun.static_type.clone()))
            .collect();

        self.analyser = Analyser::new(functions);
    }

    #[cfg(feature = "trace")]
    pub fn set_tracer(&mut self, tracer: Box<dyn tracer::VmTracer>) {
        self.tracer = Some(tracer);
    }

    pub fn functions(&self) -> impl Iterator<Item = &Rc<NativeFunction>> {
        self.function_registry.iter()
    }

    /// Returns the captured output, or `None` if this interpreter writes to stdout.
    /// Returns `Some(vec![])` for a capturing interpreter that hasn't run yet.
    pub fn get_output(&self) -> Option<Vec<u8>> {
        if self.capturing {
            let repl_output = self
                .repl_state
                .as_ref()
                .and_then(|(vm, _)| vm.get_output())
                .unwrap_or_default();
            Some([self.one_shot_output.as_slice(), repl_output].concat())
        } else {
            None
        }
    }

    pub fn source_db(&self) -> &SourceDb {
        &self.source_db
    }

    /// Lex, parse, and analyse `input`, returning the annotated AST and an
    /// [`AnalysisResult`] that includes any accumulated analysis errors.
    ///
    /// Lexer and parser errors still short-circuit (no AST to return).
    /// Analysis errors are collected into [`AnalysisResult::errors`] so the
    /// caller can inspect them while still using the partial type information.
    pub fn analyse_str(
        &mut self,
        input: &str,
    ) -> Result<(Vec<ExpressionLocation>, AnalysisResult), InterpreterError> {
        let source_id = self.source_db.add("<input>", input);
        let tokens = Lexer::new(input, source_id).collect::<Result<Vec<TokenLocation>, _>>()?;
        let mut expressions = ndc_parser::Parser::from_tokens(tokens).parse()?;

        for e in &mut expressions {
            // Hard errors (structural issues) are accumulated too.
            if let Err(e) = self.analyser.analyse(e) {
                self.analyser.emit_external(e);
            }
        }

        let result = self.analyser.take_result();
        Ok((expressions, result))
    }

    pub fn compile_str(&mut self, input: &str) -> Result<CompiledFunction, InterpreterError> {
        let source_id = self.source_db.add("<input>", input);
        let (expressions, result, _) = self.parse_and_analyse(input, source_id)?;
        Ok(Compiler::compile(expressions.into_iter(), result.analysis)?)
    }

    /// Like [`Self::compile_str`] but skips the peephole optimizer.
    /// Used by the compiler-tests crate to assert against raw bytecode.
    pub fn compile_str_unoptimized(
        &mut self,
        input: &str,
    ) -> Result<CompiledFunction, InterpreterError> {
        let source_id = self.source_db.add("<input>", input);
        let (expressions, result, _) = self.parse_and_analyse(input, source_id)?;
        Ok(Compiler::compile_unoptimized(
            expressions.into_iter(),
            result.analysis,
        )?)
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
        Ok(self.eval_named_with_timings(name, input)?.0)
    }

    /// Execute source code with a custom source name for diagnostics and
    /// return wall-clock timings for each pipeline phase.
    pub fn eval_named_with_timings(
        &mut self,
        name: impl Into<String>,
        input: &str,
    ) -> Result<(Value, ExecutionTimings), InterpreterError> {
        let source_id = self.source_db.add(name, input);
        // Save the analyser state before parsing so we can roll it back if the VM
        // fails — otherwise the analyser would remember declarations from a line
        // that never actually ran.
        let analyser_checkpoint = self.analyser.checkpoint();
        let (expressions, result, mut timings) = self.parse_and_analyse(input, source_id)?;
        let vm_result = self.interpret_vm(input, expressions.into_iter(), result.analysis);
        if vm_result.is_err() || !self.resumable {
            self.analyser.restore(analyser_checkpoint);
        }
        let (value, vm_timings) = vm_result?;
        timings.compiling = vm_timings.compiling;
        timings.running = vm_timings.running;
        Ok((value, timings))
    }

    fn parse_and_analyse(
        &mut self,
        input: &str,
        source_id: SourceId,
    ) -> Result<(Vec<ExpressionLocation>, AnalysisResult, ExecutionTimings), InterpreterError> {
        let mut timings = ExecutionTimings::default();

        let tokens = measure(&mut timings, Phase::Lexing, || {
            Lexer::new(input, source_id).collect::<Result<Vec<TokenLocation>, _>>()
        })?;

        let mut expressions = measure(&mut timings, Phase::Parsing, || {
            ndc_parser::Parser::from_tokens(tokens).parse()
        })?;

        let checkpoint = self.analyser.checkpoint();
        measure(&mut timings, Phase::Analysing, || {
            for e in &mut expressions {
                // Hard errors (structural issues) still abort immediately.
                if let Err(e) = self.analyser.analyse(e) {
                    self.analyser.restore(checkpoint.clone());
                    self.analyser.take_result();
                    return Err(InterpreterError::Resolver { causes: vec![e] });
                }
            }

            Ok(())
        })?;

        if self.analyser.has_errors() {
            self.analyser.restore(checkpoint);
            let result = self.analyser.take_result();
            return Err(InterpreterError::Resolver {
                causes: result.errors,
            });
        }

        let analysis = self.analyser.take_result();
        Ok((expressions, analysis, timings))
    }

    fn interpret_vm(
        &mut self,
        #[cfg(feature = "trace")] input: &str,
        #[cfg(not(feature = "trace"))] _input: &str,
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
    ) -> Result<(Value, ExecutionTimings), InterpreterError> {
        use ndc_vm::{Function as VmFunction, Object as VmObject, Value as VmValue};

        let globals: Vec<VmValue> = self
            .function_registry
            .iter()
            .map(|native| {
                VmValue::Object(Rc::new(VmObject::Function(VmFunction::Native(Rc::clone(
                    native,
                )))))
            })
            .collect();

        let mut timings = ExecutionTimings::default();
        if !self.resumable {
            let code = measure(&mut timings, Phase::Compiling, || {
                Self::compile_one_shot(expressions, analysis)
            })?;
            let num_locals = code.num_locals();
            let output = if self.capturing {
                OutputSink::Buffer(Vec::new())
            } else {
                OutputSink::Stdout
            };
            let mut vm = Vm::new(code, globals).with_output(output);
            #[cfg(feature = "trace")]
            {
                vm = vm.with_source(input);
                if let Some(tracer) = self.tracer.take() {
                    vm = vm.with_tracer(tracer);
                }
            }
            let run = measure(&mut timings, Phase::Running, || vm.run());
            // Keep what the run printed, up to an error too, as the VM goes away.
            if let Some(output) = vm.get_output() {
                self.one_shot_output.extend_from_slice(output);
            }
            run?;
            return Ok((vm.last_value(num_locals), timings));
        }

        let result = match self.repl_state.take() {
            None => {
                let output = if self.capturing {
                    OutputSink::Buffer(Vec::new())
                } else {
                    OutputSink::Stdout
                };
                let (code, checkpoint) = measure(&mut timings, Phase::Compiling, || {
                    Compiler::compile_resumable(expressions, analysis)
                })?;
                let mut vm = Vm::new(code, globals).with_output(output);
                #[cfg(feature = "trace")]
                {
                    vm = vm.with_source(input);
                    if let Some(tracer) = self.tracer.take() {
                        vm = vm.with_tracer(tracer);
                    }
                }
                measure(&mut timings, Phase::Running, || vm.run())?;
                let result = vm.last_value(checkpoint.num_locals());
                self.repl_state = Some((vm, checkpoint));
                result
            }
            Some((mut vm, checkpoint)) => {
                let resume_ip = checkpoint.halt_ip();
                let prev_num_locals = checkpoint.num_locals();
                // Clone before consuming: we need the old checkpoint to restore
                // repl_state if the VM errors, so subsequent REPL lines can still
                // find their locals on the stack.
                let old_checkpoint = checkpoint.clone();
                let (code, new_checkpoint) = measure(&mut timings, Phase::Compiling, || {
                    checkpoint.resume(expressions, analysis)
                })?;
                vm.resume_from_halt(code, globals, resume_ip, prev_num_locals);
                #[cfg(feature = "trace")]
                {
                    vm.set_source(input);
                }
                if let Err(e) = measure(&mut timings, Phase::Running, || vm.run()) {
                    // Restore the VM so the next resume_from_halt can cleanly
                    // truncate the stack back to the pre-error locals.
                    self.repl_state = Some((vm, old_checkpoint));
                    return Err(InterpreterError::Vm(e));
                }
                let result = vm.last_value(new_checkpoint.num_locals());
                self.repl_state = Some((vm, new_checkpoint));
                result
            }
        };

        Ok((result, timings))
    }

    fn compile_one_shot(
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
    ) -> Result<CompiledFunction, ndc_vm::CompileError> {
        Compiler::compile(expressions, analysis)
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
        causes: Vec<ndc_analyser::AnalysisError>,
    },
    #[error("Compilation error")]
    Compiler {
        #[from]
        cause: ndc_vm::CompileError,
    },
    #[error("{0}")]
    Vm(#[from] ndc_vm::VmError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_vm::chunk::OpCode;

    #[test]
    fn one_shot_compilation_runs_the_optimizer() {
        let mut interpreter = Interpreter::one_shot();
        let source_id = interpreter.source_db.add("<test>", "1;");
        let (expressions, result, _) = interpreter
            .parse_and_analyse("1;", source_id)
            .expect("analysis should succeed");

        let compiled = Interpreter::compile_one_shot(expressions.into_iter(), result.analysis)
            .expect("compilation should succeed");

        assert_eq!(compiled.opcodes(), [OpCode::Halt]);
    }

    #[test]
    fn one_shot_execution_does_not_retain_declarations() {
        let mut interpreter = Interpreter::one_shot();
        interpreter
            .eval("let value = 1;")
            .expect("first program should succeed");

        assert!(
            interpreter.eval("value;").is_err(),
            "a one-shot interpreter must treat each program as isolated",
        );
    }
}
