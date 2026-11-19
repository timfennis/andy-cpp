use ndc_analyser::Analyser;
use ndc_analysis::Analysis;
use ndc_core::FunctionRegistry;
/// The type of a value as the analyser sees it, which each of
/// [`Interpreter::natives`] has, as [`Builtins::static_type`] gives it.
pub use ndc_core::StaticType;
pub use ndc_core::{Builtins, IntrinsicFunction};
use ndc_lexer::{Lexer, SourceDb, SourceId, TokenLocation};
use ndc_parser::ExpressionLocation;
pub use ndc_vm::InputSource;
use ndc_vm::compiler::{Compiler, Session};
use ndc_vm::register::{Compiler as RegisterCompiler, Vm as RegisterVm};
use ndc_vm::value::CompiledFunction;
use ndc_vm::{OutputSink, Vm};
use std::rc::Rc;
use timing::{Phase, measure};

pub use ndc_analyser::{AnalysisError, AnalysisResult};
pub use ndc_vm::duration::format_duration;
#[cfg(feature = "trace")]
pub use ndc_vm::tracer;
pub use ndc_vm::{NativeFunction, NativeSpec, Natives, Value};
pub use timing::ExecutionTimings;

mod timing;

/// Which VM runs the code: the stack VM, or the register VM that is being
/// built to replace it, which runs part of the language so far.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Backend {
    #[default]
    Stack,
    Register,
}

pub struct Interpreter {
    /// Shared with the analyser, which asks for a native's type when a
    /// program uses it.
    natives: Rc<Natives>,
    /// The VM's globals: the registered functions, numbered the way the
    /// analyser numbers them, then the values of the program's top-level
    /// declarations. A resumable interpreter keeps them from one evaluation to
    /// the next, which is how a REPL line sees what earlier lines declared.
    globals: Vec<Value>,
    /// What the last run left on the VM's stack, such as the locals of the
    /// top-level function. Kept until the next run or until the interpreter
    /// is dropped, so a program that is about to exit can forget the
    /// interpreter and leave freeing all of it to the OS.
    stack: Vec<Value>,
    capturing: bool,
    analyser: Analyser,
    /// What compiling the evaluations so far leaves for the next, of a
    /// resumable interpreter: a later one can call an earlier one's generic
    /// functions with new types, which compiles new instances of them.
    session: Session,
    source_db: SourceDb,
    resumable: bool,
    /// Whether each evaluation is a line that more lines may follow, as in
    /// the REPL, which leaves what a line's code doesn't need settled for
    /// them to settle: see [`Analyser::take_line_result`].
    line_by_line: bool,
    /// What the evaluations printed, if output is captured.
    output: Vec<u8>,
    /// What `read_stdin` reads, with what is left of it after the
    /// evaluations so far.
    input: InputSource,
    #[cfg(feature = "trace")]
    tracer: Option<Box<dyn tracer::VmTracer>>,
    /// Whether the analyser, session and globals must be made afresh before
    /// the next evaluation. Starting over only marks them, so a program that
    /// runs once doesn't make them again after it ends, and registering the
    /// natives doesn't make them for the intrinsics alone first.
    stale: bool,
    /// Which VM runs the code.
    backend: Backend,
}

impl Interpreter {
    /// Create an interpreter that writes output to stdout and reads input
    /// from stdin.
    #[must_use]
    pub fn new() -> Self {
        Self::from_capturing(false)
    }

    /// Create an interpreter that captures output into an internal buffer,
    /// retrievable via [`get_output`], and has no input unless given some
    /// with [`with_input`](Self::with_input).
    #[must_use]
    pub fn capturing() -> Self {
        Self::from_capturing(true)
    }

    fn from_capturing(capturing: bool) -> Self {
        // No natives yet, and the language's intrinsics, which the analyser
        // and the globals know from the natives.
        Self {
            // Made with the natives before the first evaluation.
            analyser: Analyser::with_natives(Vec::new()),
            globals: Vec::new(),
            stack: Vec::new(),
            natives: Rc::new(Natives::default()),
            capturing,
            session: Session::default(),
            source_db: SourceDb::new(),
            resumable: true,
            line_by_line: false,
            output: Vec::new(),
            input: if capturing {
                InputSource::None
            } else {
                InputSource::Stdin
            },
            #[cfg(feature = "trace")]
            tracer: None,
            stale: true,
            backend: Backend::default(),
        }
    }

    /// Runs the code from now on with `backend`. A REPL line on the
    /// register VM can't call a generic function an earlier line declared
    /// with new types yet.
    ///
    /// Changing the backend forgets everything evaluated so far: a function
    /// an earlier evaluation declared is compiled for the VM it ran on, and
    /// the other one can't run it.
    pub fn set_backend(&mut self, backend: Backend) {
        if self.backend != backend {
            self.start_over();
        }
        self.backend = backend;
    }

    /// Create an interpreter for executing a complete program once.
    ///
    /// Unlike the default resumable interpreter used by the REPL, this does
    /// not retain what one evaluation declared for the next.
    #[must_use]
    pub fn one_shot() -> Self {
        Self {
            resumable: false,
            ..Self::from_capturing(false)
        }
    }

    /// This interpreter, evaluating lines that more lines may follow, as the
    /// REPL does: `let xs = [];` leaves what `xs` holds for a later line's
    /// `xs.push(1)` to settle.
    #[must_use]
    pub fn line_by_line(self) -> Self {
        Self {
            line_by_line: true,
            ..self
        }
    }

    /// This interpreter, with `read_stdin` reading from `input`.
    #[must_use]
    pub fn with_input(self, input: InputSource) -> Self {
        Self { input, ..self }
    }

    /// Registers the natives `f` declares, after the ones already registered.
    pub fn configure<F: FnOnce(&mut FunctionRegistry<NativeSpec>)>(&mut self, f: F) {
        let mut registry = FunctionRegistry::default();
        f(&mut registry);
        // `new_analyser` is the only place that shares the natives, so with
        // the analyser replaced this holds the only reference. The analyser
        // is made again before the next evaluation.
        self.analyser = Analyser::with_natives(Vec::new());
        Rc::get_mut(&mut self.natives)
            .expect("internal error: only the analyser shares the natives")
            .extend(registry);
        self.start_over();
    }

    /// An analyser that knows `natives` and nothing else.
    fn new_analyser(natives: &Rc<Natives>) -> Analyser {
        Analyser::with_builtins(Rc::clone(natives) as Rc<dyn Builtins>)
    }

    /// Forgets everything evaluated so far, keeping only what was printed,
    /// from the next evaluation on.
    fn start_over(&mut self) {
        self.stale = true;
    }

    /// Makes the analyser, session and globals afresh if the interpreter
    /// started over since the last evaluation.
    fn refresh(&mut self) {
        if std::mem::take(&mut self.stale) {
            self.analyser = Self::new_analyser(&self.natives);
            self.session = Session::default();
            self.globals = ndc_vm::native_globals(&self.natives);
        }
    }

    #[cfg(feature = "trace")]
    pub fn set_tracer(&mut self, tracer: Box<dyn tracer::VmTracer>) {
        self.tracer = Some(tracer);
    }

    /// The registered natives, in global slot order, followed by the
    /// intrinsics.
    pub fn natives(&self) -> &Natives {
        &self.natives
    }

    /// Returns the captured output, or `None` if this interpreter writes to stdout.
    /// Returns `Some(vec![])` for a capturing interpreter that hasn't run yet.
    pub fn get_output(&self) -> Option<Vec<u8>> {
        if self.capturing {
            Some(self.output.clone())
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
        self.refresh();
        let source_id = self.source_db.add("<input>", input);
        let tokens = Lexer::new(input, source_id).collect::<Result<Vec<TokenLocation>, _>>()?;
        let expressions = ndc_parser::Parser::from_tokens(tokens).parse()?;

        for e in &expressions {
            self.analyser.analyse(e);
        }

        let result = self.analyser.take_result();
        Ok((expressions, result))
    }

    pub fn compile_str(&mut self, input: &str) -> Result<CompiledFunction, InterpreterError> {
        let source_id = self.source_db.add("<input>", input);
        let (expressions, result, _) = self.parse_and_analyse(input, source_id)?;
        let compile = match self.backend {
            Backend::Stack => Compiler::compile,
            Backend::Register => RegisterCompiler::compile,
        };
        Ok(compile(
            expressions.into_iter(),
            result.analysis,
            &self.natives,
        )?)
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
            &self.natives,
        )?)
    }

    pub fn disassemble_str(&mut self, input: &str) -> Result<String, InterpreterError> {
        let compiled = self.compile_str(input)?;
        Ok(match self.backend {
            Backend::Stack => {
                ndc_vm::disassemble::disassemble(&compiled, &self.natives, Some(input))
            }
            Backend::Register => ndc_vm::register::disassemble::disassemble(&compiled, Some(input)),
        })
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
    ///
    /// A one-shot interpreter starts every evaluation afresh. A resumable one
    /// keeps what each evaluation declared for the next, until one fails after
    /// parsing: then it starts over, as the failed code may have declared or
    /// changed some things and not others. The REPL runs each line in a forked
    /// process instead, so a failed line doesn't cost it the session.
    pub fn eval_named_with_timings(
        &mut self,
        name: impl Into<String>,
        input: &str,
    ) -> Result<(Value, ExecutionTimings), InterpreterError> {
        let source_id = self.source_db.add(name, input);
        let result =
            self.parse_and_analyse(input, source_id)
                .and_then(|(expressions, result, timings)| {
                    let vm_result =
                        self.interpret_vm(input, expressions.into_iter(), result.analysis);
                    vm_result.map(|(value, vm_timings)| (value, timings, vm_timings))
                });
        match &result {
            // Nothing was analysed, so nothing needs to be forgotten.
            Err(InterpreterError::Lexer { .. } | InterpreterError::Parser { .. }) => {}
            Err(_) => self.start_over(),
            Ok(_) if !self.resumable => self.start_over(),
            Ok(_) => {}
        }
        let (value, mut timings, vm_timings) = result?;
        timings.compiling = vm_timings.compiling;
        timings.running = vm_timings.running;
        Ok((value, timings))
    }

    fn parse_and_analyse(
        &mut self,
        input: &str,
        source_id: SourceId,
    ) -> Result<(Vec<ExpressionLocation>, AnalysisResult, ExecutionTimings), InterpreterError> {
        self.refresh();
        let mut timings = ExecutionTimings::default();

        let tokens = measure(&mut timings, Phase::Lexing, || {
            Lexer::new(input, source_id).collect::<Result<Vec<TokenLocation>, _>>()
        })?;

        let expressions = measure(&mut timings, Phase::Parsing, || {
            ndc_parser::Parser::from_tokens(tokens).parse()
        })?;

        measure(&mut timings, Phase::Analysing, || {
            for e in &expressions {
                self.analyser.analyse(e);
            }
        });

        let result = if self.line_by_line {
            self.analyser.take_line_result()
        } else {
            self.analyser.take_result()
        };
        if !result.errors.is_empty() {
            return Err(InterpreterError::Analysis {
                causes: result.errors,
            });
        }
        Ok((expressions, result, timings))
    }

    /// Compiles and runs analysed code on a VM that gets the globals, and
    /// takes them back afterwards.
    fn interpret_vm(
        &mut self,
        #[cfg(feature = "trace")] input: &str,
        #[cfg(not(feature = "trace"))] _input: &str,
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
    ) -> Result<(Value, ExecutionTimings), InterpreterError> {
        if self.backend == Backend::Register {
            return self.interpret_register_vm(expressions, analysis);
        }
        let mut timings = ExecutionTimings::default();
        let code = measure(&mut timings, Phase::Compiling, || {
            if self.resumable {
                Compiler::compile_line(expressions, analysis, &self.natives, &mut self.session)
            } else {
                Compiler::compile(expressions, analysis, &self.natives)
            }
        })?;
        let num_locals = code.num_locals();
        let output = if self.capturing {
            OutputSink::Buffer(Vec::new())
        } else {
            OutputSink::Stdout
        };
        // Free what the last run left behind before this one needs the memory.
        self.stack = Vec::new();
        let stdin = std::mem::replace(&mut self.input, InputSource::None);
        let mut vm = Vm::new(code, std::mem::take(&mut self.globals), &self.natives)
            .with_output(output)
            .with_input(stdin);
        #[cfg(feature = "trace")]
        {
            vm = vm.with_source(input);
            if let Some(tracer) = self.tracer.take() {
                vm = vm.with_tracer(tracer);
            }
        }
        let run = measure(&mut timings, Phase::Running, || vm.run());
        // What was printed stays printed, even when the run failed.
        if let Some(output) = vm.get_output() {
            self.output.extend_from_slice(output);
        }
        self.input = vm.take_input();
        let value = vm.last_value(num_locals);
        (self.globals, self.stack) = vm.into_globals_and_stack();
        run?;
        Ok((value, timings))
    }

    /// [`Self::interpret_vm`] on the register VM.
    fn interpret_register_vm(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
    ) -> Result<(Value, ExecutionTimings), InterpreterError> {
        let mut timings = ExecutionTimings::default();
        let code = measure(&mut timings, Phase::Compiling, || {
            RegisterCompiler::compile(expressions, analysis, &self.natives)
        })?;
        let output = if self.capturing {
            OutputSink::Buffer(Vec::new())
        } else {
            OutputSink::Stdout
        };
        self.stack = Vec::new();
        let stdin = std::mem::replace(&mut self.input, InputSource::None);
        let mut vm = RegisterVm::new(code, std::mem::take(&mut self.globals), &self.natives)
            .with_output(output)
            .with_input(stdin);
        let run = measure(&mut timings, Phase::Running, || vm.run());
        if let Some(output) = vm.get_output() {
            self.output.extend_from_slice(output);
        }
        self.input = vm.take_input();
        let value = vm.last_value();
        (self.globals, self.stack) = vm.into_globals_and_stack();
        run?;
        Ok((value, timings))
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
    Analysis { causes: Vec<AnalysisError> },
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

    /// A native that returns `value`, as `name()`.
    fn constant(name: &'static str, value: i64) -> FunctionRegistry<NativeSpec> {
        let mut registry = FunctionRegistry::default();
        registry.declare_global_fn(NativeSpec::with_signature(
            name,
            "Returns a constant.",
            "() -> Int",
            move || ndc_vm::value::NativeFunc::Simple(Box::new(move |_| Ok(Value::Int(value)))),
        ));
        registry
    }

    #[test]
    fn natives_can_be_added_after_an_evaluation() {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(|env| *env = constant("one", 1));
        assert_eq!(interpreter.eval("one()").expect("one").to_string(), "1");
        interpreter.configure(|env| *env = constant("two", 2));
        assert_eq!(
            interpreter
                .eval("(one(), two())")
                .expect("both")
                .to_string(),
            "(1,2)"
        );
    }

    #[test]
    fn a_one_shot_interpreter_forgets_earlier_declarations() {
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
