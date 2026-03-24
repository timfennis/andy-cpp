use crate::chunk::OpCode;
use ndc_core::duration::format_duration;
use ndc_lexer::Span;
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Context passed to a tracer on every dispatched instruction.
pub struct InstructionContext<'a> {
    pub ip: usize,
    pub opcode: &'a OpCode,
    pub span: Span,
    pub source: Option<&'a str>,
}

/// Trait for observing VM instruction dispatch.
pub trait VmTracer {
    /// Called before each instruction is dispatched.
    fn on_instruction(&mut self, ctx: &InstructionContext<'_>);

    /// Called when the VM finishes execution.
    /// Tracers that accumulate data should print their summaries here.
    fn on_complete(&mut self);
}

/// Maps an `OpCode` variant to its name as a static string.
fn opcode_name(op: &OpCode) -> &'static str {
    match op {
        OpCode::Call(_) => "Call",
        OpCode::Pop => "Pop",
        OpCode::Jump(_) => "Jump",
        OpCode::JumpIfTrue(_) => "JumpIfTrue",
        OpCode::JumpIfFalse(_) => "JumpIfFalse",
        OpCode::Constant(_) => "Constant",
        OpCode::GetLocal(_) => "GetLocal",
        OpCode::GetUpvalue(_) => "GetUpvalue",
        OpCode::SetLocal(_) => "SetLocal",
        OpCode::SetUpvalue(_) => "SetUpvalue",
        OpCode::GetGlobal(_) => "GetGlobal",
        OpCode::MakeList(_) => "MakeList",
        OpCode::MakeTuple(_) => "MakeTuple",
        OpCode::MakeMap { .. } => "MakeMap",
        OpCode::Closure { .. } => "Closure",
        OpCode::GetIterator => "GetIterator",
        OpCode::IterNext(_) => "IterNext",
        OpCode::ListPush(_) => "ListPush",
        OpCode::MapInsert(_) => "MapInsert",
        OpCode::MakeRange { .. } => "MakeRange",
        OpCode::Unpack(_) => "Unpack",
        OpCode::Halt => "Halt",
        OpCode::Return => "Return",
        OpCode::CloseUpvalue(_) => "CloseUpvalue",
        OpCode::Memoize => "Memoize",
    }
}

// ---------------------------------------------------------------------------
// PrintTracer
// ---------------------------------------------------------------------------

/// Prints each instruction to stderr as it is dispatched.
pub struct PrintTracer;

impl VmTracer for PrintTracer {
    fn on_instruction(&mut self, ctx: &InstructionContext<'_>) {
        let excerpt = ctx.source.and_then(|src| {
            src.get(ctx.span.range())
                .map(|s| s.trim().replace('\n', "↵"))
        });
        let op_str = format!("{:?}", ctx.opcode);
        match excerpt {
            Some(s) => eprintln!("[VM] {:04} {:<30}  {}", ctx.ip, op_str, s),
            None => eprintln!("[VM] {:04} {}", ctx.ip, op_str),
        }
    }

    fn on_complete(&mut self) {}
}

// ---------------------------------------------------------------------------
// HistogramTracer
// ---------------------------------------------------------------------------

/// Counts how many times each instruction is dispatched and prints a summary.
pub struct HistogramTracer {
    counts: HashMap<&'static str, u64>,
}

impl HistogramTracer {
    #[must_use]
    pub fn new() -> Self {
        Self {
            counts: HashMap::new(),
        }
    }
}

impl Default for HistogramTracer {
    fn default() -> Self {
        Self::new()
    }
}

impl VmTracer for HistogramTracer {
    fn on_instruction(&mut self, ctx: &InstructionContext<'_>) {
        *self.counts.entry(opcode_name(ctx.opcode)).or_insert(0) += 1;
    }

    fn on_complete(&mut self) {
        let mut entries: Vec<_> = self.counts.drain().collect();
        entries.sort_by(|a, b| b.1.cmp(&a.1));

        let total: u64 = entries.iter().map(|(_, c)| c).sum();
        eprintln!("\n{:-<50}", "");
        eprintln!("Instruction histogram ({total} total)");
        eprintln!("{:-<50}", "");
        for (name, count) in &entries {
            let pct = *count as f64 / total as f64 * 100.0;
            eprintln!("  {name:<20} {count:>10}  ({pct:5.1}%)");
        }
    }
}

// ---------------------------------------------------------------------------
// TimingTracer
// ---------------------------------------------------------------------------

/// Measures cumulative time spent on each instruction type.
///
/// Timing for instruction N starts when `on_instruction` is called for N,
/// and ends when `on_instruction` is called for N+1 (or `on_complete`).
pub struct TimingTracer {
    times: HashMap<&'static str, Duration>,
    last: Option<(&'static str, Instant)>,
}

impl TimingTracer {
    #[must_use]
    pub fn new() -> Self {
        Self {
            times: HashMap::new(),
            last: None,
        }
    }
}

impl Default for TimingTracer {
    fn default() -> Self {
        Self::new()
    }
}

impl VmTracer for TimingTracer {
    fn on_instruction(&mut self, ctx: &InstructionContext<'_>) {
        let now = Instant::now();
        if let Some((prev_name, start)) = self.last.take() {
            *self.times.entry(prev_name).or_default() += now - start;
        }
        self.last = Some((opcode_name(ctx.opcode), now));
    }

    fn on_complete(&mut self) {
        if let Some((name, start)) = self.last.take() {
            *self.times.entry(name).or_default() += start.elapsed();
        }

        let mut entries: Vec<_> = self.times.drain().collect();
        entries.sort_by(|a, b| b.1.cmp(&a.1));

        let total: Duration = entries.iter().map(|(_, d)| d).sum();
        eprintln!("\n{:-<60}", "");
        eprintln!("Instruction timing (total: {})", format_duration(total));
        eprintln!("{:-<60}", "");
        for (name, dur) in &entries {
            let pct = dur.as_secs_f64() / total.as_secs_f64() * 100.0;
            eprintln!("  {name:<20} {:>10}  ({pct:5.1}%)", format_duration(*dur));
        }
    }
}

// ---------------------------------------------------------------------------
// CompositeTracer
// ---------------------------------------------------------------------------

/// Dispatches to multiple tracers.
pub struct CompositeTracer {
    tracers: Vec<Box<dyn VmTracer>>,
}

impl CompositeTracer {
    #[must_use]
    pub fn new(tracers: Vec<Box<dyn VmTracer>>) -> Self {
        Self { tracers }
    }
}

impl VmTracer for CompositeTracer {
    fn on_instruction(&mut self, ctx: &InstructionContext<'_>) {
        for tracer in &mut self.tracers {
            tracer.on_instruction(ctx);
        }
    }

    fn on_complete(&mut self) {
        for tracer in &mut self.tracers {
            tracer.on_complete();
        }
    }
}
