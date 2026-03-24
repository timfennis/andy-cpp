use std::time::{Duration, Instant};

#[derive(Clone, Copy, Debug, Default)]
pub struct ExecutionTimings {
    pub lexing: Duration,
    pub parsing: Duration,
    pub analysing: Duration,
    pub compiling: Duration,
    pub running: Duration,
}

impl ExecutionTimings {
    #[must_use]
    pub fn total(&self) -> Duration {
        self.lexing + self.parsing + self.analysing + self.compiling + self.running
    }

    fn set(&mut self, phase: Phase, duration: Duration) {
        match phase {
            Phase::Lexing => self.lexing = duration,
            Phase::Parsing => self.parsing = duration,
            Phase::Analysing => self.analysing = duration,
            Phase::Compiling => self.compiling = duration,
            Phase::Running => self.running = duration,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Phase {
    Lexing,
    Parsing,
    Analysing,
    Compiling,
    Running,
}

pub fn measure<T, E>(
    timings: &mut ExecutionTimings,
    phase: Phase,
    f: impl FnOnce() -> Result<T, E>,
) -> Result<T, E> {
    let start = Instant::now();
    let result = f();
    timings.set(phase, start.elapsed());
    result
}
