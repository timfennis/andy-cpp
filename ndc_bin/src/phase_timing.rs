use ndc_core::duration::format_duration;
use ndc_interpreter::ExecutionTimings;
use std::io::{self, Write};

pub fn write_phase_timings<W: Write>(writer: &mut W, timings: &ExecutionTimings) -> io::Result<()> {
    writeln!(writer)?;
    writeln!(writer, "{:-<56}", "")?;
    writeln!(
        writer,
        "Phase timings (total: {})",
        format_duration(timings.total())
    )?;
    writeln!(writer, "{:-<56}", "")?;
    writeln!(
        writer,
        "  {:<12} {}",
        "lexing",
        format_duration(timings.lexing)
    )?;
    writeln!(
        writer,
        "  {:<12} {}",
        "parsing",
        format_duration(timings.parsing)
    )?;
    writeln!(
        writer,
        "  {:<12} {}",
        "analyser",
        format_duration(timings.analysing)
    )?;
    writeln!(
        writer,
        "  {:<12} {}",
        "compiling",
        format_duration(timings.compiling)
    )?;
    writeln!(
        writer,
        "  {:<12} {}",
        "running",
        format_duration(timings.running)
    )?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::write_phase_timings;
    use ndc_interpreter::ExecutionTimings;
    use std::time::Duration;

    #[test]
    fn writes_expected_summary() {
        let timings = ExecutionTimings {
            lexing: Duration::from_micros(10),
            parsing: Duration::from_micros(20),
            analysing: Duration::from_micros(30),
            compiling: Duration::from_micros(40),
            running: Duration::from_micros(50),
        };

        let mut output = Vec::new();
        write_phase_timings(&mut output, &timings).expect("timing output should write");
        let output = String::from_utf8(output).expect("timing output should be utf8");

        assert!(output.contains("Phase timings (total: 150us)"));
        assert!(output.contains("lexing       10us"));
        assert!(output.contains("parsing      20us"));
        assert!(output.contains("analyser     30us"));
        assert!(output.contains("compiling    40us"));
        assert!(output.contains("running      50us"));
    }
}
