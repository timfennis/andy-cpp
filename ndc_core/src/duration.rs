use std::time::Duration;

#[must_use]
pub fn format_duration(duration: Duration) -> String {
    let nanos = duration.as_nanos();
    if nanos < 1_000 {
        format!("{nanos}ns")
    } else if nanos < 1_000_000 {
        format!("{:.0}us", nanos as f64 / 1_000.0)
    } else if nanos < 1_000_000_000 {
        format!("{:.1}ms", nanos as f64 / 1_000_000.0)
    } else {
        format!("{:.2}s", duration.as_secs_f64())
    }
}

#[cfg(test)]
mod tests {
    use super::format_duration;
    use std::time::Duration;

    #[test]
    fn formats_small_durations() {
        assert_eq!(format_duration(Duration::from_nanos(999)), "999ns");
        assert_eq!(format_duration(Duration::from_micros(10)), "10us");
        assert_eq!(format_duration(Duration::from_millis(12)), "12.0ms");
    }

    #[test]
    fn formats_large_durations() {
        assert_eq!(format_duration(Duration::from_secs(2)), "2.00s");
    }
}
