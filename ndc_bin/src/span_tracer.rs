use ndc_interpreter::tracer::{InstructionContext, VmTracer};
use std::collections::HashMap;
use std::io::Write;
use std::time::{Duration, Instant};
use yansi::Paint;

/// Accumulates time per source span, then renders the source code as a heat map
/// where cold regions are green and hot regions are red.
pub struct SpanTracer {
    /// Accumulated time per (offset, length) span.
    times: HashMap<(usize, usize), Duration>,
    last: Option<((usize, usize), Instant)>,
    source: Option<String>,
}

impl SpanTracer {
    pub fn new() -> Self {
        Self {
            times: HashMap::new(),
            last: None,
            source: None,
        }
    }
}

impl VmTracer for SpanTracer {
    fn on_instruction(&mut self, ctx: &InstructionContext<'_>) {
        if self.source.is_none()
            && let Some(src) = ctx.source
        {
            self.source = Some(src.to_owned());
        }

        let now = Instant::now();
        let key = (ctx.span.offset(), ctx.span.end() - ctx.span.offset());
        if let Some((prev_key, start)) = self.last.take() {
            *self.times.entry(prev_key).or_default() += now - start;
        }
        self.last = Some((key, now));
    }

    fn on_complete(&mut self) {
        // Finalize the last instruction's timing.
        if let Some((key, start)) = self.last.take() {
            *self.times.entry(key).or_default() += start.elapsed();
        }

        let Some(source) = &self.source else {
            return;
        };

        if self.times.is_empty() {
            return;
        }

        // Additive heat: each span's duration is added to every byte it covers.
        // This means bytes inside both a hot outer span and an inner span accumulate
        // both contributions, correctly showing them as hotter.
        let spans: Vec<_> = self.times.drain().collect();
        let mut heat = vec![Duration::ZERO; source.len()];
        for ((offset, length), dur) in &spans {
            let end = (offset + length).min(source.len());
            for h in &mut heat[*offset..end] {
                *h += *dur;
            }
        }

        // Find max duration for normalization.
        let max_dur = heat.iter().max().copied().unwrap_or(Duration::ZERO);

        if max_dur.is_zero() {
            print!("{source}");
            let _ = std::io::stdout().flush();
            return;
        }

        let max_nanos = max_dur.as_nanos() as f64;

        // Render: walk the source, coloring each non-whitespace character by heat.
        let mut byte_pos = 0;
        for ch in source.chars() {
            let len = ch.len_utf8();
            let s = &source[byte_pos..byte_pos + len];

            let color = if ch.is_ascii_whitespace() || byte_pos >= heat.len() {
                None
            } else {
                let d = heat[byte_pos];
                if d.is_zero() {
                    None
                } else {
                    let t = d.as_nanos() as f64 / max_nanos;
                    Some(heat_color(t))
                }
            };

            match color {
                Some((r, g, b)) => print!("{}", s.rgb(r, g, b)),
                None => print!("{s}"),
            }

            byte_pos += len;
        }

        let _ = std::io::stdout().flush();
    }
}

/// Interpolate from soft green (cold, t=0) through warm peach (t=0.5) to soft red (hot, t=1).
fn heat_color(t: f64) -> (u8, u8, u8) {
    let t = t.clamp(0.0, 1.0);
    // Mix towards white to create pastel tones: lerp between the pure hue and (255,255,255).
    let pastel = 0.4; // 0.0 = fully saturated, 1.0 = white
    let r = ((t.min(0.5) * 2.0).mul_add(1.0 - pastel, pastel) * 255.0) as u8;
    let g = ((t - 0.5).max(0.0).mul_add(-2.0, 1.0)).mul_add(1.0 - pastel, pastel) * 255.0;
    let b = (pastel * 255.0) as u8;
    (r, g as u8, b)
}
