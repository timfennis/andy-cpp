/// Microbenchmark comparing three candidate designs for the VM output sink.
///
/// - `enum`: static dispatch via a two-variant enum (current implementation)
/// - `dyn`:  heap-allocated trait object (`Box<dyn OutputTrait>`)
/// - `rc`:   `Rc<RefCell<Vec<u8>>>` shared-ownership buffer
///
/// All three implementations are defined locally so the benchmark does not
/// depend on any particular version of the VM.  The goal is to measure the
/// overhead of the *write path* in isolation from interpreter execution.
use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use std::cell::RefCell;
use std::hint::black_box;
use std::io::Write;
use std::rc::Rc;

// ---------------------------------------------------------------------------
// Implementation 1 – enum (current)
// ---------------------------------------------------------------------------

enum EnumSink {
    Buffer(Vec<u8>),
}

impl Write for EnumSink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            EnumSink::Buffer(v) => v.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Implementation 2 – trait object
// ---------------------------------------------------------------------------

trait OutputTrait: Write {
    fn captured(&self) -> Option<&[u8]> {
        None
    }
}

struct DynBuffer(Vec<u8>);

impl Write for DynBuffer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl OutputTrait for DynBuffer {
    fn captured(&self) -> Option<&[u8]> {
        Some(&self.0)
    }
}

// ---------------------------------------------------------------------------
// Implementation 3 – Rc<RefCell<Vec<u8>>>
// ---------------------------------------------------------------------------

struct RcSink(Rc<RefCell<Vec<u8>>>);

impl Write for RcSink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Benchmark helpers
// ---------------------------------------------------------------------------

/// Write `n` copies of `payload` to `sink`.
fn write_n(sink: &mut impl Write, payload: &[u8], n: usize) {
    for _ in 0..n {
        sink.write_all(black_box(payload)).unwrap();
    }
}

fn bench_sinks(c: &mut Criterion) {
    // Payloads of different sizes to expose cache / branch effects.
    let payloads: &[(&str, &[u8])] = &[
        ("4B", b"1234"),
        ("16B", b"hello, world!!!!"),
        ("64B", &[b'x'; 64]),
        ("256B", &[b'x'; 256]),
    ];
    const WRITES: usize = 10_000;

    let mut group = c.benchmark_group("output_sink");

    for (label, payload) in payloads {
        let bytes = (payload.len() * WRITES) as u64;
        group.throughput(Throughput::Bytes(bytes));

        group.bench_with_input(BenchmarkId::new("enum", label), payload, |b, payload| {
            b.iter(|| {
                let mut sink = EnumSink::Buffer(Vec::with_capacity(bytes as usize));
                write_n(&mut sink, payload, WRITES);
                sink
            });
        });

        group.bench_with_input(BenchmarkId::new("dyn", label), payload, |b, payload| {
            b.iter(|| {
                let mut sink: Box<dyn OutputTrait> =
                    Box::new(DynBuffer(Vec::with_capacity(bytes as usize)));
                write_n(&mut sink, payload, WRITES);
                sink
            });
        });

        group.bench_with_input(BenchmarkId::new("rc", label), payload, |b, payload| {
            b.iter(|| {
                let buf = Rc::new(RefCell::new(Vec::with_capacity(bytes as usize)));
                let mut sink = RcSink(Rc::clone(&buf));
                write_n(&mut sink, payload, WRITES);
                buf
            });
        });
    }

    group.finish();
}

criterion_group!(benches, bench_sinks);
criterion_main!(benches);
