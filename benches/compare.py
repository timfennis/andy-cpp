#!/usr/bin/env python3
"""Compares two `ndc` binaries on whole programs.

    benches/compare.py BASELINE CANDIDATE [--runs 5] [--aob DIR] [--filter TEXT]
                       [--baseline-env NAME=VALUE] [--candidate-env NAME=VALUE]

Runs every program in `benches/programs` and, when the directory is there,
every 2025 Advent of Code solution in an Advent of Brian checkout (a copy of it, so
nothing in the checkout is touched). Each program runs `--runs` times on each
binary, taking turns (more for a program that takes only milliseconds), and
the fastest run of each counts. Time is CPU time (user + system), so a busy
machine skews it less than wall time would.

Every run has to exit successfully and print what the baseline's first run
printed. A program that prints something else each time, as one using random
numbers does, is found by running the baseline twice; only its exit status is
checked.

With `perf` installed, it also counts the instructions each program runs
(`instructions:u`, the fewest of a few runs). That count hardly moves with
where the compiler and linker happen to put the interpreter's code in memory,
which time does: adding an arm to the dispatch loop can make an unrelated
program 10% slower or faster by moving the hot functions onto the same
instruction-cache lines, without running one instruction more.

It ends with the totals and a gate that every change should pass, whether or
not it's meant to make programs faster: the candidate passes when no program
behaves differently and none runs more than 2% more instructions. A program
that takes more than 5% longer (and more than two milliseconds longer, which
is noise) on no more instructions is listed to check: rebuild both binaries
with another function alignment (`RUSTFLAGS="-C
llvm-args=-align-all-functions=6"`) or another linker, and if it's no longer
slower there, it was where the code landed. A program whose instructions
couldn't be counted on both binaries, as without `perf` or when a profiling
run fails, goes by time instead: it may not take more than 5% longer.

Build both binaries the same way (same toolchain, same profile) and don't build
anything else while this runs. To compare two settings of one binary, give it
twice with `--baseline-env` and `--candidate-env`, as in
`compare.py ndc ndc --candidate-env NAME=VALUE`, and not through a wrapper
script on one side: starting a shell costs about a millisecond, which a short
program would be charged for.
"""

import argparse
import hashlib
import os
import resource
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass
from pathlib import Path

MEMORY_LIMIT_BYTES = 16_000_000_000
TIMEOUT_SECONDS = 120
MEMORY_SAMPLE_SECONDS = 0.002
GATE_LONGEST_SLOWDOWN = 1.05
GATE_MOST_INSTRUCTIONS = 1.02
INSTRUCTION_RUNS = 3
# A program that takes a few milliseconds varies by more than 5% from run to
# run, so it gets more runs, and a difference this small is never a slowdown.
SHORT_PROGRAM_MS = 250
SHORT_PROGRAM_RUNS = 40
NOISE_MS = 2.0


@dataclass
class Run:
    """One run of a program: how it ended, what it printed, what it cost."""

    status: int
    output: str
    cpu_ms: float
    peak_mb: float


@dataclass
class Result:
    """A program's fastest run on each binary."""

    name: str
    baseline_ms: float
    candidate_ms: float
    baseline_mb: float
    candidate_mb: float
    output_compared: bool
    baseline_instructions: int | None
    candidate_instructions: int | None

    @property
    def speedup(self) -> float:
        return self.baseline_ms / self.candidate_ms if self.candidate_ms else float("inf")

    @property
    def too_slow(self) -> bool:
        longer = self.candidate_ms - self.baseline_ms
        return self.candidate_ms > self.baseline_ms * GATE_LONGEST_SLOWDOWN and longer > NOISE_MS

    @property
    def instruction_ratio(self) -> float | None:
        """The candidate's instructions per instruction of the baseline."""
        if not self.baseline_instructions or self.candidate_instructions is None:
            return None
        return self.candidate_instructions / self.baseline_instructions

    @property
    def more_instructions(self) -> bool:
        ratio = self.instruction_ratio
        return ratio is not None and ratio > GATE_MOST_INSTRUCTIONS


@dataclass(frozen=True)
class Binary:
    """An `ndc` to measure: the executable and what its environment adds."""

    path: Path
    env: tuple[tuple[str, str], ...]


def run(binary: Binary, program: Path) -> Run:
    """Runs `program` from its own directory, where it finds its input files."""
    with tempfile.TemporaryFile() as out:
        process = subprocess.Popen(
            [str(binary.path), program.name],
            cwd=program.parent,
            env={**os.environ, **dict(binary.env)},
            stdin=subprocess.DEVNULL,
            stdout=out,
            stderr=subprocess.DEVNULL,
        )
        memory = PeakMemory(process.pid)
        try:
            status, usage = wait_with_timeout(process)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
            return Run(status=-1, output="", cpu_ms=float("inf"), peak_mb=0.0)
        out.seek(0)
        return Run(
            status=status,
            output=hashlib.file_digest(out, "sha256").hexdigest(),
            cpu_ms=(usage.ru_utime + usage.ru_stime) * 1000,
            peak_mb=memory.finish(),
        )


def count_instructions(binary: Binary, program: Path) -> int | None:
    """The instructions `program` runs in user space, the fewest of a few
    runs, or `None` without `perf`, when it can't count them, or when a run
    fails, as a run that stops early would count too few."""
    perf = shutil.which("perf")
    if perf is None:
        return None
    counts = []
    for _ in range(INSTRUCTION_RUNS):
        with tempfile.NamedTemporaryFile(mode="r") as stats:
            try:
                completed = subprocess.run(
                    [perf, "stat", "--no-scale", "-x", ",", "-e", "instructions:u", "-o", stats.name, "--"]
                    + [str(binary.path)]
                    + [program.name],
                    cwd=program.parent,
                    env={**os.environ, **dict(binary.env)},
                    stdin=subprocess.DEVNULL,
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    timeout=TIMEOUT_SECONDS,
                    check=False,
                )
            except subprocess.TimeoutExpired:
                return None
            if completed.returncode != 0:
                return None
            # A processor with two kinds of core has a line for each, which
            # counts while the program runs on that kind. Unscaled, as `perf`
            # would otherwise scale each count up to the whole run.
            values = [line.split(",", 1)[0] for line in stats.read().splitlines() if "instructions" in line]
            if not any(value.isdigit() for value in values):
                return None
            counts.append(sum(int(value) for value in values if value.isdigit()))
    return min(counts)


class PeakMemory:
    """The most memory a running process has held, read every few milliseconds.

    The kernel's own figure for a finished child (`ru_maxrss`) counts what the
    child held before it became the program it runs, when it was still a copy
    of this runner, so it can't read lower than the runner's footprint. The
    process's `VmHWM` in `/proc` counts the program alone, but is gone once the
    process ends, so this samples it while the process runs and can miss what
    the program allocates in its last moments.
    """

    def __init__(self, pid: int) -> None:
        self.status = Path(f"/proc/{pid}/status")
        self.peak_kb = 0
        self.thread = threading.Thread(target=self.sample, daemon=True)
        self.thread.start()

    def sample(self) -> None:
        while True:
            try:
                text = self.status.read_text()
            except OSError:
                return
            line = next((line for line in text.splitlines() if line.startswith("VmHWM:")), None)
            # A process that has ended but isn't reaped yet has no memory left.
            if line is None:
                return
            self.peak_kb = max(self.peak_kb, int(line.split()[1]))
            time.sleep(MEMORY_SAMPLE_SECONDS)

    def finish(self) -> float:
        """The peak in megabytes, once the process has ended."""
        self.thread.join()
        return self.peak_kb / 1024


def wait_with_timeout(process: subprocess.Popen):
    """The child's exit code and resource usage, giving up after `TIMEOUT_SECONDS`."""

    def on_alarm(_signum, _frame):
        raise subprocess.TimeoutExpired(process.args, TIMEOUT_SECONDS)

    previous = signal.signal(signal.SIGALRM, on_alarm)
    signal.alarm(TIMEOUT_SECONDS)
    try:
        # `wait4` gives this child's own CPU time and peak memory.
        _, status, usage = os.wait4(process.pid, 0)
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, previous)
    # `Popen` must not wait again for a child that is already reaped.
    process.returncode = os.waitstatus_to_exitcode(status)
    return process.returncode, usage


def measure(name: str, program: Path, baseline: Binary, candidate: Binary, runs: int):
    """The fastest of `runs` runs on each binary, or why the program doesn't count.

    A reason starting with "skipped" is a program neither binary runs; any
    other reason is the candidate behaving differently, which fails the gate.
    """
    first = run(baseline, program)
    first_candidate = run(candidate, program)
    if first.status != 0 and first_candidate.status != 0:
        return "skipped: fails on both"

    baseline_runs = [first, run(baseline, program)]
    candidate_runs = [first_candidate]
    while len(candidate_runs) < runs:
        if len(baseline_runs) < runs:
            baseline_runs.append(run(baseline, program))
        candidate_runs.append(run(candidate, program))
    # A short program runs until each binary has spent a while on it.
    while (
        len(candidate_runs) < SHORT_PROGRAM_RUNS
        and min(sum(r.cpu_ms for r in baseline_runs), sum(r.cpu_ms for r in candidate_runs)) < SHORT_PROGRAM_MS
    ):
        baseline_runs.append(run(baseline, program))
        candidate_runs.append(run(candidate, program))

    if any(r.status != 0 for r in baseline_runs):
        return "skipped: the baseline doesn't run it every time"
    # A program that prints something else each time on one binary can't be
    # compared by what it prints.
    output_compared = all(r.output == first.output for r in baseline_runs)
    for number, r in enumerate(candidate_runs, start=1):
        if r.status != 0:
            return f"candidate run {number} failed with status {r.status}"
        if output_compared and r.output != first.output:
            return f"candidate run {number} printed something else"

    return Result(
        name=name,
        baseline_ms=min(r.cpu_ms for r in baseline_runs),
        candidate_ms=min(r.cpu_ms for r in candidate_runs),
        baseline_mb=max(r.peak_mb for r in baseline_runs),
        candidate_mb=max(r.peak_mb for r in candidate_runs),
        output_compared=output_compared,
        baseline_instructions=count_instructions(baseline, program),
        candidate_instructions=count_instructions(candidate, program),
    )


def bench_programs(repo: Path) -> list[tuple[str, Path]]:
    programs = sorted((repo / "benches" / "programs").glob("*.ndc"))
    return [(f"benches/{p.stem}", p) for p in programs]


def aob_programs(aob: Path, scratch: Path) -> list[tuple[str, Path]]:
    """The 2025 solutions, in a copy of the checkout's working tree."""
    source = aob / "2025"
    if not source.is_dir():
        return []
    copy = scratch / "2025"
    shutil.copytree(source, copy)
    return [(f"aob/{p.parent.name}/{p.stem}", p) for p in sorted(copy.glob("*/part*.ndc"))]


def total_speedup(results: list[Result]) -> float:
    return sum(r.baseline_ms for r in results) / sum(r.candidate_ms for r in results)


def instruction_change(ratio: float | None) -> str:
    """`ratio` as a change in instructions: `+1.2%`, or `?` when not counted."""
    return "?" if ratio is None else f"{(ratio - 1) * 100:+.1f}%"


def total_instruction_ratio(results: list[Result]) -> float | None:
    if any(r.instruction_ratio is None for r in results):
        return None
    return sum(r.candidate_instructions for r in results) / sum(r.baseline_instructions for r in results)


def report(title: str, results: list[Result]) -> None:
    if not results:
        return
    width = max(len(r.name) for r in results)
    print(f"\n{title}")
    print(
        f"{'program':<{width}}  {'baseline':>10}  {'candidate':>10}  {'speedup':>8}  "
        f"{'instructions':>12}  {'peak memory':>17}"
    )
    for r in results:
        memory = f"{r.baseline_mb:.0f} -> {r.candidate_mb:.0f} MB"
        note = "" if r.output_compared else "  (output differs from run to run, not compared)"
        print(
            f"{r.name:<{width}}  {r.baseline_ms:>8.0f}ms  {r.candidate_ms:>8.0f}ms  "
            f"{r.speedup:>7.2f}x  {instruction_change(r.instruction_ratio):>12}  {memory:>17}{note}"
        )
    baseline = sum(r.baseline_ms for r in results)
    candidate = sum(r.candidate_ms for r in results)
    print(
        f"{'total':<{width}}  {baseline:>8.0f}ms  {candidate:>8.0f}ms  {total_speedup(results):>7.2f}x  "
        f"{instruction_change(total_instruction_ratio(results)):>12}"
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("baseline", type=Path)
    parser.add_argument("candidate", type=Path)
    parser.add_argument("--runs", type=int, default=5, help="runs of each program on each binary, at least 2")
    parser.add_argument(
        "--aob",
        type=Path,
        default=Path.home() / "Projects" / "personal" / "advent-of-brian",
        help="an Advent of Brian checkout; skipped when it isn't there",
    )
    parser.add_argument("--filter", default="", help="only programs whose name contains this")
    parser.add_argument(
        "--baseline-env", action="append", default=[], metavar="NAME=VALUE", help="set for the baseline's runs"
    )
    parser.add_argument(
        "--candidate-env", action="append", default=[], metavar="NAME=VALUE", help="set for the candidate's runs"
    )
    args = parser.parse_args()
    if args.runs < 2:
        parser.error("--runs must be at least 2")

    def binary(path: Path, settings: list[str]) -> Binary:
        if any("=" not in setting for setting in settings):
            parser.error("an environment setting is written NAME=VALUE")
        return Binary(path.resolve(), tuple(tuple(setting.split("=", 1)) for setting in settings))

    baseline = binary(args.baseline, args.baseline_env)
    candidate = binary(args.candidate, args.candidate_env)
    repo = Path(__file__).resolve().parent.parent
    # The programs inherit the limit.
    resource.setrlimit(resource.RLIMIT_AS, (MEMORY_LIMIT_BYTES, MEMORY_LIMIT_BYTES))

    with tempfile.TemporaryDirectory(prefix="ndc-compare-") as scratch:
        sets = {
            "benches/programs": bench_programs(repo),
            "Advent of Brian 2025": aob_programs(args.aob, Path(scratch)),
        }
        results: dict[str, list[Result]] = {}
        skipped: list[tuple[str, str]] = []
        differing: list[tuple[str, str]] = []
        for title, programs in sets.items():
            results[title] = []
            for name, program in programs:
                if args.filter not in name:
                    continue
                print(f"  {name} ...", end="", flush=True, file=sys.stderr)
                outcome = measure(name, program, baseline, candidate, args.runs)
                if isinstance(outcome, Result):
                    results[title].append(outcome)
                    print(f" {outcome.speedup:.2f}x", file=sys.stderr)
                elif outcome.startswith("skipped: "):
                    skipped.append((name, outcome.removeprefix("skipped: ")))
                    print(f" {outcome}", file=sys.stderr)
                else:
                    differing.append((name, outcome))
                    print(f" DIFFERS: {outcome}", file=sys.stderr)

    for title, rows in results.items():
        report(title, rows)
    print(f"\nPeak memory is sampled every {MEMORY_SAMPLE_SECONDS * 1000:.0f} ms while a program runs.")

    if skipped:
        print("\nSkipped")
        for name, why in skipped:
            print(f"  {name}: {why}")
    if differing:
        print("\nBehaves differently")
        for name, why in differing:
            print(f"  {name}: {why}")

    measured = [rows for rows in results.values() if rows]
    print("\nMerge gate")
    if not measured:
        print("  nothing was measured")
        print("  FAIL")
        return 1
    every = [r for rows in measured for r in rows]
    # A program whose instructions were counted on both binaries goes by them;
    # one whose weren't goes by time.
    counted = [r for r in every if r.instruction_ratio is not None]
    uncounted = [r for r in every if r.instruction_ratio is None]
    more = [r.name for r in counted if r.more_instructions]
    layout = [r.name for r in counted if r.too_slow and not r.more_instructions]
    slower = [r.name for r in uncounted if r.too_slow]
    print(f"  programs that behave differently: {', '.join(name for name, _ in differing) or 'none'}")
    print(f"  programs running more than 2% more instructions: {', '.join(more) or 'none'}")
    if uncounted:
        programs = "1 program" if len(uncounted) == 1 else f"{len(uncounted)} programs"
        print(f"  {programs} without an instruction count (perf is missing or couldn't count them), going by time")
        print(f"  of those, taking more than 5% longer: {', '.join(slower) or 'none'}")
    passed = not more and not slower and not differing
    print(f"  {'PASS' if passed else 'FAIL'}")
    if layout:
        print("\nCheck the layout")
        print(f"  more than 5% longer on no more instructions: {', '.join(layout)}")
        print("  rebuild both with another function alignment or linker to see whether it stays")
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
