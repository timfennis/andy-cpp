#!/usr/bin/env python3
"""Compares two `ndc` binaries on whole programs.

    benches/compare.py BASELINE CANDIDATE [--runs 5] [--aob DIR] [--filter TEXT]

Runs every program in `benches/programs` and, when the directory is there,
every Advent of Code solution in an Advent of Brian checkout (a copy of it, so
nothing in the checkout is touched). Each program runs once on both binaries to
check they print the same and exit the same, then `--runs` times on each, taking
turns, and the fastest run of each counts. Time is CPU time (user + system), so
a busy machine skews it less than wall time would.

It ends with the totals and the merge gate of docs/register-vm/plan.md: the
candidate passes when both totals are at least 1.3x faster and no program is
more than 5% slower.

Build both binaries the same way (same toolchain, same profile) and don't build
anything else while this runs.
"""

import argparse
import os
import resource
import shutil
import signal
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

MEMORY_LIMIT_BYTES = 16_000_000_000
TIMEOUT_SECONDS = 120
GATE_TOTAL_SPEEDUP = 1.3
GATE_SLOWEST_PROGRAM = 0.95


@dataclass
class Run:
    """One run of a program: how it ended, what it printed, what it cost."""

    status: int
    stdout: bytes
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

    @property
    def speedup(self) -> float:
        return self.baseline_ms / self.candidate_ms if self.candidate_ms else float("inf")


def limit_memory() -> None:
    resource.setrlimit(resource.RLIMIT_AS, (MEMORY_LIMIT_BYTES, MEMORY_LIMIT_BYTES))


def run(binary: Path, program: Path) -> Run:
    """Runs `program` from its own directory, where it finds its input files."""
    with tempfile.TemporaryFile() as out:
        process = subprocess.Popen(
            [str(binary), program.name],
            cwd=program.parent,
            stdin=subprocess.DEVNULL,
            stdout=out,
            stderr=subprocess.DEVNULL,
            preexec_fn=limit_memory,
        )
        try:
            _, status, usage = wait_with_timeout(process)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
            return Run(status=-1, stdout=b"", cpu_ms=float("inf"), peak_mb=0.0)
        out.seek(0)
        return Run(
            status=status,
            stdout=out.read(),
            cpu_ms=(usage.ru_utime + usage.ru_stime) * 1000,
            peak_mb=usage.ru_maxrss / 1024,
        )


def wait_with_timeout(process: subprocess.Popen):
    """The child's exit code and resource usage, giving up after `TIMEOUT_SECONDS`."""

    def on_alarm(_signum, _frame):
        raise subprocess.TimeoutExpired(process.args, TIMEOUT_SECONDS)

    previous = signal.signal(signal.SIGALRM, on_alarm)
    signal.alarm(TIMEOUT_SECONDS)
    try:
        # `wait4` gives this child's own CPU time and peak memory.
        pid, status, usage = os.wait4(process.pid, 0)
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, previous)
    # `Popen` must not wait again for a child that is already reaped.
    process.returncode = os.waitstatus_to_exitcode(status)
    return pid, process.returncode, usage


def measure(name: str, program: Path, baseline: Path, candidate: Path, runs: int):
    """The fastest of `runs` runs on each binary, or why the program is skipped."""
    first_baseline = run(baseline, program)
    first_candidate = run(candidate, program)
    if first_baseline.status != 0 and first_candidate.status != 0:
        return "fails on both"
    if first_baseline.status != first_candidate.status:
        return f"exit status differs: baseline {first_baseline.status}, candidate {first_candidate.status}"
    if first_baseline.stdout != first_candidate.stdout:
        return "output differs"

    baseline_runs = [first_baseline]
    candidate_runs = [first_candidate]
    for _ in range(runs - 1):
        baseline_runs.append(run(baseline, program))
        candidate_runs.append(run(candidate, program))
    return Result(
        name=name,
        baseline_ms=min(r.cpu_ms for r in baseline_runs),
        candidate_ms=min(r.cpu_ms for r in candidate_runs),
        baseline_mb=max(r.peak_mb for r in baseline_runs),
        candidate_mb=max(r.peak_mb for r in candidate_runs),
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


def report(title: str, results: list[Result]) -> None:
    if not results:
        return
    width = max(len(r.name) for r in results)
    print(f"\n{title}")
    print(f"{'program':<{width}}  {'baseline':>10}  {'candidate':>10}  {'speedup':>8}  {'peak memory':>17}")
    for r in results:
        memory = f"{r.baseline_mb:.0f} -> {r.candidate_mb:.0f} MB"
        print(
            f"{r.name:<{width}}  {r.baseline_ms:>8.0f}ms  {r.candidate_ms:>8.0f}ms  "
            f"{r.speedup:>7.2f}x  {memory:>17}"
        )
    baseline = sum(r.baseline_ms for r in results)
    candidate = sum(r.candidate_ms for r in results)
    print(f"{'total':<{width}}  {baseline:>8.0f}ms  {candidate:>8.0f}ms  {baseline / candidate:>7.2f}x")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("baseline", type=Path)
    parser.add_argument("candidate", type=Path)
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument(
        "--aob",
        type=Path,
        default=Path.home() / "Projects" / "personal" / "advent-of-brian",
        help="an Advent of Brian checkout; skipped when it isn't there",
    )
    parser.add_argument("--filter", default="", help="only programs whose name contains this")
    args = parser.parse_args()

    baseline = args.baseline.resolve()
    candidate = args.candidate.resolve()
    repo = Path(__file__).resolve().parent.parent

    with tempfile.TemporaryDirectory(prefix="ndc-compare-") as scratch:
        sets = {
            "benches/programs": bench_programs(repo),
            "Advent of Brian 2025": aob_programs(args.aob, Path(scratch)),
        }
        results: dict[str, list[Result]] = {}
        skipped: list[tuple[str, str]] = []
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
                else:
                    skipped.append((name, outcome))
                    print(f" skipped: {outcome}", file=sys.stderr)

    for title, rows in results.items():
        report(title, rows)

    if skipped:
        print("\nSkipped")
        for name, why in skipped:
            print(f"  {name}: {why}")

    everything = [r for rows in results.values() for r in rows]
    slower = [r for r in everything if r.speedup < GATE_SLOWEST_PROGRAM]
    differing = [name for name, why in skipped if why != "fails on both"]
    totals_pass = all(
        sum(r.baseline_ms for r in rows) / sum(r.candidate_ms for r in rows) >= GATE_TOTAL_SPEEDUP
        for rows in results.values()
        if rows
    )
    print("\nMerge gate")
    print(f"  totals at least {GATE_TOTAL_SPEEDUP}x faster: {'yes' if totals_pass else 'no'}")
    print(f"  programs more than 5% slower: {', '.join(r.name for r in slower) or 'none'}")
    print(f"  programs that behave differently: {', '.join(differing) or 'none'}")
    passed = totals_pass and not slower and not differing
    print(f"  {'PASS' if passed else 'FAIL'}")
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
