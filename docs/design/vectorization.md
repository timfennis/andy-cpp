# Vectorization

Operator syntax broadcasts element-wise over tuples. `a + b` where both
arguments are `Tuple<Int, Int>` resolves to two `+(Int, Int)` calls and a
tuple build. The mechanism is gated to operator syntax so regular function
calls never accidentally broadcast.

## Background

PR [#140] widened `Binding::Dynamic` return types to `Any` to fix issue
[#139]: the analyser had been LUB-ing declared overload returns, but the
value-level dispatcher could fall through to vec dispatch and produce a
value no declared overload returned. The widening pessimised every dynamic
caller — including ones with no vec path at all. The current design
restores that precision by tracking vec-ness on the binding rather than
on a separate fallback path, and broadens vec to cover n-ary operators
and non-numeric overloads.

[#139]: https://github.com/timfennis/andy-cpp/issues/139
[#140]: https://github.com/timfennis/andy-cpp/pull/140

## Three pieces

### 1. `Expression::OperatorCall` distinguishes operator desugars

The parser emits `Expression::OperatorCall { function, arguments }` for
`a + b`, `-x`, `op=`, and `not x` — same shape as `Call` but a distinct
variant. Downstream layers pattern-match exhaustively: the analyser opts
into vec dispatch on `OperatorCall` only, while `Call` keeps regular
semantics. No flag, no curated list of operator names anywhere outside
the parser.

### 2. `Candidate` distinguishes scalar from vec overloads

```rust
pub enum Candidate {
    Scalar(ResolvedVar),
    /// Element-wise tuple broadcast over the scalar that `var()` returns.
    Vec(ResolvedVar),
}
```

`Binding::{Resolved,Dynamic}` carry `Candidate`/`Vec<Candidate>`. The
analyser pins `Resolved(Candidate::Vec(scalar))` when per-position
resolution unanimously picks one scalar; it carries a mixed list as
`Dynamic` when types aren't precise enough.

### 3. Per-position vec resolution

For an operator-form call `op(a₁, …, aₙ)` where at least one `aᵢ` is
statically a non-empty tuple of length `k`, the analyser:

1. Builds a per-position signature for each `i ∈ 0..k`: tuple args
   contribute `arg[i]`, scalar args broadcast unchanged.
2. Looks up scalar overloads for each position signature.
3. **All positions pick the same scalar**: emit
   `Binding::Resolved(Candidate::Vec(scalar))`, result type
   `Tuple<scalar_return; k>`.
4. **Mixed positions**: emit `Binding::Dynamic(merged_candidates)`,
   result type = per-position LUB wrapped as `Tuple<…>`.
5. **Any position has zero candidates**: emit `Binding::None`. The call
   can't succeed at runtime either, so we error at compile time with
   `function_not_found`.

## Runtime dispatch

Two opcodes carry vec work:

* `CallVec(args)` — the compiler emits this for `Resolved(Vec)`. The
  scalar is loaded directly (no `OverloadSet` wrapper); the VM reads the
  broadcast axis from the tuple args at runtime and calls the known
  scalar `axis_len` times. This is the fast path that recovers the perf
  the per-element re-probe would cost.

* `Call(args)` with an `OverloadSet` callee — used for `Dynamic`. The
  dispatcher walks candidates in priority order: scalars first
  (first-match-wins), then vec candidates produce a `Callable::Vec`
  carrying the list of scalars that the broadcast loop narrows per
  element pair. The pinned-single-scalar case (one vec candidate) skips
  the per-element probe via the same fast path `CallVec` uses.

Element-call failures surface with `while vectorising '<name>' at index N`
prefixed to the inner message, so the outer call and failing position
appear in the error.

## What changed vs the old design

| Old | New |
|---|---|
| Binary numeric vec only | n-ary, any scalar overload |
| `Binding::Dynamic` widened all returns to `Any` | LUB-d for pure scalar; precise `Tuple<…>` for vec |
| Runtime `try_vectorized_call` post-check | First-class candidate in `OverloadSet` + `CallVec` opcode |
| Mixed-element tuples crashed mid-iteration | Compile-time `function_not_found` |
| Unary `-(1, 2, 3)` errored | Broadcasts to `(-1, -2, -3)` |

## Notes

* **Per-position LUB collapse**: `(Int, Float) + (Float, Int)` infers
  `Tuple<Number, Number>` rather than the per-element-precise
  `Tuple<Int, Number>`. The simpler uniform return type keeps the
  candidate list small; the cost is rare in practice.
* **Empty tuples** decline vec resolution — they have no broadcast axis.
* **Indexing** (`a[i]`) parses as `Call`, not `OperatorCall`: there's no
  natural broadcast story for `(list_a, list_b)[i]`.
