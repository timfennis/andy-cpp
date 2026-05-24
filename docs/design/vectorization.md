# RFC: Vectorization scope

Status: Implemented in PR [#141] (commits `df7b11b`, `e367145`). RFC steps
1–5 and 7 shipped together; step 6 (compiler unrolling of `Resolved` vec
candidates) is deferred — see the [Compiler optimisation](#compiler-optimisation)
section.

[#141]: https://github.com/timfennis/andy-cpp/pull/141

## Summary

Extend vectorization beyond binary numeric operators. Gate the broader
scope on operator syntax so stdlib function calls never accidentally
vec. Make vectorization a property of the binding to recover the type
precision PR [#140] sacrificed for soundness.

## Motivation

PR [#140] widened `Binding::Dynamic` to `Any` to fix issue [#139]. The
analyser had been using the LUB of declared overload returns as the
result type. The value-level dispatcher could falsify that LUB by
falling through to vectorized dispatch, producing a runtime value no
overload declared. Widening to `Any` restored soundness. It also
pessimised every dynamic-binding caller, including ones that have no
path to vec.

Three gaps remain:

1. The VM only vec's binary calls, so unary `-(1, 2, 3)` errors. Unary
   tuple-neg is a near-term goal.
2. The VM only vec's tuples of numbers. `("a", "b") ++ ("c", "d")`
   errors even though `++(String, String)` exists.
3. The analyser can't distinguish a vec-eligible call from a regular
   one. `f(x)` where `f` is a regular function loses the LUB-derived
   return type even though no vec path exists.

This RFC closes all three.

[#139]: https://github.com/timfennis/andy-cpp/issues/139
[#140]: https://github.com/timfennis/andy-cpp/pull/140

## Design

### Operator-syntax marker on the AST

The parser desugars `a + b` (infix) and `-x` (prefix) into
`Expression::Call` with the same shape as `foo(a, b)`. The syntactic
origin is lost. Preserve it as a flag:

```rust
Expression::Call {
    function: Box<ExpressionLocation>,
    arguments: Vec<ExpressionLocation>,
    operator_form: bool,
}
```

Both infix and prefix-unary parser paths set `operator_form: true`.
Regular call parsing sets `false`. Downstream layers read the flag
without knowing which operator names are special.

Prefix-unary needs the same treatment as infix; "operator-syntax"
covers both.

### Dispatch rule

Vec candidates live alongside scalar overloads in the candidate list.
`find_overload` searches the augmented list in one pass; the dedicated
`try_vectorized_call` fallback step goes away:

1. `find_overload(candidates, args)` over scalar overloads followed by
   their vec variants.
2. If `None`, error.

Scalar overloads come first in the list; the first-match-wins semantic
gives "first-class wins, vec falls back" for free.

### Vec variants as candidates

Concrete example. The `+` operator has scalar overloads roughly like:

- `+(Int, Int) -> Int`
- `+(Float, Float) -> Float`
- `+(Number, Number) -> Number`

When scope resolution looks up `+` for an `operator_form` call, the
candidate list it considers includes each scalar overload **plus** a
synthesised vec variant of each:

```
search space for `+` (operator_form = true):
  1. scalar  +(Int, Int) -> Int
  2. scalar  +(Float, Float) -> Float
  3. scalar  +(Number, Number) -> Number
  4. vec     +(Int, Int)     — matches Tuple<Int,…> args
  5. vec     +(Float, Float) — matches Tuple<Float,…> args
  6. vec     +(Number, Number)
```

Each vec entry points to its scalar counterpart's slot. The
`vectorized: bool` on the candidate is the only thing distinguishing
the two. For a regular call (`operator_form: false`), scope skips the
vec-variant synthesis and the search space contains only the registered
scalar overloads.

A vec candidate matches the call's arguments when:

- At least one argument is `Tuple<…>` (statically) or an
  `Object::Tuple` (at runtime).
- All tuple-shaped args have the same length.
- Each per-position element matches the underlying scalar overload's
  parameter type. Scalar args in non-tuple positions broadcast.

Static and runtime checks differ in how they realise the per-position
rule. The analyser collapses each tuple-shaped arg to the LUB of its
element types (so `Tuple<Int, Float>` becomes `Number` for the lookup
sig) and then queries the existing overload-resolution helpers
(`find_function` for exact-subtype, `find_function_candidates` for
loose). The runtime checks every element pair individually against the
scalar's parameter types — that's what catches mixed-element tuples
like `(1, "a") + (2, "b")` that today's probe-first dispatch crashes
on at element 1.

This search space drives both `Resolved` and `Dynamic` bindings:

- If the analyser can pin exactly one candidate at compile time (the
  arg types match one entry and rule out the others), it emits
  `Resolved(candidate)`. No runtime dispatch.
- If multiple candidates remain in play because one or more args are
  `Any`, it emits `Dynamic(candidate_list)` and the full list goes to
  runtime.

Worked examples on the search space above:

| call                                  | arg types                              | binding   | candidate                           | result                  |
|---------------------------------------|----------------------------------------|-----------|-------------------------------------|-------------------------|
| `1 + 2`                               | `Int`, `Int`                           | Resolved  | #1 scalar                           | `Int`                   |
| `1.0 + 2`                             | `Float`, `Int`                         | Resolved  | #3 scalar (numeric coercion)        | `Number`                |
| `(1, 2) + (3, 4)`                     | `Tuple<Int, Int>`, `Tuple<Int, Int>`   | Resolved  | #4 vec                              | `Tuple<Int, Int>`       |
| `(1, 2) + 5`                          | `Tuple<Int, Int>`, `Int`               | Resolved  | #4 vec (right scalar broadcasts)    | `Tuple<Int, Int>`       |
| `(1.0, 2.0) + (3, 4)`                 | `Tuple<Float, Float>`, `Tuple<Int, Int>` | Resolved | #6 vec                              | `Tuple<Number, Number>` |
| `a + b` where `a, b: Any`             | `Any`, `Any`                           | Dynamic   | full list #1-#6 carried to runtime  | `Any`                   |
| `(a, b) + (c, d)` where `a..d: Any`   | `Tuple<Any, Any>`, `Tuple<Any, Any>`   | Dynamic   | vec variants compatible; list carried | `Any`                 |
| `foo((1, 2))` (regular call)          | per `foo`                              | per `foo` | scope synthesised no vec variants   | per `foo`               |

The `(1, 2) + (3, 4)` row is the case worth highlighting. Both arg
types are statically `Tuple<Int, Int>`, so the analyser picks vec
candidate #4 at compile time. Once the [compiler unrolling
optimisation](#compiler-optimisation) lands, this case emits unrolled
element calls directly with no `OverloadSet` construction. As shipped
the candidate is pushed via a single-entry overload set and dispatched
through the same value-level path Dynamic uses; the analyser-side win
is the precise `Tuple<Int, Int>` result type.

The Any rows are where `Dynamic` shows up: the analyser cannot narrow
the search space, so it carries the candidate list forward and lets the
value-level dispatcher pick at runtime. That dispatcher iterates the
same list the analyser built; scalars first, then vec variants, first
match wins.

Invoking a vec candidate (whether at compile time via Resolved or at
runtime via Dynamic) calls the scalar counterpart once per element
pair and gathers results into a tuple. For `(1, 2) + (3, 4)` the
candidate is #4, so the calls are `+(1, 3)` and `+(2, 4)` through the
slot of scalar `+(Int, Int)`, producing `(4, 6)`.

Synthesis happens at scope-resolution time, gated on `operator_form`.
The runtime never needs to know which entries were synthetic — it
dispatches over the same list the analyser produced.

### Runtime broadening

`try_vectorized_call` is gone. `find_overload` walks the augmented
candidate list once and routes the call:

- Scalar candidate: `Function::matches_value_args` checks the args
  directly; success returns `Callable::Scalar(func)`.
- Vec candidate: every element pair must satisfy the underlying scalar's
  parameter types (not just the first pair — the old probe-based
  dispatch silently miscoupled mixed-type tuples). Success returns
  `Callable::Vec(scalar_fn)`, which `dispatch_vec_call` invokes once per
  axis position, broadcasting non-tuple args.

The n-ary broadcast rule: any tuple-shaped arg defines the axis; all
tuple-shaped args must have equal length; scalars broadcast. Empty
tuples and length mismatches decline the vec match and the call falls
through to the regular "no overload found" error.

After these changes:

- `-(1, 2, 3)` vec's to `(-1, -2, -3)`.
- `("a", "b") ++ ("c", "d")` vec's to `("ac", "bd")` via
  `++(String, String)`.
- `([1], [2]) ++ ([3], [4])` vec's to `([1, 3], [2, 4])` via
  `++(List, List)`.

### Analyser binding shape

Vec is a property of the candidate, not of the binding. Each entry in
the candidate list carries a kind:

```rust
struct Candidate {
    var: ResolvedVar,    // pointer to the scalar overload's slot
    vectorized: bool,    // true → synthesised vec variant
}

enum Binding {
    None,
    Resolved(Candidate),
    Dynamic(Vec<Candidate>),
}
```

`Resolved(Candidate { vectorized: false })` is the common scalar
dispatch case. `Resolved(Candidate { vectorized: true })` happens when
exactly one vec variant matches the static arg types — the analyser
has pinned the call to a specific element overload.

Type inference per binding shape:

| binding                                  | result type                                |
|------------------------------------------|--------------------------------------------|
| `Resolved` to a scalar candidate         | overload's declared return                 |
| `Resolved` to a vec candidate            | `Tuple<elem_return; max_len>`              |
| `Dynamic`                                | LUB across every candidate's inferred return |

The analyser computes each candidate's contribution (declared return
for scalars, `Tuple<elem_return; max_len>` for vecs where `max_len` is
the statically known broadcast axis) and LUBs them. In the all-scalar
case this recovers the LUB precision PR #140 had to pessimise. In the
mixed-candidate case the type lattice naturally collapses
`LUB(Tuple<…>, scalar)` to `Any` because tuples only join with tuples
of equal arity — no special-casing needed.

Vec candidate return type uses **uniform LUB collapse**: each tuple-arg
contributes the LUB of its element types in a single position, and the
result tuple is filled with the scalar overload's return repeated
`max_len` times. So `(Int, Float) + (Float, Int)` → `Tuple<Number,
Number>`, not the per-element-precise `Tuple<Int, Number>`. The
per-position alternative was considered (see [Alternatives](#alternatives-considered))
and rejected for simplicity.

Vec candidate resolution mirrors the scalar path's two-stage lookup:
an exact-subtype `find_function` hit on the synthetic sig wins over
the looser `find_function_candidates` set. This is what makes
`Tuple<Int, Int> - Tuple<Int, Int>` resolve to `Tuple<Int, Int>`
instead of LUB'ing every compatible `-` overload into `Tuple<Number,
Number>`.

### Compiler optimisation

A `Resolved` binding to a vec candidate admits compile-time resolution.
The compiler knows the tuple length and which scalar overload the
candidate points to. Emit unrolled element calls plus `MakeTuple`
instead of `OverloadSet` dispatch:

```text
(1, 2) + (3, 4)   →   LoadConst 1; LoadConst 3; Call +(Int,Int);
                      LoadConst 2; LoadConst 4; Call +(Int,Int);
                      MakeTuple 2
```

The unrolled path skips OverloadSet construction and runtime dispatch.
This optimisation is independent of correctness; it has not yet landed.
As shipped, `Resolved(vec)` flows through a single-entry overload set
and the same dispatch path as `Dynamic`.

## Implementation

Steps 1–5 and 7 shipped together as one PR. Step 6 is deferred.

1. ✅ Parser: `operator_form: bool` added to `Expression::Call`. Derived
   `Clone`/`Debug` propagate it automatically.
2. ✅ Scope: vec candidates synthesised via a per-position LUB-collapsed
   sig when `operator_form` is true. Both the loose-compatibility set
   and the exact-subtype match are tracked so `Resolved(vec)` mirrors
   `Resolved(scalar)`'s precision.
3. ✅ Runtime: `try_vectorized_call` removed; vec dispatch lives in
   `find_overload` and `dispatch_vec_call`. N-ary broadcast settled
   (any tuple-shaped arg defines the axis; equal-length required;
   non-tuple args broadcast).
4. ✅ Runtime: element-call failures wrapped with `"while vectorising
   '<name>' at index N"`.
5. ✅ Analyser: type inference produces per-candidate types and LUBs
   them, recovering scalar LUB precision and giving precise tuple
   types for `Resolved(vec)`.
6. ⏸ Compiler: unrolled emission for `Resolved(vec)` not yet
   implemented. Today the analyser-side win (precise return type) lands;
   the runtime still dispatches through the overload-set path.
7. ✅ `BinaryOperator::supports_vectorization` and the
   `StaticType::supports_vectorization{,_with}` helpers deleted.

## Alternatives considered

### Universal vec

Drop the operator-syntax gate. Any call with tuple-shaped args can vec
when no overload matches.

Rejected. Every higher-order stdlib function (`map`, `filter`, `fold`,
…) would need a first-class `Tuple` overload registered or risk silent
vec'ing into nonsense. `map((1, 2, 3), f)` becomes
`(map(1, f), map(2, f), map(3, f))`, calling `map` on scalars. The
mitigation is a stdlib audit on every new HOF addition, easy to miss.
The operator-syntax marker gets the same expressive power for the
operator cases without that audit burden.

### Leak `BinaryOperator::supports_vectorization` into the analyser

Have the analyser consult the parser's curated operator list by name.

Rejected. The curated list becomes load-bearing in two crates (parser
and analyser). The operator-syntax marker keeps the fact where the
parser generates it; downstream layers read it without knowing which
operator names are special.

## Open questions

### P1: silent semantic shift on operator overloads — open

Adding `fn +(t: Tuple, u: Tuple) -> X` would shift `(1, 2) + (3, 4)`
from vec to first-class dispatch. The hazard is bounded to operator
overloads because regular calls never vec. The set of operator names
is small, fixed, and known to the parser. No mitigation shipped;
stdlib discipline is the de-facto safeguard for now.

### P2: element-call errors lose outer-call context — resolved

Element-call failures are now wrapped with `"while vectorising
'<name>' at index N: <inner>"`, so the outer call name and the failing
index appear in the error message.

### Length mismatch error — accepted as "no function found"

`(1, 2, 3) + (4, 5)`: the vec candidate match declines (length
mismatch on the tuple axis) and the call falls through to the regular
`"no function called '+' found matches the arguments: (Tuple<Int, Int,
Int>, Tuple<Int, Int>)"` error. A dedicated `"vec arity mismatch"`
message would be friendlier but isn't required for correctness.

### Empty tuple — errors as recommended

`() + ()`: `synthetic_vec_sig` rejects empty tuples, so no vec
candidate is synthesised. The call falls through to `"no function
found"` rather than returning `()`.

### Per-position scalar resolution for vec candidates — resolved (LUB collapse)

`(Int, Float) + (Float, Int)` resolves to a single vec candidate via
per-position LUB, so the result type is `Tuple<Number, Number>` rather
than the per-element-precise `Tuple<Int, Number>`. The precision loss
only affects operator-form calls over genuinely heterogeneous tuples,
which in practice are rare (product-style tuples like `("Tim", 35,
"NL")` never trigger vec at all), so the candidate-list simplicity won
out. Revisit if the imprecision starts to bite.

### Unrolling ceiling — pending

Relevant once step 6 (compiler unrolling) lands. Suggested threshold
N ≤ 8; confirm with bench data when implementing.

## References

- Issue [#139]: original regression.
- PR [#140]: soundness fix (`Binding::Dynamic` → `Any`).
- PR [#141]: implementation of this RFC.
- `ndc_vm/src/vm.rs::dispatch_vec_call`: runtime vec dispatch.
- `ndc_analyser/src/scope.rs::synthetic_vec_sig`: per-position LUB sig
  used for static vec candidate lookup.
