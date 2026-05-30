# Analyser readability review (`ndc_analyser`)

> Point-in-time review captured alongside the LSP work (PR #164). File/line
> references are a snapshot and will drift as the code changes; treat them as
> starting points, not exact coordinates.

## Context

The semantic analyser is the hardest part of the project to hold in one's head.
This review answers two questions, with the upcoming DefId/LSP-resolution work in
mind: (1) are there any *big* issues that only a major refactor can fix, and (2)
what *incremental* readability improvements are worth making? It proposes no
behaviour changes — it is an assessment plus a backlog. Sizes at time of writing:
`analyser.rs` ~930 lines, `scope.rs` ~1432 (~1038 production + ~394 tests),
`lib.rs` 5.

## Headline verdict

**No mandatory major refactor.** The architecture is sound for the language's feature
set: `Analyser` is a thin client over `ScopeTree`; types flow through side tables
(`expr_types`, `inferred_return_types`) plus in-place AST annotation (`resolved`,
`captures`, `inferred_type`); the compiler consumes the annotated AST. Nothing is
boxed-in. The difficulty is **accumulated local complexity**, which is entirely
addressable incrementally.

---

## Part 1 — Big issues

### The one architectural theme: `ScopeTree` conflates resolution + slot allocation

`ScopeTree` (`scope.rs`) does two jobs at once: **lexical name resolution** (which
declaration a name means) and **VM stack-slot allocation** (the concrete `usize`
`ResolvedVar::{Local,Upvalue,Global}{slot}`). This entanglement is the root of both
the intricacy (the `base_offset` / `function_scope_idx` / upvalue-hoisting math
threaded through every lookup) and the "slot isn't a stable identity" limitation that
bit the LSP.

**Feasibility (investigated):**
- The compiler is already **semi-independent** of analyser slots: it keeps its own
  `num_locals` and only `max`es it against declared slots (`compiler.rs:513,758,792,811`),
  and allocates its own temporaries (`compiler.rs:280-282,483-484`). So *local* slot
  numbering could plausibly move to the compiler.
- **Globals** are purely positional in the `FunctionRegistry` iteration order
  (`interpreter/src/lib.rs:221-229`, `scope.rs:370-384`, `vm.rs:204`) — moving their
  assignment needs a stable name→slot map handed compiler-side. Contained, not hard.
- **Upvalues/captures are the hard, tightly-coupled part** and *not cleanly
  separable*: the analyser computes `CaptureSource::{Local,Upvalue}(index)`
  (`scope.rs:474-480,988-1025`), the compiler embeds it verbatim into `OpCode::Closure`
  (`compiler.rs:736-742`), and the VM indexes `upvalues[slot]` directly (`vm.rs:491-532`).
  Critically, **discovering captures *is* a name-resolution activity** (you must
  resolve names across function boundaries to know what escapes), and the index *is*
  the layout — so "separating resolution from layout" buys little here.
- Other consumers: REPL resume leans on `Compiler::num_locals` (`interpreter/src/lib.rs`
  ~251/257/276); the LSP reads `ResolvedVar` but not slot numbers. Coupling surface is
  small (~6 files, ~70 lines).

**Verdict: feasible but low-ROI — recommend shelving.** It would mostly relocate
local-slot bookkeeping; it would *not* simplify the genuinely hard code (upvalue
hoisting, overload resolution — both intrinsic). It also touches the runtime hot path
(closure creation, REPL resume) for moderate risk. The planned **DefId side-table**
gives the LSP the stable identity it needs *without* this refactor, and would be the
natural seam if this is ever revisited. **Do DefId first; reconsider this only if a
concrete need appears.**

### Not-big, but worth knowing
- `resolve_call` / `scalar_walk` (the 5-case overload + tuple-broadcast cascade,
  `scope.rs:531-715`) is the most complex algorithm, but the complexity is *intrinsic*
  (overloading × vectorization × closures). It is well-documented; it can be made more
  readable (Part 2) but not fundamentally simpler without dropping features.

---

## Part 2 — Incremental readability backlog (prioritized)

All behaviour-preserving. Ordered by (value ÷ risk). Each is independently shippable.

### Batch 1 — High value, near-zero risk (pure moves/renames/docs)
1. **Extract big `analyse_inner` arms into methods.** `analyse_inner` is ~367 lines
   (`analyser.rs:104-470`). Move `FunctionDeclaration` (`282-371`, ~90 lines) →
   `analyse_function_declaration`, `OpAssignment` (`194-281`, ~88 lines) →
   `analyse_op_assignment`, `Assignment` (`169-193`) → `analyse_assignment`. Leaves the
   dispatcher a scannable table of one-liners. **Biggest single win.**
2. **Fix `span` shadowing** in `resolve_lvalue_declarative` (`analyser.rs:721-757`): the
   `Lvalue::Identifier { span, .. }` destructure shadows the method's `span` param —
   rename one. Genuine footgun.
3. **Module-level orientation docs.** Add a short "how analysis works" header to
   `analyser.rs` and `scope.rs` (two-phase function pre-registration; slot numbering &
   `base_offset`; upvalue hoisting; the 5-case resolution). Document the `base_offset` /
   `function_scope_idx` / `env_scopes` invariants once at their definitions
   (`scope.rs:128-135`). Highest orientation ROI.
4. **Fix the `TOOD` typo** (`analyser.rs:538`) and capture the "get this from the AST
   when the parser adds it" note as a real TODO.md entry / issue.

### Batch 2 — Dedupe tricky logic (low risk, removes copy-paste)
5. **Unify "widen binding or error".** The same widen-then-check-annotation block
   appears 3×: `analyser.rs:178-189` (Assignment), `244-255` (OpAssignment ident),
   `265-272` (OpAssignment index). Extract one helper
   `widen_binding(target, widened, value_type, span)`.
6. **Extract the upvalue-chain follower** in `scope.rs`. The `CaptureSource::Local |
   Upvalue` walk is duplicated in `get_type` (`387-413`) and `get_binding_mut`
   (`898-932`), and echoed in `hoist_upvalue` (`988-1025`). A `follow_upvalue_chain`
   helper removes the worst `scope.rs` duplication.
7. **Naming pass.** `sig`/`type_sig` → consistent `arg_types`; `loose` →
   `compatible_candidates`; `scope_ptr` → `scope_idx`; `env_scopes` →
   `crossed_fn_boundaries` (+ doc). Cheap, high comprehension value.

### Batch 3 — Structural tidy (small risk, needs tests first)
8. **Collapse dual error storage.** `Analyser.errors` (`analyser.rs:37`) duplicates
   `AnalysisResult.errors`, reconciled in `take_result` (`60-64`). Emit straight into
   `result.errors` and drop the field (check `emit`/`emit_external`/`has_errors`).
9. **Split `scalar_walk`** (`scope.rs:639-715`): factor the per-scope body
   (find-exact / collect-loose / collect-all-by-name) into a `scan_scope` helper used by
   both the loop and the global-scope fallback, removing the duplicated fallback block
   (`647-651` vs `683-688`).
10. **Extract `resolve_lvalue_declarative`'s Sequence arm** (`analyser.rs:~762-819`)
    into `resolve_sequence_lvalue`; it's long, nested, and has a shadow of `found_type`.
11. *(Optional)* **Error-constructor boilerplate** (`analyser.rs:~853-929`): 11
    `Self { text: format!(...), span }` constructors — a tiny macro or `new(span, msg)`
    helper trims repetition. Low priority (currently readable).

### Supporting: characterization tests (do before Batch 3)
`scope.rs` tests (~`1039-1432`, 20 cases) cover scope/upvalue mechanics well but **omit
`resolve_call`'s vec/dynamic paths** (`resolve_vec`, `VecResolution`, `Binding::Dynamic`,
`dynamic_return_type`, `extend_dedup`). Add characterization tests for the 5 resolution
cases first — they document behaviour *and* de-risk items 8–10.

---

## Recommended sequence & verification

If/when executed: Batch 1 → Batch 2 → (add resolution tests) → Batch 3, one small PR
per item, each gated on `cargo test` (workspace), `cargo clippy` clean, `cargo fmt`. The
functional suite (`tests/functional`) plus the `scope.rs` unit tests are the safety net;
the new characterization tests harden the riskiest area before it's touched. No item
changes runtime behaviour, so a green suite is sufficient verification.
