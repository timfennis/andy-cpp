# TODO

Open tasks collected from in-code comments. Resolve by implementing or opening a GitHub issue.

---

## Medium: Type-checking warnings and errors

These can be implemented incrementally once the basic type system is stable.

- **Logical-operator operand types** (`ndc_analyser/src/analyser.rs` ~line 58)
  `and` / `or` operands are not checked to be `Bool`. Should emit a warning or error when the
  operand type is known and incompatible.

- **Mismatched `if` branch types** (`ndc_analyser/src/analyser.rs` ~line 178)
  When `true`-branch and `false`-branch types differ (and neither is `Any`), a warning could be
  emitted.

- **Missing semicolon warning in `if`** (`ndc_analyser/src/analyser.rs` ~line 174)
  When the `true`-branch of an `if` produces a non-unit value but no `else` is present, a warning
  for the missing semicolon would be helpful.

- **`never` type for variable declarations** (`ndc_analyser/src/analyser.rs` ~line 66)
  `let x = …` currently resolves to `unit`. Introducing a `never` / `!` type might be more
  accurate, once the type lattice is fleshed out.

---

## Medium: Semantic analysis correctness

- **Conflicting binding on re-declaration** (`ndc_analyser/src/analyser.rs` ~line 141)
  When a function name is declared a second time in the same scope, the analyser silently creates
  a new binding instead of either updating the old one or raising an error. The right policy needs
  to be decided and implemented.

- **`debug_assert` → `unreachable!` in `find_function_candidates`** (`ndc_analyser/src/scope.rs` ~line 99)
  A variadic function match should be impossible at this call-site. The `debug_assert!(false, …)`
  should be replaced with `unreachable!` once we are confident the invariant holds.

---

## Medium: Number / arithmetic semantics

- **Bitwise NOT on non-integer numbers** (`ndc_core/src/num.rs` ~line 181)
  Currently `!float` and `!rational` return `NaN` (matching Noulith behaviour). Decide whether this
  is intentional for this language or whether it should be an error.

- **`bigint → int` rounding in floor/ceil/round** (`ndc_core/src/num.rs` ~line 584)
  After rounding a `Rational`, the result is converted to `BigInt` rather than trying to fit it
  back into a machine `i64`. Should attempt the smaller representation first.

- **Division performance** (`ndc_core/src/num.rs` ~line 323)
  `Div` always promotes both operands to `Rational`. In the common `Int / Int` case this is
  unnecessary. A fast path for integer operands would avoid the allocation.

---

## Small: Lexer improvements

- **Unicode escape sequences** (`ndc_lexer/src/string.rs` ~line 72)
  String literals do not support `\uXXXX` escape sequences. Add support.

- **`_` separator after decimal point** (`ndc_lexer/src/number.rs` ~line 130)
  `1_000.0` is valid, but `1.0_0` is probably not intended. Consider rejecting `_` after `.`.

- **Number suffix error interception** (`ndc_lexer/src/number.rs` ~line 48)
  The suffix-error checks inside `lex_number` may be redundant since no numeric suffixes are
  supported. Consider moving the check to after the lexer returns so it applies uniformly.

- **`validator_for_radix` performance** (`ndc_lexer/src/number.rs` ~line 231)
  The string-slice approach for validating digits by radix is O(radix) per character. A lookup
  table or `char::to_digit` would be faster.

- **`consume()` internal error handling** (`ndc_lexer/src/lib.rs` ~line 202)
  `consume()` panics with `expect` on underflow. Document the invariant or add a proper internal
  error type.

---

## Small: Parser error messages

- **Better error for boolean-returning `if` without semicolon** (`ndc_parser/src/parser.rs` ~line 738)
  The pattern `if x == y { true } else { false }` triggers a generic parse error. A targeted
  diagnostic would be more helpful.

- **"Expected expression" error quality** (`ndc_parser/src/parser.rs` ~line 1001)
  The fallback "Expected an expression but got '…'" message may not always accurately describe
  the failure. Audit and improve.

---

## Small: Test / debug

- **Error rendering in block-scope test** (`tests/programs/004_basic/005_block_scope_destroys_local_variables.ndc` line 6)
  The error is reported correctly but rendered weirdly in the test output. Investigate why and fix
  the display.
