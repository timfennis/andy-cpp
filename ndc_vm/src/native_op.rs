//! What each [`NativeOp`] does: the VM runs it as an instruction where a
//! call resolves to a native that does it, and the native runs the same code
//! when it's called as a value, as tuple vectorisation does.

use crate::error::VmError;
use crate::value::Value;
use ndc_core::NativeOp;
use std::cmp::Ordering;

/// What `op` returns for `args`, as its native returns it.
pub fn apply(op: NativeOp, args: &[Value]) -> Result<Value, VmError> {
    match args {
        [value] if op.arity() == 1 => unary(op, value),
        [left, right] if op.arity() == 2 => binary(op, left, right),
        _ => Err(VmError::native(format!(
            "expected {} arguments, got {}",
            op.arity(),
            args.len()
        ))),
    }
}

/// What the binary `op` returns for `left` and `right`: `3 - 1` is `2`,
/// and an `Int` operation that overflows fails.
#[inline(always)]
pub(crate) fn binary(op: NativeOp, left: &Value, right: &Value) -> Result<Value, VmError> {
    match op {
        NativeOp::AddInt => int_operation(left, right, "+", i64::checked_add),
        NativeOp::SubInt => int_operation(left, right, "-", i64::checked_sub),
        NativeOp::MulInt => int_operation(left, right, "*", i64::checked_mul),
        NativeOp::AddFloat => float_operation(left, right, |left, right| left + right),
        NativeOp::SubFloat => float_operation(left, right, |left, right| left - right),
        NativeOp::MulFloat => float_operation(left, right, |left, right| left * right),
        NativeOp::DivFloat => float_operation(left, right, |left, right| left / right),
        NativeOp::Less => compare(left, right).map(|ordering| Value::Bool(ordering.is_lt())),
        NativeOp::LessEqual => compare(left, right).map(|ordering| Value::Bool(ordering.is_le())),
        NativeOp::Greater => compare(left, right).map(|ordering| Value::Bool(ordering.is_gt())),
        NativeOp::GreaterEqual => {
            compare(left, right).map(|ordering| Value::Bool(ordering.is_ge()))
        }
        NativeOp::Equal => Ok(Value::Bool(equal(left, right))),
        NativeOp::NotEqual => Ok(Value::Bool(!equal(left, right))),
        NativeOp::NegInt | NativeOp::NegFloat | NativeOp::Not => {
            unreachable!("internal error: {op:?} takes one operand")
        }
    }
}

/// What the unary `op` returns for `value`.
#[inline(always)]
pub(crate) fn unary(op: NativeOp, value: &Value) -> Result<Value, VmError> {
    match (op, value) {
        (NativeOp::NegInt, Value::Int(value)) => value
            .checked_neg()
            .map(Value::Int)
            .ok_or_else(|| VmError::native("integer negation overflowed")),
        (NativeOp::NegFloat, Value::Float(value)) => Ok(Value::Float(-value)),
        (NativeOp::Not, Value::Bool(value)) => Ok(Value::Bool(!value)),
        (NativeOp::NegInt, value) => Err(wrong_operand(
            "(Int)",
            &format!("({})", value.diagnostic_type()),
        )),
        (NativeOp::NegFloat, value) => Err(wrong_operand(
            "(Float)",
            &format!("({})", value.diagnostic_type()),
        )),
        (NativeOp::Not, value) => Err(wrong_operand(
            "(Bool)",
            &format!("({})", value.diagnostic_type()),
        )),
        _ => unreachable!("internal error: {op:?} takes two operands"),
    }
}

/// The order of two values, as the comparisons find it.
#[inline(always)]
fn compare(left: &Value, right: &Value) -> Result<Ordering, VmError> {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => Ok(left.cmp(right)),
        _ => left.try_cmp(right).map_err(VmError::native),
    }
}

/// Whether `left` and `right` are equal, as `==` says.
#[inline(always)]
fn equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => left == right,
        _ => left == right,
    }
}

/// `operation` of two `Int`s, `operator` being its name, which fails when
/// the result doesn't fit an `Int`.
#[inline(always)]
fn int_operation(
    left: &Value,
    right: &Value,
    operator: &str,
    operation: fn(i64, i64) -> Option<i64>,
) -> Result<Value, VmError> {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => operation(*left, *right)
            .map(Value::Int)
            .ok_or_else(|| VmError::native(integer_overflow(*left, operator, *right))),
        (Value::Int(_), other) | (other, _) => {
            Err(wrong_operand("Int", &other.diagnostic_type().to_string()))
        }
    }
}

/// `operation` of two `Float`s.
#[inline(always)]
fn float_operation(
    left: &Value,
    right: &Value,
    operation: fn(f64, f64) -> f64,
) -> Result<Value, VmError> {
    match (left, right) {
        (Value::Float(left), Value::Float(right)) => Ok(Value::Float(operation(*left, *right))),
        (Value::Float(_), other) | (other, _) => {
            Err(wrong_operand("Float", &other.diagnostic_type().to_string()))
        }
    }
}

/// Operands that aren't what the op takes, as its native reports them.
#[cold]
#[inline(never)]
fn wrong_operand(expected: &str, got: &str) -> VmError {
    VmError::native(format!("expected {expected}, got {got}"))
}

/// What an `Int` operation that overflows, or has no result, fails with:
/// `integer operation overflowed or is undefined: 9223372036854775807 + 1`.
#[cold]
#[inline(never)]
pub fn integer_overflow(left: i64, operator: &str, right: i64) -> String {
    format!("integer operation overflowed or is undefined: {left} {operator} {right}")
}
