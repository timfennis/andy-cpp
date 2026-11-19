//! The natives the VM has instructions for: it runs a [`NativeOp`] as an
//! instruction where a call resolves to a native that does it, and the
//! native runs the same code when it's called as a value, as tuple
//! vectorisation does.

use crate::error::VmError;
use crate::value::Value;
use std::cmp::Ordering;

/// What a native does, when the VM has an instruction that does the same:
/// the compiler writes the instruction in place of a call the analyser
/// resolved to that native, as `n - 1` where `n` is an `Int`. Unlike an
/// [`ndc_core::Intrinsic`], the native is still there, typed by its
/// signature, for calls the compiler can't resolve and for using it as a
/// value, and the analyser doesn't know about it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NativeOp {
    /// `+(Int, Int)`, an error if it overflows.
    AddInt,
    /// `-(Int, Int)`, an error if it overflows.
    SubInt,
    /// `*(Int, Int)`, an error if it overflows.
    MulInt,
    /// `+(Float, Float)`.
    AddFloat,
    /// `-(Float, Float)`.
    SubFloat,
    /// `*(Float, Float)`.
    MulFloat,
    /// `/(Float, Float)`.
    DivFloat,
    /// `-(Int)`, an error if it overflows.
    NegInt,
    /// `-(Float)`.
    NegFloat,
    /// `!(Bool)`.
    Not,
    /// `<` of two values that have an order.
    Less,
    /// `<=` of two values that have an order.
    LessEqual,
    /// `>` of two values that have an order.
    Greater,
    /// `>=` of two values that have an order.
    GreaterEqual,
    /// `==` of any two values.
    Equal,
    /// `!=` of any two values.
    NotEqual,
}

impl NativeOp {
    /// How many operands it takes.
    #[must_use]
    pub fn arity(self) -> usize {
        match self {
            Self::NegInt | Self::NegFloat | Self::Not => 1,
            _ => 2,
        }
    }

    /// What it returns for `args`, as its native returns it.
    pub fn apply(self, args: &[Value]) -> Result<Value, VmError> {
        match args {
            [value] if self.arity() == 1 => self.unary(value),
            [left, right] if self.arity() == 2 => self.binary(left, right),
            _ => Err(VmError::native(format!(
                "expected {} arguments, got {}",
                self.arity(),
                args.len()
            ))),
        }
    }

    /// What a binary op returns for `left` and `right`: `3 - 1` is `2`, and
    /// an `Int` operation that overflows fails.
    #[inline(always)]
    pub(crate) fn binary(self, left: &Value, right: &Value) -> Result<Value, VmError> {
        match self {
            Self::AddInt => int_operation(left, right, "+", i64::checked_add),
            Self::SubInt => int_operation(left, right, "-", i64::checked_sub),
            Self::MulInt => int_operation(left, right, "*", i64::checked_mul),
            Self::AddFloat => float_operation(left, right, |left, right| left + right),
            Self::SubFloat => float_operation(left, right, |left, right| left - right),
            Self::MulFloat => float_operation(left, right, |left, right| left * right),
            Self::DivFloat => float_operation(left, right, |left, right| left / right),
            Self::Less => compare(left, right).map(|ordering| Value::Bool(ordering.is_lt())),
            Self::LessEqual => compare(left, right).map(|ordering| Value::Bool(ordering.is_le())),
            Self::Greater => compare(left, right).map(|ordering| Value::Bool(ordering.is_gt())),
            Self::GreaterEqual => {
                compare(left, right).map(|ordering| Value::Bool(ordering.is_ge()))
            }
            Self::Equal => Ok(Value::Bool(equal(left, right))),
            Self::NotEqual => Ok(Value::Bool(!equal(left, right))),
            Self::NegInt | Self::NegFloat | Self::Not => {
                unreachable!("internal error: {self:?} takes one operand")
            }
        }
    }

    /// What a unary op returns for `value`.
    #[inline(always)]
    pub(crate) fn unary(self, value: &Value) -> Result<Value, VmError> {
        match (self, value) {
            (Self::NegInt, Value::Int(value)) => value
                .checked_neg()
                .map(Value::Int)
                .ok_or_else(|| VmError::native("integer negation overflowed")),
            (Self::NegFloat, Value::Float(value)) => Ok(Value::Float(-value)),
            (Self::Not, Value::Bool(value)) => Ok(Value::Bool(!value)),
            (Self::NegInt, value) => Err(wrong_operand(
                "(Int)",
                &format!("({})", value.diagnostic_type()),
            )),
            (Self::NegFloat, value) => Err(wrong_operand(
                "(Float)",
                &format!("({})", value.diagnostic_type()),
            )),
            (Self::Not, value) => Err(wrong_operand(
                "(Bool)",
                &format!("({})", value.diagnostic_type()),
            )),
            _ => unreachable!("internal error: {self:?} takes two operands"),
        }
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
