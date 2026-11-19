//! The operators the compiler writes as their own instruction.

use crate::chunk::OpCode;
use crate::error::VmError;
use crate::value::Value;

/// What a native does that the compiler writes as an instruction of its own
/// where a call resolves to that native: `a < b` is one `Less` instead of a
/// call of `<`. The native, when it's called as a value, as in
/// `reduce(xs, +)`, runs the same code through [`Operator::apply`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    /// `<` of any two values that have an order.
    Less,
    /// `<=` of any two values that have an order.
    LessEqual,
    /// `>` of any two values that have an order.
    Greater,
    /// `>=` of any two values that have an order.
    GreaterEqual,
    /// `==` of any two values.
    Equal,
    /// `!=` of any two values.
    NotEqual,
    /// `+` of two `Int`s.
    AddInt,
    /// `-` of two `Int`s.
    SubInt,
    /// `*` of two `Int`s.
    MulInt,
    /// `+` of two `Float`s.
    AddFloat,
    /// `-` of two `Float`s.
    SubFloat,
    /// `*` of two `Float`s.
    MulFloat,
    /// `/` of two `Float`s.
    DivFloat,
    /// `-` of an `Int`.
    NegInt,
    /// `-` of a `Float`.
    NegFloat,
    /// `!` of a `Bool`.
    Not,
}

impl Operator {
    /// How many operands it takes.
    #[must_use]
    pub fn arity(self) -> usize {
        match self {
            Self::NegInt | Self::NegFloat | Self::Not => 1,
            _ => 2,
        }
    }

    /// The operator's result for `args`, as its native returns it.
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

    /// The instruction that applies it to the operands on top of the stack.
    pub(crate) fn opcode(self) -> OpCode {
        match self {
            Self::Less => OpCode::Less,
            Self::LessEqual => OpCode::LessEqual,
            Self::Greater => OpCode::Greater,
            Self::GreaterEqual => OpCode::GreaterEqual,
            Self::Equal => OpCode::Equal,
            Self::NotEqual => OpCode::NotEqual,
            Self::AddInt => OpCode::AddInt,
            Self::SubInt => OpCode::SubInt,
            Self::MulInt => OpCode::MulInt,
            Self::AddFloat => OpCode::AddFloat,
            Self::SubFloat => OpCode::SubFloat,
            Self::MulFloat => OpCode::MulFloat,
            Self::DivFloat => OpCode::DivFloat,
            Self::NegInt => OpCode::NegInt,
            Self::NegFloat => OpCode::NegFloat,
            Self::Not => OpCode::Not,
        }
    }

    /// The result of a binary operator for `left` and `right`.
    #[inline(always)]
    pub(crate) fn binary(self, left: &Value, right: &Value) -> Result<Value, VmError> {
        let ordered = |accepts: fn(std::cmp::Ordering) -> bool| match (left, right) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(accepts(left.cmp(right)))),
            _ => left
                .try_cmp(right)
                .map(|ordering| Value::Bool(accepts(ordering)))
                .map_err(VmError::native),
        };
        match self {
            Self::Less => ordered(std::cmp::Ordering::is_lt),
            Self::LessEqual => ordered(std::cmp::Ordering::is_le),
            Self::Greater => ordered(std::cmp::Ordering::is_gt),
            Self::GreaterEqual => ordered(std::cmp::Ordering::is_ge),
            Self::Equal => Ok(Value::Bool(equal(left, right))),
            Self::NotEqual => Ok(Value::Bool(!equal(left, right))),
            Self::AddInt => int_operation(left, right, "+", i64::checked_add),
            Self::SubInt => int_operation(left, right, "-", i64::checked_sub),
            Self::MulInt => int_operation(left, right, "*", i64::checked_mul),
            Self::AddFloat => float_operation(left, right, |left, right| left + right),
            Self::SubFloat => float_operation(left, right, |left, right| left - right),
            Self::MulFloat => float_operation(left, right, |left, right| left * right),
            Self::DivFloat => float_operation(left, right, |left, right| left / right),
            Self::NegInt | Self::NegFloat | Self::Not => {
                unreachable!("internal error: {self:?} takes one operand")
            }
        }
    }

    /// The result of a unary operator for `value`.
    #[inline(always)]
    pub(crate) fn unary(self, value: &Value) -> Result<Value, VmError> {
        match (self, value) {
            (Self::NegInt, Value::Int(value)) => value
                .checked_neg()
                .map(Value::Int)
                .ok_or_else(|| VmError::native("integer negation overflowed".to_string())),
            (Self::NegFloat, Value::Float(value)) => Ok(Value::Float(-value)),
            (Self::Not, Value::Bool(value)) => Ok(Value::Bool(!value)),
            (Self::NegInt, value) => Err(wrong_operand("(Int)", value)),
            (Self::NegFloat, value) => Err(wrong_operand("(Float)", value)),
            (Self::Not, value) => Err(wrong_operand("(Bool)", value)),
            _ => unreachable!("internal error: {self:?} takes two operands"),
        }
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

/// `operation` of two `Int`s, `name` being its operator, which fails when
/// the result doesn't fit an `Int`.
#[inline(always)]
fn int_operation(
    left: &Value,
    right: &Value,
    name: &str,
    operation: fn(i64, i64) -> Option<i64>,
) -> Result<Value, VmError> {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => operation(*left, *right)
            .map(Value::Int)
            .ok_or_else(|| overflowed(*left, name, *right)),
        (Value::Int(_), other) | (other, _) => Err(wrong_mode("Int", other)),
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
        (Value::Float(_), other) | (other, _) => Err(wrong_mode("Float", other)),
    }
}

#[cold]
#[inline(never)]
fn overflowed(left: i64, name: &str, right: i64) -> VmError {
    VmError::native(format!(
        "integer operation overflowed or is undefined: {left} {name} {right}"
    ))
}

#[cold]
#[inline(never)]
fn wrong_mode(expected: &str, value: &Value) -> VmError {
    VmError::native(format!(
        "expected {expected}, got {}",
        value.diagnostic_type()
    ))
}

#[cold]
#[inline(never)]
fn wrong_operand(expected: &str, value: &Value) -> VmError {
    VmError::native(format!(
        "expected {expected}, got ({})",
        value.diagnostic_type()
    ))
}
