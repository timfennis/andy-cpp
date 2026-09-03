use factorial::Factorial;
use ndc_core::{FunctionRegistry, StaticType};
use ndc_vm::error::VmError;
use ndc_vm::value::{AdvancedNumber, BinaryOperatorError};
use ndc_vm::value::{NativeFunc, NativeFunction, NumericMode, NumericRef, Object, Value};
use num::complex::Complex64;
use num::{BigInt, BigUint, Integer, ToPrimitive};
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};
use std::rc::Rc;

#[derive(Clone, Copy)]
enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Rem,
    RemEuclid,
    Pow,
}

impl BinaryOperation {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::FloorDiv => "\\",
            Self::Rem => "%",
            Self::RemEuclid => "%%",
            Self::Pow => "^",
        }
    }

    fn documentation(self) -> &'static str {
        match self {
            Self::Add => "Adds two numbers.",
            Self::Sub => "Subtracts two numbers.",
            Self::Mul => "Multiplies two numbers.",
            Self::Div => "Divides two numbers.",
            Self::FloorDiv => {
                "Divides two numbers and rounds the quotient toward negative infinity."
            }
            Self::Rem => "Returns the remainder paired with truncating division.",
            Self::RemEuclid => "Returns the Euclidean remainder.",
            Self::Pow => "Raises the left operand to the power of the right operand.",
        }
    }
}

pub fn register(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    register_binary_arithmetic(env);
    register_unary_arithmetic(env);
    register_comparisons(env);
    register_bitwise(env);
    register_constructors(env);
    register_aggregates(env);
    register_number_helpers(env);
    register_integer_helpers(env);
    register_conversions(env);
    register_transcendentals(env);
}

fn declare(
    env: &mut FunctionRegistry<Rc<NativeFunction>>,
    name: &str,
    parameters: Vec<StaticType>,
    return_type: StaticType,
    documentation: impl Into<String>,
    func: impl Fn(&[Value]) -> Result<Value, VmError> + 'static,
) {
    env.declare_global_fn(Rc::new(NativeFunction {
        name: name.to_string(),
        documentation: Some(documentation.into()),
        static_type: StaticType::Function {
            parameters: Some(parameters),
            return_type: Box::new(return_type),
        },
        func: NativeFunc::Simple(Box::new(func)),
    }));
}

/// Declares a native whose operands and result have fixed types.
///
/// A type name is written once and used twice: as the operand's declared
/// [`StaticType`] and as the [`Value`] variant it must arrive in. A signature
/// therefore cannot drift away from the destructuring that enforces it, and
/// the mismatch error is reported from one place.
///
/// Each operand is bound by name to a reference to its payload, and the body
/// evaluates to the result's payload — or, with a `Result<_>` result type, to a
/// `Result` carrying it:
///
/// ```ignore
/// declare_typed!(env, "&", (left: Int, right: Int) -> Int, "…", left & right);
/// declare_typed!(env, "-", (value: Int) -> Result<Int>, "…",
///     value.checked_neg().ok_or_else(|| overflowed()));
/// ```
macro_rules! declare_typed {
    ($env:expr, $name:expr, ($($operand:ident: $operand_type:ident),+) -> Result<$result:ident>,
     $documentation:expr, $body:expr) => {
        declare(
            $env,
            $name,
            vec![$(StaticType::$operand_type),+],
            StaticType::$result,
            $documentation,
            move |args| {
                let [$(Value::$operand_type($operand)),+] = args else {
                    return Err(wrong_operands(&[$(StaticType::$operand_type),+], args));
                };
                $body.map(|payload| wrap_payload!($result, payload))
            },
        )
    };
    ($env:expr, $name:expr, ($($operand:ident: $operand_type:ident),+) -> $result:ident,
     $documentation:expr, $body:expr) => {
        declare_typed!(
            $env, $name, ($($operand: $operand_type),+) -> Result<$result>, $documentation,
            Ok::<_, VmError>($body)
        )
    };
}

/// Wraps a result payload as a [`Value`]. `Number` is not a plain variant
/// wrapper, so it needs its own arm.
macro_rules! wrap_payload {
    (Number, $payload:expr) => {
        Value::from_number($payload)
    };
    ($variant:ident, $payload:expr) => {
        Value::$variant($payload)
    };
}

/// Reports operands that do not match a native's declared signature.
#[cold]
#[inline(never)]
fn wrong_operands(expected: &[StaticType], args: &[Value]) -> VmError {
    let list = |types: Vec<String>| types.join(", ");
    let expected = list(expected.iter().map(StaticType::to_string).collect());
    let got = list(
        args.iter()
            .map(|arg| arg.static_type().to_string())
            .collect(),
    );

    VmError::native(format!("expected ({expected}), got ({got})"))
}

fn arity(args: &[Value], expected: usize) -> Result<(), VmError> {
    if args.len() == expected {
        Ok(())
    } else {
        Err(VmError::native(format!(
            "expected {expected} arguments, got {}",
            args.len()
        )))
    }
}

fn native_error(error: impl std::fmt::Display) -> VmError {
    VmError::native(error.to_string())
}

/// Runtime overload candidates are inspected in reverse registration order.
/// Keep homogeneous primitive pairs first in that runtime order: dynamic code
/// usually preserves one numeric representation across repeated operations.
const NUMERIC_PAIRS_BY_DYNAMIC_PRIORITY: [(NumericMode, NumericMode); 9] = [
    (NumericMode::Int, NumericMode::Int),
    (NumericMode::Float, NumericMode::Float),
    (NumericMode::Number, NumericMode::Number),
    (NumericMode::Int, NumericMode::Float),
    (NumericMode::Float, NumericMode::Int),
    (NumericMode::Int, NumericMode::Number),
    (NumericMode::Number, NumericMode::Int),
    (NumericMode::Float, NumericMode::Number),
    (NumericMode::Number, NumericMode::Float),
];

fn register_binary_arithmetic(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    const OPERATIONS: [BinaryOperation; 8] = [
        BinaryOperation::Add,
        BinaryOperation::Sub,
        BinaryOperation::Mul,
        BinaryOperation::Div,
        BinaryOperation::FloorDiv,
        BinaryOperation::Rem,
        BinaryOperation::RemEuclid,
        BinaryOperation::Pow,
    ];

    for operation in OPERATIONS {
        for (left_mode, right_mode) in NUMERIC_PAIRS_BY_DYNAMIC_PRIORITY.into_iter().rev() {
            let output_mode = left_mode.promote(right_mode);
            let parameters = vec![left_mode.static_type(), right_mode.static_type()];
            let (name, documentation) = (operation.name(), operation.documentation());

            // Choosing the evaluator per overload here, instead of letting one
            // shared closure rediscover the output mode on every call, is what
            // lets `1 + 2` read two `i64` slots and add them.
            match output_mode {
                NumericMode::Int => declare(
                    env,
                    name,
                    parameters,
                    StaticType::Int,
                    documentation,
                    move |args| {
                        let [left, right] = args else {
                            return Err(wrong_arity(args));
                        };
                        eval_int_operands(operation, left, right)
                    },
                ),
                NumericMode::Float => declare(
                    env,
                    name,
                    parameters,
                    StaticType::Float,
                    documentation,
                    move |args| {
                        let [left, right] = args else {
                            return Err(wrong_arity(args));
                        };
                        eval_float_operands(operation, left_mode, right_mode, left, right)
                    },
                ),
                NumericMode::Number => declare(
                    env,
                    name,
                    parameters,
                    StaticType::Number,
                    documentation,
                    move |args| {
                        let [left, right] = args else {
                            return Err(wrong_arity(args));
                        };
                        eval_number_operands(operation, left_mode, right_mode, left, right)
                    },
                ),
            }
        }
    }
}

fn eval_binary(
    operation: BinaryOperation,
    left_mode: NumericMode,
    right_mode: NumericMode,
    output_mode: NumericMode,
    left: &Value,
    right: &Value,
) -> Result<Value, VmError> {
    match output_mode {
        NumericMode::Int => eval_int_operands(operation, left, right),
        NumericMode::Float => eval_float_operands(operation, left_mode, right_mode, left, right),
        NumericMode::Number => eval_number_operands(operation, left_mode, right_mode, left, right),
    }
}

/// `promote` only answers `Int` for two `Int` operands, so both are plain
/// `i64` slots and need no numeric-mode round trip.
#[inline]
fn eval_int_operands(
    operation: BinaryOperation,
    left: &Value,
    right: &Value,
) -> Result<Value, VmError> {
    let Value::Int(left_int) = left else {
        return Err(wrong_mode(NumericMode::Int, left));
    };
    let Value::Int(right_int) = right else {
        return Err(wrong_mode(NumericMode::Int, right));
    };

    eval_int_binary(operation, *left_int, *right_int).map(Value::Int)
}

#[inline]
fn eval_float_operands(
    operation: BinaryOperation,
    left_mode: NumericMode,
    right_mode: NumericMode,
    left: &Value,
    right: &Value,
) -> Result<Value, VmError> {
    let Some(left_float) = primitive_float(left_mode, left) else {
        return Err(wrong_mode(left_mode, left));
    };
    let Some(right_float) = primitive_float(right_mode, right) else {
        return Err(wrong_mode(right_mode, right));
    };

    Ok(Value::Float(eval_float_binary(
        operation,
        left_float,
        right_float,
    )))
}

fn eval_number_operands(
    operation: BinaryOperation,
    left_mode: NumericMode,
    right_mode: NumericMode,
    left: &Value,
    right: &Value,
) -> Result<Value, VmError> {
    let left = numeric_ref(left_mode, left)?.to_advanced_number();
    let right = numeric_ref(right_mode, right)?.to_advanced_number();

    eval_number_binary(operation, left, right)
        .map(Value::from_number)
        .map_err(native_error)
}

/// Reads an operand of a `Float` overload: an `Int` widens and a `Float`
/// passes through. A `Number` operand never reaches one, because any `Number`
/// promotes the whole operation to `Number`.
#[inline]
fn primitive_float(mode: NumericMode, value: &Value) -> Option<f64> {
    match (mode, value) {
        (NumericMode::Int, Value::Int(value)) => Some(*value as f64),
        (NumericMode::Float, Value::Float(value)) => Some(*value),
        _ => None,
    }
}

/// Kept out of line so the operand destructuring above stays inlineable.
#[cold]
#[inline(never)]
fn wrong_arity(args: &[Value]) -> VmError {
    VmError::native(format!("expected 2 arguments, got {}", args.len()))
}

#[inline]
fn numeric_ref(mode: NumericMode, value: &Value) -> Result<NumericRef<'_>, VmError> {
    match value.numeric_ref() {
        Some(number) if number.mode() == mode => Ok(number),
        _ => Err(wrong_mode(mode, value)),
    }
}

/// Kept out of line so the operand check above stays small enough to inline
/// into each registered overload.
#[cold]
#[inline(never)]
fn wrong_mode(mode: NumericMode, value: &Value) -> VmError {
    VmError::native(format!(
        "expected {}, got {}",
        mode.static_type(),
        value.static_type()
    ))
}

fn eval_int_binary(operation: BinaryOperation, left: i64, right: i64) -> Result<i64, VmError> {
    let failed = || {
        VmError::native(format!(
            "integer operation overflowed or is undefined: {left} {} {right}",
            operation.name()
        ))
    };

    // Each operation carries its own precondition, so add, subtract and
    // multiply test nothing but their own overflow flag.
    match operation {
        BinaryOperation::Add => left.checked_add(right).ok_or_else(failed),
        BinaryOperation::Sub => left.checked_sub(right).ok_or_else(failed),
        BinaryOperation::Mul => left.checked_mul(right).ok_or_else(failed),
        BinaryOperation::Div => {
            nonzero_divisor(right)?;
            left.checked_div(right).ok_or_else(failed)
        }
        BinaryOperation::FloorDiv => {
            nonzero_divisor(right)?;
            checked_floor_div(left, right).ok_or_else(failed)
        }
        BinaryOperation::Rem => {
            nonzero_divisor(right)?;
            left.checked_rem(right).ok_or_else(failed)
        }
        BinaryOperation::RemEuclid => {
            nonzero_divisor(right)?;
            left.checked_rem_euclid(right).ok_or_else(failed)
        }
        BinaryOperation::Pow => {
            if right < 0 {
                return Err(negative_exponent());
            }
            u32::try_from(right)
                .ok()
                .and_then(|right| left.checked_pow(right))
                .ok_or_else(failed)
        }
    }
}

/// A zero divisor is reported as such rather than as the overflow the checked
/// operation would otherwise report.
#[inline]
fn nonzero_divisor(divisor: i64) -> Result<(), VmError> {
    if divisor == 0 {
        return Err(division_by_zero());
    }
    Ok(())
}

#[cold]
#[inline(never)]
fn division_by_zero() -> VmError {
    VmError::native("division by zero".to_string())
}

#[cold]
#[inline(never)]
fn negative_exponent() -> VmError {
    VmError::native("negative integer exponents require Number operands".to_string())
}

fn checked_floor_div(left: i64, right: i64) -> Option<i64> {
    let quotient = left.checked_div(right)?;
    let remainder = left.checked_rem(right)?;
    if remainder != 0 && (left < 0) != (right < 0) {
        quotient.checked_sub(1)
    } else {
        Some(quotient)
    }
}

fn eval_float_binary(operation: BinaryOperation, left: f64, right: f64) -> f64 {
    match operation {
        BinaryOperation::Add => left + right,
        BinaryOperation::Sub => left - right,
        BinaryOperation::Mul => left * right,
        BinaryOperation::Div => left / right,
        BinaryOperation::FloorDiv => (left / right).floor(),
        BinaryOperation::Rem => left % right,
        BinaryOperation::RemEuclid => left.rem_euclid(right),
        BinaryOperation::Pow => left.powf(right),
    }
}

fn eval_number_binary(
    operation: BinaryOperation,
    left: AdvancedNumber,
    right: AdvancedNumber,
) -> Result<AdvancedNumber, BinaryOperatorError> {
    match operation {
        BinaryOperation::Add => left.add(right),
        BinaryOperation::Sub => left.sub(right),
        BinaryOperation::Mul => left.mul(right),
        BinaryOperation::Div => left.div(right),
        BinaryOperation::FloorDiv => left.floor_div(right),
        BinaryOperation::Rem => left.rem(right),
        BinaryOperation::RemEuclid => left.checked_rem_euclid(&right),
        BinaryOperation::Pow => left.pow(right),
    }
}

fn register_unary_arithmetic(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    declare_typed!(
        env,
        "-",
        (value: Int) -> Result<Int>,
        "Negates an integer.",
        value
            .checked_neg()
            .ok_or_else(|| VmError::native("integer negation overflowed".to_string()))
    );
    declare_typed!(
        env,
        "-",
        (value: Float) -> Float,
        "Negates a floating-point number.",
        -value
    );
    declare_typed!(
        env,
        "-",
        (value: Number) -> Number,
        "Negates an advanced number.",
        value.as_ref().clone().neg()
    );
}

/// Orders two operands, reporting the pair that has no ordering between them.
fn compare_operands(args: &[Value]) -> Result<Ordering, VmError> {
    let [left, right] = args else {
        return Err(wrong_arity(args));
    };

    left.partial_cmp(right).ok_or_else(|| {
        VmError::native(format!(
            "cannot compare {} and {}",
            left.static_type(),
            right.static_type()
        ))
    })
}

fn register_comparisons(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    for (name, predicate, docs) in [
        (
            ">",
            (|ordering| ordering == Ordering::Greater) as fn(Ordering) -> bool,
            "Returns whether the left value is greater than the right.",
        ),
        (
            ">=",
            |ordering| matches!(ordering, Ordering::Greater | Ordering::Equal),
            "Returns whether the left value is greater than or equal to the right.",
        ),
        (
            "<",
            |ordering| ordering == Ordering::Less,
            "Returns whether the left value is less than the right.",
        ),
        (
            "<=",
            |ordering| matches!(ordering, Ordering::Less | Ordering::Equal),
            "Returns whether the left value is less than or equal to the right.",
        ),
    ] {
        declare(
            env,
            name,
            vec![StaticType::Any, StaticType::Any],
            StaticType::Bool,
            docs,
            move |args| Ok(Value::Bool(predicate(compare_operands(args)?))),
        );
    }

    declare(
        env,
        "==",
        vec![StaticType::Any, StaticType::Any],
        StaticType::Bool,
        "Returns whether two values are equal.",
        |args| {
            arity(args, 2)?;
            Ok(Value::Bool(args[0] == args[1]))
        },
    );
    declare(
        env,
        "!=",
        vec![StaticType::Any, StaticType::Any],
        StaticType::Bool,
        "Returns whether two values are not equal.",
        |args| {
            arity(args, 2)?;
            Ok(Value::Bool(args[0] != args[1]))
        },
    );

    for (name, reverse, docs) in [
        ("<=>", false, "Performs a three-way comparison."),
        (">=<", true, "Performs a reverse three-way comparison."),
    ] {
        declare(
            env,
            name,
            vec![StaticType::Any, StaticType::Any],
            StaticType::Int,
            docs,
            move |args| {
                let result = match compare_operands(args)? {
                    Ordering::Less => -1,
                    Ordering::Equal => 0,
                    Ordering::Greater => 1,
                };
                Ok(Value::Int(if reverse { -result } else { result }))
            },
        );
    }
}

fn register_bitwise(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    for (name, operation, docs) in [
        (
            "&",
            (|left, right| left & right) as fn(i64, i64) -> i64,
            "Computes bitwise AND of two integers.",
        ),
        (
            "|",
            |left, right| left | right,
            "Computes bitwise OR of two integers.",
        ),
        (
            "~",
            |left, right| left ^ right,
            "Computes bitwise XOR of two integers.",
        ),
    ] {
        declare_typed!(
            env,
            name,
            (left: Int, right: Int) -> Int,
            docs,
            operation(*left, *right)
        );
    }

    for (name, operation, docs) in [
        (
            "&",
            (|left, right| left & right) as fn(bool, bool) -> bool,
            "Computes logical AND of two booleans.",
        ),
        (
            "|",
            |left, right| left | right,
            "Computes logical OR of two booleans.",
        ),
        (
            "~",
            |left, right| left ^ right,
            "Computes logical XOR of two booleans.",
        ),
    ] {
        declare_typed!(
            env,
            name,
            (left: Bool, right: Bool) -> Bool,
            docs,
            operation(*left, *right)
        );
    }

    declare_typed!(
        env,
        "~",
        (value: Int) -> Int,
        "Computes bitwise NOT of an integer.",
        value.not()
    );

    for name in ["!", "not"] {
        declare_typed!(
            env,
            name,
            (value: Bool) -> Bool,
            "Computes logical negation.",
            !value
        );
    }

    for (name, left_shift) in [("<<", true), (">>", false)] {
        declare_typed!(
            env,
            name,
            (left: Int, right: Int) -> Result<Int>,
            "Shifts an integer by a checked non-negative amount.",
            {
                let amount = u32::try_from(*right)
                    .map_err(|_error| VmError::native("invalid shift amount".to_string()))?;
                if left_shift {
                    left.checked_shl(amount)
                } else {
                    left.checked_shr(amount)
                }
                .ok_or_else(|| VmError::native("invalid shift amount".to_string()))
            }
        );
    }
}

fn register_constructors(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    for mode in NumericMode::ALL {
        declare(
            env,
            "Number",
            vec![mode.static_type()],
            StaticType::Number,
            "Wraps a primitive numeric value as a Number.",
            move |args| {
                arity(args, 1)?;
                Ok(Value::from_number(
                    numeric_ref(mode, &args[0])?.to_advanced_number(),
                ))
            },
        );
    }
}

fn register_aggregates(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    for mode in NumericMode::ALL {
        for (name, product) in [("sum", false), ("product", true)] {
            declare(
                env,
                name,
                vec![StaticType::Sequence(Box::new(mode.static_type()))],
                mode.static_type(),
                if product {
                    "Returns the product of a numeric sequence."
                } else {
                    "Returns the sum of a numeric sequence."
                },
                move |args| aggregate(args, mode, product),
            );
        }
    }
}

fn aggregate(args: &[Value], mode: NumericMode, product: bool) -> Result<Value, VmError> {
    arity(args, 1)?;

    if let Value::Object(object) = &args[0] {
        match object.as_ref() {
            Object::List(values) => {
                let values = values.borrow();
                return aggregate_values(values.iter(), mode, product);
            }
            Object::Tuple(values) => return aggregate_values(values.iter(), mode, product),
            Object::Deque(values) => {
                let values = values.borrow();
                return aggregate_values(values.iter(), mode, product);
            }
            _ => {}
        }
    }

    let values = args[0]
        .clone()
        .try_into_iter()
        .ok_or_else(|| VmError::native("expected a sequence".to_string()))?;
    aggregate_values(values, mode, product)
}

fn aggregate_values<I>(mut values: I, mode: NumericMode, product: bool) -> Result<Value, VmError>
where
    I: Iterator,
    I::Item: std::borrow::Borrow<Value>,
{
    match mode {
        NumericMode::Int => {
            let initial: i64 = if product { 1 } else { 0 };
            let value = values.try_fold(initial, |accumulator, value| {
                let value = std::borrow::Borrow::borrow(&value);
                let Value::Int(value) = value else {
                    return Err(VmError::native("expected a sequence of Int".to_string()));
                };
                if product {
                    accumulator.checked_mul(*value)
                } else {
                    accumulator.checked_add(*value)
                }
                .ok_or_else(|| VmError::native("integer aggregate overflowed".to_string()))
            })?;
            Ok(Value::Int(value))
        }
        NumericMode::Float => {
            let initial = if product { 1.0 } else { 0.0 };
            let value = values.try_fold(initial, |accumulator, value| {
                let value = std::borrow::Borrow::borrow(&value);
                let Value::Float(value) = value else {
                    return Err(VmError::native("expected a sequence of Float".to_string()));
                };
                Ok::<_, VmError>(if product {
                    accumulator * *value
                } else {
                    accumulator + *value
                })
            })?;
            Ok(Value::Float(value))
        }
        NumericMode::Number => {
            let initial = AdvancedNumber::Int(BigInt::from(if product { 1 } else { 0 }));
            let value = values.try_fold(initial, |accumulator, value| {
                let value = std::borrow::Borrow::borrow(&value);
                let Value::Number(value) = value else {
                    return Err(VmError::native("expected a sequence of Number".to_string()));
                };
                if product {
                    accumulator.mul(value.as_ref().clone())
                } else {
                    accumulator.add(value.as_ref().clone())
                }
                .map_err(native_error)
            })?;
            Ok(Value::from_number(value))
        }
    }
}

#[derive(Clone, Copy)]
enum PreservingUnary {
    Signum,
    Ceil,
    Floor,
    Round,
    Abs,
}

fn register_number_helpers(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    for mode in NumericMode::ALL {
        declare(
            env,
            "signum",
            vec![mode.static_type()],
            mode.static_type(),
            "Returns the sign of a number.",
            move |args| unary_preserving(args, mode, PreservingUnary::Signum),
        );
        for (name, operation) in [
            ("ceil", PreservingUnary::Ceil),
            ("floor", PreservingUnary::Floor),
            ("round", PreservingUnary::Round),
            ("abs", PreservingUnary::Abs),
        ] {
            declare(
                env,
                name,
                vec![mode.static_type()],
                mode.static_type(),
                "Applies a numeric operation while preserving the numeric mode.",
                move |args| unary_preserving(args, mode, operation),
            );
        }
    }

    for left in NumericMode::ALL {
        for right in NumericMode::ALL {
            let output = left.promote(right);
            declare(
                env,
                "abs_diff",
                vec![left.static_type(), right.static_type()],
                output.static_type(),
                "Returns the absolute difference between two numbers.",
                move |args| {
                    arity(args, 2)?;
                    let difference = eval_binary(
                        BinaryOperation::Sub,
                        left,
                        right,
                        output,
                        &args[0],
                        &args[1],
                    )?;
                    unary_preserving(&[difference], output, PreservingUnary::Abs)
                },
            );
        }
    }

    for (name, imaginary) in [("real", false), ("imag", true)] {
        declare_typed!(
            env,
            name,
            (value: Number) -> Number,
            "Returns a component of an advanced number.",
            match (value.as_ref(), imaginary) {
                (AdvancedNumber::Complex(value), false) => AdvancedNumber::Float(value.re),
                (AdvancedNumber::Complex(value), true) => AdvancedNumber::Float(value.im),
                (_, false) => value.as_ref().clone(),
                (_, true) => AdvancedNumber::Int(BigInt::from(0)),
            }
        );
    }

    for (name, numerator) in [("numerator", true), ("denominator", false)] {
        declare_typed!(
            env,
            name,
            (value: Number) -> Number,
            "Returns a component of an exact Number fraction.",
            match value.as_ref() {
                AdvancedNumber::Int(value) if numerator => AdvancedNumber::Int(value.clone()),
                AdvancedNumber::Int(_) => AdvancedNumber::Int(BigInt::from(1)),
                AdvancedNumber::Rational(value) if numerator => {
                    AdvancedNumber::Int(value.numer().clone())
                }
                AdvancedNumber::Rational(value) => AdvancedNumber::Int(value.denom().clone()),
                _ => {
                    return Err(VmError::native(
                        "expected an exact integer or rational Number".to_string(),
                    ));
                }
            }
        );
    }
}

fn unary_preserving(
    args: &[Value],
    mode: NumericMode,
    operation: PreservingUnary,
) -> Result<Value, VmError> {
    arity(args, 1)?;
    match (mode, &args[0]) {
        (NumericMode::Int, Value::Int(value)) => {
            let result = match operation {
                PreservingUnary::Signum => value.signum(),
                PreservingUnary::Ceil | PreservingUnary::Floor | PreservingUnary::Round => *value,
                PreservingUnary::Abs => value.checked_abs().ok_or_else(|| {
                    VmError::native("integer absolute value overflowed".to_string())
                })?,
            };
            Ok(Value::Int(result))
        }
        (NumericMode::Float, Value::Float(value)) => {
            let result = match operation {
                PreservingUnary::Signum => value.signum(),
                PreservingUnary::Ceil => value.ceil(),
                PreservingUnary::Floor => value.floor(),
                PreservingUnary::Round => value.round(),
                PreservingUnary::Abs => value.abs(),
            };
            Ok(Value::Float(result))
        }
        (NumericMode::Number, Value::Number(value)) => {
            let result = match operation {
                PreservingUnary::Signum => value.signum(),
                PreservingUnary::Ceil => value.ceil(),
                PreservingUnary::Floor => value.floor(),
                PreservingUnary::Round => value.round(),
                PreservingUnary::Abs => value.abs(),
            };
            Ok(Value::from_number(result))
        }
        _ => Err(VmError::native(format!(
            "expected {}, got {}",
            mode.static_type(),
            args[0].static_type()
        ))),
    }
}

fn register_integer_helpers(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    declare_typed!(
        env,
        "factorial",
        (value: Int) -> Result<Int>,
        "Returns the checked factorial of a non-negative Int.",
        {
            if *value < 0 {
                return Err(VmError::native(
                    "cannot compute the factorial of a negative number".to_string(),
                ));
            }
            (1..=*value).try_fold(1i64, i64::checked_mul).ok_or_else(|| {
                VmError::native("integer factorial overflowed; use a Number argument".to_string())
            })
        }
    );
    declare(
        env,
        "factorial",
        vec![StaticType::Number],
        StaticType::Number,
        "Returns the arbitrary-precision factorial of an exact non-negative Number.",
        |args| {
            let value = exact_number_integer(args)?;
            let value = BigUint::try_from(value).map_err(|_error| {
                VmError::native("cannot compute the factorial of a negative number".to_string())
            })?;
            Ok(bigint_number(value.factorial().into()))
        },
    );

    for (name, operation) in [
        (
            "gcd",
            (|left: &BigInt, right: &BigInt| left.gcd(right)) as fn(&BigInt, &BigInt) -> BigInt,
        ),
        ("lcm", |left: &BigInt, right: &BigInt| left.lcm(right)),
    ] {
        declare_typed!(
            env,
            name,
            (left: Int, right: Int) -> Result<Int>,
            "Computes an integer divisor operation with checked i64 output.",
            operation(&BigInt::from(*left), &BigInt::from(*right))
                .to_i64()
                .ok_or_else(|| VmError::native("integer result overflowed".to_string()))
        );
        declare(
            env,
            name,
            vec![StaticType::Number, StaticType::Number],
            StaticType::Number,
            "Computes an arbitrary-precision exact integer divisor operation.",
            move |args| {
                arity(args, 2)?;
                let left = exact_integer(&args[0])?;
                let right = exact_integer(&args[1])?;
                Ok(bigint_number(operation(&left, &right)))
            },
        );
    }
}

fn exact_number_integer(args: &[Value]) -> Result<BigInt, VmError> {
    arity(args, 1)?;
    exact_integer(&args[0])
}

fn exact_integer(value: &Value) -> Result<BigInt, VmError> {
    let Value::Number(value) = value else {
        return Err(VmError::native(
            "expected an exact integer Number".to_string(),
        ));
    };
    match value.as_ref() {
        AdvancedNumber::Int(value) => Ok(value.clone()),
        AdvancedNumber::Rational(value) if value.is_integer() => Ok(value.to_integer()),
        _ => Err(VmError::native(
            "expected an exact integer Number".to_string(),
        )),
    }
}

fn bigint_number(value: BigInt) -> Value {
    Value::from_number(AdvancedNumber::Int(value))
}

fn register_conversions(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    declare(
        env,
        "int",
        vec![StaticType::Any],
        StaticType::Int,
        "Converts a value to a checked i64 Int.",
        |args| {
            arity(args, 1)?;
            convert_to_int(&args[0]).map(Value::Int)
        },
    );
    declare(
        env,
        "float",
        vec![StaticType::Any],
        StaticType::Float,
        "Converts a value to a Float.",
        |args| {
            arity(args, 1)?;
            convert_to_float(&args[0]).map(Value::Float)
        },
    );

    for left_mode in NumericMode::ALL {
        for right_mode in NumericMode::ALL {
            let output = left_mode.promote(right_mode);
            let output = if output == NumericMode::Int {
                NumericMode::Float
            } else {
                output
            };
            declare(
                env,
                "atan2",
                vec![left_mode.static_type(), right_mode.static_type()],
                output.static_type(),
                "Computes the four-quadrant arctangent of y and x.",
                move |args| {
                    arity(args, 2)?;
                    let left = numeric_ref(left_mode, &args[0])?;
                    let right = numeric_ref(right_mode, &args[1])?;
                    if output == NumericMode::Number {
                        let left = left.to_f64().ok_or_else(|| {
                            VmError::native("atan2 requires real Number operands".to_string())
                        })?;
                        let right = right.to_f64().ok_or_else(|| {
                            VmError::native("atan2 requires real Number operands".to_string())
                        })?;
                        Ok(Value::from_number(AdvancedNumber::Float(left.atan2(right))))
                    } else {
                        let left = left
                            .to_primitive_float()
                            .expect("Float atan2 only combines primitive operands");
                        let right = right
                            .to_primitive_float()
                            .expect("Float atan2 only combines primitive operands");
                        Ok(Value::Float(left.atan2(right)))
                    }
                },
            );
        }
    }
}

fn convert_to_int(value: &Value) -> Result<i64, VmError> {
    let static_type = value.static_type();
    if let Some(number) = value.numeric_ref() {
        return number
            .to_i64_truncating()
            .ok_or_else(|| VmError::native(format!("cannot convert {static_type} to Int")));
    }

    let converted = match value {
        Value::Bool(value) => return Ok(if *value { 1 } else { 0 }),
        Value::Object(value) => match value.as_ref() {
            Object::String(value) => return value.borrow().parse::<i64>().map_err(native_error),
            _ => None,
        },
        Value::None => None,
        Value::Int(_) | Value::Float(_) | Value::Number(_) => unreachable!("handled above"),
    };
    converted.ok_or_else(|| VmError::native(format!("cannot convert {static_type} to Int")))
}

fn convert_to_float(value: &Value) -> Result<f64, VmError> {
    if let Some(number) = value.numeric_ref() {
        return number.to_f64().ok_or_else(|| {
            VmError::native("cannot convert a complex Number to Float".to_string())
        });
    }

    match value {
        Value::Bool(value) => Ok(if *value { 1.0 } else { 0.0 }),
        Value::Object(value) => match value.as_ref() {
            Object::String(value) => value.borrow().parse::<f64>().map_err(native_error),
            _ => Err(VmError::native("cannot convert value to Float".to_string())),
        },
        Value::None => Err(VmError::native("cannot convert None to Float".to_string())),
        Value::Int(_) | Value::Float(_) | Value::Number(_) => unreachable!("handled above"),
    }
}

#[derive(Clone, Copy)]
enum Transcendental {
    Acos,
    Acosh,
    Asin,
    Asinh,
    Atan,
    Atanh,
    Cbrt,
    Cos,
    Exp,
    Ln,
    Log2,
    Log10,
    Sin,
    Sqrt,
    Tan,
    Tanh,
}

impl Transcendental {
    const ALL: [Self; 16] = [
        Self::Acos,
        Self::Acosh,
        Self::Asin,
        Self::Asinh,
        Self::Atan,
        Self::Atanh,
        Self::Cbrt,
        Self::Cos,
        Self::Exp,
        Self::Ln,
        Self::Log2,
        Self::Log10,
        Self::Sin,
        Self::Sqrt,
        Self::Tan,
        Self::Tanh,
    ];

    fn name(self) -> &'static str {
        match self {
            Self::Acos => "acos",
            Self::Acosh => "acosh",
            Self::Asin => "asin",
            Self::Asinh => "asinh",
            Self::Atan => "atan",
            Self::Atanh => "atanh",
            Self::Cbrt => "cbrt",
            Self::Cos => "cos",
            Self::Exp => "exp",
            Self::Ln => "ln",
            Self::Log2 => "log2",
            Self::Log10 => "log10",
            Self::Sin => "sin",
            Self::Sqrt => "sqrt",
            Self::Tan => "tan",
            Self::Tanh => "tanh",
        }
    }

    fn description(self) -> &'static str {
        match self {
            Self::Acos => "Computes the inverse cosine in radians.",
            Self::Acosh => "Computes the inverse hyperbolic cosine.",
            Self::Asin => "Computes the inverse sine in radians.",
            Self::Asinh => "Computes the inverse hyperbolic sine.",
            Self::Atan => "Computes the inverse tangent in radians.",
            Self::Atanh => "Computes the inverse hyperbolic tangent.",
            Self::Cbrt => "Returns the cube root of the input.",
            Self::Cos => "Computes the cosine of an angle in radians.",
            Self::Exp => "Raises e to the power of the input.",
            Self::Ln => "Returns the natural logarithm of the input.",
            Self::Log2 => "Returns the base-2 logarithm of the input.",
            Self::Log10 => "Returns the base-10 logarithm of the input.",
            Self::Sin => "Computes the sine of an angle in radians.",
            Self::Sqrt => "Returns the square root of the input.",
            Self::Tan => "Computes the tangent of an angle in radians.",
            Self::Tanh => "Computes the hyperbolic tangent.",
        }
    }

    fn real_domain_description(self) -> &'static str {
        match self {
            Self::Acos | Self::Asin => " Real inputs outside [-1, 1] produce NaN.",
            Self::Acosh => " Real inputs below 1 produce NaN.",
            Self::Atanh => " Real inputs with an absolute value greater than 1 produce NaN.",
            Self::Ln | Self::Log2 | Self::Log10 | Self::Sqrt => {
                " Negative real inputs produce NaN."
            }
            Self::Asinh
            | Self::Atan
            | Self::Cbrt
            | Self::Cos
            | Self::Exp
            | Self::Sin
            | Self::Tan
            | Self::Tanh => "",
        }
    }

    fn documentation(self, mode: NumericMode) -> String {
        let mode_description = match mode {
            NumericMode::Int => " Converts Int input to Float before evaluation and returns Float.",
            NumericMode::Float => " Returns Float for Float input.",
            NumericMode::Number => {
                " Returns Number for real or complex Number input. Values outside the real domain continue into the complex plane."
            }
        };
        let domain = if mode == NumericMode::Number {
            ""
        } else {
            self.real_domain_description()
        };

        format!("{}{domain}{mode_description}", self.description())
    }

    fn apply_float(self, value: f64) -> f64 {
        match self {
            Self::Acos => value.acos(),
            Self::Acosh => value.acosh(),
            Self::Asin => value.asin(),
            Self::Asinh => value.asinh(),
            Self::Atan => value.atan(),
            Self::Atanh => value.atanh(),
            Self::Cbrt => value.cbrt(),
            Self::Cos => value.cos(),
            Self::Exp => value.exp(),
            Self::Ln => value.ln(),
            Self::Log2 => value.log2(),
            Self::Log10 => value.log10(),
            Self::Sin => value.sin(),
            Self::Sqrt => value.sqrt(),
            Self::Tan => value.tan(),
            Self::Tanh => value.tanh(),
        }
    }

    fn apply_complex(self, value: Complex64) -> Complex64 {
        match self {
            Self::Acos => value.acos(),
            Self::Acosh => value.acosh(),
            Self::Asin => value.asin(),
            Self::Asinh => value.asinh(),
            Self::Atan => value.atan(),
            Self::Atanh => value.atanh(),
            Self::Cbrt => value.powf(1.0 / 3.0),
            Self::Cos => value.cos(),
            Self::Exp => value.exp(),
            Self::Ln => value.ln(),
            Self::Log2 => value.ln() / std::f64::consts::LN_2,
            Self::Log10 => value.ln() / std::f64::consts::LN_10,
            Self::Sin => value.sin(),
            Self::Sqrt => value.sqrt(),
            Self::Tan => value.tan(),
            Self::Tanh => value.tanh(),
        }
    }
}

fn register_transcendentals(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    for function in Transcendental::ALL {
        declare_typed!(
            env,
            function.name(),
            (value: Int) -> Float,
            function.documentation(NumericMode::Int),
            function.apply_float(*value as f64)
        );
        declare_typed!(
            env,
            function.name(),
            (value: Float) -> Float,
            function.documentation(NumericMode::Float),
            function.apply_float(*value)
        );
        declare_typed!(
            env,
            function.name(),
            (value: Number) -> Number,
            function.documentation(NumericMode::Number),
            match value.as_ref() {
                AdvancedNumber::Complex(value) => {
                    AdvancedNumber::Complex(function.apply_complex(*value))
                }
                value => {
                    let input = value.to_f64().expect("non-complex Number is real");
                    let result = function.apply_float(input);
                    if result.is_nan() && !input.is_nan() {
                        AdvancedNumber::Complex(function.apply_complex(Complex64::new(input, 0.0)))
                    } else {
                        AdvancedNumber::Float(result)
                    }
                }
            }
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_arithmetic_operator_has_a_three_by_three_numeric_matrix() {
        let mut registry = FunctionRegistry::default();
        register(&mut registry);

        let kinds = [StaticType::Int, StaticType::Float, StaticType::Number];
        for operator in ["+", "-", "*", "/", "\\", "%", "%%", "^"] {
            for left in &kinds {
                for right in &kinds {
                    let expected_return = if matches!(left, StaticType::Number)
                        || matches!(right, StaticType::Number)
                    {
                        StaticType::Number
                    } else if matches!(left, StaticType::Float)
                        || matches!(right, StaticType::Float)
                    {
                        StaticType::Float
                    } else {
                        StaticType::Int
                    };

                    assert!(registry.iter().any(|function| {
                        function.name == operator
                            && function.static_type
                                == StaticType::Function {
                                    parameters: Some(vec![left.clone(), right.clone()]),
                                    return_type: Box::new(expected_return.clone()),
                                }
                    }));
                }
            }
        }
    }

    #[test]
    fn arithmetic_registration_prefers_homogeneous_dynamic_operands() {
        let mut registry = FunctionRegistry::default();
        register(&mut registry);

        for operator in ["+", "-", "*", "/", "\\", "%", "%%", "^"] {
            let dynamic_order = registry
                .iter()
                .filter(|function| function.name == operator)
                .filter_map(|function| match &function.static_type {
                    StaticType::Function {
                        parameters: Some(parameters),
                        ..
                    } if parameters.len() == 2 && parameters.iter().all(StaticType::is_number) => {
                        Some((parameters[0].clone(), parameters[1].clone()))
                    }
                    _ => None,
                })
                .rev()
                .collect::<Vec<_>>();
            let expected = NUMERIC_PAIRS_BY_DYNAMIC_PRIORITY
                .iter()
                .map(|(left, right)| (left.static_type(), right.static_type()))
                .collect::<Vec<_>>();

            assert_eq!(dynamic_order, expected, "unexpected {operator} priority");
        }
    }

    #[test]
    fn transcendental_overloads_have_function_specific_documentation() {
        let mut registry = FunctionRegistry::default();
        register(&mut registry);

        for transcendental in Transcendental::ALL {
            for mode in NumericMode::ALL {
                let expected_type = StaticType::Function {
                    parameters: Some(vec![mode.static_type()]),
                    return_type: Box::new(match mode {
                        NumericMode::Int | NumericMode::Float => StaticType::Float,
                        NumericMode::Number => StaticType::Number,
                    }),
                };
                let function = registry
                    .iter()
                    .find(|function| {
                        function.name == transcendental.name()
                            && function.static_type == expected_type
                    })
                    .unwrap_or_else(|| {
                        panic!(
                            "missing {}({}) transcendental overload",
                            transcendental.name(),
                            mode.static_type()
                        )
                    });

                assert_eq!(
                    function.documentation.as_deref(),
                    Some(transcendental.documentation(mode).as_str())
                );
            }
        }
    }
}
