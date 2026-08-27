use factorial::Factorial;
use ndc_core::int::Int;
use ndc_core::num::{AdvancedNumber, BinaryOperatorError};
use ndc_core::{FunctionRegistry, StaticType};
use ndc_vm::error::VmError;
use ndc_vm::value::{NativeFunc, NativeFunction, Object, Value};
use num::complex::Complex64;
use num::{BigInt, BigUint, FromPrimitive, Integer, ToPrimitive};
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};
use std::rc::Rc;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum NumericKind {
    Int,
    Float,
    Number,
}

impl NumericKind {
    fn static_type(self) -> StaticType {
        match self {
            Self::Int => StaticType::Int,
            Self::Float => StaticType::Float,
            Self::Number => StaticType::Number,
        }
    }
}

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
    documentation: &str,
    func: impl Fn(&[Value]) -> Result<Value, VmError> + 'static,
) {
    env.declare_global_fn(Rc::new(NativeFunction {
        name: name.to_string(),
        documentation: Some(documentation.to_string()),
        static_type: StaticType::Function {
            parameters: Some(parameters),
            return_type: Box::new(return_type),
        },
        func: NativeFunc::Simple(Box::new(func)),
    }));
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

fn result_kind(left: NumericKind, right: NumericKind) -> NumericKind {
    if left == NumericKind::Number || right == NumericKind::Number {
        NumericKind::Number
    } else if left == NumericKind::Float || right == NumericKind::Float {
        NumericKind::Float
    } else {
        NumericKind::Int
    }
}

fn register_binary_arithmetic(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    const KINDS: [NumericKind; 3] = [NumericKind::Int, NumericKind::Float, NumericKind::Number];
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
        for left_kind in KINDS {
            for right_kind in KINDS {
                let output_kind = result_kind(left_kind, right_kind);
                declare(
                    env,
                    operation.name(),
                    vec![left_kind.static_type(), right_kind.static_type()],
                    output_kind.static_type(),
                    operation.documentation(),
                    move |args| {
                        arity(args, 2)?;
                        eval_binary(
                            operation,
                            left_kind,
                            right_kind,
                            output_kind,
                            &args[0],
                            &args[1],
                        )
                    },
                );
            }
        }
    }
}

fn eval_binary(
    operation: BinaryOperation,
    left_kind: NumericKind,
    right_kind: NumericKind,
    output_kind: NumericKind,
    left: &Value,
    right: &Value,
) -> Result<Value, VmError> {
    match output_kind {
        NumericKind::Int => {
            let (Value::Int(left), Value::Int(right)) = (left, right) else {
                return Err(VmError::native("expected two Int operands".to_string()));
            };
            eval_int_binary(operation, *left, *right).map(Value::Int)
        }
        NumericKind::Float => {
            let left = primitive_float(left_kind, left)?;
            let right = primitive_float(right_kind, right)?;
            Ok(Value::Float(eval_float_binary(operation, left, right)))
        }
        NumericKind::Number => {
            let left = promoted_number(left_kind, left)?;
            let right = promoted_number(right_kind, right)?;
            eval_number_binary(operation, left, right)
                .map(Value::from_number)
                .map_err(native_error)
        }
    }
}

fn primitive_float(kind: NumericKind, value: &Value) -> Result<f64, VmError> {
    match (kind, value) {
        (NumericKind::Int, Value::Int(value)) => Ok(*value as f64),
        (NumericKind::Float, Value::Float(value)) => Ok(*value),
        _ => Err(VmError::native(format!(
            "expected {}, got {}",
            kind.static_type(),
            value.static_type()
        ))),
    }
}

fn promoted_number(kind: NumericKind, value: &Value) -> Result<AdvancedNumber, VmError> {
    match (kind, value) {
        (NumericKind::Int, Value::Int(value)) => Ok(AdvancedNumber::Int(Int::Int64(*value))),
        (NumericKind::Float, Value::Float(value)) => Ok(AdvancedNumber::Float(*value)),
        (NumericKind::Number, Value::Number(value)) => Ok(value.as_ref().clone()),
        _ => Err(VmError::native(format!(
            "expected {}, got {}",
            kind.static_type(),
            value.static_type()
        ))),
    }
}

fn eval_int_binary(operation: BinaryOperation, left: i64, right: i64) -> Result<i64, VmError> {
    if right == 0
        && matches!(
            operation,
            BinaryOperation::Div
                | BinaryOperation::FloorDiv
                | BinaryOperation::Rem
                | BinaryOperation::RemEuclid
        )
    {
        return Err(VmError::native("division by zero".to_string()));
    }
    if right < 0 && matches!(operation, BinaryOperation::Pow) {
        return Err(VmError::native(
            "negative integer exponents require Number operands".to_string(),
        ));
    }
    let failed = || {
        VmError::native(format!(
            "integer operation overflowed or is undefined: {left} {} {right}",
            operation.name()
        ))
    };
    match operation {
        BinaryOperation::Add => left.checked_add(right).ok_or_else(failed),
        BinaryOperation::Sub => left.checked_sub(right).ok_or_else(failed),
        BinaryOperation::Mul => left.checked_mul(right).ok_or_else(failed),
        BinaryOperation::Div => left.checked_div(right).ok_or_else(failed),
        BinaryOperation::FloorDiv => checked_floor_div(left, right).ok_or_else(failed),
        BinaryOperation::Rem => left.checked_rem(right).ok_or_else(failed),
        BinaryOperation::RemEuclid => left.checked_rem_euclid(right).ok_or_else(failed),
        BinaryOperation::Pow => u32::try_from(right)
            .ok()
            .and_then(|right| left.checked_pow(right))
            .ok_or_else(failed),
    }
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
    declare(
        env,
        "-",
        vec![StaticType::Int],
        StaticType::Int,
        "Negates an integer.",
        |args| {
            let [Value::Int(value)] = args else {
                return Err(VmError::native("expected one Int argument".to_string()));
            };
            value
                .checked_neg()
                .map(Value::Int)
                .ok_or_else(|| VmError::native("integer negation overflowed".to_string()))
        },
    );
    declare(
        env,
        "-",
        vec![StaticType::Float],
        StaticType::Float,
        "Negates a floating-point number.",
        |args| {
            let [Value::Float(value)] = args else {
                return Err(VmError::native("expected one Float argument".to_string()));
            };
            Ok(Value::Float(-value))
        },
    );
    declare(
        env,
        "-",
        vec![StaticType::Number],
        StaticType::Number,
        "Negates an advanced number.",
        |args| {
            let [Value::Number(value)] = args else {
                return Err(VmError::native("expected one Number argument".to_string()));
            };
            Ok(Value::from_number(value.as_ref().clone().neg()))
        },
    );
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
            move |args| {
                arity(args, 2)?;
                let ordering = args[0].partial_cmp(&args[1]).ok_or_else(|| {
                    VmError::native(format!(
                        "cannot compare {} and {}",
                        args[0].static_type(),
                        args[1].static_type()
                    ))
                })?;
                Ok(Value::Bool(predicate(ordering)))
            },
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
                arity(args, 2)?;
                let ordering = args[0].partial_cmp(&args[1]).ok_or_else(|| {
                    VmError::native(format!(
                        "cannot compare {} and {}",
                        args[0].static_type(),
                        args[1].static_type()
                    ))
                })?;
                let result = match ordering {
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
        declare(
            env,
            name,
            vec![StaticType::Int, StaticType::Int],
            StaticType::Int,
            docs,
            move |args| {
                let [Value::Int(left), Value::Int(right)] = args else {
                    return Err(VmError::native("expected two Int arguments".to_string()));
                };
                Ok(Value::Int(operation(*left, *right)))
            },
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
        declare(
            env,
            name,
            vec![StaticType::Bool, StaticType::Bool],
            StaticType::Bool,
            docs,
            move |args| {
                let [Value::Bool(left), Value::Bool(right)] = args else {
                    return Err(VmError::native("expected two Bool arguments".to_string()));
                };
                Ok(Value::Bool(operation(*left, *right)))
            },
        );
    }

    declare(
        env,
        "~",
        vec![StaticType::Int],
        StaticType::Int,
        "Computes bitwise NOT of an integer.",
        |args| {
            let [Value::Int(value)] = args else {
                return Err(VmError::native("expected one Int argument".to_string()));
            };
            Ok(Value::Int(value.not()))
        },
    );

    for name in ["!", "not"] {
        declare(
            env,
            name,
            vec![StaticType::Bool],
            StaticType::Bool,
            "Computes logical negation.",
            |args| {
                let [Value::Bool(value)] = args else {
                    return Err(VmError::native("expected one Bool argument".to_string()));
                };
                Ok(Value::Bool(!value))
            },
        );
    }

    for (name, left_shift) in [("<<", true), (">>", false)] {
        declare(
            env,
            name,
            vec![StaticType::Int, StaticType::Int],
            StaticType::Int,
            "Shifts an integer by a checked non-negative amount.",
            move |args| {
                let [Value::Int(left), Value::Int(right)] = args else {
                    return Err(VmError::native("expected two Int arguments".to_string()));
                };
                let right = u32::try_from(*right)
                    .map_err(|_error| VmError::native("invalid shift amount".to_string()))?;
                let result = if left_shift {
                    left.checked_shl(right)
                } else {
                    left.checked_shr(right)
                };
                result
                    .map(Value::Int)
                    .ok_or_else(|| VmError::native("invalid shift amount".to_string()))
            },
        );
    }
}

fn register_constructors(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    for kind in [NumericKind::Int, NumericKind::Float, NumericKind::Number] {
        declare(
            env,
            "Number",
            vec![kind.static_type()],
            StaticType::Number,
            "Wraps a primitive numeric value as a Number.",
            move |args| {
                arity(args, 1)?;
                promoted_number(kind, &args[0]).map(Value::from_number)
            },
        );
    }
}

fn register_aggregates(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    for kind in [NumericKind::Int, NumericKind::Float, NumericKind::Number] {
        for (name, product) in [("sum", false), ("product", true)] {
            declare(
                env,
                name,
                vec![StaticType::Sequence(Box::new(kind.static_type()))],
                kind.static_type(),
                if product {
                    "Returns the product of a numeric sequence."
                } else {
                    "Returns the sum of a numeric sequence."
                },
                move |args| aggregate(args, kind, product),
            );
        }
    }
}

fn aggregate(args: &[Value], kind: NumericKind, product: bool) -> Result<Value, VmError> {
    arity(args, 1)?;
    let mut values = args[0]
        .clone()
        .try_into_iter()
        .ok_or_else(|| VmError::native("expected a sequence".to_string()))?;
    match kind {
        NumericKind::Int => {
            let initial: i64 = if product { 1 } else { 0 };
            let value = values.try_fold(initial, |accumulator, value| {
                let Value::Int(value) = value else {
                    return Err(VmError::native("expected a sequence of Int".to_string()));
                };
                if product {
                    accumulator.checked_mul(value)
                } else {
                    accumulator.checked_add(value)
                }
                .ok_or_else(|| VmError::native("integer aggregate overflowed".to_string()))
            })?;
            Ok(Value::Int(value))
        }
        NumericKind::Float => {
            let initial = if product { 1.0 } else { 0.0 };
            let value = values.try_fold(initial, |accumulator, value| {
                let Value::Float(value) = value else {
                    return Err(VmError::native("expected a sequence of Float".to_string()));
                };
                Ok::<_, VmError>(if product {
                    accumulator * value
                } else {
                    accumulator + value
                })
            })?;
            Ok(Value::Float(value))
        }
        NumericKind::Number => {
            let initial = AdvancedNumber::Int(Int::Int64(if product { 1 } else { 0 }));
            let value = values.try_fold(initial, |accumulator, value| {
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
    for kind in [NumericKind::Int, NumericKind::Float, NumericKind::Number] {
        declare(
            env,
            "signum",
            vec![kind.static_type()],
            kind.static_type(),
            "Returns the sign of a number.",
            move |args| unary_preserving(args, kind, PreservingUnary::Signum),
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
                vec![kind.static_type()],
                kind.static_type(),
                "Applies a numeric operation while preserving the numeric mode.",
                move |args| unary_preserving(args, kind, operation),
            );
        }
    }

    for left in [NumericKind::Int, NumericKind::Float, NumericKind::Number] {
        for right in [NumericKind::Int, NumericKind::Float, NumericKind::Number] {
            let output = result_kind(left, right);
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
        declare(
            env,
            name,
            vec![StaticType::Number],
            StaticType::Number,
            "Returns a component of an advanced number.",
            move |args| {
                let [Value::Number(value)] = args else {
                    return Err(VmError::native("expected one Number argument".to_string()));
                };
                let component = match (value.as_ref(), imaginary) {
                    (AdvancedNumber::Complex(value), false) => AdvancedNumber::Float(value.re),
                    (AdvancedNumber::Complex(value), true) => AdvancedNumber::Float(value.im),
                    (_, false) => value.as_ref().clone(),
                    (_, true) => AdvancedNumber::Int(Int::Int64(0)),
                };
                Ok(Value::from_number(component))
            },
        );
    }

    for (name, numerator) in [("numerator", true), ("denominator", false)] {
        declare(
            env,
            name,
            vec![StaticType::Number],
            StaticType::Number,
            "Returns a component of an exact Number fraction.",
            move |args| {
                let [Value::Number(value)] = args else {
                    return Err(VmError::native("expected one Number argument".to_string()));
                };
                let value = match value.as_ref() {
                    AdvancedNumber::Int(value) if numerator => AdvancedNumber::Int(value.clone()),
                    AdvancedNumber::Int(_) => AdvancedNumber::Int(Int::Int64(1)),
                    AdvancedNumber::Rational(value) if numerator => {
                        AdvancedNumber::Int(Int::BigInt(value.numer().clone()).simplified())
                    }
                    AdvancedNumber::Rational(value) => {
                        AdvancedNumber::Int(Int::BigInt(value.denom().clone()).simplified())
                    }
                    _ => {
                        return Err(VmError::native(
                            "expected an exact integer or rational Number".to_string(),
                        ));
                    }
                };
                Ok(Value::from_number(value))
            },
        );
    }
}

fn unary_preserving(
    args: &[Value],
    kind: NumericKind,
    operation: PreservingUnary,
) -> Result<Value, VmError> {
    arity(args, 1)?;
    match (kind, &args[0]) {
        (NumericKind::Int, Value::Int(value)) => {
            let result = match operation {
                PreservingUnary::Signum => value.signum(),
                PreservingUnary::Ceil | PreservingUnary::Floor | PreservingUnary::Round => *value,
                PreservingUnary::Abs => value.checked_abs().ok_or_else(|| {
                    VmError::native("integer absolute value overflowed".to_string())
                })?,
            };
            Ok(Value::Int(result))
        }
        (NumericKind::Float, Value::Float(value)) => {
            let result = match operation {
                PreservingUnary::Signum => value.signum(),
                PreservingUnary::Ceil => value.ceil(),
                PreservingUnary::Floor => value.floor(),
                PreservingUnary::Round => value.round(),
                PreservingUnary::Abs => value.abs(),
            };
            Ok(Value::Float(result))
        }
        (NumericKind::Number, Value::Number(value)) => {
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
            kind.static_type(),
            args[0].static_type()
        ))),
    }
}

fn register_integer_helpers(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    declare(
        env,
        "factorial",
        vec![StaticType::Int],
        StaticType::Int,
        "Returns the checked factorial of a non-negative Int.",
        |args| {
            let [Value::Int(value)] = args else {
                return Err(VmError::native("expected one Int argument".to_string()));
            };
            if *value < 0 {
                return Err(VmError::native(
                    "cannot compute the factorial of a negative number".to_string(),
                ));
            }
            let result = (1..=*value)
                .try_fold(1i64, i64::checked_mul)
                .ok_or_else(|| {
                    VmError::native(
                        "integer factorial overflowed; use a Number argument".to_string(),
                    )
                })?;
            Ok(Value::Int(result))
        },
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
        declare(
            env,
            name,
            vec![StaticType::Int, StaticType::Int],
            StaticType::Int,
            "Computes an integer divisor operation with checked i64 output.",
            move |args| {
                let [Value::Int(left), Value::Int(right)] = args else {
                    return Err(VmError::native("expected two Int arguments".to_string()));
                };
                operation(&BigInt::from(*left), &BigInt::from(*right))
                    .to_i64()
                    .map(Value::Int)
                    .ok_or_else(|| VmError::native("integer result overflowed".to_string()))
            },
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
        AdvancedNumber::Int(value) => Ok(value.to_bigint()),
        AdvancedNumber::Rational(value) if value.is_integer() => Ok(value.to_integer()),
        _ => Err(VmError::native(
            "expected an exact integer Number".to_string(),
        )),
    }
}

fn bigint_number(value: BigInt) -> Value {
    Value::from_number(AdvancedNumber::Int(Int::BigInt(value).simplified()))
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

    for left_kind in [NumericKind::Int, NumericKind::Float, NumericKind::Number] {
        for right_kind in [NumericKind::Int, NumericKind::Float, NumericKind::Number] {
            let output = result_kind(left_kind, right_kind);
            let output = if output == NumericKind::Int {
                NumericKind::Float
            } else {
                output
            };
            declare(
                env,
                "atan2",
                vec![left_kind.static_type(), right_kind.static_type()],
                output.static_type(),
                "Computes the four-quadrant arctangent of y and x.",
                move |args| {
                    arity(args, 2)?;
                    if output == NumericKind::Number {
                        let left = promoted_number(left_kind, &args[0])?;
                        let right = promoted_number(right_kind, &args[1])?;
                        let left = left.to_f64().ok_or_else(|| {
                            VmError::native("atan2 requires real Number operands".to_string())
                        })?;
                        let right = right.to_f64().ok_or_else(|| {
                            VmError::native("atan2 requires real Number operands".to_string())
                        })?;
                        Ok(Value::from_number(AdvancedNumber::Float(left.atan2(right))))
                    } else {
                        let left = primitive_float(left_kind, &args[0])?;
                        let right = primitive_float(right_kind, &args[1])?;
                        Ok(Value::Float(left.atan2(right)))
                    }
                },
            );
        }
    }
}

fn convert_to_int(value: &Value) -> Result<i64, VmError> {
    let static_type = value.static_type();
    let converted = match value {
        Value::Int(value) => return Ok(*value),
        Value::Float(value) => float_to_i64(*value),
        Value::Number(value) => match value.as_ref() {
            AdvancedNumber::Int(value) => value.to_bigint().to_i64(),
            AdvancedNumber::Float(value) => float_to_i64(*value),
            AdvancedNumber::Rational(value) => value.to_integer().to_i64(),
            AdvancedNumber::Complex(_) => None,
        },
        Value::Bool(value) => return Ok(if *value { 1 } else { 0 }),
        Value::Object(value) => match value.as_ref() {
            Object::String(value) => return value.borrow().parse::<i64>().map_err(native_error),
            _ => None,
        },
        Value::None => None,
    };
    converted.ok_or_else(|| VmError::native(format!("cannot convert {static_type} to Int")))
}

fn float_to_i64(value: f64) -> Option<i64> {
    if value.is_finite() {
        BigInt::from_f64(value.trunc())?.to_i64()
    } else {
        None
    }
}

fn convert_to_float(value: &Value) -> Result<f64, VmError> {
    match value {
        Value::Int(value) => Ok(*value as f64),
        Value::Float(value) => Ok(*value),
        Value::Number(value) => value
            .to_f64()
            .ok_or_else(|| VmError::native("cannot convert a complex Number to Float".to_string())),
        Value::Bool(value) => Ok(if *value { 1.0 } else { 0.0 }),
        Value::Object(value) => match value.as_ref() {
            Object::String(value) => value.borrow().parse::<f64>().map_err(native_error),
            _ => Err(VmError::native("cannot convert value to Float".to_string())),
        },
        Value::None => Err(VmError::native("cannot convert None to Float".to_string())),
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
    const FUNCTIONS: [Transcendental; 16] = [
        Transcendental::Acos,
        Transcendental::Acosh,
        Transcendental::Asin,
        Transcendental::Asinh,
        Transcendental::Atan,
        Transcendental::Atanh,
        Transcendental::Cbrt,
        Transcendental::Cos,
        Transcendental::Exp,
        Transcendental::Ln,
        Transcendental::Log2,
        Transcendental::Log10,
        Transcendental::Sin,
        Transcendental::Sqrt,
        Transcendental::Tan,
        Transcendental::Tanh,
    ];

    for function in FUNCTIONS {
        declare(
            env,
            function.name(),
            vec![StaticType::Int],
            StaticType::Float,
            "Applies a transcendental function and returns a Float.",
            move |args| {
                let [Value::Int(value)] = args else {
                    return Err(VmError::native("expected one Int argument".to_string()));
                };
                Ok(Value::Float(function.apply_float(*value as f64)))
            },
        );
        declare(
            env,
            function.name(),
            vec![StaticType::Float],
            StaticType::Float,
            "Applies a transcendental function to a Float.",
            move |args| {
                let [Value::Float(value)] = args else {
                    return Err(VmError::native("expected one Float argument".to_string()));
                };
                Ok(Value::Float(function.apply_float(*value)))
            },
        );
        declare(
            env,
            function.name(),
            vec![StaticType::Number],
            StaticType::Number,
            "Applies a transcendental function with complex continuation.",
            move |args| {
                let [Value::Number(value)] = args else {
                    return Err(VmError::native("expected one Number argument".to_string()));
                };
                let result = match value.as_ref() {
                    AdvancedNumber::Complex(value) => {
                        AdvancedNumber::Complex(function.apply_complex(*value))
                    }
                    value => {
                        let input = value.to_f64().expect("non-complex Number is real");
                        let result = function.apply_float(input);
                        if result.is_nan() && !input.is_nan() {
                            AdvancedNumber::Complex(
                                function.apply_complex(Complex64::new(input, 0.0)),
                            )
                        } else {
                            AdvancedNumber::Float(result)
                        }
                    }
                };
                Ok(Value::from_number(result))
            },
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
}
