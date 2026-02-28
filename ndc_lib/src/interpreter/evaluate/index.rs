//! # Indexing
//!
//! Visual explanation of how indexing works
//!
//! +----------------+-----+----+----+----+----+----+----+----+----+----+
//! | List values    |   0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 |
//! +----------------+-----+----+----+----+----+----+----+----+----+----+
//! | Forward index  |   0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 |
//! | Backward index | -10 | -9 | -8 | -7 | -6 | -5 | -4 | -3 | -2 | -1 |
//! +----------------+-----+----+----+----+----+----+----+----+----+----+

use super::{EvaluationError, EvaluationResult, IntoEvaluationResult, evaluate_expression};
use crate::interpreter::environment::Environment;
use crate::{
    ast::{Expression, ExpressionLocation},
    interpreter::{function::FunctionCarrier, sequence::Sequence, value::Value},
};
use itertools::Itertools;
use ndc_lexer::Span;
use std::cell::RefCell;
use std::cmp::min;
use std::ops::IndexMut;
use std::rc::Rc;

#[derive(Clone)]
pub enum EvaluatedIndex {
    Index(Value),
    Slice {
        from: Option<Value>,
        to: Option<Value>,
        inclusive: bool,
    },
}

impl EvaluatedIndex {
    // TODO: improve error contract so we don't need EvaluationError (maybe??)
    pub fn try_into_offset(self, size: usize, span: Span) -> Result<Offset, EvaluationError> {
        Ok(match self {
            Self::Index(idx) => {
                Offset::Element(value_to_bounded_forward_index(idx, size, false, span)?)
            }
            Self::Slice {
                from,
                to,
                inclusive,
            } => {
                let from_idx = if let Some(from) = from {
                    value_to_bounded_forward_index(from, size, true, span)?
                } else {
                    0
                };

                let to_idx = if let Some(to) = to {
                    value_to_bounded_forward_index(to, size, true, span)?
                } else {
                    size
                };

                Offset::Range(from_idx, to_idx + usize::from(inclusive))
            }
        })
    }
}

pub(crate) fn evaluate_as_index(
    expression_location: &ExpressionLocation,
    environment: &Rc<RefCell<Environment>>,
) -> Result<EvaluatedIndex, FunctionCarrier> {
    let (range_start, range_end, inclusive) = match expression_location.expression {
        Expression::RangeExclusive {
            start: ref range_start,
            end: ref range_end,
        } => (range_start, range_end, false),
        Expression::RangeInclusive {
            start: ref range_start,
            end: ref range_end,
        } => (range_start, range_end, true),
        _ => {
            let result = evaluate_expression(expression_location, environment)?;
            return Ok(EvaluatedIndex::Index(result));
        }
    };

    if inclusive && range_end.is_none() {
        return Err(EvaluationError::new(
            "inclusive ranges must have an end".to_string(),
            expression_location.span,
        )
        .into());
    }

    let start = if let Some(range_start) = range_start {
        Some(evaluate_expression(range_start, environment)?)
    } else {
        None
    };

    let end = if let Some(range_end) = range_end {
        Some(evaluate_expression(range_end, environment)?)
    } else {
        None
    };

    Ok(EvaluatedIndex::Slice {
        from: start,
        to: end,
        inclusive,
    })
}

fn invalid_index_err<T>(span: Span) -> impl Fn(T) -> EvaluationError {
    move |_: T| {
        EvaluationError::with_help(
            "Invalid list index".to_string(),
            span,
            "The value used as a list index is not valid. List indices must be convertible to a signed 64-bit integer. Ensure the index is a valid integer within the range of -2^63 to 2^63-1".to_string(),
        )
    }
}

/// This function converts a native Andy C++ `Value` (hopefully a number) into a valid usize index
/// into a vector of size `size`. The `allow_oob` argument allows the argument to be out of bounds
/// which is needed when evaluating range expressions.
fn value_to_bounded_forward_index(
    value: Value,
    size: usize,
    for_slice: bool,
    span: Span,
) -> Result<usize, EvaluationError> {
    let index = i64::try_from(value).map_err(invalid_index_err(span))?;

    if index.is_negative() {
        let index = usize::try_from(index.abs())
            .map_err(|_err| EvaluationError::new("invalid index: too large".to_string(), span))?;

        if for_slice {
            Ok(size.saturating_sub(index))
        } else {
            size.checked_sub(index)
                .ok_or_else(|| EvaluationError::new("index out of bounds".to_string(), span))
        }
    } else {
        let index = usize::try_from(index).map_err(invalid_index_err(span))?;
        if for_slice {
            return Ok(min(index, size));
        }

        if index >= size {
            return Err(EvaluationError::new(
                "index out of bounds".to_string(),
                span,
            ));
        }
        Ok(index)
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Offset {
    Element(usize),
    Range(usize, usize),
}

impl Offset {
    pub fn into_tuple(self) -> (usize, usize) {
        match self {
            Self::Element(idx) => (idx, idx + 1),
            Self::Range(from, to) => (from, to),
        }
    }
}

pub fn get_at_index(
    lhs: &Value,
    index: EvaluatedIndex,
    span: Span,
    environment: &Rc<RefCell<Environment>>,
) -> Result<Value, FunctionCarrier> {
    let Some(size) = lhs.sequence_length() else {
        return Err(EvaluationError::new(
            "cannot index into this type because it doesn't have a length".to_string(),
            span,
        )
        .into());
    };

    match lhs {
        Value::Sequence(Sequence::List(list)) => {
            let list = list.borrow();
            let index = index.try_into_offset(size, span)?;

            match index {
                Offset::Element(index_usize) => Ok(list[index_usize].clone()),
                Offset::Range(from_usize, to_usize) => Ok(Value::list(&list[from_usize..to_usize])),
            }
        }
        Value::Sequence(Sequence::String(insertion_target)) => {
            let index = index.try_into_offset(size, span)?;
            Ok(Value::string(match index {
                Offset::Element(e) => insertion_target
                    .borrow()
                    .chars()
                    .nth(e)
                    .map(String::from)
                    .expect("Safe because bounds were already checked"),
                Offset::Range(s, e) => insertion_target
                    .borrow()
                    .chars()
                    .dropping(s)
                    .take(e)
                    .collect::<String>(),
            }))
        }
        Value::Sequence(Sequence::Map(map, default)) => {
            let key = match index {
                EvaluatedIndex::Index(idx) => idx,
                EvaluatedIndex::Slice { .. } => {
                    return Err(EvaluationError::syntax_error(
                        "cannot use range expression as index in map".to_string(),
                        span,
                    )
                    .into());
                }
            };

            let value = map
                .try_borrow()
                .into_evaluation_result(span)?
                .get(&key)
                .cloned();

            if let Some(value) = value {
                Ok(value)
            } else if let Some(default) = default {
                let default_value = produce_default_value(default, environment, span)?;
                map.try_borrow_mut()
                    .into_evaluation_result(span)?
                    .insert(key, default_value.clone());
                Ok(default_value)
            } else {
                Err(EvaluationError::key_not_found(&key, span).into())
            }
        }
        _ => Err(EvaluationError::syntax_error(
            format!("cannot insert into {} at index", lhs.static_type()),
            span,
        )
        .into()),
    }
}

pub(super) fn produce_default_value(
    default: &Value,
    environment: &Rc<RefCell<Environment>>,
    span: Span,
) -> EvaluationResult {
    match default {
        Value::Function(function) => match function.call_checked(&mut [], environment) {
            Err(FunctionCarrier::FunctionTypeMismatch) => {
                Err(FunctionCarrier::EvaluationError(EvaluationError::new(
                    "default function is not callable without arguments".to_string(),
                    span,
                )))
            }
            a => a,
        },
        value => Ok(value.clone()),
    }
}
pub fn set_at_index(
    lhs: &mut Value,
    rhs: Value,
    index: EvaluatedIndex,
    span: Span,
) -> Result<(), FunctionCarrier> {
    let Some(size) = lhs.sequence_length() else {
        return Err(EvaluationError::new(
            "cannot index into this type because it doesn't have a length".to_string(),
            span,
        )
        .into());
    };

    match lhs {
        Value::Sequence(Sequence::List(list)) => {
            let mut list = list.try_borrow_mut().map_err(|_err| {
                EvaluationError::mutation_error(
                    "you cannot mutate a value in a list while you're iterating over this list",
                    span,
                )
            })?;

            let index = index.try_into_offset(size, span)?;

            match index {
                Offset::Element(index_usize) => {
                    let x = list.index_mut(index_usize);
                    *x = rhs;
                }
                Offset::Range(from_usize, to_usize) => {
                    let tail = list.drain(from_usize..).collect::<Vec<_>>();

                    // TODO: why is this unwrap safe
                    list.extend(rhs.try_into_iter().unwrap());

                    list.extend_from_slice(&tail[(to_usize - from_usize)..]);
                }
            }
        }
        Value::Sequence(Sequence::String(insertion_target)) => {
            if let Value::Sequence(Sequence::String(target_string)) = rhs {
                let target_string = target_string.borrow();

                let mut insertion_target = insertion_target.borrow_mut();

                let index = index.try_into_offset(size, span)?;

                match index {
                    Offset::Element(index) => {
                        insertion_target.replace_range(index..=index, target_string.as_str());
                    }
                    Offset::Range(from, to) => {
                        insertion_target.replace_range(from..to, target_string.as_str());
                    }
                }
            } else {
                return Err(EvaluationError::syntax_error(
                    format!("cannot insert {} into a string", rhs.static_type()),
                    span,
                )
                .into());
            }
        }
        Value::Sequence(Sequence::Map(map, _)) => {
            let mut map = map.try_borrow_mut().into_evaluation_result(span)?;

            let key = match index {
                EvaluatedIndex::Index(idx) => idx,
                EvaluatedIndex::Slice { .. } => {
                    return Err(EvaluationError::syntax_error(
                        "cannot use range expression as index".to_string(),
                        span,
                    )
                    .into());
                }
            };
            map.insert(key, rhs);
        }
        _ => {
            return Err(EvaluationError::syntax_error(
                format!("cannot insert into {} at index", lhs.static_type()),
                span,
            )
            .into());
        }
    };
    Ok(())
}
