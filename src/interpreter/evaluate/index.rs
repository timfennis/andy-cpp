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

use crate::{
    ast::{Expression, ExpressionLocation},
    interpreter::{
        environment::EnvironmentRef, function::FunctionCarrier, sequence::Sequence, value::Value,
    },
    lexer::Span,
};
use std::ops::IndexMut;

use super::{evaluate_expression, EvaluationError, IntoEvaluationResult};

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
            EvaluatedIndex::Index(idx) => {
                Offset::Element(value_to_forward_index_usize(idx, size, span)?)
            }
            EvaluatedIndex::Slice {
                from,
                to,
                inclusive,
            } => {
                let from_idx = if let Some(from) = from {
                    value_to_forward_index_usize(from, size, span)?
                } else {
                    0
                };

                let to_idx = if let Some(to) = to {
                    value_to_forward_index_usize(to, size, span)?
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
    environment: &mut EnvironmentRef,
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

fn value_to_forward_index_usize(
    value: Value,
    size: usize,
    // Add one to the final index found (useful when dealing with inclusive ranges ..=)
    span: Span,
) -> Result<usize, EvaluationError> {
    let index = i64::try_from(value).map_err(|_err| {
        EvaluationError::with_help(
            "Invalid list index".to_string(),
            span,
            "The value used as a list index is not valid. List indices must be convertible to a signed 64-bit integer. Ensure the index is a valid integer within the range of -2^63 to 2^63-1".to_string(),
        )
    })?;

    if index.is_negative() {
        let index = usize::try_from(index.abs())
            .map_err(|_err| EvaluationError::syntax_error("invalid index too large", span))?;

        size.checked_sub(index)
            .ok_or_else(|| EvaluationError::syntax_error("index out of bounds", span))
    } else {
        usize::try_from(index).map_err(|_| EvaluationError::syntax_error("kapot", span))
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
            Offset::Element(idx) => (idx, idx + 1),
            Offset::Range(from, to) => (from, to),
        }
    }
}

pub fn set_at_index(
    lhs: &mut Value,
    rhs: Value,
    index: EvaluatedIndex,
    span: Span,
) -> Result<(), FunctionCarrier> {
    let Some(size) = lhs.sequence_length() else {
        return Err(EvaluationError::type_error(
            "cannot index into this type because it doesn't have a length",
            span,
        )
        .into());
    };

    match lhs {
        Value::Sequence(Sequence::List(list)) => {
            let mut list = list.try_borrow_mut().map_err(|_| {
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
                    &format!("cannot insert {} into a string", rhs.value_type()),
                    span,
                )
                .into());
            }
        }
        Value::Sequence(Sequence::Map(map, _)) => {
            let mut map = map.try_borrow_mut().into_evaluation_result(span)?;

            let key = match index {
                EvaluatedIndex::Index(idx) => idx,
                EvaluatedIndex::Slice {
                    from: _,
                    to: _,
                    inclusive: _,
                } => {
                    // PROBLEM: when we evaluate the indexes the following expression become similar, but should they also map to the same dictionary keys?
                    // 1..5 == 1..=4 ?
                    todo!("TODO: implement inserting ranges as dict keys")
                }
            };
            map.insert(key, rhs);
        }
        _ => {
            return Err(EvaluationError::syntax_error(
                &format!("cannot insert into {} at index", lhs.value_type()),
                span,
            )
            .into());
        }
    };
    Ok(())
}
