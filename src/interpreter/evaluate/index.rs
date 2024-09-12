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
    lexer::Location,
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
    pub fn try_into_offset(
        self,
        size: usize,
        start: Location,
        end: Location,
    ) -> Result<Offset, EvaluationError> {
        Ok(match self {
            EvaluatedIndex::Index(idx) => {
                Offset::Element(value_to_forward_index_usize(idx, size, start, end)?)
            }
            EvaluatedIndex::Slice {
                from,
                to,
                inclusive,
            } => {
                let from_idx = if let Some(from) = from {
                    value_to_forward_index_usize(from, size, start, end)?
                } else {
                    0
                };

                let to_idx = if let Some(to) = to {
                    value_to_forward_index_usize(to, size, start, end)?
                } else {
                    size
                };

                Offset::Range(from_idx, to_idx + if inclusive { 1 } else { 0 })
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
    start: Location,
    end: Location,
) -> Result<usize, EvaluationError> {
    let typ = value.value_type();
    let index = i64::try_from(value).map_err(|_| {
        EvaluationError::type_error(
            &format!("cannot use {typ} to index into a sequence, possibly because it's too big"),
            start,
            end,
        )
    })?;

    if index.is_negative() {
        let index = usize::try_from(index.abs())
            .map_err(|_err| EvaluationError::syntax_error("invalid index too large", start, end))?;

        size.checked_sub(index)
            .ok_or_else(|| EvaluationError::syntax_error("index out of bounds", start, end))
    } else {
        usize::try_from(index).map_err(|_| EvaluationError::syntax_error("kapot", start, end))
    }
}

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
    start: Location,
    end: Location,
) -> Result<(), FunctionCarrier> {
    let Some(size) = lhs.sequence_length() else {
        return Err(EvaluationError::type_error(
            "cannot index into this type because it doesn't have a length",
            start,
            end,
        )
        .into());
    };

    match lhs {
        Value::Sequence(Sequence::List(list)) => {
            let mut list = list.try_borrow_mut().map_err(|_| {
                EvaluationError::mutation_error(
                    "you cannot mutate a value in a list while you're iterating over this list",
                    start,
                    end,
                )
            })?;

            let index = index.try_into_offset(size, start, end)?;

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

                let index = index.try_into_offset(size, start, end)?;

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
                    start,
                    end,
                )
                .into());
            }
        }
        Value::Sequence(Sequence::Map(map, _)) => {
            let mut map = map.try_borrow_mut().into_evaluation_result(start, end)?;

            let key = match index {
                EvaluatedIndex::Index(idx) => idx,
                EvaluatedIndex::Slice {
                    from: _,
                    to: _,
                    inclusive: _,
                } => {
                    todo!("TODO: implement inserting ranges as dict keys")
                }
            };
            map.insert(key, rhs);
        }
        _ => {
            return Err(EvaluationError::syntax_error(
                &format!("cannot insert into {} at index", lhs.value_type()),
                start,
                end,
            )
            .into());
        }
    };
    Ok(())
}
