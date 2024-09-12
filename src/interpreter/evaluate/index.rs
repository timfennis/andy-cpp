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
use std::{cell::Ref, fmt};

use super::{evaluate_expression, EvaluationError, IntoEvaluationResult};

pub enum Index {
    Element(usize),
    Range(usize, usize),
}

impl Index {
    pub fn into_tuple(&self) -> (usize, usize) {
        match self {
            Index::Element(idx) => (*idx, *idx + 1),
            Index::Range(from, to) => (*from, *to),
        }
    }
}

pub(crate) fn expression_to_forward_index(
    expression_location: &ExpressionLocation,
    environment: &mut EnvironmentRef,
    size: usize,
    start: Location,
    end: Location,
) -> Result<Index, FunctionCarrier> {
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
            let index = value_to_forward_index(result, size, start, end)?;
            return Ok(index);
        }
    };

    let start_index = if let Some(range_start) = range_start {
        value_to_forward_index_usize(
            evaluate_expression(range_start, environment)?,
            size,
            start,
            end,
        )
    } else {
        Ok(0)
    }?;

    let end_index = if let Some(range_end) = range_end {
        value_to_forward_index_usize(
            evaluate_expression(range_end, environment)?,
            size,
            start,
            end,
        )
    } else {
        Ok(size)
    }?;

    Ok(Index::Range(
        start_index,
        end_index + if inclusive { 1 } else { 0 },
    ))
}

/// Takes a value from the Andy C runtime and converts it to a forward index respecting bi-directional
/// indexing rules.
pub(crate) fn value_to_forward_index(
    value: Value,
    size: usize,
    start: Location,
    end: Location,
) -> Result<Index, EvaluationError> {
    value_to_forward_index_usize(value, size, start, end).map(|idx| Index::Element(idx))
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

pub fn set_at_index(
    lhs: &mut Value,
    rhs: Value,
    index: Index,
    start: Location,
    end: Location,
) -> Result<(), FunctionCarrier> {
    match lhs {
        Value::Sequence(Sequence::List(list)) => {
            let mut list = list.try_borrow_mut().map_err(|_| {
                EvaluationError::mutation_error(
                    "you cannot mutate a value in a list while you're iterating over this list",
                    start,
                    end,
                )
            })?;

            match index {
                Index::Element(index_usize) => {
                    let x = list.index_mut(index_usize);
                    *x = rhs;
                }
                Index::Range(from_usize, to_usize) => {
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
                match index {
                    Index::Element(index) => {
                        insertion_target.replace_range(index..=index, target_string.as_str());
                    }
                    Index::Range(from, to) => {
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

            map.insert(todo!("add a key variant to index?"), rhs);
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

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Index::Element(idx) => write!(f, "{}", idx),
            Index::Range(from, to) => write!(f, "{}..{}", from, to),
        }
    }
}
