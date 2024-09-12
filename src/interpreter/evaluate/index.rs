//! Indexing
//!
//! -------------------------------------------------------------------
//! List values    |   0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 |
//! ---------------+-----+----+----+----+----+----+----+----+----+----+
//! Forward index  |   0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 |
//! Backward index | -10 | -9 | -8 | -7 | -6 | -5 | -4 | -3 | -2 | -1 |
use crate::{
    ast::{Expression, ExpressionLocation},
    interpreter::{environment::EnvironmentRef, function::FunctionCarrier, value::Value},
    lexer::Location,
};
use std::fmt;

use super::{evaluate_expression, EvaluationError};

pub enum Index {
    Element(usize),
    Range(usize, usize),
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

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Index::Element(idx) => write!(f, "{}", idx),
            Index::Range(from, to) => write!(f, "{}..{}", from, to),
        }
    }
}
