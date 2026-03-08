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

use super::{EvaluationError, EvaluationResult, evaluate_expression};
use crate::environment::Environment;
use crate::iterator::ValueIterator;
use crate::{function::FunctionCarrier, sequence::Sequence, value::Value};
use itertools::Itertools;
use ndc_parser::{Expression, ExpressionLocation};
use std::cell::RefCell;
use std::cmp::min;
use std::ops::IndexMut;
use std::rc::Rc;

#[derive(thiserror::Error, Debug)]
#[error("{0}")]
pub struct IndexError(String);

impl IndexError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self(msg.into())
    }
}

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
    pub fn try_into_offset(self, size: usize) -> Result<Offset, IndexError> {
        Ok(match self {
            Self::Index(idx) => {
                Offset::Element(value_to_bounded_forward_index(idx, size, false)?)
            }
            Self::Slice {
                from,
                to,
                inclusive,
            } => {
                let from_idx = if let Some(from) = from {
                    value_to_bounded_forward_index(from, size, true)?
                } else {
                    0
                };

                let to_idx = if let Some(to) = to {
                    value_to_bounded_forward_index(to, size, true)?
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

fn value_to_bounded_forward_index(
    value: Value,
    size: usize,
    for_slice: bool,
) -> Result<usize, IndexError> {
    let index = i64::try_from(value).map_err(|_| IndexError::new("Invalid list index. List indices must be convertible to a signed 64-bit integer."))?;

    if index.is_negative() {
        let index = usize::try_from(index.abs())
            .map_err(|_| IndexError::new("invalid index: too large"))?;

        if for_slice {
            Ok(size.saturating_sub(index))
        } else {
            size.checked_sub(index)
                .ok_or_else(|| IndexError::new("index out of bounds"))
        }
    } else {
        let index = usize::try_from(index)
            .map_err(|_| IndexError::new("Invalid list index. List indices must be convertible to a signed 64-bit integer."))?;
        if for_slice {
            return Ok(min(index, size));
        }

        if index >= size {
            return Err(IndexError::new("index out of bounds"));
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
    environment: &Rc<RefCell<Environment>>,
) -> Result<Value, FunctionCarrier> {
    let Some(size) = lhs.sequence_length() else {
        return Err(IndexError::new(
            "cannot index into this type because it doesn't have a length",
        )
        .into());
    };

    match lhs {
        Value::Sequence(Sequence::List(list)) => {
            let list = list.borrow();
            let index = index.try_into_offset(size)?;

            match index {
                Offset::Element(index_usize) => Ok(list[index_usize].clone()),
                Offset::Range(from_usize, to_usize) => {
                    let Some(values) = list.get(from_usize..to_usize) else {
                        return Err(IndexError::new(format!(
                            "{from_usize}..{to_usize} out of bounds"
                        ))
                        .into());
                    };
                    Ok(Value::list(values))
                }
            }
        }
        Value::Sequence(Sequence::String(insertion_target)) => {
            let index = index.try_into_offset(size)?;
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
                    return Err(
                        IndexError::new("cannot use range expression as index in map").into()
                    );
                }
            };

            let value = map
                .try_borrow()
                .map_err(|e| -> FunctionCarrier { IndexError::new(e.to_string()).into() })?
                .get(&key)
                .cloned();

            if let Some(value) = value {
                Ok(value)
            } else if let Some(default) = default {
                let default_value = produce_default_value(default, environment)?;
                map.try_borrow_mut()
                    .map_err(|e| -> FunctionCarrier { IndexError::new(e.to_string()).into() })?
                    .insert(key, default_value.clone());
                Ok(default_value)
            } else {
                Err(IndexError::new(format!("Key not found in map: {key}")).into())
            }
        }
        Value::Sequence(Sequence::Tuple(tuple)) => {
            let index = index.try_into_offset(size)?;
            match index {
                Offset::Element(index_usize) => {
                    let Some(value) = tuple.get(index_usize) else {
                        return Err(IndexError::new("index out of bounds").into());
                    };
                    Ok(value.clone())
                }
                Offset::Range(from_usize, to_usize) => {
                    let Some(values) = tuple.get(from_usize..to_usize) else {
                        return Err(IndexError::new("index out of bounds").into());
                    };
                    Ok(Value::Sequence(Sequence::Tuple(Rc::new(values.to_vec()))))
                }
            }
        }
        Value::Sequence(Sequence::Deque(deque)) => {
            let index = index.try_into_offset(size)?;
            match index {
                Offset::Element(usize_index) => {
                    let list = deque.borrow();
                    let Some(value) = list.get(usize_index) else {
                        return Err(IndexError::new("index out of bounds").into());
                    };
                    Ok(value.clone())
                }
                Offset::Range(from_usize, to_usize) => {
                    let list = deque.borrow();
                    let out = list
                        .iter()
                        .dropping(from_usize)
                        .take(to_usize - from_usize)
                        .cloned()
                        .collect::<Vec<_>>();
                    Ok(Value::list(out))
                }
            }
        }
        _ => Err(IndexError::new(format!("cannot index into {}", lhs.static_type())).into()),
    }
}

pub fn produce_default_value(
    default: &Value,
    environment: &Rc<RefCell<Environment>>,
) -> EvaluationResult {
    match default {
        Value::Function(function) => match function.call_checked(&mut [], environment) {
            Err(FunctionCarrier::FunctionTypeMismatch) => Err(IndexError::new(
                "default function is not callable without arguments",
            )
            .into()),
            a => a,
        },
        value => Ok(value.clone()),
    }
}

pub fn set_at_index(
    lhs: &mut Value,
    rhs: Value,
    index: EvaluatedIndex,
) -> Result<(), FunctionCarrier> {
    let Some(size) = lhs.sequence_length() else {
        return Err(IndexError::new(
            "cannot index into this type because it doesn't have a length",
        )
        .into());
    };

    match lhs {
        Value::Sequence(Sequence::List(list)) => {
            let mut list = list.try_borrow_mut().map_err(|_| -> FunctionCarrier {
                IndexError::new("Mutation error: you cannot mutate a value in a list while you're iterating over this list").into()
            })?;

            let index = index.try_into_offset(size)?;

            match index {
                Offset::Element(index_usize) => {
                    let x = list.index_mut(index_usize);
                    *x = rhs;
                }
                Offset::Range(from_usize, to_usize) => {
                    let tail = list.drain(from_usize..).collect::<Vec<_>>();

                    list.extend(
                        rhs.try_into_vec()
                            .expect("this must succeed, but not sure why")
                            .into_iter(),
                    );

                    list.extend_from_slice(&tail[(to_usize - from_usize)..]);
                }
            }
        }
        Value::Sequence(Sequence::String(insertion_target)) => {
            if let Value::Sequence(Sequence::String(target_string)) = rhs {
                let target_string = target_string.borrow();

                let mut insertion_target = insertion_target.borrow_mut();

                let index = index.try_into_offset(size)?;

                match index {
                    Offset::Element(index) => {
                        insertion_target.replace_range(index..=index, target_string.as_str());
                    }
                    Offset::Range(from, to) => {
                        insertion_target.replace_range(from..to, target_string.as_str());
                    }
                }
            } else {
                return Err(IndexError::new(format!(
                    "cannot insert {} into a string",
                    rhs.static_type()
                ))
                .into());
            }
        }
        Value::Sequence(Sequence::Map(map, _)) => {
            let mut map = map
                .try_borrow_mut()
                .map_err(|e| -> FunctionCarrier { IndexError::new(e.to_string()).into() })?;

            let key = match index {
                EvaluatedIndex::Index(idx) => idx,
                EvaluatedIndex::Slice { .. } => {
                    return Err(
                        IndexError::new("cannot use range expression as index").into()
                    );
                }
            };
            map.insert(key, rhs);
        }
        _ => {
            return Err(IndexError::new(format!(
                "cannot insert into {} at index",
                lhs.static_type()
            ))
            .into());
        }
    };
    Ok(())
}

pub fn value_to_evaluated_index(value: Value) -> EvaluatedIndex {
    if let Value::Sequence(Sequence::Iterator(ref rc)) = value {
        let iter = rc.borrow();
        match &*iter {
            ValueIterator::ValueRange(r) => {
                return EvaluatedIndex::Slice {
                    from: Some(Value::from(r.0.start)),
                    to: Some(Value::from(r.0.end)),
                    inclusive: false,
                };
            }
            ValueIterator::ValueRangeInclusive(r) => {
                return EvaluatedIndex::Slice {
                    from: Some(Value::from(*r.0.start())),
                    to: Some(Value::from(*r.0.end())),
                    inclusive: true,
                };
            }
            ValueIterator::ValueRangeFrom(r) => {
                return EvaluatedIndex::Slice {
                    from: Some(Value::from(r.0.start)),
                    to: None,
                    inclusive: false,
                };
            }
            _ => {}
        }
    }
    EvaluatedIndex::Index(value)
}
