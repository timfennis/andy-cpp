//! Lazy sequences: what `lazy(xs)` makes, and the functions that take one
//! give.
//!
//! A [`Lazy`] is a description: the value its elements come from and the
//! steps that make its elements from those. It's a value like any other,
//! which reading never changes: each `for` loop, or each function that
//! takes one, starts a [`LazyCursor`] at the source and runs every step, the
//! functions the steps call included. A step that calls a function needs
//! the VM, so a cursor is read with [`LazyCursor::next`], which takes it.

use crate::iterator::ValueIter;
use crate::value::{Function, Object, Value};
use crate::{Vm, VmError};
use std::fmt;
use std::rc::Rc;

/// A lazy sequence: `lazy(1..10).map(f).take(3)`.
#[derive(Clone)]
pub struct Lazy {
    /// What the elements come from: a value a `for` loop iterates over, but
    /// not a lazy sequence, whose steps a lazy sequence made from it takes
    /// over.
    source: Value,
    steps: Rc<[Step]>,
}

/// One step of a lazy sequence, which makes its elements from those of the
/// steps before it.
#[derive(Clone)]
pub enum Step {
    /// Each element is what the function gives for one.
    Map(Function),
    /// The elements the function gives `true` for.
    Filter(Function),
    /// The elements of what the function gives for each element, in turn.
    FlatMap(Function),
    /// The elements up to the first the function gives `false` for.
    TakeWhile(Function),
    /// The elements from the first the function gives `false` for.
    SkipWhile(Function),
    /// Each element with its position: `(0, a)`, `(1, b)`, …
    Enumerate,
    /// The first so many elements.
    Take(usize),
    /// The elements after the first so many.
    Skip(usize),
}

impl Lazy {
    /// The lazy sequence of the elements of `source`, or `source` itself if
    /// it's a lazy sequence already. `None` if `source` isn't something a
    /// `for` loop iterates over.
    pub fn new(source: Value) -> Option<Self> {
        if let Value::Object(object) = &source
            && let Object::Lazy(lazy) = object.as_ref()
        {
            return Some(lazy.clone());
        }
        source.iter()?;
        Some(Self {
            source,
            steps: Rc::from([]),
        })
    }

    /// This sequence with `step` after its steps.
    #[must_use]
    pub fn then(&self, step: Step) -> Self {
        let steps: Vec<Step> = self.steps.iter().cloned().chain([step]).collect();
        Self {
            source: self.source.clone(),
            steps: Rc::from(steps),
        }
    }

    /// A cursor at the start of the sequence.
    pub fn cursor(&self) -> LazyCursor {
        let source = self
            .source
            .iter()
            .expect("internal error: a lazy sequence's source is iterable");
        LazyCursor {
            source,
            stages: self.steps.iter().map(Stage::new).collect(),
        }
    }

    /// Every element of the sequence, made one after the other.
    pub fn collect(&self, vm: &mut Vm) -> Result<Vec<Value>, VmError> {
        let mut cursor = self.cursor();
        // How many the source has says little: the steps may leave out or
        // add elements, and a range without an end has no end.
        let mut values = Vec::new();
        while let Some(value) = cursor.next(vm)? {
            values.push(value);
        }
        Ok(values)
    }

    /// A copy whose source is a deep copy of this one's.
    #[must_use]
    pub fn deep_copy_source(&self, copy: impl FnOnce(&Value) -> Value) -> Self {
        Self {
            source: copy(&self.source),
            steps: Rc::clone(&self.steps),
        }
    }
}

/// Shows the description, never the elements, as making them may call
/// functions: `lazy([1,2,3]).map(<fn f>).take(2)`.
impl fmt::Display for Lazy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "lazy({:?})", self.source)?;
        for step in self.steps.iter() {
            match step {
                Step::Map(function) => write!(f, ".map({function})")?,
                Step::Filter(function) => write!(f, ".filter({function})")?,
                Step::FlatMap(function) => write!(f, ".flat_map({function})")?,
                Step::TakeWhile(function) => write!(f, ".take_while({function})")?,
                Step::SkipWhile(function) => write!(f, ".skip_while({function})")?,
                Step::Enumerate => write!(f, ".enumerate()")?,
                Step::Take(n) => write!(f, ".take({n})")?,
                Step::Skip(n) => write!(f, ".skip({n})")?,
            }
        }
        Ok(())
    }
}

/// Where reading a lazy sequence has got to: a cursor over its source and
/// what each step has kept so far.
pub struct LazyCursor {
    source: ValueIter,
    stages: Vec<Stage>,
}

/// A step, with what it keeps while the sequence is read.
enum Stage {
    Map(Function),
    Filter(Function),
    FlatMap {
        function: Function,
        /// The elements of what the function gave for the last element,
        /// still to be given.
        inner: Option<ValueIter>,
    },
    TakeWhile {
        function: Function,
        done: bool,
    },
    SkipWhile {
        function: Function,
        skipping: bool,
    },
    Enumerate(i64),
    Take(usize),
    Skip(usize),
}

impl Stage {
    fn new(step: &Step) -> Self {
        match step {
            Step::Map(function) => Self::Map(function.clone()),
            Step::Filter(function) => Self::Filter(function.clone()),
            Step::FlatMap(function) => Self::FlatMap {
                function: function.clone(),
                inner: None,
            },
            Step::TakeWhile(function) => Self::TakeWhile {
                function: function.clone(),
                done: false,
            },
            Step::SkipWhile(function) => Self::SkipWhile {
                function: function.clone(),
                skipping: true,
            },
            Step::Enumerate => Self::Enumerate(0),
            Step::Take(n) => Self::Take(*n),
            Step::Skip(n) => Self::Skip(*n),
        }
    }
}

impl LazyCursor {
    /// The next element, made by running the steps on the VM, or `None` once
    /// there are no more.
    pub fn next(&mut self, vm: &mut Vm) -> Result<Option<Value>, VmError> {
        pull(&mut self.source, &mut self.stages, vm)
    }
}

/// The next element of the steps `stages` over `source`: the last stage's
/// next one, which it makes from the stages before it.
fn pull(
    source: &mut ValueIter,
    stages: &mut [Stage],
    vm: &mut Vm,
) -> Result<Option<Value>, VmError> {
    let Some((stage, before)) = stages.split_last_mut() else {
        return Ok(source.next());
    };
    match stage {
        Stage::Map(function) => match pull(source, before, vm)? {
            Some(value) => Ok(Some(vm.call_callback(function.clone(), &[value])?)),
            None => Ok(None),
        },
        Stage::Filter(function) => {
            while let Some(value) = pull(source, before, vm)? {
                if predicate(vm, function, &value)? {
                    return Ok(Some(value));
                }
            }
            Ok(None)
        }
        Stage::FlatMap { function, inner } => loop {
            if let Some(value) = inner.as_mut().and_then(Iterator::next) {
                return Ok(Some(value));
            }
            let Some(value) = pull(source, before, vm)? else {
                return Ok(None);
            };
            let elements = vm.call_callback(function.clone(), &[value])?;
            *inner = Some(elements.iter().ok_or_else(|| {
                VmError::native(format!(
                    "flat_map's function gave {}, which isn't a sequence",
                    elements.static_type()
                ))
            })?);
        },
        Stage::TakeWhile { function, done } => {
            if *done {
                return Ok(None);
            }
            match pull(source, before, vm)? {
                Some(value) if predicate(vm, function, &value)? => Ok(Some(value)),
                _ => {
                    *done = true;
                    Ok(None)
                }
            }
        }
        Stage::SkipWhile { function, skipping } => {
            while let Some(value) = pull(source, before, vm)? {
                if !*skipping || !predicate(vm, function, &value)? {
                    *skipping = false;
                    return Ok(Some(value));
                }
            }
            Ok(None)
        }
        Stage::Enumerate(index) => match pull(source, before, vm)? {
            Some(value) => {
                let position = Value::Int(*index);
                *index += 1;
                Ok(Some(Value::tuple(vec![position, value])))
            }
            None => Ok(None),
        },
        Stage::Take(remaining) => {
            if *remaining == 0 {
                return Ok(None);
            }
            let value = pull(source, before, vm)?;
            if value.is_some() {
                *remaining -= 1;
            }
            Ok(value)
        }
        Stage::Skip(remaining) => {
            while *remaining > 0 {
                if pull(source, before, vm)?.is_none() {
                    return Ok(None);
                }
                *remaining -= 1;
            }
            pull(source, before, vm)
        }
    }
}

/// What `function` says of `value`: a `Bool`, as its type promises.
fn predicate(vm: &mut Vm, function: &Function, value: &Value) -> Result<bool, VmError> {
    match vm.call_callback(function.clone(), std::slice::from_ref(value))? {
        Value::Bool(answer) => Ok(answer),
        other => Err(VmError::native(format!(
            "expected the function to give a Bool, but it gave {}",
            other.static_type()
        ))),
    }
}
