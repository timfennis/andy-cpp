use crate::function::{Function, StaticType};

use crate::value::Value;
use ndc_parser::{CaptureSource, ResolvedVar};
use std::cell::RefCell;
use std::fmt;
use std::fmt::Formatter;
use std::io::{Stdout, Write, stdout};
use std::rc::Rc;

pub struct RootEnvironment {
    pub output: Box<dyn InterpreterOutput>,
    // These are global values
    global_functions: Vec<Function>,
}

pub struct Environment {
    root: Rc<RefCell<RootEnvironment>>,
    parent: Option<Rc<RefCell<Self>>>,
    values: Vec<Value>,
    upvalues: Vec<Rc<RefCell<Value>>>,
    base_offset: usize,
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Environment[has_parent: {:?}, values.len(): {}]",
            self.parent.is_some(),
            self.values.len(),
        )
    }
}

impl Environment {
    pub fn with_output<F>(&mut self, f: F) -> Result<(), std::io::Error>
    where
        F: FnOnce(&mut Box<dyn InterpreterOutput>) -> Result<(), std::io::Error>,
    {
        let mut root = self.root.borrow_mut();
        let output = &mut root.output;
        f(output)
    }

    #[must_use]
    pub fn get_output(&self) -> Option<Vec<u8>> {
        let root = self.root.clone();
        let root = root.borrow();
        let output = root.output.get_output();
        output.cloned()
    }

    #[must_use]
    pub fn new(writer: Box<dyn InterpreterOutput>) -> Self {
        let root = RootEnvironment {
            output: writer,
            global_functions: Default::default(),
        };

        Self {
            root: Rc::new(RefCell::new(root)),
            parent: None,
            values: Default::default(),
            upvalues: Default::default(),
            base_offset: 0,
        }
    }

    pub fn get_global_identifiers(&self) -> Vec<(String, StaticType)> {
        self.root
            .borrow()
            .global_functions
            .iter()
            .map(|function| (function.name().to_string(), function.static_type()))
            .collect::<Vec<(String, StaticType)>>()
    }

    #[must_use]
    pub fn get_all_functions(&self) -> Vec<Function> {
        self.root.borrow().global_functions.clone()
    }

    pub fn set(&mut self, var: ResolvedVar, value: Value) {
        match var {
            ResolvedVar::Local { slot } if slot >= self.base_offset => {
                let local_idx = slot - self.base_offset;
                if local_idx < self.values.len() {
                    self.values[local_idx] = value;
                } else {
                    debug_assert!(local_idx == self.values.len());
                    self.values.push(value);
                }
            }
            ResolvedVar::Local { .. } => {
                self.parent
                    .clone()
                    .expect("Local slot below base_offset but no parent")
                    .borrow_mut()
                    .set(var, value);
            }
            ResolvedVar::Upvalue { slot } => {
                *self.upvalues[slot].borrow_mut() = value;
            }
            ResolvedVar::Global { .. } => {
                unreachable!("cannot assign value to global")
            }
        }
    }

    /// Declare a function globally using its self-exposed name, if there already exists a function
    /// with the same name it's simply overloaded.
    pub fn declare_global_fn(&mut self, function: impl Into<Function>) {
        let new_function = function.into();

        let root: &mut RootEnvironment = &mut self.root.borrow_mut();

        root.global_functions.push(new_function.clone());
    }

    #[must_use]
    pub fn get(&self, var: ResolvedVar) -> Value {
        match var {
            ResolvedVar::Local { slot } if slot >= self.base_offset => {
                self.values[slot - self.base_offset].clone()
            }
            ResolvedVar::Local { .. } => self
                .parent
                .as_ref()
                .expect("Local slot below base_offset but no parent")
                .borrow()
                .get(var),
            ResolvedVar::Upvalue { slot } => self.upvalues[slot].borrow().clone(),
            ResolvedVar::Global { slot } => {
                Value::function(self.root.borrow().global_functions[slot].clone())
            }
        }
    }

    /// Takes the named variable from memory and leaves `Value::unit()` in its place
    #[must_use]
    pub fn take(&mut self, var: ResolvedVar) -> Option<Value> {
        match var {
            ResolvedVar::Local { slot } if slot >= self.base_offset => {
                let local_idx = slot - self.base_offset;
                Some(std::mem::replace(
                    self.values.get_mut(local_idx).expect("slot can't be empty"),
                    Value::unit(),
                ))
            }
            ResolvedVar::Local { .. } => self
                .parent
                .clone()
                .expect("Local slot below base_offset but no parent")
                .borrow_mut()
                .take(var),
            ResolvedVar::Upvalue { slot } => {
                let cell = &self.upvalues[slot];
                Some(std::mem::replace(&mut *cell.borrow_mut(), Value::unit()))
            }
            ResolvedVar::Global { .. } => panic!("cannot take global variable from environment"),
        }
    }

    pub fn new_function_scope(parent: &Rc<RefCell<Self>>, captures: &[CaptureSource]) -> Self {
        let parent_env = parent.borrow();
        let root_ref = Rc::clone(&parent_env.root);
        let upvalues = captures
            .iter()
            .map(|cap| match cap {
                CaptureSource::Local(slot) => {
                    parent_env.get_local_cell(*slot)
                }
                CaptureSource::Upvalue(idx) => {
                    Rc::clone(&parent_env.upvalues[*idx])
                }
            })
            .collect();
        drop(parent_env);
        Self {
            parent: Some(parent.clone()),
            root: root_ref,
            values: Default::default(),
            upvalues,
            base_offset: 0,
        }
    }

    pub fn new_iteration_scope(parent: &Rc<RefCell<Self>>, base_offset: usize) -> Self {
        let root_ref = Rc::clone(&parent.borrow().root);
        Self {
            parent: Some(parent.clone()),
            root: root_ref,
            values: Default::default(),
            upvalues: Default::default(),
            base_offset,
        }
    }

    fn get_local_cell(&self, slot: usize) -> Rc<RefCell<Value>> {
        let local_idx = slot - self.base_offset;
        Rc::new(RefCell::new(self.values[local_idx].clone()))
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new(Box::new(stdout()))
    }
}

pub trait InterpreterOutput: Write {
    fn get_output(&self) -> Option<&Vec<u8>>;
}

impl InterpreterOutput for Vec<u8> {
    fn get_output(&self) -> Option<&Vec<u8>> {
        Some(self)
    }
}

impl InterpreterOutput for Stdout {
    fn get_output(&self) -> Option<&Vec<u8>> {
        None
    }
}
