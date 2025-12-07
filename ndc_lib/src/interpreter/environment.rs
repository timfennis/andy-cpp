use crate::interpreter::function::{Function, StaticType};

use crate::ast::ResolvedVar;
use crate::interpreter::value::Value;
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
    parent: Option<Rc<RefCell<Environment>>>,
    values: Vec<Value>,
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
    pub fn new_with_stdlib(writer: Box<dyn InterpreterOutput>) -> Self {
        let root = RootEnvironment {
            output: writer,
            global_functions: Default::default(),
        };

        let mut env = Self {
            root: Rc::new(RefCell::new(root)),
            parent: None,
            values: Default::default(),
        };

        crate::stdlib::register(&mut env);

        env
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
            ResolvedVar::Captured { depth: 0, slot } => {
                if self.values.len() > slot {
                    self.values[slot] = value
                } else {
                    debug_assert!(slot == self.values.len());
                    self.values.push(value);
                }
            }

            // Recursively insert
            ResolvedVar::Captured { depth, slot } => {
                self.parent
                    .clone()
                    .expect("tried to get parent but failed")
                    .borrow_mut()
                    .set(
                        ResolvedVar::Captured {
                            depth: depth - 1,
                            slot,
                        },
                        value,
                    );
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

    fn get_copy_from_slot(&self, depth: usize, slot: usize) -> Value {
        if depth == 0 {
            assert!(
                self.values.len() > slot,
                "failed to take item out of slot {slot} because it was empty"
            );
            self.values[slot].clone()
        } else {
            self.parent
                .clone()
                .expect("expected parent env did not exist")
                .borrow()
                .get_copy_from_slot(depth - 1, slot)
        }
    }

    #[must_use]
    pub fn get(&self, var: ResolvedVar) -> Value {
        match var {
            ResolvedVar::Captured { depth, slot } => self.get_copy_from_slot(depth, slot),
            ResolvedVar::Global { slot } => {
                Value::function(self.root.borrow().global_functions[slot].clone())
            }
        }
    }

    /// Takes the named variable from memory and leaves `Value::unit()` in its place
    #[must_use]
    pub fn take(&mut self, var: ResolvedVar) -> Option<Value> {
        match var {
            ResolvedVar::Captured { depth: 0, slot } => Some(std::mem::replace(
                self.values.get_mut(slot).expect("slot can't be empty"),
                Value::unit(),
            )),
            ResolvedVar::Captured { depth, slot } => self
                .parent
                .clone()
                .expect("expected parent env did not exist")
                .borrow_mut()
                .take(ResolvedVar::Captured {
                    depth: depth - 1,
                    slot,
                }),
            ResolvedVar::Global { .. } => panic!("cannot take global variable from environment"),
        }
    }

    pub fn new_scope(parent: &Rc<RefCell<Self>>) -> Self {
        let root_ref = Rc::clone(&parent.borrow().root);
        Self {
            parent: Some(parent.clone()),
            root: root_ref,
            values: Default::default(),
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new_with_stdlib(Box::new(stdout()))
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
