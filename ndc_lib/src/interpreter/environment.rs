use crate::interpreter::function::{Function, OverloadedFunction};

use crate::ast::ResolvedVar;
use crate::interpreter::resolve::LexicalIdentifier;
use crate::interpreter::value::Value;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Formatter;
use std::io::{Stdout, Write, stdout};
use std::rc::Rc;

pub type EnvironmentRef = Rc<RefCell<Environment>>;

pub struct RootEnvironment {
    pub output: Box<dyn InterpreterOutput>,
    // These are global values
    global_functions: Vec<OverloadedFunction>,
}

pub struct Environment {
    root: Rc<RefCell<RootEnvironment>>,
    parent: Option<EnvironmentRef>,
    values: Vec<Rc<RefCell<Value>>>,
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
            values: Vec::new(),
        };

        crate::stdlib::register(&mut env);

        env
    }

    pub fn create_global_scope_mapping(&self) -> Vec<LexicalIdentifier> {
        self.root
            .borrow()
            .global_functions
            .iter()
            .filter_map(|function| {
                function.name().map(|name| LexicalIdentifier::Function {
                    name: name.to_string(),
                    arity: function.arity(),
                })
            })
            .collect::<Vec<LexicalIdentifier>>()
    }

    #[must_use]
    pub fn get_all_functions(&self) -> Vec<OverloadedFunction> {
        let mut values = Vec::new();

        for value in &self.values {
            if let Value::Function(func) = &*value.borrow() {
                values.push(func.borrow().clone())
            }
        }

        if let Some(parent) = &self.parent {
            values.extend(parent.borrow().get_all_functions());
        }

        values
    }

    pub fn set(&mut self, var: ResolvedVar, value: Value) {
        match var {
            ResolvedVar::Captured { depth: 0, slot } => {
                if let Value::Function(value_to_insert) = &value {
                    if let Some(existing_value) = self.values.get(slot) {
                        let existing_value = &*existing_value.borrow();
                        if let Value::Function(func) = existing_value {
                            func.borrow_mut().merge(&mut value_to_insert.borrow_mut())
                        }
                    } else {
                        self.values.insert(slot, Rc::new(RefCell::new(value)));
                    }

                    return;
                }

                if self.values.len() > slot {
                    self.values[slot] = Rc::new(RefCell::new(value))
                } else {
                    self.values.insert(slot, Rc::new(RefCell::new(value)))
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
                todo!("convert to panic?? you cannot assign to the global scope right?")
            }
        }
    }

    /// Declare a function globally using its self-exposed name, if there already exists a function
    /// with the same name it's simply overloaded.
    pub fn declare_global_fn(&mut self, function: impl Into<Function>) {
        // TODO: we might need to group functions together if they have the same name

        let new_function = function.into();

        let gb = &mut self.root.borrow_mut().global_functions;

        // Try to add it to an existing definition
        for fun in gb.iter_mut() {
            if fun.name() == Some(new_function.name()) && fun.arity() == new_function.arity() {
                fun.add(new_function);
                return;
            }
        }

        // Create a new definition
        gb.push(OverloadedFunction::from_multiple(vec![new_function]))
    }

    fn get_slot(&self, depth: usize, slot: usize) -> Rc<RefCell<Value>> {
        if depth == 0 {
            self.values[slot].clone()
        } else {
            self.parent
                .clone()
                .expect("expected parent env did not exist")
                .borrow()
                .get_slot(depth - 1, slot)
        }
    }
    #[must_use]
    pub fn get(&self, var: ResolvedVar) -> Rc<RefCell<Value>> {
        match var {
            ResolvedVar::Captured { depth, slot } => self.get_slot(depth, slot),
            ResolvedVar::Global { slot } => Rc::new(RefCell::new(Value::function(
                self.root.borrow().global_functions[slot].clone(),
            ))),
        }
    }

    /// Takes the named variable from memory and leaves `Value::unit()` in its place
    #[must_use]
    pub fn take(&self, var: ResolvedVar) -> Option<Value> {
        match var {
            ResolvedVar::Captured { depth: 0, slot } => Some(std::mem::replace(
                &mut *self.values[slot].borrow_mut(),
                Value::unit(),
            )),
            ResolvedVar::Captured { depth, slot } => self
                .parent
                .clone()
                .expect("expected parent env did not exist")
                .borrow()
                .take(ResolvedVar::Captured {
                    depth: depth - 1,
                    slot,
                }),
            ResolvedVar::Global { .. } => panic!("cannot take global variable from environment"),
        }
    }

    pub fn new_scope(parent: &EnvironmentRef) -> EnvironmentRef {
        let root_ref = Rc::clone(&parent.borrow().root);
        Rc::new(RefCell::new(Self {
            parent: Some(Rc::clone(parent)),
            root: root_ref,
            values: Vec::new(),
        }))
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
