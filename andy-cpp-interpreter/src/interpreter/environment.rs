use crate::interpreter::function::Function;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;
use std::io::{stdout, Stdout, Write};
use std::rc::Rc;

use crate::interpreter::stdlib;
use crate::interpreter::value::Value;

pub type EnvironmentRef = Rc<RefCell<Environment>>;

pub struct RootEnvironment {
    pub output: Box<dyn InterpreterOutput>,
}

pub struct Environment {
    root: Rc<RefCell<RootEnvironment>>,
    parent: Option<EnvironmentRef>,
    values: HashMap<String, RefCell<Value>>,
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Environment[has_parent: {:?}, values.len(): {}]",
            self.parent.is_some(),
            self.values.len()
        )
    }
}

// #[cfg(debug_assertions)]
// impl Drop for Environment {
//     fn drop(&mut self) {
//         eprintln!("dropping {self:?}");
//     }
// }

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
        let root = RootEnvironment { output: writer };

        let mut env = Self {
            root: Rc::new(RefCell::new(root)),
            parent: None,
            values: HashMap::default(),
        };

        stdlib::bind_to_environment(&mut env);

        env
    }

    #[must_use]
    pub fn get_all(&self, name: &str) -> Vec<RefCell<Value>> {
        let mut values: Vec<RefCell<Value>> = Vec::new();

        if let Some(value) = self.values.get(name) {
            values.push(value.clone());
        }

        // TODO: there is probably a much faster implementation possible
        if let Some(parent) = &self.parent {
            values.extend(parent.borrow().get_all(name));
        }

        values
    }

    pub fn declare_function(&mut self, name: &str, function: Function) {
        if let Some(existing) = self.values.get(name) {
            // Overload existing function
            let existing = &*existing.borrow();
            if let Value::Function(existing_function) = existing {
                let mut existing_function = existing_function.borrow_mut();
                existing_function.add(function);
                return;
            }
        }

        // Fallback
        self.declare(name, Value::from(function));
    }
    pub fn declare(&mut self, name: &str, value: Value) {
        self.values.insert(name.into(), RefCell::new(value));
    }

    pub fn assign(&mut self, name: String, value: Value) -> bool {
        // Clippy wants us to use self.values.entry(name) but that moves name and breaks the recursive case
        #[allow(clippy::map_entry)]
        return if self.values.contains_key(&name) {
            self.values.insert(name, RefCell::new(value));
            true
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().assign(name, value)
        } else {
            false
        };
    }

    #[must_use]
    pub fn contains(&self, name: &str) -> bool {
        self.values.contains_key(name)
            || self
                .parent
                .clone()
                .is_some_and(|p| p.borrow().contains(name))
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<RefCell<Value>> {
        if let Some(v) = self.values.get(name) {
            Some(v.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    pub fn new_scope(parent: &EnvironmentRef) -> EnvironmentRef {
        let root_ref = Rc::clone(&parent.borrow().root);
        Rc::new(RefCell::new(Self {
            parent: Some(Rc::clone(parent)),
            root: root_ref,
            values: HashMap::default(),
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

#[cfg(test)]
mod test {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::interpreter::environment::Environment;

    #[test]
    fn create_and_destroy_scope() {
        let env = Rc::new(RefCell::new(Environment::default()));
        env.borrow_mut().declare("parent_value", 123.into());
        env.borrow_mut().declare("mutated_value", 69.into());
        env.borrow_mut().declare("shadowed", 1.into());

        assert_eq!(
            *env.borrow().get("parent_value").unwrap().borrow(),
            123.into()
        );
        assert_eq!(
            *env.borrow().get("mutated_value").unwrap().borrow(),
            69.into()
        );

        let scope = Environment::new_scope(&env);
        scope.borrow_mut().declare("inner_value", 234.into());
        scope.borrow_mut().declare("shadowed", 2.into());
        assert!(scope
            .borrow_mut()
            .assign("mutated_value".into(), 420.into()));

        assert_eq!(
            *scope.borrow().get("parent_value").unwrap().borrow(),
            123.into()
        );
        assert_eq!(
            *scope.borrow().get("mutated_value").unwrap().borrow(),
            420.into()
        );
        assert_eq!(
            *scope.borrow().get("inner_value").unwrap().borrow(),
            234.into()
        );
        assert_eq!(*scope.borrow().get("shadowed").unwrap().borrow(), 2.into());

        drop(scope);

        assert_eq!(
            *env.borrow().get("parent_value").unwrap().borrow(),
            123.into(),
            "original value still exists and hasn't changed"
        );
        assert_eq!(
            *env.borrow().get("mutated_value").unwrap().borrow(),
            420.into(),
            "value in the original scope was modified"
        );
        assert_eq!(
            env.borrow().get("inner_value"),
            None,
            "a local variable is completely gone"
        );
        assert_eq!(
            *env.borrow().get("shadowed").unwrap().borrow(),
            1.into(),
            "a shadowed value reverts back to the value in the original scope"
        );
    }
}
