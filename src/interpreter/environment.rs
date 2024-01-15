use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{stdout, Stdout, Write};
use std::rc::Rc;

use crate::interpreter::stdlib;
use crate::interpreter::value::Value;

pub type EnvironmentRef = Rc<RefCell<Environment>>;

pub struct RootEnvironment {
    pub output: Box<dyn InterpreterOutput>,
    // TODO: Global functions should be declared here, allow dead code for now
    #[allow(dead_code)]
    values: HashMap<String, Value>,
}

pub struct Environment {
    root: Rc<RefCell<RootEnvironment>>,
    parent: Option<EnvironmentRef>,
    values: HashMap<String, RefCell<Value>>,
}

impl Environment {
    /// # Errors
    /// see `std::io::Error`
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
        //TODO this clone is probably a very bad idea and should be removed but CBA right now
        output.cloned()
    }

    #[must_use]
    pub fn new_with_stdlib(writer: Box<dyn InterpreterOutput>) -> Self {
        let root = RootEnvironment {
            output: writer,
            values: HashMap::default(),
        };

        let mut env = Self {
            root: Rc::new(RefCell::new(root)),
            parent: None,
            values: HashMap::default(),
        };

        stdlib::bind_to_environment(&mut env);

        env
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
        Environment::new_with_stdlib(Box::new(stdout()))
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
