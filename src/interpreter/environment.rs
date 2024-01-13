use crate::interpreter::{stdlib, Value};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    contexts: Vec<Context>,
}

#[derive(Debug)]
struct Context {
    values: HashMap<String, Value>,
}

impl Context {}

impl Default for Context {
    fn default() -> Self {
        Self {
            values: HashMap::with_capacity(0),
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new_with_stdlib()
    }
}

impl Environment {
    pub fn new_with_stdlib() -> Self {
        let mut env = Self {
            contexts: vec![Context::default()],
        };
        stdlib::bind_to_environment(&mut env);
        env
    }
    pub fn declare(&mut self, name: &str, value: Value) {
        self.contexts
            .last_mut()
            .expect("Environment should guarantee there is always at least one context")
            .values
            .insert(name.into(), value);
    }

    pub fn assign(&mut self, name: String, value: Value) -> bool {
        for ctx in &mut self.contexts {
            // The map_entry suggestion isn't helpful because entry wants to take ownership of name
            #[allow(clippy::map_entry)]
            if ctx.values.contains_key(&name) {
                ctx.values.insert(name, value);
                return true;
            }
        }

        false
    }

    pub fn contains(&self, name: &str) -> bool {
        self.contexts
            .iter()
            .any(|ctx| ctx.values.contains_key(name))
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        for ctx in self.contexts.iter().rev() {
            if let Some(v) = ctx.values.get(name) {
                return Some(v);
            }
        }

        None
    }

    pub fn new_scope(&mut self) {
        self.contexts.push(Context::default());
    }

    pub fn destroy_scope(&mut self) {
        self.contexts.pop().expect("no scope to destroy");
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::environment::Environment;

    #[test]
    fn create_and_destroy_scope() {
        let mut env = Environment::new_with_stdlib();
        env.declare("parent_value", 123.into());
        env.declare("mutated_value", 69.into());
        env.declare("shadowed", 1.into());

        assert_eq!(env.get("parent_value"), Some(&123.into()));
        assert_eq!(env.get("mutated_value"), Some(&69.into()));

        env.new_scope();
        env.declare("inner_value", 234.into());
        env.declare("shadowed", 2.into());
        assert!(env.assign("mutated_value".into(), 420.into()));

        assert_eq!(env.get("parent_value"), Some(&123.into()));
        assert_eq!(env.get("mutated_value"), Some(&420.into()));
        assert_eq!(env.get("inner_value"), Some(&234.into()));
        assert_eq!(env.get("shadowed"), Some(&2.into()));

        env.destroy_scope();

        assert_eq!(
            env.get("parent_value"),
            Some(&123.into()),
            "original value still exists and hasn't changed"
        );
        assert_eq!(
            env.get("mutated_value"),
            Some(&420.into()),
            "value in the original scope was modified"
        );
        assert_eq!(
            env.get("inner_value"),
            None,
            "a local variable is completely gone"
        );
        assert_eq!(
            env.get("shadowed"),
            Some(&1.into()),
            "a shadowed value reverts back to the value in the original scope"
        );
    }
}
