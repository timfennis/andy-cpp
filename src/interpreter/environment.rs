use crate::ast::Literal;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    contexts: Vec<Context>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            contexts: vec![Context::default()],
        }
    }
}

#[derive(Debug)]
struct Context {
    values: HashMap<String, Literal>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            values: HashMap::with_capacity(0),
        }
    }
}

impl Environment {
    pub fn declare(&mut self, name: &str, value: Literal) {
        self.contexts
            .last_mut()
            .expect("Environment should guarantee there is always at least one context")
            .values
            .insert(name.into(), value);
    }

    //TODO: maybe take an owned String for name here
    pub fn assign(&mut self, name: &str, value: Literal) -> bool {
        for ctx in &mut self.contexts {
            if ctx.values.contains_key(name) {
                ctx.values.insert(name.into(), value);
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

    pub fn get(&self, name: &str) -> Option<&Literal> {
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
    use crate::ast::Literal;
    use crate::interpreter::environment::Environment;

    #[test]
    fn create_and_destroy_scope() {
        let mut env = Environment::default();
        env.declare("parent_value", Literal::Integer(123));
        env.declare("mutated_value", Literal::Integer(69));
        env.declare("shadowed", Literal::Integer(1));

        assert_eq!(env.get("parent_value"), Some(&Literal::Integer(123)));
        assert_eq!(env.get("mutated_value"), Some(&Literal::Integer(69)));

        env.new_scope();
        env.declare("inner_value", Literal::Integer(234));
        env.declare("shadowed", Literal::Integer(2));
        assert!(env.assign("mutated_value", Literal::Integer(420)));

        assert_eq!(env.get("parent_value"), Some(&Literal::Integer(123)));
        assert_eq!(env.get("mutated_value"), Some(&Literal::Integer(420)));
        assert_eq!(env.get("inner_value"), Some(&Literal::Integer(234)));
        assert_eq!(env.get("shadowed"), Some(&Literal::Integer(2)));

        env.destroy_scope();

        assert_eq!(
            env.get("parent_value"),
            Some(&Literal::Integer(123)),
            "original value still exists and hasn't changed"
        );
        assert_eq!(
            env.get("mutated_value"),
            Some(&Literal::Integer(420)),
            "value in the original scope was modified"
        );
        assert_eq!(
            env.get("inner_value"),
            None,
            "a local variable is completely gone"
        );
        assert_eq!(
            env.get("shadowed"),
            Some(&Literal::Integer(1)),
            "a shadowed value reverts back to the value in the original scope"
        );
    }
}
