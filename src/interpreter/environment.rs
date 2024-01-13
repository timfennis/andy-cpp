use crate::interpreter::num::SingleNumberFunction;
use crate::interpreter::{Number, Value};
use num::ToPrimitive;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Environment {
    contexts: Vec<Context>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            contexts: vec![Context::stdlib()],
        }
    }
}

#[derive(Debug)]
struct Context {
    values: HashMap<String, Value>,
}

impl Context {
    fn stdlib() -> Self {
        let mut map = HashMap::default();
        map.insert(
            "sqrt".to_string(),
            Value::Function(Rc::new(SingleNumberFunction {
                function: |num: Number| match num {
                    Number::Int(i) => Number::Float(f64::from(i).sqrt()),
                    Number::Float(f) => Number::Float(f.sqrt()),
                    Number::Rational(r) => Number::Float(r.to_f64().unwrap_or(f64::NAN).sqrt()),
                    Number::Complex(c) => Number::Complex(c.sqrt()),
                },
            })),
        );

        Self { values: map }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            values: HashMap::with_capacity(0),
        }
    }
}

impl Environment {
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
        let mut env = Environment::default();
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
