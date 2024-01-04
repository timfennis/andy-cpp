use crate::ast::Literal;
use std::collections::HashMap;

#[derive(Default, Debug)]
pub struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn declare(&mut self, name: &str, value: Literal) {
        self.values.insert(name.into(), value);
    }

    //TODO: maybe take an owned String for name here
    pub fn assign(&mut self, name: &str, value: Literal) -> bool {
        if !self.values.contains_key(name) {
            return false;
        }
        self.values.insert(name.into(), value);

        true
    }

    pub fn get(&self, name: &str) -> Option<&Literal> {
        self.values.get(name)
    }
}
