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

    pub fn get(&self, name: &str) -> Option<&Literal> {
        self.values.get(name)
    }
}
