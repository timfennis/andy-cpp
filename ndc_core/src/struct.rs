use crate::StaticType;
use std::ops::Index;
use std::rc::Rc;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct StructId(usize);

#[derive(Clone, Debug, PartialEq)]
pub struct StructInfo {
    pub id: StructId,
    pub name: Box<str>,
    pub fields: Vec<(String, StaticType)>,
}

impl StructInfo {
    pub fn static_type(&self) -> StaticType {
        StaticType::Struct {
            id: self.id,
            name: self.name.clone(),
        }
    }

    pub fn constructor_type(&self) -> StaticType {
        StaticType::Function {
            parameters: Some(self.fields.iter().cloned().map(|f| f.1).collect()),
            return_type: Box::new(self.static_type()),
        }
    }

    pub fn field_name(&self, index: usize) -> &str {
        &self.fields[index].0
    }

    /// Type of the synthetic getter for field `index`: `(Self) -> FieldType`.
    pub fn getter_type(&self, index: usize) -> StaticType {
        StaticType::Function {
            parameters: Some(vec![self.static_type()]),
            return_type: Box::new(self.fields[index].1.clone()),
        }
    }

    /// Type of the synthetic setter for field `index`: `(Self, FieldType) -> ()`.
    pub fn setter_type(&self, index: usize) -> StaticType {
        StaticType::Function {
            parameters: Some(vec![self.static_type(), self.fields[index].1.clone()]),
            return_type: Box::new(StaticType::unit()),
        }
    }
}

#[derive(Debug, Default)]
pub struct StructRegistry {
    data: Vec<Rc<StructInfo>>,
}

impl StructRegistry {
    pub fn register(&mut self, name: &str, fields: Vec<(String, StaticType)>) -> StructId {
        let id = StructId(self.data.len());
        let info = Rc::new(StructInfo {
            id,
            name: Box::from(name),
            fields,
        });
        self.data.push(info);
        id
    }
}

impl Index<StructId> for StructRegistry {
    type Output = Rc<StructInfo>;

    fn index(&self, index: StructId) -> &Self::Output {
        self.data.index(index.0)
    }
}
