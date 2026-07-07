use crate::StaticType;
use crate::StaticType::Struct;
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
