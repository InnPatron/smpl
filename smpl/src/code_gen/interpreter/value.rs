use std::collections::HashMap;

use analysis::{FieldId, FnId};
use analysis::smpl_type::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
    Function(FnId),
    Struct(Struct),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct(HashMap<FieldId, Value>);

impl Struct {
    pub fn new() -> Struct {
        Struct(HashMap::new())
    }

    pub fn set_field(&mut self, id: FieldId, v: Value) -> Option<Value> {
        self.0.insert(id, v)
    }

    pub fn get_field(&self, id: FieldId) -> Option<&Value> {
        self.0.get(&id)
    }

    pub fn get_field_mut(&mut self, id: FieldId) -> Option<&mut Value> {
        self.0.get_mut(&id)
    }
}
