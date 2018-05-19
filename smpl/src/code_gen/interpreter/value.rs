use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use analysis::{FieldId, FnId};
use analysis::smpl_type::*;

use super::vm_i::FnHandle;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Array(Vec<Rc<RefCell<Value>>>),
    Function(FnHandle),
    Struct(Struct),
    BuiltIn(Box<fn(Vec<Value>) -> Value>),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct(HashMap<FieldId, Rc<RefCell<Value>>>);

impl Struct {
    pub fn new() -> Struct {
        Struct(HashMap::new())
    }

    pub fn set_field(&mut self, id: FieldId, v: Value) -> Option<Value> {
        self.0.insert(id, Rc::new(RefCell::new(v))).map(|rc| rc.borrow().clone())
    }

    pub fn get_field(&self, id: FieldId) -> Option<Value> {
        self.0.get(&id).map(|rc| (*rc.borrow()).clone())
    }

    pub fn ref_field(&self, id: FieldId) -> Option<Rc<RefCell<Value>>> {
        self.0.get(&id).map(|rc| rc.clone())
    }
}
