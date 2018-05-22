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
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct(HashMap<String, Rc<RefCell<Value>>>);

impl Struct {
    pub fn new() -> Struct {
        Struct(HashMap::new())
    }

    pub fn set_field(&mut self, name: String, v: Value) -> Option<Value> {
        self.0.insert(name, Rc::new(RefCell::new(v))).map(|rc| rc.borrow().clone())
    }

    pub fn get_field(&self, name: &str) -> Option<Value> {
        self.0.get(name).map(|rc| (*rc.borrow()).clone())
    }

    pub fn ref_field(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.0.get(name).map(|rc| rc.clone())
    }

    pub fn fields(&self) -> impl Iterator<Item=(&str, Rc<RefCell<Value>>)> {
        self.0.iter().map(|(k, v)| (k.as_str(), v.clone()))
    }
}
