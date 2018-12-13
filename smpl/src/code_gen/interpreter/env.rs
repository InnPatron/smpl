use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use crate::analysis::*;

use super::value::Value;

#[derive(Debug, Clone)]
pub struct Env {
    env: HashMap<String, Rc<RefCell<Value>>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            env: HashMap::new(),
        }
    }

    pub fn map_var(&mut self, var: VarId, value: Value) -> Option<Value> {
        self.map_value(Env::var_id(var), value)
    }

    pub fn map_tmp(&mut self, tmp: TmpId, value: Value) -> Option<Value> {
        self.map_value(Env::tmp_id(tmp), value)
    }

    pub fn map_value(&mut self, name: String, value: Value) -> Option<Value> {
        self.env
            .insert(name, Rc::new(RefCell::new(value)))
            .map(|rc| rc.borrow().clone())
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.env.get(name).map(|r| (*r.borrow()).clone())
    }

    pub fn get_var(&self, id: VarId) -> Option<Value> {
        self.get(&Env::var_id(id))
    }

    pub fn get_tmp(&self, id: TmpId) -> Option<Value> {
        self.get(&Env::tmp_id(id))
    }

    pub fn ref_value(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.env.get(name).map(|r| r.clone())
    }

    pub fn ref_var(&self, id: VarId) -> Option<Rc<RefCell<Value>>> {
        self.ref_value(&Env::var_id(id))
    }

    pub fn ref_tmp(&self, id: TmpId) -> Option<Rc<RefCell<Value>>> {
        self.ref_value(&Env::tmp_id(id))
    }

    fn tmp_id(id: TmpId) -> String {
        format!("_tmp_{}", id.raw())
    }

    fn var_id(id: VarId) -> String {
        format!("_var_{}", id.raw())
    }
}
