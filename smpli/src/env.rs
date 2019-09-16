use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::value::Value;

#[derive(Debug, Clone)]
pub struct Env {
    env: HashMap<String, Rc<RefCell<Value>>>,
    tmp_store: HashMap<String, Rc<RefCell<Value>>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            env: HashMap::new(),
            tmp_store: HashMap::new(),
        }
    }

    pub fn map_value(&mut self, name: String, value: Value) -> Option<Value> {
        self.env
            .insert(name, Rc::new(RefCell::new(value)))
            .map(|rc| rc.borrow().clone())
    }

    pub fn map_tmp(&mut self, name: String, value: Value) -> Option<Value> {
        self.tmp_store
            .insert(name, Rc::new(RefCell::new(value)))
            .map(|rc| rc.borrow().clone())
    }

    pub fn get_value(&self, name: &str) -> Option<Value> {
        self.env.get(name).map(|r| (*r.borrow()).clone())
    }

    pub fn ref_value(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.env.get(name).map(|r| r.clone())
    }

    pub fn get_tmp(&self, name: &str) -> Option<Value> {
        self.tmp_store.get(name).map(|r| (*r.borrow()).clone())
    }

    pub fn ref_tmp(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.tmp_store.get(name).map(|r| r.clone())
    }

    pub fn wipe_tmps(&mut self) {
        self.tmp_store.clear();
    }
}
