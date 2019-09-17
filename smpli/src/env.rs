use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::value::{ Value, ReferableValue };

#[derive(Debug)]
pub struct Env {
    env: HashMap<String, ReferableValue>,
    tmp_store: HashMap<String, ReferableValue>,
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
            .insert(name, ReferableValue::new(value))
            .map(|rv| rv.clone_value())
    }

    pub fn map_tmp(&mut self, name: String, value: Value) -> Option<Value> {
        self.tmp_store
            .insert(name, ReferableValue::new(value))
            .map(|rv| rv.clone_value())
    }

    pub fn get_value(&self, name: &str) -> Option<Value> {
        self.env.get(name).map(|r| r.clone_value())
    }

    pub fn ref_value(&self, name: &str) -> Option<ReferableValue> {
        self.env.get(name).map(|r| r.ref_clone())
    }

    pub fn get_tmp(&self, name: &str) -> Option<Value> {
        self.tmp_store.get(name).map(|r| r.clone_value())
    }

    pub fn ref_tmp(&self, name: &str) -> Option<ReferableValue> {
        self.tmp_store.get(name).map(|r| r.ref_clone())
    }

    pub fn wipe_tmps(&mut self) {
        self.tmp_store.clear();
    }
}
