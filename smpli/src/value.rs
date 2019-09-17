use std::cell::{ RefCell, Ref, RefMut };
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use super::vm_i::FnHandle;

#[derive(Debug, PartialEq)]
pub struct ReferableValue(Rc<RefCell<Value>>);

impl ReferableValue {
    pub fn new(v: Value) -> ReferableValue {
        ReferableValue(Rc::new(RefCell::new(v)))
    }

    pub fn ref_val(&self) -> Rc<RefCell<Value>> {
        self.0.clone()
    }

    pub fn clone_value(&self) -> Value {
        self.0.borrow().clone()
    }

    pub fn ref_clone(&self) -> ReferableValue {
        ReferableValue(self.0.clone())
    }

    pub fn inner_ref(&self) -> Ref<Value> {
        self.0.borrow()
    }

    pub fn inner_ref_mut(&self) -> RefMut<Value> {
        self.0.borrow_mut()
    }

    pub fn hard_clone(&self) -> ReferableValue {
        ReferableValue::new(self.clone_value())
    }
}


#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Vec<ReferableValue>),
    Function(FnHandle),
    Struct(Struct),
    Unit,
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match *self {
            Value::Int(i) => Value::Int(i),

            Value::Float(f) => Value::Float(f),

            Value::Bool(b) => Value::Bool(b),

            Value::String(ref s) => Value::String(s.clone()),

            Value::Array(ref a) => Value::Array(
                a.into_iter()
                    .map(|rc| ReferableValue::new(rc.clone_value()))
                    .collect(),
            ),

            Value::Function(f) => Value::Function(f),

            Value::Struct(ref s) => Value::Struct(s.clone()),

            Value::Unit => Value::Unit,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(flt) => write!(f, "{}", flt),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(ref s) => write!(f, "{}", s),
            Value::Array(ref a) => {
                write!(f, "[ ")?;
                for value in a {
                    write!(f, "{},", value.0.borrow())?
                }
                write!(f, " ]")
            }

            Value::Struct(ref s) => {
                write!(f, "{{ ")?;
                for (k, v) in s.fields() {
                    write!(f, "{}: {},", k, v.0.borrow())?;
                }
                write!(f, " }}")
            }

            Value::Function(..) => write!(f, "Function"), // TODO: Add more information

            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Struct(HashMap<String, ReferableValue>);

impl Struct {
    pub fn new() -> Struct {
        Struct(HashMap::new())
    }

    pub fn set_field(&mut self, name: String, v: Value) -> Option<Value> {
        self.0
            .insert(name, ReferableValue::new(v))
            .map(|rv| rv.clone_value())
    }

    pub fn get_field(&self, name: &str) -> Option<Value> {
        self.0.get(name).map(|rv| rv.clone_value())
    }

    pub fn ref_field(&self, name: &str) -> Option<ReferableValue> {
        self.0.get(name).map(|rc| rc.ref_clone())
    }

    pub fn fields(&self) -> impl Iterator<Item = (&str, ReferableValue)> {
        self.0.iter().map(|(k, v)| (k.as_str(), v.ref_clone()))
    }
}

impl Clone for Struct {
    fn clone(&self) -> Struct {
        Struct(
            self.0
                .iter()
                .map(|(key, rc)| {
                    (key.clone(), rc.hard_clone())
                })
                .collect(),
        )
    }
}
