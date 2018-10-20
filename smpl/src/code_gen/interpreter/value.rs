use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

use super::vm_i::FnHandle;

#[derive(Debug, PartialEq)]
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

impl Clone for Value {
    fn clone(&self) -> Value {
        match *self {
            Value::Int(i) => Value::Int(i),

            Value::Float(f) => Value::Float(f),

            Value::Bool(b) => Value::Bool(b),

            Value::String(ref s) => Value::String(s.clone()),

            Value::Array(ref a) => {
                Value::Array(
                    a
                    .into_iter()
                    .map(|rc| {
                        let borrow = rc.borrow();
                        Rc::new(RefCell::new((*borrow).clone()))
                    })
                    .collect())
            }

            Value::Function(f) => Value::Function(f),

            Value::Struct(ref s) => Value::Struct(s.clone()),

            Value::Unit => Value::Unit
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
                    let value = value.borrow();
                    write!(f, "{},", value)?
                }
                write!(f, " ]")
            }

            Value::Struct(ref s) => {
                write!(f, "{{ ")?;
                for (k, v) in s.fields() {
                    let v = v.borrow();
                    write!(f, "{}: {},", k, v)?;
                }
                write!(f, " }}")
            }

            Value::Function(..) => write!(f, "Function"), // TODO: Add more information

            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct(HashMap<String, Rc<RefCell<Value>>>);

impl Struct {
    pub fn new() -> Struct {
        Struct(HashMap::new())
    }

    pub fn set_field(&mut self, name: String, v: Value) -> Option<Value> {
        self.0
            .insert(name, Rc::new(RefCell::new(v)))
            .map(|rc| rc.borrow().clone())
    }

    pub fn get_field(&self, name: &str) -> Option<Value> {
        self.0.get(name).map(|rc| (*rc.borrow()).clone())
    }

    pub fn ref_field(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.0.get(name).map(|rc| rc.clone())
    }

    pub fn fields(&self) -> impl Iterator<Item = (&str, Rc<RefCell<Value>>)> {
        self.0.iter().map(|(k, v)| (k.as_str(), v.clone()))
    }
}
