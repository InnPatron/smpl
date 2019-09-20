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
    Array(Array),
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

    pub fn new_init(init: HashMap<String, ReferableValue>) -> Struct {
        Struct(init)
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

#[derive(Debug, PartialEq)]
pub struct Array {
    vec: Vec<ReferableValue>
}

impl Array {
    pub fn new() -> Array {
        Array {
            vec: Vec::new(),
        }
    }

    pub fn new_init<T: IntoIterator<Item=Value>>(iter: T) -> Array {
        Array {
            vec: iter.into_iter().map(|v| ReferableValue::new(v)).collect(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Array {
        Array {
            vec: Vec::with_capacity(capacity)
        }
    }

    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.vec.reserve(additional);
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.vec.reserve_exact(additional)
    }

    pub fn shrink_to_fit(&mut self) {
        self.vec.shrink_to_fit();
    }

    pub fn into_boxed_slice(self) -> Box<[ReferableValue]> {
        self.vec.into_boxed_slice()
    }

    pub fn truncate(&mut self, len: usize) {
        self.vec.truncate(len)
    }

    pub fn as_slice(&self) -> &[ReferableValue] {
        self.vec.as_slice()
    }

    pub fn as_mut_slice(&mut self) -> &mut [ReferableValue] {
        self.vec.as_mut_slice()
    }

    pub fn insert(&mut self, index: usize, value: ReferableValue) {
        self.vec.insert(index, value);
    }

    pub fn remove(&mut self, index: usize) -> ReferableValue {
        self.vec.remove(index)
    }

    pub fn push(&mut self, value: ReferableValue) {
        self.vec.push(value);
    }

    pub fn pop(&mut self) -> Option<ReferableValue> {
        self.vec.pop()
    }

    pub fn append(&mut self, other: &mut Array) {
        self.vec.append(&mut other.vec);
    }

    pub fn clear(&mut self) {
        self.vec.clear();
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    } 
}

impl std::ops::Deref for Array {
    type Target = [ReferableValue];

    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl std::iter::FromIterator<ReferableValue> for Array {
    fn from_iter<I: IntoIterator<Item=ReferableValue>>(iter: I) -> Self {
        let mut vec = Vec::new();

        Array::new_init(vec)
    }
}

impl std::iter::IntoIterator for Array {
    type Item = ReferableValue;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}

impl<'a> IntoIterator for &'a Array {
    type Item = &'a ReferableValue;
    type IntoIter = std::slice::Iter<'a, ReferableValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter()
    }
}

impl<'a> IntoIterator for &'a mut Array {
    type Item = &'a mut ReferableValue;
    type IntoIter = std::slice::IterMut<'a, ReferableValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter_mut()
    }
}
