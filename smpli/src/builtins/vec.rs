use failure::Error;
use smpl::{UnparsedModule, parse_module};

use std::cell::RefCell;
use std::rc::Rc;

use crate::*;

pub const MOD_VEC: &'static str = "vec";
pub const VEC_NEW: &'static str = "new";
pub const VEC_LEN: &'static str = "len";
pub const VEC_CONTAINS: &'static str = "contains";
pub const VEC_PUSH: &'static str = "push";
pub const VEC_INSERT: &'static str = "insert";
pub const VEC_GET: &'static str = "get";
pub const VEC_REMOVE: &'static str = "remove";

pub const VEC_DATA_KEY: &'static str = "__DATA";
pub const VEC_LEN_KEY: &'static str = "__LEN";

const VEC_DECLARATION: &'static str = include_str!("vec.smpl");

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(VEC_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(VEC_NEW, new)
        .add_builtin(VEC_LEN, len)
        .add_builtin(VEC_CONTAINS, contains)
        .add_builtin(VEC_PUSH, push)
        .add_builtin(VEC_INSERT, insert)
        .add_builtin(VEC_GET, get)
        .add_builtin(VEC_REMOVE, remove);

    module
}

#[derive(Fail, Debug)]
pub enum VecError {
    #[fail(display = "Index '{}' out of range ('{}')", _0, _1)]
    IndexOutOfRange(i64, usize),
}

fn new(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let _args: Option<Vec<Value>> = no_args!(args)?;

    let mut vec = Struct::new();
    vec.set_field(VEC_DATA_KEY.to_string(), Value::Array(Vec::new()));
    vec.set_field(VEC_LEN_KEY.to_string(), Value::Int(0));

    Ok(Value::Struct(vec))
}

fn len(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    let length = vec_struct.get_field(VEC_LEN_KEY).unwrap();

    Ok(length)
}

fn contains(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let to_search = args.pop().unwrap();

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

    let borrow = data.borrow();
    let data = irmatch!(*borrow; Value::Array(ref a) => a);

    for element in data {
        let element = element.borrow();
        if *element == to_search {
            return Ok(Value::Bool(true));
        }
    }

    Ok(Value::Bool(false))
}

fn insert(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(3, args)?;

    let to_insert = args.pop().unwrap();
    let index = args.pop().unwrap();
    let index = irmatch!(index; Value::Int(i) => i);

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    {
        let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

        let mut borrow = data.borrow_mut();
        let data = irmatch!(*borrow; Value::Array(ref mut a) => a);
        data.insert(index as usize, Rc::new(RefCell::new(to_insert)));
    }

    {
        let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
        let mut borrow = len.borrow_mut();
        let len = irmatch!(*borrow; Value::Int(ref mut i) => i);
        *len += 1;
    }

    Ok(Value::Struct(vec_struct))
}

fn push(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let to_insert = args.pop().unwrap();

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    {
        let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

        let mut borrow = data.borrow_mut();
        let data = irmatch!(*borrow; Value::Array(ref mut a) => a);
        data.push(Rc::new(RefCell::new(to_insert)));
    }

    {
        let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
        let mut borrow = len.borrow_mut();
        let len = irmatch!(*borrow; Value::Int(ref mut i) => i);
        *len += 1;
    }

    Ok(Value::Struct(vec_struct))
}

fn get(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let index = args.pop().unwrap();
    let smpl_index = irmatch!(index; Value::Int(i) => i) as i64;

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

    let borrow = data.borrow();
    let data = irmatch!(*borrow; Value::Array(ref a) => a);

    let index: usize = if smpl_index < 0 {
        return Err(VecError::IndexOutOfRange(smpl_index, data.len()))?;
    } else {
        smpl_index as usize
    };

    let item = data
        .get(index)
        .map(|rc| (*rc.borrow()).clone())
        .ok_or(VecError::IndexOutOfRange(smpl_index, data.len()))?;

    Ok(item)
}

fn remove(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let index = args.pop().unwrap();
    let smpl_index = irmatch!(index; Value::Int(i) => i) as i64;

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    {
        let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();
        let mut borrow = data.borrow_mut();
        let data = irmatch!(*borrow; Value::Array(ref mut a) => a);

        let index: usize = if smpl_index < 0 {
            return Err(VecError::IndexOutOfRange(smpl_index, data.len()))?;
        } else {
            smpl_index as usize
        };

        if index >= data.len() {
            return Err(VecError::IndexOutOfRange(smpl_index, data.len()))?;
        } else {
            data.remove(index);
        }
    }

    {
        let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
        let mut borrow = len.borrow_mut();
        let len = irmatch!(*borrow; Value::Int(ref mut i) => i);
        *len -= 1;
    }

    Ok(Value::Struct(vec_struct))
}

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod tests {

use crate::module::*;
use super::*;

macro_rules! wrap_input {
    ($input: expr) => {{ 
        UnparsedModule::anonymous($input)
    }}
}

#[test]
fn interpreter_vec_new() {
    let mod1 =
"
mod mod1;
use vec;

fn vec_new() {
    let v: vec::Vec(type int)= vec::new(type int)();
}
";
    let mut modules = vec![vm_module(), 
        VmModule::new(parse_module(wrap_input!(mod1)).unwrap())];

    let mut vm = AVM::new(Std::no_std(), modules).unwrap();

    let fn_handle = vm.query_module("mod1", "vec_new").unwrap().unwrap();

    let result = vm.eval_fn_sync(fn_handle).unwrap();

    assert_eq!(Value::Unit, result);
}

#[test]
fn interpreter_vec_push() {
    let mod1 =
"
mod mod1;
use vec;

fn test() -> int {
let v = vec::new(type int)();
v = vec::push(type int)(v, 123);
v = vec::push(type int)(v, 456);

return vec::len(type int)(v);
}
";
    let mut modules = vec![vm_module(), 
        VmModule::new(parse_module(wrap_input!(mod1)).unwrap())];

    let mut vm = AVM::new(Std::no_std(), modules).unwrap();

    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn_sync(fn_handle).unwrap();

    assert_eq!(Value::Int(2), result);
}

#[test]
fn interpreter_vec_get() {
    let mod1 =
"
mod mod1;
use vec;

fn test() -> int {
let v = vec::new(type int)();
v = vec::push(type int)(v, 123);
v = vec::push(type int)(v, 456);

let a = vec::get(type int)(v, 0);
let b = vec::get(type int)(v, 1);

return a * b;
}
";
    let mut modules = vec![vm_module(), 
        VmModule::new(parse_module(wrap_input!(mod1)).unwrap())];

    let mut vm = AVM::new(Std::no_std(), modules).unwrap();

    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn_sync(fn_handle).unwrap();

    assert_eq!(Value::Int(123 * 456), result);
}

#[test]
fn interpreter_vec_remove() {
    let mod1 =
"
mod mod1;
use vec;

fn test() -> int {
let v = vec::new(type int)();
v = vec::push(type int)(v, 123);
v = vec::push(type int)(v, 456);
v = vec::push(type int)(v, 789);

v = vec::remove(type int)(v, 1);

return vec::get(type int)(v, 1);
}
";
    let mut modules = vec![vm_module(), 
        VmModule::new(parse_module(wrap_input!(mod1)).unwrap())];

    let mut vm = AVM::new(Std::no_std(), modules).unwrap();

    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn_sync(fn_handle).unwrap();

    assert_eq!(Value::Int(789), result);
}

#[test]
fn interpreter_vec_insert() {
    let mod1 =
"
mod mod1;
use vec;

fn test() -> int {
let v = vec::new(type int)();
v = vec::push(type int)(v, 123);
v = vec::push(type int)(v, 456);

v = vec::insert(type int)(v, 0, 1337);

let a = vec::get(type int)(v, 0);

return a;
}
";
    let mut modules = vec![vm_module(), 
        VmModule::new(parse_module(wrap_input!(mod1)).unwrap())];

    let mut vm = AVM::new(Std::no_std(), modules).unwrap();

    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn_sync(fn_handle).unwrap();

    assert_eq!(Value::Int(1337), result);
}

#[test]
fn interpreter_vec_contains() {
    let mod1 =
"
mod mod1;
use vec;

fn test() -> bool {
let v = vec::new(type int)();
v = vec::push(type int)(v, 1);
v = vec::push(type int)(v, 2);
v = vec::push(type int)(v, 3);
v = vec::push(type int)(v, 4);
v = vec::push(type int)(v, 5);
v = vec::push(type int)(v, 6);
v = vec::push(type int)(v, 7);

return vec::contains(type int)(v, 5);
}

fn test2() -> bool {
let v = vec::new(type int)();
v = vec::push(type int)(v, 1);
v = vec::push(type int)(v, 2);
v = vec::push(type int)(v, 3);
v = vec::push(type int)(v, 4);
v = vec::push(type int)(v, 5);
v = vec::push(type int)(v, 6);
v = vec::push(type int)(v, 7);

return vec::contains(type int)(v, 20);
}
";
    let mut modules = vec![vm_module(), 
        VmModule::new(parse_module(wrap_input!(mod1)).unwrap())];
    let mut vm = AVM::new(Std::no_std(), modules).unwrap(); 
    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn_sync(fn_handle).unwrap();

    assert_eq!(Value::Bool(true), result);

    let fn_handle = vm.query_module("mod1", "test2").unwrap().unwrap();
    let result = vm.eval_fn_sync(fn_handle).unwrap();

    assert_eq!(Value::Bool(false), result);
}
}
