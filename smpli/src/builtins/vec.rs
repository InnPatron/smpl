use failure::Error;
use smpl::prelude::{UnparsedModule, parse_module};

use std::cell::RefCell;
use std::rc::Rc;

use crate::*;

pub const MOD_VEC: &'static str = "vec";
pub const VEC_NEW: &'static str = "new";
pub const VEC_LEN: &'static str = "len";
pub const VEC_CONTAINS: &'static str = "contains";
pub const VEC_PUSH: &'static str = "push";
pub const VEC_INSERT: &'static str = "insert";
pub const VEC_GET_VALUE: &'static str = "get_value";
pub const VEC_GET: &'static str = "get";
pub const VEC_REMOVE: &'static str = "remove";
pub const VEC_CLEAR: &'static str = "clear";

pub const VEC_DATA_KEY: &'static str = "__DATA";
pub const VEC_LEN_KEY: &'static str = "__LEN";

const VEC_DECLARATION: &'static str = include_str!("vec.smpl");

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(VEC_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(VEC_NEW,       super::erase(new))
        .add_builtin(VEC_LEN,       super::erase(len))
        .add_builtin(VEC_CONTAINS,  super::erase(contains))
        .add_builtin(VEC_PUSH,      super::erase(push))
        .add_builtin(VEC_INSERT,    super::erase(insert))
        .add_builtin(VEC_GET_VALUE, super::erase(get_value))
        .add_builtin(VEC_GET,       super::erase(get))
        .add_builtin(VEC_REMOVE,    super::erase(remove))
        .add_builtin(VEC_CLEAR,     super::erase(clear));

    module
}

#[derive(Fail, Debug)]
pub enum VecError {
    #[fail(display = "Index '{}' out of range ('{}')", _0, _1)]
    IndexOutOfRange(i64, usize),
}

async fn new(args: Vec<Value>) -> Result<Value, Error> {
    no_args!(args)?;

    let mut vec = Struct::new();
    vec.set_field(VEC_DATA_KEY.to_string(), Value::Array(Array::new()));
    vec.set_field(VEC_LEN_KEY.to_string(), Value::Int(0));

    Ok(Value::Struct(vec))
}

async fn len(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    let length = vec_struct.get_field(VEC_LEN_KEY).unwrap();

    Ok(length)
}

async fn contains(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let to_search = args.pop().unwrap();

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

    let borrow = data.inner_ref();
    let data = irmatch!(*borrow; Value::Array(ref a) => a);

    for element in data {
        let element = element.inner_ref();
        if *element == to_search {
            return Ok(Value::Bool(true));
        }
    }

    Ok(Value::Bool(false))
}

async fn insert(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(3, args)?;

    let to_insert = args.pop().unwrap();
    let index = args.pop().unwrap();
    let index = irmatch!(index; Value::Int(i) => i);

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    {
        let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

        let mut borrow = data.inner_ref_mut();
        let data = irmatch!(*borrow; Value::Array(ref mut a) => a);
        data.insert(index as usize, to_insert);
    }

    {
        let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
        let mut borrow = len.inner_ref_mut();
        let len = irmatch!(*borrow; Value::Int(ref mut i) => i);
        *len += 1;
    }

    Ok(Value::Struct(vec_struct))
}

async fn push(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let to_insert = args.pop().unwrap();

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    {
        let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

        let mut borrow = data.inner_ref_mut();
        let data = irmatch!(*borrow; Value::Array(ref mut a) => a);
        data.push(to_insert);
    }

    {
        let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
        let mut borrow = len.inner_ref_mut();
        let len = irmatch!(*borrow; Value::Int(ref mut i) => i);
        *len += 1;
    }

    Ok(Value::Struct(vec_struct))
}

async fn get_value(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let index = args.pop().unwrap();
    let smpl_index = irmatch!(index; Value::Int(i) => i) as i64;

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

    let borrow = data.inner_ref();
    let data = irmatch!(*borrow; Value::Array(ref a) => a);

    let index: usize = if smpl_index < 0 {
        return Err(VecError::IndexOutOfRange(smpl_index, data.len()))?;
    } else {
        smpl_index as usize
    };

    let item = data
        .get(index)
        .map(|rc| rc.clone_value())
        .ok_or(VecError::IndexOutOfRange(smpl_index, data.len()))?;

    Ok(item)
}

async fn get(args: Vec<Value>) -> Result<Value, Error> {
    use super::option;

    let mut args = exact_args!(2, args)?;

    let index = args.pop().unwrap();
    let smpl_index = irmatch!(index; Value::Int(i) => i) as i64;

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

    let borrow = data.inner_ref();
    let data = irmatch!(*borrow; Value::Array(ref a) => a);

    let index: usize = if smpl_index < 0 {
        return Ok(option::make_none());
    } else {
        smpl_index as usize
    };

    let item = data
        .get(index)
        .map(|rc| rc.clone_value())
        .ok_or(VecError::IndexOutOfRange(smpl_index, data.len()))?;

    Ok(option::make_some(item))
}

async fn remove(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let index = args.pop().unwrap();
    let smpl_index = irmatch!(index; Value::Int(i) => i) as i64;

    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    {
        let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();
        let mut borrow = data.inner_ref_mut();
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
        let mut borrow = len.inner_ref_mut();
        let len = irmatch!(*borrow; Value::Int(ref mut i) => i);
        *len -= 1;
    }

    Ok(Value::Struct(vec_struct))
}

async fn clear(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let vec_struct = args.pop().unwrap();
    let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

    // Clear the internal buffer
    {
        let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

        let mut borrow = data.inner_ref_mut();
        let data = irmatch!(*borrow; Value::Array(ref mut a) => a);
        data.clear();
    }

    // Set the length to 0
    {
        let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
        let mut borrow = len.inner_ref_mut();
        let len = irmatch!(*borrow; Value::Int(ref mut i) => i);
        *len = 0 ;
    }

    Ok(Value::Struct(vec_struct))
}

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod tests {

use smpl::prelude::*;
use super::*;

macro_rules! wrap_input {
    ($input: expr) => {{
        UnparsedModule::anonymous($input)
    }}
}

macro_rules! vec_test {
    ($mod: expr, $mod_name: expr, $fn_name: expr, $args: expr) => {{

        let mut modules = vec![vm_module(), super::super::option::vm_module(),
            VmModule::new(parse_module(wrap_input!($mod)).unwrap())];

        let mut vm = AVM::new(Std::no_std(), modules).unwrap();

        let fn_handle = vm.query_module($mod_name, $fn_name).unwrap().unwrap();
        let result = vm.spawn_executor(fn_handle, $args, SpawnOptions {
            type_check: false
        })
            .unwrap()
            .execute_sync()
            .unwrap();
        result
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
    let result = vec_test!(mod1, "mod1", "vec_new", vec![]);

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

    let result = vec_test!(mod1, "mod1", "test", vec![]);

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

let a = vec::get_value(type int)(v, 0);
let b = vec::get_value(type int)(v, 1);

return a * b;
}
";


    let result = vec_test!(mod1, "mod1", "test", vec![]);

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

return vec::get_value(type int)(v, 1);
}
";

    let result = vec_test!(mod1, "mod1", "test", vec![]);

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

let a = vec::get_value(type int)(v, 0);

return a;
}
";

    let result = vec_test!(mod1, "mod1", "test", vec![]);

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

    let result = vec_test!(mod1, "mod1", "test", vec![]);

    assert_eq!(Value::Bool(true), result);

    let result = vec_test!(mod1, "mod1", "test2", vec![]);

    assert_eq!(Value::Bool(false), result);
}

#[test]
fn interpreter_vec_clear() {
    let mod1 =
"
mod mod1;
use vec;

fn test() -> int {
let v = vec::new(type int)();
v = vec::push(type int)(v, 123);
v = vec::push(type int)(v, 456);

let cleared = vec::clear(type int)(v);

return vec::len(type int)(cleared);
}
";

    let result = vec_test!(mod1, "mod1", "test", vec![]);

    assert_eq!(Value::Int(0), result);
}
}
