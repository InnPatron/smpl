use failure::Fail;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use strfmt::strfmt;

use ast::Module;
use parser::parse_module;

use code_gen::interpreter::*;

pub const MOD_VEC: &'static str = "vec_{item_type}";
pub const VEC_NEW: &'static str = "new";
pub const VEC_LEN: &'static str = "len";
pub const VEC_CONTAINS: &'static str = "contains";
pub const VEC_PUSH: &'static str = "push";
pub const VEC_INSERT: &'static str = "insert";
pub const VEC_GET: &'static str = "get";
pub const VEC_REMOVE: &'static str = "remove";

pub const VEC_DATA_KEY: &'static str = "__DATA";
pub const VEC_LEN_KEY: &'static str = "__LEN";

const VEC_FMT_ITEM_TYPE: &'static str = "item_type";
const VEC_FMT_ITEM_TYPE_MOD: &'static str = "item_type_mod";
const VEC_FMT_ITEM_USE: &'static str = "item_mod_use";

const VEC_DECLARATION: &'static str = include_str!("vec.smpl");

pub fn include(modules: &mut Vec<Module>, item_type_mod: Option<&str>, item_type: &str) {
    let item_mod_use = match item_type_mod {
        Some(str) => format!("use {};", str),
        None => "".to_string(),
    };

    let item_type_mod = match item_type_mod {
        Some(str) => format!("{}::", str),
        None => "".to_string(),
    };

    let mut vars = HashMap::new();
    vars.insert(VEC_FMT_ITEM_TYPE.to_string(), item_type);
    vars.insert(VEC_FMT_ITEM_TYPE_MOD.to_string(), &item_type_mod);
    vars.insert(VEC_FMT_ITEM_USE.to_string(), &item_mod_use);

    let decl = strfmt(&VEC_DECLARATION, &vars).unwrap();
    modules.push(parse_module(&decl).unwrap());
}

pub fn add<MAP: BuiltinMap>(vm: &mut MAP, item_type: &str) {
    let mut vars = HashMap::new();
    vars.insert(VEC_FMT_ITEM_TYPE.to_string(), item_type);

    let mod_name = strfmt(MOD_VEC, &vars).unwrap();

    vm.insert_builtin(&mod_name, VEC_NEW, Box::new(New))
        .unwrap();
    vm.insert_builtin(&mod_name, VEC_LEN, Box::new(Len))
        .unwrap();
    vm.insert_builtin(&mod_name, VEC_CONTAINS, Box::new(Contains))
        .unwrap();
    vm.insert_builtin(&mod_name, VEC_PUSH, Box::new(Push))
        .unwrap();
    vm.insert_builtin(&mod_name, VEC_INSERT, Box::new(Insert))
        .unwrap();
    vm.insert_builtin(&mod_name, VEC_GET, Box::new(Get))
        .unwrap();
    vm.insert_builtin(&mod_name, VEC_REMOVE, Box::new(Remove))
        .unwrap();
}

pub struct New;

impl BuiltinFn for New {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        let mut vec = Struct::new();
        vec.set_field(VEC_DATA_KEY.to_string(), Value::Array(Vec::new()));
        vec.set_field(VEC_LEN_KEY.to_string(), Value::Int(0));

        Ok(Value::Struct(vec))
    }
}

pub struct Len;

impl BuiltinFn for Len {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        let mut args = args.unwrap();
        let vec_struct = args.pop().unwrap();
        let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

        let length = vec_struct.get_field(VEC_LEN_KEY).unwrap();

        Ok(length)
    }
}

pub struct Contains;

impl BuiltinFn for Contains {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        let mut args = args.unwrap();

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
}

pub struct Insert;

impl BuiltinFn for Insert {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        let mut args = args.unwrap();

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
}

pub struct Push;

impl BuiltinFn for Push {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        let mut args = args.unwrap();

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
}

pub struct Get;

impl BuiltinFn for Get {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        let mut args = args.unwrap();

        let index = args.pop().unwrap();
        let index: usize = irmatch!(index; Value::Int(i) => i) as usize;

        let vec_struct = args.pop().unwrap();
        let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

        let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();

        let borrow = data.borrow();
        let data = irmatch!(*borrow; Value::Array(ref a) => a);

        Ok(data.get(index).map(|rc| (*rc.borrow()).clone()).unwrap())
    }
}

pub struct Remove;

impl BuiltinFn for Remove {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        let mut args = args.unwrap();

        let index = args.pop().unwrap();
        let index: usize = irmatch!(index; Value::Int(i) => i) as usize;

        let vec_struct = args.pop().unwrap();
        let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

        {
            let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();
            let mut borrow = data.borrow_mut();
            let data = irmatch!(*borrow; Value::Array(ref mut a) => a);

            data.remove(index);
        }

        {
            let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
            let mut borrow = len.borrow_mut();
            let len = irmatch!(*borrow; Value::Int(ref mut i) => i);
            *len -= 1;
        }

        Ok(Value::Struct(vec_struct))
    }
}

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod tests {

    use super::*;

    #[test]
    fn interpreter_vec_new() {
        let mod1 =
"
mod mod1;
use vec_int;

fn vec_new() {
    let v = vec_int::new();
}
";
        let mut modules = vec![parse_module(mod1).unwrap()];
        include(&mut modules, None, "int");

        let mut vm = AVM::new(modules).unwrap();
        add(&mut vm, "int");

        let fn_handle = vm.query_module("mod1", "vec_new").unwrap().unwrap();

        let result = vm.eval_fn_sync(fn_handle).unwrap();

        assert_eq!(Value::Unit, result);
    }

    #[test]
    fn interpreter_vec_push() {
        let mod1 =
"
mod mod1;
use vec_int;

fn test() -> int {
    let v = vec_int::new();
    v = vec_int::push(v, 123);
    v = vec_int::push(v, 456);
    
    return vec_int::len(v);
}
";
        let mut modules = vec![parse_module(mod1).unwrap()];
        include(&mut modules, None, "int");

        let mut vm = AVM::new(modules).unwrap();
        add(&mut vm, "int");

        let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

        let result = vm.eval_fn_sync(fn_handle).unwrap();

        assert_eq!(Value::Int(2), result);
    }

    #[test]
    fn interpreter_vec_get() {
        let mod1 =
"
mod mod1;
use vec_int;

fn test() -> int {
    let v = vec_int::new();
    v = vec_int::push(v, 123);
    v = vec_int::push(v, 456);
    
    let a = vec_int::get(v, 0);
    let b = vec_int::get(v, 1);

    return a * b;
}
";
        let mut modules = vec![parse_module(mod1).unwrap()];
        include(&mut modules, None, "int");

        let mut vm = AVM::new(modules).unwrap();
        add(&mut vm, "int");

        let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

        let result = vm.eval_fn_sync(fn_handle).unwrap();

        assert_eq!(Value::Int(123 * 456), result);
    }

    #[test]
    fn interpreter_vec_remove() {
        let mod1 =
"
mod mod1;
use vec_int;

fn test() -> int {
    let v = vec_int::new();
    v = vec_int::push(v, 123);
    v = vec_int::push(v, 456);
    v = vec_int::push(v, 789);
    
    v = vec_int::remove(v, 1);

    return vec_int::get(v, 1);
}
";
        let mut modules = vec![parse_module(mod1).unwrap()];
        include(&mut modules, None, "int");

        let mut vm = AVM::new(modules).unwrap();
        add(&mut vm, "int");

        let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

        let result = vm.eval_fn_sync(fn_handle).unwrap();

        assert_eq!(Value::Int(789), result);
    }

    #[test]
    fn interpreter_vec_insert() {
        let mod1 =
"
mod mod1;
use vec_int;

fn test() -> int {
    let v = vec_int::new();
    v = vec_int::push(v, 123);
    v = vec_int::push(v, 456);

    v = vec_int::insert(v, 0, 1337);
    
    let a = vec_int::get(v, 0);

    return a;
}
";
        let mut modules = vec![parse_module(mod1).unwrap()];
        include(&mut modules, None, "int");

        let mut vm = AVM::new(modules).unwrap();
        add(&mut vm, "int");

        let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

        let result = vm.eval_fn_sync(fn_handle).unwrap();

        assert_eq!(Value::Int(1337), result);
    }

    #[test]
    fn interpreter_vec_contains() {
        let mod1 =
"
mod mod1;
use vec_int;

fn test() -> bool {
    let v = vec_int::new();
    v = vec_int::push(v, 1);
    v = vec_int::push(v, 2);
    v = vec_int::push(v, 3);
    v = vec_int::push(v, 4);
    v = vec_int::push(v, 5);
    v = vec_int::push(v, 6);
    v = vec_int::push(v, 7);

    return vec_int::contains(v, 5);
}

fn test2() -> bool {
    let v = vec_int::new();
    v = vec_int::push(v, 1);
    v = vec_int::push(v, 2);
    v = vec_int::push(v, 3);
    v = vec_int::push(v, 4);
    v = vec_int::push(v, 5);
    v = vec_int::push(v, 6);
    v = vec_int::push(v, 7);

    return vec_int::contains(v, 20);
}
";
        let mut modules = vec![parse_module(mod1).unwrap()];
        include(&mut modules, None, "int");

        let mut vm = AVM::new(modules).unwrap();
        add(&mut vm, "int");

        let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

        let result = vm.eval_fn_sync(fn_handle).unwrap();

        assert_eq!(Value::Bool(true), result);

        let fn_handle = vm.query_module("mod1", "test2").unwrap().unwrap();
        let result = vm.eval_fn_sync(fn_handle).unwrap();

        assert_eq!(Value::Bool(false), result);
    }
}
