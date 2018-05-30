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

const VEC_DECLARATION: &'static str =
"
mod vec_{item_type};

{item_mod_use}
struct Vec_{item_type} {{ }}

builtin fn new() -> Vec_{item_type};
builtin fn len(v: Vec_{item_type}) -> i32;
builtin fn contains(v: Vec_{item_type}, val: {item_type_mod}{item_type}) -> bool;
builtin fn push(v: Vec_{item_type}, val: {item_type_mod}{item_type}) -> Vec_{item_type};
builtin fn insert(v: Vec_{item_type}, i: i32, val: {item_type_mod}{item_type}) -> Vec_{item_type};
builtin fn get(v: Vec_{item_type}, i: i32) -> {item_type_mod}{item_type};
builtin fn remove(v: Vec_{item_type}, i: i32) -> Vec_{item_type};
";

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

pub fn add(vm: &mut VM, item_type: &str) {
    let mut vars = HashMap::new();
    vars.insert(VEC_FMT_ITEM_TYPE.to_string(), item_type);

    let mod_name = strfmt(MOD_VEC, &vars).unwrap();

    vm.insert_builtin(&mod_name, VEC_NEW, Box::new(New));
    vm.insert_builtin(&mod_name, VEC_LEN, Box::new(Len));
    vm.insert_builtin(&mod_name, VEC_CONTAINS, Box::new(Contains));
    vm.insert_builtin(&mod_name, VEC_PUSH, Box::new(Push));
    vm.insert_builtin(&mod_name, VEC_INSERT, Box::new(Insert));
    vm.insert_builtin(&mod_name, VEC_GET, Box::new(Get));
    vm.insert_builtin(&mod_name, VEC_REMOVE, Box::new(Remove));
}

pub struct New;

impl BuiltInFn for New {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut vec = Struct::new();
        vec.set_field(VEC_DATA_KEY.to_string(), Value::Array(Vec::new()));
        vec.set_field(VEC_LEN_KEY.to_string(), Value::Int(0));

        Value::Struct(vec)
    }
}

pub struct Len;

impl BuiltInFn for Len {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let vec_struct = args.pop().unwrap();
        let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

        let length = vec_struct.get_field(VEC_LEN_KEY).unwrap();

        length
    }
}

pub struct Contains;

impl BuiltInFn for Contains {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
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
                return Value::Bool(true);
            }
        }

        Value::Bool(false)
    }
}

pub struct Insert;

impl BuiltInFn for Insert {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let to_insert = args.pop().unwrap();
        let index = args.pop().unwrap();
        let index = irmatch!(index; Value::Int(i) => i);

        let vec_struct = args.pop().unwrap();
        let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

        {
            let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();
            
            let mut borrow = data.borrow_mut();
            let mut data = irmatch!(*borrow; Value::Array(ref mut a) => a);
            data.insert(index as usize, Rc::new(RefCell::new(to_insert)));
        }

        {
            let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
            let mut borrow = len.borrow_mut();
            let mut len = irmatch!(*borrow; Value::Int(ref mut i) => i);
            *len += 1;
        }

        Value::Struct(vec_struct)
    }
}

pub struct Push;

impl BuiltInFn for Push {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let to_insert = args.pop().unwrap();

        let vec_struct = args.pop().unwrap();
        let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

        {
            let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();
            
            let mut borrow = data.borrow_mut();
            let mut data = irmatch!(*borrow; Value::Array(ref mut a) => a);
            data.push(Rc::new(RefCell::new(to_insert)));
        }

        {
            let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
            let mut borrow = len.borrow_mut();
            let mut len = irmatch!(*borrow; Value::Int(ref mut i) => i);
            *len += 1;
        }

        Value::Struct(vec_struct)
    }
}

pub struct Get;

impl BuiltInFn for Get {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let index = args.pop().unwrap();
        let index: usize = irmatch!(index; Value::Int(i) => i) as usize;

        let vec_struct = args.pop().unwrap();
        let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

        let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();
        
        let borrow = data.borrow();
        let data = irmatch!(*borrow; Value::Array(ref a) => a);
        
        data.get(index).map(|rc| (*rc.borrow()).clone()).unwrap()
    }
}

pub struct Remove;

impl BuiltInFn for Remove {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let index = args.pop().unwrap();
        let index: usize = irmatch!(index; Value::Int(i) => i) as usize;

        let vec_struct = args.pop().unwrap();
        let vec_struct = irmatch!(vec_struct; Value::Struct(s) => s);

        {
            let data = vec_struct.ref_field(VEC_DATA_KEY).unwrap();
            let mut borrow = data.borrow_mut();
            let mut data = irmatch!(*borrow; Value::Array(ref mut a) => a);

            data.remove(index);
        }

        {
            let len = vec_struct.ref_field(VEC_LEN_KEY).unwrap();
            let mut borrow = len.borrow_mut();
            let mut len = irmatch!(*borrow; Value::Int(ref mut i) => i);
            *len -= 1;
        }


        Value::Struct(vec_struct)
    }
}
