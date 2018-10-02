use parser::parse_module;
use code_gen::interpreter::*;

use parser::parse_module;
use code_gen::interpreter::*;

struct Add;

impl BuiltinFn for Add {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let args = args.unwrap();
        let lhs = args.get(0).unwrap();
        let rhs = args.get(1).unwrap();

        let lhs = irmatch!(lhs; Value::Int(i) => i);
        let rhs = irmatch!(rhs; Value::Int(i) => i);

        return Value::Int(lhs + rhs);
    }
}

struct VarArgSum;

impl BuiltinFn for VarArgSum {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let args = args.unwrap();

        let mut sum = 0;

        for arg in args.iter() {
            let value = irmatch!(arg; Value::Int(i) => i);
            sum += value;
        }

        return Value::Int(sum);
    }
} 

#[test]
fn interpreter_basic() {
    let mod1 =
"mod mod1;

fn test(a: int, b: int) -> int {
return a + b;
}";

    let modules = vec![parse_module(mod1).unwrap()];

    let vm = VM::new(modules).unwrap();
    
    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn_args(fn_handle, Some(vec![Value::Int(5), Value::Int(7)]));

    assert_eq!(Value::Int(12), result);
}

#[test]
fn interpreter_struct() {
    let mod1 =
"mod mod1;

struct T {
f: int
}

fn test(a: int, b: int) -> T {
return init T { f: a + b };
}";

    let modules = vec![parse_module(mod1).unwrap()];

    let vm = VM::new(modules).unwrap();
    
    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn_args(fn_handle, Some(vec![Value::Int(5), Value::Int(7)]));

    let result = irmatch!(result; Value::Struct(s) => s.get_field("f").unwrap());
    let result = irmatch!(result; Value::Int(i) => i);

    assert_eq!(12, result);
}

#[test]
fn interpreter_builtin() {
    let mod1 =
"mod mod1;

builtin fn add(a: int, b: int) -> int;

fn test(a: int, b: int) -> int {
return add(a, b);
}";

    let modules = vec![parse_module(mod1).unwrap()];

    let mut vm = VM::new(modules).unwrap();
    vm.insert_builtin("mod1", "add", Box::new(Add)).unwrap();
    
    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn_args(fn_handle, Some(vec![Value::Int(5), Value::Int(7)]));

    assert_eq!(Value::Int(12), result);
}

#[test]
fn interpreter_builtin_unchecked_params() {
    let mod1 =
"mod mod1;

builtin fn sum(UNCHECKED) -> int;

fn test(a: int, b: int) -> int {
return sum(a, b, 100, 2);
}";

    let modules = vec![parse_module(mod1).unwrap()];


    let mut vm = VM::new(modules).unwrap();
    vm.insert_builtin("mod1", "sum", Box::new(VarArgSum)).unwrap();
    
    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn_args(fn_handle, Some(vec![Value::Int(5), Value::Int(7)]));

    assert_eq!(Value::Int(114), result);
}

#[test]
fn interpreter_intermod_builtin() {
    let mod1 =
"mod mod1;

builtin fn add(a: int, b: int) -> int;

fn test(a: int, b: int) -> int {
return add(a, b);
}";

    let mod2 =
"mod mod2;

use mod1;

fn test2() -> int {
return mod1::add(1, 2);
}
";

    let modules = vec![parse_module(mod1).unwrap(), parse_module(mod2).unwrap()];

    let mut vm = VM::new(modules).unwrap();
    vm.insert_builtin("mod1", "add", Box::new(Add)).unwrap();
    
    let fn_handle = vm.query_module("mod2", "test2").unwrap().unwrap();

    let result = vm.eval_fn(fn_handle);

    assert_eq!(Value::Int(3), result);
}

#[test]
fn interpreter_field_access() {
    let mod1 =
"mod mod1;

struct T {
f: int
}

fn test() -> int {
let t: T = init T { f: 1335 };

t.f = t.f + 1;

return t.f + 1;
}

";

    let modules = vec![parse_module(mod1).unwrap()];

    let vm = VM::new(modules).unwrap();
    
    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn(fn_handle);

    assert_eq!(Value::Int(1337), result);
}

#[test]
fn interpreter_array() {
    let mod1 =
"mod mod1;

fn test() -> int {
let t: [int; 5] = [1, 2, 3, 4, 5];

return t[0] + t[1] + t[2] + t[3] + t[4];
}

";

    let modules = vec![parse_module(mod1).unwrap()];

    let vm = VM::new(modules).unwrap();
    
    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn(fn_handle);

    assert_eq!(Value::Int(1 + 2 + 3 + 4 + 5), result);
}

#[test]
fn interpreter_fn_value() {
    let mod1 =
"mod mod1;

fn test2(a: int) -> int {
return a * 2;
}

fn test() -> int {
let func: fn(int) -> int = test2;

return func(210);
}

";

    let modules = vec![parse_module(mod1).unwrap()];

    let vm = VM::new(modules).unwrap();
    
    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn(fn_handle);

    assert_eq!(Value::Int(420), result);
}

#[test]
fn interpreter_optional_local_type_annotation() {
    let mod1 =
"mod mod1;

fn test2(a: int) -> int {
return a * 2;
}

fn test() -> int {
let func = test2;

return func(210);
}

";

    let modules = vec![parse_module(mod1).unwrap()];

    let vm = VM::new(modules).unwrap();
    
    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn(fn_handle);

    assert_eq!(Value::Int(420), result);
}

#[test]
fn interpreter_recursive_fn_call() {
    let mod1 =
"
mod mod1;

fn recurse(i: int) -> int {
if (i == 0) {
    return 0;
} else {
    return i + recurse(i - 1);
}
}
";
    let modules = vec![parse_module(mod1).unwrap()];

    let vm = VM::new(modules).unwrap();

    let fn_handle = vm.query_module("mod1", "recurse").unwrap().unwrap();

    let result = vm.eval_fn_args(fn_handle, Some(vec![Value::Int(2)]));

    assert_eq!(Value::Int(3), result);
}

#[test]
fn interpreter_mutually_recursive_fn_call() {
    let mod1 =
"
mod mod1;

fn recurse_a(i: int) -> int {
if (i == 0) {
    return 5;
} else {
    return recurse_b(i - 1);
}
}

fn recurse_b(i: int) -> int {
if (i == 0) {
    return -5;
} else {
    return recurse_a(i - 1);
}
}
";

    let modules = vec![parse_module(mod1).unwrap()];

    let vm = VM::new(modules).unwrap();

    let fn_handle = vm.query_module("mod1", "recurse_a").unwrap().unwrap();

    let result = vm.eval_fn_args(fn_handle, Some(vec![Value::Int(1)]));

    assert_eq!(Value::Int(-5), result);    
}

#[test]
fn interpreter_loaded_builtin() {
    let mod1 =
"
mod mod1;
use math;

fn test_floor() -> float {
let f = math::floor(1.5);
return f;
}
";
    let modules = vec![parse_module(mod1).unwrap()];

    let vm = VM::new(modules).unwrap();

    let fn_handle = vm.query_module("mod1", "test_floor").unwrap().unwrap();

    let result = vm.eval_fn(fn_handle);

    assert_eq!(Value::Float(1.0), result);
}

#[test]
fn interpreter_anonymous_fn_call() {
    let mod1 =
"
mod mod1;

fn test() -> int {
let func = fn (foo: int) -> int {
    return foo + 5;
};

return func(10);
}";

    let mod1 = parse_module(mod1).unwrap();
    
    let vm = VM::new(vec![mod1]).unwrap();

    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn(fn_handle);

    assert_eq!(Value::Int(15), result);
}

#[test]
fn interpreter_anonymous_fn_arg() {
    let mod1 =
"mod mod1;

fn test2(func: fn(int) -> int) -> int {
return func(10);
}

fn test() -> int {
let func = fn (foo: int) -> int {
    return foo + 5;
};

return test2(func);
}";

    let mod1 = parse_module(mod1).unwrap();
    
    let vm = VM::new(vec![mod1]).unwrap();

    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn(fn_handle);

    assert_eq!(Value::Int(15), result);
}

#[test]
fn interpreter_fn_piping() {
    let mod1 =
"
mod mod1;

fn add(i: int, a: int) -> int {
return i + a;
}

fn test() -> int {
return add(0, 1) |> add(1) |> add(1) |> add(2);
}";

    let mod1 = parse_module(mod1).unwrap();

    let vm = VM::new(vec![mod1]).unwrap();

    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.eval_fn(fn_handle);

    assert_eq!(Value::Int(5), result);
}
