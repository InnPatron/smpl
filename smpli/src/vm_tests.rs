use failure::Error;

use smpl::*;
use smpl::parse_module;

use crate::*;

macro_rules! setup_and_run {
    ($mod1: expr, $mod_name: expr, $fn_name: expr, $args: expr) => {{

        let module = UnparsedModule::anonymous($mod1);
        let parsed = parse_module(module).expect("Failed to parse module");
        let module = VmModule::new(parsed);

        let modules = vec![module];
        let avm = AVM::new(Std::std(), modules).unwrap();
        
        let a_fn_handle = avm.query_module($mod_name, $fn_name).unwrap().unwrap();

        let a_result = avm.spawn_executor(a_fn_handle, $args, SpawnOptions {
            type_check: false   
        })
            .expect("Executor spawn error error")
            .execute_sync()
            .expect("Executor run error");

        a_result
    }};

    ($mod1: expr, $mod_name: expr, $fn_name: expr, $args: expr, add) => {{

        let module = UnparsedModule::anonymous($mod1);
        let parsed = parse_module(module).expect("Failed to parse module");
        let module = VmModule::new(parsed)
            .add_builtin("add", add);

        let modules = vec![module];
        let avm = AVM::new(Std::std(), modules).unwrap();
        
        let a_fn_handle = avm.query_module($mod_name, $fn_name).unwrap().unwrap();

        let a_result = avm.spawn_executor(a_fn_handle, $args, SpawnOptions {
            type_check: false
        })
            .expect("Executor spawn error error")
            .execute_sync()
            .expect("Executor run error");

        a_result
    }};

    ($mod1: expr, $mod_name: expr, $fn_name: expr, $args: expr, sum) => {{

        let module = UnparsedModule::anonymous($mod1);
        let parsed = parse_module(module).expect("Failed to parse module");
        let module = VmModule::new(parsed)
            .add_builtin("sum", var_arg_sum);

        let modules = vec![module];
        let avm = AVM::new(Std::std(), modules).unwrap();
        
        let a_fn_handle = avm.query_module($mod_name, $fn_name).unwrap().unwrap();

        let a_result = avm.spawn_executor(a_fn_handle, $args, SpawnOptions {
            type_check: false    
        })
            .expect("Executor spawn error error")
            .execute_sync()
            .expect("Executor run error");

        a_result
    }};

    ($mod1: expr, $mod_name: expr, $fn_name: expr, $args: expr, add, sum) => {{

        let module = UnparsedModule::anonymous($mod1);
        let parsed = parse_module(module).expect("Failed to parse module");
        let module = VmModule::new(parsed)
            .add_builtin("add", add)
            .add_builtin("sum", var_arg_sum);

        let modules = vec![module];
        let avm = AVM::new(Std::std(), modules).unwrap();
        
        let a_fn_handle = avm.query_module($mod_name, $fn_name).unwrap().unwrap();

        let a_result = avm.spawn_executor(a_fn_handle, $args, SpawnOptions {
            type_check: false    
        })
            .expect("Executor spawn error error")
            .execute_sync()
            .expect("Executor run error");

        a_result
    }};
}

macro_rules! wrap_input {
    ($input: expr) => {{ 
        UnparsedModule::anonymous($input)
    }}
}

fn add(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = args.unwrap();
    let lhs = args.get(0).unwrap();
    let rhs = args.get(1).unwrap();

    let lhs = irmatch!(lhs; Value::Int(i) => i);
    let rhs = irmatch!(rhs; Value::Int(i) => i);

    return Ok(Value::Int(lhs + rhs));
}

fn var_arg_sum(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = args.unwrap();

    let mut sum = 0;

    for arg in args.iter() {
        let value = irmatch!(arg; Value::Int(i) => i);
        sum += value;
    }

    return Ok(Value::Int(sum));
} 

#[test]
fn interpreter_basic() {
    let result = setup_and_run!(
"mod mod1;

fn test(a: int, b: int) -> int {
return a + b;
}",
        "mod1",
        "test",
        Some(vec![Value::Int(5), Value::Int(7)]));
    
    assert_eq!(Value::Int(12), result);
}

#[test]
fn interpreter_struct() {
    let result = setup_and_run!(
"mod mod1;

struct T {
f: int
}

fn test(a: int, b: int) -> T {
return init T { f: a + b };
}",
        "mod1",
        "test",
        Some(vec![Value::Int(5), Value::Int(7)]));

    let result = irmatch!(result; Value::Struct(s) => s.get_field("f").unwrap());
    let result = irmatch!(result; Value::Int(i) => i);
        
    assert_eq!(12, result);
}

#[test]
fn interpreter_builtin() {
    let result = setup_and_run!(
"mod mod1;

builtin fn add(a: int, b: int) -> int;

fn test(a: int, b: int) -> int {
return add(a, b);
}",
        "mod1",
        "test",
        Some(vec![Value::Int(5), Value::Int(7)]), add);

    assert_eq!(Value::Int(12), result);
}

#[test]
fn interpreter_builtin_unchecked_params() {
    let result = setup_and_run!(
"mod mod1;

builtin fn sum(UNCHECKED) -> int;

fn test(a: int, b: int) -> int {
return sum(a, b, 100, 2);
}",
        "mod1",
        "test",
        Some(vec![Value::Int(5), Value::Int(7)]), sum);

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

    let m1 = parse_module(wrap_input!(mod1)).unwrap();
    let m2 = parse_module(wrap_input!(mod2)).unwrap();

    let m1 = VmModule::new(m1)
        .add_builtin("add", add);
    let m2 = VmModule::new(m2);

    let modules = vec![m1, m2];

    let mut avm = AVM::new(Std::no_std(), modules).unwrap();
    
    let a_fn_handle = avm.query_module("mod2", "test2").unwrap().unwrap();

    let a_result = avm.spawn_executor(a_fn_handle, None, SpawnOptions {
        type_check: false,    
    })
        .unwrap()
        .execute_sync()
        .unwrap();
        

    assert_eq!(Value::Int(3), a_result);
}

#[test]
fn interpreter_field_access() {
    let result = setup_and_run!(
"mod mod1;

struct T {
f: int
}

fn test() -> int {
let t: T = init T { f: 1335 };

t.f = t.f + 1;

return t.f + 1;
}

",
        "mod1",
        "test",
        None);

    assert_eq!(Value::Int(1337), result);
}

#[test]
fn interpreter_array() {
    let result = setup_and_run!(
"mod mod1;

fn test() -> int {
let t: [int; 5] = [1, 2, 3, 4, 5];

return t[0] + t[1] + t[2] + t[3] + t[4];
}

",
        "mod1",
        "test",
        None);

    assert_eq!(Value::Int(1 + 2 + 3 + 4 + 5), result);
}

#[test]
fn interpreter_fn_value() {
    let result = setup_and_run!(
"mod mod1;

fn test2(a: int) -> int {
return a * 2;
}

fn test() -> int {
let func: fn(int) -> int = test2;

return func(210);
}

",
        "mod1",
        "test",
        None);

    assert_eq!(Value::Int(420), result);
}

#[test]
fn interpreter_optional_local_type_annotation() {
    let result = setup_and_run!(
"mod mod1;

fn test2(a: int) -> int {
return a * 2;
}

fn test() -> int {
let func = test2;

return func(210);
}

",
        "mod1",
        "test",
        None);

    assert_eq!(Value::Int(420), result);
}

#[test]
fn interpreter_recursive_fn_call() {
    let result = setup_and_run!(
"
mod mod1;

fn recurse(i: int) -> int {
if (i == 0) {
    return 0;
} else {
    return i + recurse(i - 1);
}
}
",
    "mod1",
    "recurse",
    Some(vec![Value::Int(2)]));

    assert_eq!(Value::Int(3), result);
}

#[test]
fn interpreter_mutually_recursive_fn_call() {
    let result = setup_and_run!(
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
",
        "mod1",
        "recurse_a",
        Some(vec![Value::Int(1)]));

    assert_eq!(Value::Int(-5), result);    
}

#[test]
fn interpreter_loaded_builtin() {
    let result = setup_and_run!(
"
mod mod1;
use math;

fn test_floor() -> float {
let f = math::floor(1.5);
return f;
}
",
        "mod1",
        "test_floor",
        None);

    assert_eq!(Value::Float(1.0), result);
}

#[test]
fn interpreter_anonymous_fn_call() {
    let result = setup_and_run!(
"
mod mod1;

fn test() -> int {
let func = fn (foo: int) -> int {
    return foo + 5;
};

return func(10);
}", 
        "mod1",
        "test",
        None);

    assert_eq!(Value::Int(15), result);
}

#[test]
fn interpreter_anonymous_fn_arg() {
    let result = setup_and_run!(
"mod mod1;

fn test2(func: fn(int) -> int) -> int {
return func(10);
}

fn test() -> int {
let func = fn (foo: int) -> int {
    return foo + 5;
};

return test2(func);
}",
        "mod1",
        "test",
        None);

    assert_eq!(Value::Int(15), result);
}

#[test]
fn interpreter_fn_piping() {
    let result = setup_and_run!(
"
mod mod1;

fn add(i: int, a: int) -> int {
return i + a;
}

fn test() -> int {
return add(0, 1) |> add(1) |> add(1) |> add(2);
}",
        "mod1",
        "test",
        None);

    assert_eq!(Value::Int(5), result);
}

#[test]
fn interpreter_builtin_bind() {
    let result = setup_and_run!(
"mod mod1;

builtin fn add(a: int, b: int) -> int;

fn bar() -> int {
    let f = add;
    return f(3, 5);
}",
    "mod1",
    "bar",
    None, add);

    assert_eq!(Value::Int(8), result);
}

#[test]
fn interpreter_complex_if() {
    let result = setup_and_run!(
"mod mod1;

fn foo() -> int {
    if false {
        return 0;
    } elif false {
        return 0;
    } else {
        return 1000;
    }
}
",
    "mod1",
    "foo",
    None);

    assert_eq!(Value::Int(1000), result);
}

#[test]
fn interpreter_uni_expr() {
    let result = setup_and_run!(
"mod mod1;

fn foo() -> bool {
    let bar = !false;
    return bar;
}",
    "mod1",
    "foo",
    None);

    assert_eq!(Value::Bool(true), result);
}

#[test]
fn interpreter_2d_array() {
    let result = setup_and_run!(
"mod mod1;

fn foo() -> int {
    let array = [[0,0,0]; 3];

    let curRow = array[0];
    curRow[0] = 5;

    let total = 0;

    let curRow = array[2];
    total = total + curRow[0];

    let curRow = array[1];
    total = total + curRow[0];

    return total;
}",
    "mod1",
    "foo",
    None);

    assert_eq!(Value::Int(0), result);
}

#[test]
fn interpreter_structs_complex() {
    let result = setup_and_run!(
"mod mod1;

struct Bar {
    i: int,
}

fn foo() -> int {
    let b1 = init Bar { i: 0 };
    let b2 = b1;
    b2.i = 5;

    return b1.i;
}",
    "mod1",
    "foo",
    None);

    assert_eq!(Value::Int(0), result);
}

#[test]
fn interpreter_while_loop() {
    let result = setup_and_run!(
"mod mod1;

fn foo() -> int {

    let accu = 0;

    while accu < 100 {
        accu = accu + 1;
    }

    let bar = 37;

    return accu + bar;
}",

    "mod1",
    "foo",
    None);

    assert_eq!(Value::Int(137), result);
}

#[test]
fn interpreter_bind_fn_type_app_mod_access() {
    let mod1 =
"mod mod1;

fn ident(type T)(t: T) -> T {
    return t;
}";

    let mod2 =
"mod mod2;

use mod1;

fn test() -> int {
    let my_ident: fn(int) -> int = mod1::ident(type int);
    let result: int = my_ident(1337);

    return result;
}";


    let mod1 = UnparsedModule::anonymous(mod1);
    let mod1 = parse_module(mod1).expect("Failed to parse module");
    let mod1 = VmModule::new(mod1);

    let mod2 = UnparsedModule::anonymous(mod2);
    let mod2 = parse_module(mod2).expect("Failed to parse module");
    let mod2 = VmModule::new(mod2);

    let modules = vec![mod1, mod2];
    let avm = AVM::new(Std::std(), modules).unwrap();
    
    let fn_handle = avm.query_module("mod2", "test").unwrap().unwrap();

    let result = avm.spawn_executor(fn_handle, None, SpawnOptions {
        type_check: false   
    })
        .expect("Executor spawn error error")
        .execute_sync()
        .expect("Executor run error");

    assert_eq!(result, Value::Int(1337));
}
