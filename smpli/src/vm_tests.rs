use failure::Error;

use smpl::*;
use smpl::parse_module;

use crate::*;

macro_rules! include_test {
    ($file_name: expr) => {{
        include_str!(concat!("../../eval-tests/", $file_name))
    }}
}

macro_rules! expect_value {
    ($name: ident, module :: $mod_name: expr, eval :: $fn_name: expr, args :: $args: expr, finalizer :: $finalizer: expr) => {
        expect_value!($name, 
            module :: $mod_name, 
            eval :: $fn_name, 
            args :: $args, 
            finalizer :: $finalizer,
            builtins :: |vm| { vm }
        );
    };

    ($name: ident, module :: $mod_name: expr, eval :: $fn_name: expr, args :: $args: expr, finalizer :: $finalizer: expr, builtins :: $builtins: expr) => {
        #[test]
        fn $name() {
            let code = include_test!(concat!(stringify!($name), ".smpl"));
            let result = setup_and_run!(code, $mod_name, $fn_name, $args, $builtins);

            $finalizer(result);
        }
    };

    ($name: ident, module :: $mod_name: expr, eval :: $fn_name: expr, args :: $args: expr, expect :: $expect: expr, builtins :: $builtins: expr) => {
        expect_value!($name, 
            module :: $mod_name, 
            eval :: $fn_name, 
            args :: $args, 
            finalizer :: |result| {
                assert_eq!($expect, result);   
            },
            builtins :: $builtins
        );
    };

    ($name: ident, module :: $mod_name: expr, eval :: $fn_name: expr, args :: $args: expr, expect :: $expect: expr) => {
        expect_value!($name, 
            module :: $mod_name, 
            eval :: $fn_name, 
            args :: $args, 
            finalizer :: |result| {
                assert_eq!($expect, result);   
            },
            builtins :: |vm| { vm }
        );
    };
}

macro_rules! setup_and_run {
    ($mod1: expr, $mod_name: expr, $fn_name: expr, $args: expr) => {{
        setup_and_run!(
            $mod1,
            $mod_name,
            $fn_name,
            $args,
            |vm: VmModule | { vm }
        )
    }};

    ($mod1: expr, $mod_name: expr, $fn_name: expr, $args: expr, $builtins: expr) => {{

        let module = UnparsedModule::anonymous($mod1);
        let parsed = parse_module(module).expect("Failed to parse module");
        let module = $builtins(VmModule::new(parsed));

        let modules = vec![module];
        let avm = AVM::new(Std::std(), modules).unwrap();
        
        let a_fn_handle = avm.query_module($mod_name, $fn_name)
            .expect("Query error")
            .expect("Unknown query");

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

async fn add(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = args.unwrap();
    let lhs = args.get(0).unwrap();
    let rhs = args.get(1).unwrap();

    let lhs = irmatch!(lhs; Value::Int(i) => i);
    let rhs = irmatch!(rhs; Value::Int(i) => i);

    return Ok(Value::Int(lhs + rhs));
}

async fn var_arg_sum(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = args.unwrap();

    let mut sum = 0;

    for arg in args.iter() {
        let value = irmatch!(arg; Value::Int(i) => i);
        sum += value;
    }

    return Ok(Value::Int(sum));
} 

expect_value!(interpreter_basic,
    module :: "mod1",
    eval :: "test",
    args :: Some(vec![Value::Int(5), Value::Int(7)]),
    expect :: Value::Int(12)
);

expect_value!(interpreter_struct,
    module :: "mod1",
    eval :: "test",
    args :: Some(vec![Value::Int(5), Value::Int(7)]),
    finalizer :: |result|  {
        let result = irmatch!(result; Value::Struct(s) => s.get_field("f").unwrap());
        let result = irmatch!(result; Value::Int(i) => i);
            
        assert_eq!(12, result);
    }
);

expect_value!(interpreter_builtin,
    module :: "mod1",
    eval :: "test",
    args :: Some(vec![Value::Int(5), Value::Int(7)]), 
    expect :: Value::Int(12),
    builtins :: |vm: VmModule| {
        vm.add_builtin("add", erase(add))
    }
);

expect_value!(interpreter_builtin_unchecked_params,
    module :: "mod1",
    eval :: "test",
    args :: Some(vec![Value::Int(5), Value::Int(7)]),
    expect :: Value::Int(114),
    builtins :: |vm: VmModule| {
        vm.add_builtin("sum", erase(var_arg_sum))
    }
);

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
        .add_builtin("add", erase(add));
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

expect_value!(interpreter_field_access,
    module :: "mod1",
    eval :: "test",
    args :: None,
    expect :: Value::Int(1337)
);

expect_value!(interpreter_array,
    module :: "mod1",
    eval :: "test",
    args :: None,
    expect :: Value::Int(1 + 2 + 3 + 4 + 5)
);

expect_value!(interpreter_fn_value,
    module :: "mod1",
    eval :: "test",
    args :: None,
    expect :: Value::Int(420)
);

expect_value!(interpreter_optional_local_type_annotation,
    module :: "mod1",
    eval :: "test",
    args :: None,
    expect :: Value::Int(420)
);

expect_value!(interpreter_recursive_fn_call,
    module :: "mod1",
    eval :: "recurse",
    args :: Some(vec![Value::Int(2)]),
    expect :: Value::Int(3)
);

expect_value!(interpreter_mutually_recursive_fn_call,
    module :: "mod1",
    eval :: "recurse_a",
    args :: Some(vec![Value::Int(1)]),
    expect :: Value::Int(-5)
);

expect_value!(interpreter_loaded_builtin,
    module :: "mod1",
    eval :: "test_floor",
    args :: None,
    expect :: Value::Float(1.0)
);

expect_value!(interpreter_anonymous_fn_call,
    module :: "mod1",
    eval :: "test",
    args :: None,
    expect :: Value::Int(15)
);

expect_value!(interpreter_anonymous_fn_arg,
    module :: "mod1",
    eval :: "test",
    args :: None,
    expect :: Value::Int(15)
);

expect_value!(interpreter_fn_piping,
    module :: "mod1",
    eval :: "test",
    args :: None,
    expect :: Value::Int(5)
);

expect_value!(interpreter_builtin_bind,
    module :: "mod1",
    eval :: "bar",
    args :: None,
    expect :: Value::Int(8),
    builtins :: |vm: VmModule| {
        vm.add_builtin("add", erase(add))
    }
);

expect_value!(interpreter_complex_if,
    module :: "mod1",
    eval :: "foo",
    args :: None,
    expect :: Value::Int(1000)
);

expect_value!(interpreter_uni_expr,
    module :: "mod1",
    eval :: "foo",
    args :: None,
    expect :: Value::Bool(true)
);

expect_value!(interpreter_2d_array,
    module :: "mod1",
    eval :: "foo",
    args :: None,
    expect :: Value::Int(0)
);

expect_value!(interpreter_structs_complex,
    module :: "mod1",
    eval :: "foo",
    args :: None,
    expect :: Value::Int(0)
);

expect_value!(interpreter_while_loop,
    module :: "mod1",
    eval :: "foo",
    args :: None,
    expect :: Value::Int(137)
);

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

expect_value!(interpreter_array_path_assignment,
    module :: "mod1",
    eval :: "test",
    args :: None,
    expect :: {
        let array = Array::new_init(vec![
            Value::Int(1),
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
        ]);

        Value::Array(array)
    }
);
