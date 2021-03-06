use failure::Error;
use smpl::prelude::{UnparsedModule, parse_module};

use crate::*;

const MOD_STRING: &'static str = "str";

const STRING_LEN: &'static str = "len";
const STRING_TO_STRING: &'static str = "to_string";
const STRING_APPEND: &'static str = "append";
const STRING_TO_LOWER: &'static str = "to_lower";
const STRING_TO_UPPER: &'static str = "to_upper";

const STRING_DECLARATION: &'static str = include_str!("str.smpl");

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(STRING_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(STRING_LEN,        super::erase(len))
        .add_builtin(STRING_TO_STRING,  super::erase(to_string))
        .add_builtin(STRING_APPEND,     super::erase(append))
        .add_builtin(STRING_TO_LOWER,   super::erase(to_lower))
        .add_builtin(STRING_TO_UPPER,   super::erase(to_upper));

    module
}

async fn len(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let string = args.pop().unwrap();
    let string = irmatch!(string; Value::String(s) => s);

    Ok(Value::Int(string.len() as i64))
}

async fn to_string(args: Vec<Value>) -> Result<Value, Error> {
    let args = min_args!(1, args)?;

    let mut s = String::new();

    for a in args {
        s.push_str(&a.to_string());
    }

    Ok(Value::String(s))
}

async fn append(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = min_args!(1, args)?;

    let to_append = args.pop().unwrap();
    let to_append = irmatch!(to_append; Value::String(s) => s);

    let base = args.pop().unwrap();
    let mut base = irmatch!(base; Value::String(s) => s);

    base.push_str(&to_append);

    Ok(Value::String(base))
}

async fn to_lower(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let string = args.pop().unwrap();
    let string = irmatch!(string; Value::String(s) => s);

    Ok(Value::String(string.to_lowercase()))
}

async fn to_upper(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let string = args.pop().unwrap();
    let string = irmatch!(string; Value::String(s) => s);

    Ok(Value::String(string.to_uppercase()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use smpl::prelude::*;

    macro_rules! wrap_input {
        ($input: expr) => {{
            UnparsedModule::anonymous($input)
        }};
    }

    #[test]
    fn interpreter_str_len() {
        let mut vm = AVM::new(Std::std(), Vec::new()).unwrap();

        let fn_handle = vm.query_module(MOD_STRING, STRING_LEN).unwrap().unwrap();

        let result = vm
            .spawn_executor(fn_handle, vec![Value::String("".to_string())], SpawnOptions {
                type_check: false
            })
            .unwrap()
            .execute_sync()
            .unwrap();
        assert_eq!(Value::Int(0), result);

        let result = vm
            .spawn_executor(fn_handle, vec![Value::String("1".to_string())], SpawnOptions {
                type_check: false
            })
            .unwrap()
            .execute_sync()
            .unwrap();
        assert_eq!(Value::Int(1), result);

        let result = vm
            .spawn_executor(
                fn_handle,
                vec![Value::String("123456789".to_string())],
                SpawnOptions {
                    type_check: false
                }
            )
            .unwrap()
            .execute_sync()
            .unwrap();
        assert_eq!(Value::Int(9), result);
    }

    #[test]
    fn interpreter_str_to_string() {
        let mut vm = AVM::new(Std::std(), Vec::new()).unwrap();

        let fn_handle = vm
            .query_module(MOD_STRING, STRING_TO_STRING)
            .unwrap()
            .unwrap();

        let result = vm
            .spawn_executor(
                fn_handle,
                vec![
                    Value::String("I am ".to_string()),
                    Value::Int(1337),
                    Value::String("!".to_string()),
                ],
                SpawnOptions {
                    type_check: false
                }
            )
            .unwrap()
            .execute_sync()
            .unwrap();
        assert_eq!(Value::String("I am 1337!".to_string()), result);
    }

    #[test]
    fn interpreter_str_append() {
        let mut vm = AVM::new(Std::std(), Vec::new()).unwrap();

        let fn_handle = vm.query_module(MOD_STRING, STRING_APPEND).unwrap().unwrap();

        let result = vm
            .spawn_executor(
                fn_handle,
                vec![
                    Value::String("I'll ".to_string()),
                    Value::String("be back.".to_string()),
                ],
                SpawnOptions {
                    type_check: false
                }
            )
            .unwrap()
            .execute_sync()
            .unwrap();
        assert_eq!(Value::String("I'll be back.".to_string()), result);
    }

    #[test]
    fn interpreter_str_to_lower() {
        let mut vm = AVM::new(Std::std(), Vec::new()).unwrap();

        let fn_handle = vm
            .query_module(MOD_STRING, STRING_TO_LOWER)
            .unwrap()
            .unwrap();

        let result = vm
            .spawn_executor(
                fn_handle,
                vec![Value::String("LOUD NOISES".to_string())],
                SpawnOptions {
                    type_check: false,
                }
            )
            .unwrap()
            .execute_sync()
            .unwrap();
        assert_eq!(Value::String("loud noises".to_string()), result);
    }

    #[test]
    fn interpreter_str_to_upper() {
        let mut vm = AVM::new(Std::std(), Vec::new()).unwrap();

        let fn_handle = vm
            .query_module(MOD_STRING, STRING_TO_UPPER)
            .unwrap()
            .unwrap();

        let result = vm
            .spawn_executor(
                fn_handle,
                vec![Value::String("loud noises".to_string())],
                SpawnOptions {
                    type_check: false,
                }
            )
            .unwrap()
            .execute_sync()
            .unwrap();
        assert_eq!(Value::String("LOUD NOISES".to_string()), result);
    }

    #[test]
#[cfg_attr(rustfmt, rustfmt_skip)]
fn interpreter_str_intermodule_to_string() {
    let mod1 =
"
mod mod1;

use str;

fn test() -> String {
return str::to_string(\"Cannot\", \" touch\", \" this!?\");
}
";
    let modules = vec![VmModule::new(parse_module(wrap_input!(mod1)).unwrap())];

    let mut vm = AVM::new(Std::std(), modules).unwrap();

    let fn_handle = vm.query_module("mod1", "test").unwrap().unwrap();

    let result = vm.spawn_executor(fn_handle, vec![], SpawnOptions {
        type_check: false
    })
        .unwrap()
        .execute_sync()
        .unwrap();
    assert_eq!(Value::String("Cannot touch this!?".to_string()), result);
}
}
