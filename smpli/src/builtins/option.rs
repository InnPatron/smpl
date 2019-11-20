use failure::Error;
use smpl::{UnparsedModule, parse_module};

use crate::*;

pub const OPTION_SOME: &'static str = "some";
pub const OPTION_IS_SOME: &'static str = "is_some";

pub const OPTION_UNWRAP: &'static str = "unwrap";
pub const OPTION_EXPECT: &'static str = "expect";

pub const OPTION_NONE: &'static str = "none";
pub const OPTION_IS_NONE: &'static str = "is_none";

pub const OPTION_DATA_KEY: &'static str = "__DATA";
pub const OPTION_TAG_KEY: &'static str = "__TAG";

pub const OPTION_TAG_SOME: i64 = 1;
pub const OPTION_TAG_NONE: i64 = 0;

const OPTION_DECLARATION: &'static str = include_str!("option.smpl");

#[derive(Fail, Debug)]
pub enum OptionError {
    #[fail(display = "Value not an option")]
    NonOption,
    #[fail(display = "Optional was not 'some'")]
    UnwrapFailed,
    #[fail(display = "Optional was not 'some': {}", _0)]
    ExpectFailed(String)
}

pub const fn some_tag() -> Value {
    Value::Int(OPTION_TAG_SOME)
}

pub const fn none_tag() -> Value {
    Value::Int(OPTION_TAG_NONE)
}

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(OPTION_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(OPTION_SOME,    super::erase(builtin_make_some))
        .add_builtin(OPTION_IS_SOME, super::erase(builtin_is_some))
        .add_builtin(OPTION_UNWRAP,  super::erase(builtin_unwrap))
        .add_builtin(OPTION_EXPECT,  super::erase(builtin_expect))
        .add_builtin(OPTION_NONE,    super::erase(builtin_make_none))
        .add_builtin(OPTION_IS_NONE, super::erase(builtin_is_none))
    ;

    module
}

pub fn make_some(value: Value) -> Value {
    let mut some_struct = Struct::new();
    
    let tag = some_tag();

    some_struct.set_field(OPTION_DATA_KEY.to_string(), value);
    some_struct.set_field(OPTION_TAG_KEY.to_string(), tag);

    Value::Struct(some_struct)
}

pub fn make_none() -> Value {
    let mut none_struct = Struct::new();
    
    let tag = none_tag();
    none_struct.set_field(OPTION_TAG_KEY.to_string(), tag);

    Value::Struct(none_struct)
}

pub fn is_some(value: Value) -> bool {
    match value {
        Value::Struct(inner) => {

            inner.get_field(OPTION_TAG_KEY)
                .map(|tag_value| tag_value == some_tag())
                .unwrap_or(false)
        }

        _ => false,
    }
}

pub fn is_none(value: Value) -> bool {
    match value {
        Value::Struct(inner) => {
            inner.get_field(OPTION_TAG_KEY)
                .map(|tag_value| tag_value == none_tag())
                .unwrap_or(false)
        }

        _ => false,
    }
}

pub fn unwrap(value: Value) -> Result<Value, Error> {
    match value {
        Value::Struct(inner) => {

            inner.get_field(OPTION_TAG_KEY)
                .map(|tag_value| tag_value == some_tag())
                .ok_or(OptionError::NonOption.into())
                .and_then(|tag_is_some| {

                    if tag_is_some {
                        inner.get_field(OPTION_DATA_KEY)
                            .ok_or(OptionError::NonOption)
                    } else {
                        Err(OptionError::UnwrapFailed)
                    }
                })
            .map_err(|e| e.into())

        }

        v => Err(OptionError::NonOption)?,
    }
}

pub fn expect(value: Value, message: String) -> Result<Value, Error> {
    match value {
        Value::Struct(inner) => {

            inner.get_field(OPTION_TAG_KEY)
                .map(|tag_value| tag_value == some_tag())
                .ok_or(OptionError::NonOption.into())
                .and_then(|tag_is_some| {

                    if tag_is_some {
                        inner.get_field(OPTION_DATA_KEY)
                            .ok_or(OptionError::NonOption)
                    } else {
                        Err(OptionError::ExpectFailed(message))
                    }
                })
            .map_err(|e| e.into())

        }

        v => Err(OptionError::NonOption)?,
    }
}

async fn builtin_make_some(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let data = args.pop().unwrap();

    Ok(make_some(data))
}

async fn builtin_is_some(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let data = args.pop().unwrap();

    Ok(Value::Bool(is_some(data)))
}

async fn builtin_unwrap(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let data = args.pop().unwrap();

    unwrap(data)
}

async fn builtin_expect(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let message = args.pop().unwrap();
    let message = irmatch!(message; Value::String(s) => s);

    let data = args.pop().unwrap();

    expect(data, message)
}

async fn builtin_make_none(args: Vec<Value>) -> Result<Value, Error> {
    no_args!(args)?;

    Ok(make_none())
}

async fn builtin_is_none(args: Vec<Value>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let data = args.pop().unwrap();

    Ok(Value::Bool(is_none(data)))
}

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod tests {

    use smpl::*;
    use super::*;

    macro_rules! option_test {
        ($mod: expr, $mod_name: expr, $fn_name: expr, $args: expr) => {{

            let mut modules = vec![vm_module(), 
                VmModule::new(parse_module(UnparsedModule::anonymous($mod)).unwrap())];

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
    fn interpreter_option_some() {
        let mod1 =
"mod mod1;

use option;

fn test() -> option::Option(type String) {
    let o1: option::Option(type String) = option::some(type String)(\"Hello world\");
    return o1;
}
";
        let result = option_test!(mod1, "mod1", "test", None);

        let result = irmatch!(result; Value::Struct(inner) => inner);

        let tag: Value = result.get_field(OPTION_TAG_KEY).expect("No tag");
        let value: Value = result.get_field(OPTION_DATA_KEY).expect("No data");

        assert_eq!(tag, some_tag());
        assert_eq!(value, Value::String("Hello world".to_string()));
    }

    #[test]
    fn interpreter_option_none() {
        let mod1 =
"mod mod1;

use option;

fn test() -> option::Option(type String) {
    let o1: option::Option(type String) = option::none(type String)();
    return o1;
}
";
        let result = option_test!(mod1, "mod1", "test", None);

        let result = irmatch!(result; Value::Struct(inner) => inner);

        let tag: Value = result.get_field(OPTION_TAG_KEY).expect("No tag");

        assert_eq!(tag, none_tag());
    }

    #[test]
    fn interpreter_option_is_variant() {
        let mod1 =
"mod mod1;

use option;

fn is_none_true() -> bool {
    let o1: option::Option(type String) = option::none(type String)();
    return option::is_none(type String)(o1);
}

fn is_some_true() -> bool {
    let o1: option::Option(type String) = option::some(type String)(\"Hello world\");
    return option::is_some(type String)(o1);
}

fn is_none_false() -> bool {
    let o1: option::Option(type String) = option::some(type String)(\"Hello world\");
    return option::is_none(type String)(o1);
}

fn is_some_false() -> bool {
    let o1: option::Option(type String) = option::none(type String)();
    return option::is_some(type String)(o1);
}";
        let is_none_true = option_test!(mod1, "mod1", "is_none_true", None);
        let is_some_true = option_test!(mod1, "mod1", "is_some_true", None);
        let is_none_false = option_test!(mod1, "mod1", "is_none_false", None);
        let is_some_false = option_test!(mod1, "mod1", "is_some_false", None);

        assert_eq!(is_none_true, Value::Bool(true));
        assert_eq!(is_some_true, Value::Bool(true));
        assert_eq!(is_none_false, Value::Bool(false));
        assert_eq!(is_some_false, Value::Bool(false));
    }

    #[test]
    fn interpreter_option_unwrap_expect() {
        let mod1 =
"mod mod1;

use option;

fn unwrap() -> String {
    let o1 = option::some(type String)(\"Hello world\");
    return option::unwrap(type String)(o1); 
}

fn expect() -> String {
    let o1 = option::some(type String)(\"Hello world\");
    return option::expect(type String)(o1, \"ERROR\"); 
}";

        let unwrap = option_test!(mod1, "mod1", "unwrap", None);
        let expect = option_test!(mod1, "mod1", "expect", None);

        assert_eq!(unwrap, Value::String("Hello world".to_string()));
        assert_eq!(expect, Value::String("Hello world".to_string()));
    }
}
