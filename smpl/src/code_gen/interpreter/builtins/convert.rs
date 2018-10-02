use ast::Module;
use parser::parse_module;

use code_gen::interpreter::*;

pub const MOD_CONVERT: &'static str = "convert";

pub const CONVERT_INT_TO_FLOAT: &'static str = "int_to_float";
pub const CONVERT_FLOAT_TO_INT: &'static str = "float_to_int";

pub const CONVERT_IS_FLOAT: &'static str = "is_float";
pub const CONVERT_IS_INT: &'static str = "is_int";

pub const CONVERT_STRING_TO_FLOAT: &'static str = "string_to_float";
pub const CONVERT_STRING_TO_INT: &'static str = "string_to_int";

pub const CONVERT_DECLARATION: &'static str = include_str!("convert.smpl");

pub fn include(modules: &mut Vec<Module>) {
    modules.push(parse_module(CONVERT_DECLARATION).unwrap());
}

pub fn add<MAP: BuiltinMap>(vm: &mut MAP) {
    vm.insert_builtin(MOD_CONVERT, CONVERT_INT_TO_FLOAT, Box::new(IntToFloat))
        .unwrap();
    vm.insert_builtin(MOD_CONVERT, CONVERT_FLOAT_TO_INT, Box::new(FloatToInt))
        .unwrap();

    vm.insert_builtin(MOD_CONVERT, CONVERT_IS_FLOAT, Box::new(IsFloat))
        .unwrap();
    vm.insert_builtin(MOD_CONVERT, CONVERT_IS_INT, Box::new(IsInt))
        .unwrap();

    vm.insert_builtin(
        MOD_CONVERT,
        CONVERT_STRING_TO_FLOAT,
        Box::new(StringToFloat),
    ).unwrap();
    vm.insert_builtin(MOD_CONVERT, CONVERT_STRING_TO_INT, Box::new(StringToInt))
        .unwrap();
}

pub struct IntToFloat;

impl BuiltinFn for IntToFloat {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let a = args.remove(0);
        match a {
            Value::Int(i) => Value::Float(i as f32),
            _ => unreachable!(),
        }
    }
}

pub struct FloatToInt;

impl BuiltinFn for FloatToInt {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let a = args.remove(0);
        match a {
            Value::Float(f) => Value::Int(f as i32),
            _ => unreachable!(),
        }
    }
}

pub struct IsFloat;

impl BuiltinFn for IsFloat {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        match a {
            Value::String(s) => Value::Bool(s.parse::<f32>().is_ok()),
            _ => unreachable!(),
        }
    }
}

pub struct IsInt;

impl BuiltinFn for IsInt {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        match a {
            Value::String(s) => Value::Bool(s.parse::<i32>().is_ok()),
            _ => unreachable!(),
        }
    }
}

pub struct StringToFloat;

impl BuiltinFn for StringToFloat {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        match a {
            Value::String(s) => Value::Float(
                s.parse::<f32>()
                    .expect(&format!("{} was not a valid float.", s)),
            ),
            _ => unreachable!(),
        }
    }
}

pub struct StringToInt;

impl BuiltinFn for StringToInt {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        match a {
            Value::String(s) => Value::Int(
                s.parse::<i32>()
                    .expect(&format!("{} was not a valid int.", s)),
            ),
            _ => unreachable!(),
        }
    }
}
