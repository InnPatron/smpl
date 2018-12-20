use failure::Error;

use crate::exact_args;
use crate::module::*;
use crate::parser::parse_module;

use crate::code_gen::interpreter::*;

pub const MOD_MATH: &'static str = "math";

pub const MATH_SIN: &'static str = "sin";
pub const MATH_COS: &'static str = "cos";
pub const MATH_TAN: &'static str = "tan";

pub const MATH_ASIN: &'static str = "asin";
pub const MATH_ACOS: &'static str = "acos";
pub const MATH_ATAN: &'static str = "atan";
pub const MATH_ATAN2: &'static str = "atan2";

pub const MATH_TO_RADIANS: &'static str = "to_radians";
pub const MATH_TO_DEGREES: &'static str = "to_degrees";

pub const MATH_FPOWF: &'static str = "fpowf";
pub const MATH_FPOWI: &'static str = "fpowi";
pub const MATH_IPOW: &'static str = "ipow";

pub const MATH_FLOOR: &'static str = "floor";
pub const MATH_CEIL: &'static str = "ceil";
pub const MATH_ROUND: &'static str = "round";

pub const MATH_DECLARATION: &'static str = include_str!("math.smpl");

pub fn include(modules: &mut Vec<ParsedModule>) {
    let input = UnparsedModule::anonymous(MATH_DECLARATION.to_string());
    modules.push(parse_module(input).unwrap());
}

pub fn add<MAP: BuiltinMap>(vm: &mut MAP) {
    vm.insert_builtin(MOD_MATH, MATH_SIN, sin)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_COS, cos)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_TAN, tan)
        .unwrap();

    vm.insert_builtin(MOD_MATH, MATH_ASIN, asin)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_ACOS, acos)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_ATAN, atan)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_ATAN2, atan2)
        .unwrap();

    vm.insert_builtin(MOD_MATH, MATH_TO_RADIANS, to_radians)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_TO_DEGREES, to_degrees)
        .unwrap();

    vm.insert_builtin(MOD_MATH, MATH_FPOWF, fpowf)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_FPOWI, fpowi)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_IPOW, ipow)
        .unwrap();

    vm.insert_builtin(MOD_MATH, MATH_FLOOR, floor)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_CEIL, ceil)
        .unwrap();
    vm.insert_builtin(MOD_MATH, MATH_ROUND, round)
        .unwrap();
}

/// In radians
fn sin(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.sin())),
        _ => panic!(),
    }
}

/// In radians
fn cos(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.cos())),
        _ => panic!(),
    }
}

/// In radians
fn tan(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.tan())),
        _ => panic!(),
    }
}

/// In radians
fn asin(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.asin())),
        _ => panic!(),
    }
}

/// In radians
fn acos(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.acos())),
        _ => panic!(),
    }
}

/// In radians
fn atan(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.atan())),
        _ => panic!(),
    }
}

/// In radians
fn atan2(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = exact_args!(2, args)?;
    let v = args.get(0).unwrap().clone();
    let a = args.get(1).unwrap().clone();

    match (v, a) {
        (Value::Float(v), Value::Float(a)) => Ok(Value::Float(v.atan2(a))),
        _ => panic!(),
    }
}

fn to_radians(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.to_radians())),
        _ => panic!(),
    }
}

fn to_degrees(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.to_degrees())),
        _ => panic!(),
    }
}

fn fpowf(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = exact_args!(2, args)?;
    let b = args.get(0).unwrap().clone();
    let p = args.get(1).unwrap().clone();

    match (b, p) {
        (Value::Float(b), Value::Float(p)) => Ok(Value::Float(b.powf(p))),
        _ => panic!(),
    }
}

fn fpowi(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = exact_args!(2, args)?;
    let b = args.get(0).unwrap().clone();
    let p = args.get(1).unwrap().clone();

    match (b, p) {
        (Value::Float(b), Value::Int(p)) => Ok(Value::Float(b.powi(p))),
        _ => panic!(),
    }
}

fn ipow(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = exact_args!(2, args)?;
    let b = args.get(0).unwrap().clone();
    let p = args.get(1).unwrap().clone();

    match (b, p) {
        (Value::Int(b), Value::Int(p)) => Ok(Value::Int(b.pow(p as u32))),
        _ => panic!(),
    }
}

fn floor(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.floor())),
        _ => panic!(),
    }
}

fn ceil(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.ceil())),
        _ => panic!(),
    }
}

fn round(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.round())),
        _ => panic!(),
    }
}
