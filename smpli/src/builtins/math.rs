use failure::Error;
use smpl::{UnparsedModule, parse_module};

use crate::*;
use crate::err::*;

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

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(MATH_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(MATH_SIN,          super::erase(sin))
        .add_builtin(MATH_COS,          super::erase(cos))
        .add_builtin(MATH_TAN,          super::erase(tan))
        .add_builtin(MATH_ASIN,         super::erase(asin))
        .add_builtin(MATH_ACOS,         super::erase(acos))
        .add_builtin(MATH_ATAN,         super::erase(atan))
        .add_builtin(MATH_ATAN2,        super::erase(atan2))
        .add_builtin(MATH_TO_RADIANS,   super::erase(to_radians))
        .add_builtin(MATH_TO_DEGREES,   super::erase(to_degrees))
        .add_builtin(MATH_FPOWF,        super::erase(fpowf))
        .add_builtin(MATH_FPOWI,        super::erase(fpowi))
        .add_builtin(MATH_IPOW,         super::erase(ipow))
        .add_builtin(MATH_FLOOR,        super::erase(floor))
        .add_builtin(MATH_CEIL,         super::erase(ceil))
        .add_builtin(MATH_ROUND,        super::erase(round));

    module
}

/// In radians
async fn sin(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.sin())),
        _ => panic!(),
    }
}

/// In radians
async fn cos(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.cos())),
        _ => panic!(),
    }
}

/// In radians
async fn tan(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.tan())),
        _ => panic!(),
    }
}

/// In radians
async fn asin(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.asin())),
        _ => panic!(),
    }
}

/// In radians
async fn acos(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.acos())),
        _ => panic!(),
    }
}

/// In radians
async fn atan(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.atan())),
        _ => panic!(),
    }
}

/// In radians
async fn atan2(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = exact_args!(2, args)?;
    let v = args.get(0).unwrap().clone();
    let a = args.get(1).unwrap().clone();

    match (v, a) {
        (Value::Float(v), Value::Float(a)) => Ok(Value::Float(v.atan2(a))),
        _ => panic!(),
    }
}

async fn to_radians(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.to_radians())),
        _ => panic!(),
    }
}

async fn to_degrees(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.to_degrees())),
        _ => panic!(),
    }
}

async fn fpowf(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = exact_args!(2, args)?;
    let b = args.get(0).unwrap().clone();
    let p = args.get(1).unwrap().clone();

    match (b, p) {
        (Value::Float(b), Value::Float(p)) => Ok(Value::Float(b.powf(p))),
        _ => panic!(),
    }
}

async fn fpowi(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = exact_args!(2, args)?;
    let b = args.get(0).unwrap().clone();
    let p = args.get(1).unwrap().clone();

    match (b, p) {
        (Value::Float(b), Value::Int(p)) => {

            if p <= std::i32::MAX as i64 {
                Ok(Value::Float(b.powi(p as i32)))
            } else {
                Err(InternalError::IntegerOutOfRange {
                    v: p,
                    min_inclusive: std::i32::MIN as i64,
                    max_inclusive: std::i32::MAX as i64,
                })?
            }
        }
        _ => panic!(),
    }
}

async fn ipow(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = exact_args!(2, args)?;
    let b = args.get(0).unwrap().clone();
    let p = args.get(1).unwrap().clone();

    match (b, p) {
        (Value::Int(b), Value::Int(p)) => {

            if (p >= 0) && (p < std::u32::MAX as i64) {
                Ok(Value::Int(b.pow(p as u32)))
            } else {
                Err(InternalError::IntegerOutOfRange {
                    v: p,
                    min_inclusive: 0,
                    max_inclusive: std::u32::MAX as i64,
                })?
            }

        },
        _ => panic!(),
    }
}

async fn floor(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.floor())),
        _ => panic!(),
    }
}

async fn ceil(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.ceil())),
        _ => panic!(),
    }
}

async fn round(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;
    let v = args.remove(0);

    match v {
        Value::Float(f) => Ok(Value::Float(f.round())),
        _ => panic!(),
    }
}
