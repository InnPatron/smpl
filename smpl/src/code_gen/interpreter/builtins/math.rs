use ast::Module;
use parser::parse_module;

use code_gen::interpreter::*;

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

pub fn include(modules: &mut Vec<Module>) {
    modules.push(parse_module(MATH_DECLARATION).unwrap());
}

pub fn add(vm: &mut VM) {
    vm.insert_builtin(MOD_MATH, MATH_SIN, Box::new(Sin));
    vm.insert_builtin(MOD_MATH, MATH_COS, Box::new(Cos));
    vm.insert_builtin(MOD_MATH, MATH_TAN, Box::new(Tan));

    vm.insert_builtin(MOD_MATH, MATH_ASIN, Box::new(Asin));
    vm.insert_builtin(MOD_MATH, MATH_ACOS, Box::new(Acos));
    vm.insert_builtin(MOD_MATH, MATH_ATAN, Box::new(Atan));
    vm.insert_builtin(MOD_MATH, MATH_ATAN2, Box::new(Atan2));

    vm.insert_builtin(MOD_MATH, MATH_TO_RADIANS, Box::new(ToRadians));
    vm.insert_builtin(MOD_MATH, MATH_TO_DEGREES, Box::new(ToDegrees));

    vm.insert_builtin(MOD_MATH, MATH_FPOWF, Box::new(FPowF));
    vm.insert_builtin(MOD_MATH, MATH_FPOWI, Box::new(FPowI));
    vm.insert_builtin(MOD_MATH, MATH_IPOW, Box::new(IPow));

    vm.insert_builtin(MOD_MATH, MATH_FLOOR, Box::new(Floor));
    vm.insert_builtin(MOD_MATH, MATH_CEIL, Box::new(Ceil));
    vm.insert_builtin(MOD_MATH, MATH_ROUND, Box::new(Round));
}

/// In radians
pub struct Sin;

impl BuiltinFn for Sin {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.sin()),
            _ => panic!(),
        }
    }
}

/// In radians
pub struct Cos;

impl BuiltinFn for Cos {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.cos()),
            _ => panic!(),
        }
    }
}

/// In radians
pub struct Tan;

impl BuiltinFn for Tan {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.tan()),
            _ => panic!(),
        }
    }
}

/// In radians
pub struct Asin;

impl BuiltinFn for Asin {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.asin()),
            _ => panic!(),
        }
    }
}

/// In radians
pub struct Acos;

impl BuiltinFn for Acos {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.acos()),
            _ => panic!(),
        }
    }
}

/// In radians
pub struct Atan;

impl BuiltinFn for Atan {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.atan()),
            _ => panic!(),
        }
    }
}

/// In radians
pub struct Atan2;

impl BuiltinFn for Atan2 {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let args = args.unwrap();
        let v = args.get(0).unwrap().clone();
        let a = args.get(1).unwrap().clone();

        match (v, a) {
            (Value::Float(v), Value::Float(a)) => Value::Float(v.atan2(a)),
            _ => panic!(),
        }
    }
}

pub struct ToRadians;

impl BuiltinFn for ToRadians {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.to_radians()),
            _ => panic!(),
        }
    }
}

pub struct ToDegrees;

impl BuiltinFn for ToDegrees {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.to_degrees()),
            _ => panic!(),
        }
    }
}

pub struct FPowF;

impl BuiltinFn for FPowF {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let args = args.unwrap();
        let b = args.get(0).unwrap().clone();
        let p = args.get(1).unwrap().clone();

        match (b, p) {
            (Value::Float(b), Value::Float(p)) => Value::Float(b.powf(p)),
            _ => panic!(),
        }
    }
}

pub struct FPowI;

impl BuiltinFn for FPowI {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let args = args.unwrap();
        let b = args.get(0).unwrap().clone();
        let p = args.get(1).unwrap().clone();

        match (b, p) {
            (Value::Float(b), Value::Int(p)) => Value::Float(b.powi(p)),
            _ => panic!(),
        }
    }
}

pub struct IPow;

impl BuiltinFn for IPow {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let args = args.unwrap();
        let b = args.get(0).unwrap().clone();
        let p = args.get(1).unwrap().clone();

        match (b, p) {
            (Value::Int(b), Value::Int(p)) => Value::Int(b.pow(p as u32)),
            _ => panic!(),
        }
    }
}

pub struct Floor;

impl BuiltinFn for Floor {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.floor()),
            _ => panic!(),
        }
    }
}

pub struct Ceil;

impl BuiltinFn for Ceil {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.ceil()),
            _ => panic!(),
        }
    }
}

pub struct Round;

impl BuiltinFn for Round {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.round()),
            _ => panic!(),
        }
    }
}