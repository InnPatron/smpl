use code_gen::interpreter::*;

/// In radians
pub struct Sin;

impl BuiltInFn for Sin {
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

impl BuiltInFn for Cos {
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

impl BuiltInFn for Tan {
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

impl BuiltInFn for Asin {
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

impl BuiltInFn for Acos {
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

impl BuiltInFn for Atan {
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

impl BuiltInFn for Atan2 {
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

impl BuiltInFn for ToRadians {
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

impl BuiltInFn for ToDegrees {
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

impl BuiltInFn for FPowF {
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

impl BuiltInFn for FPowI {
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

impl BuiltInFn for IPow {
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

impl BuiltInFn for Floor {
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

impl BuiltInFn for Ceil {
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

impl BuiltInFn for Round {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let v = args.remove(0);

        match v {
            Value::Float(f) => Value::Float(f.round()),
            _ => panic!(),
        }
    }
}
