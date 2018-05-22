use code_gen::interpreter::*;

pub struct IntToFloat;

impl BuiltInFn for IntToFloat {
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

impl BuiltInFn for FloatToInt {
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

impl BuiltInFn for IsFloat {
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

impl BuiltInFn for IsInt {
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

impl BuiltInFn for StringToFloat {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        match a {
            Value::String(s) => Value::Float(s
                                             .parse::<f32>()
                                             .expect(&format!("{} was not a valid float.", s)
                                                     )
                                             ),
            _ => unreachable!(),
        }
    }
}

pub struct StringToInt;

impl BuiltInFn for StringToInt {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        match a {
            Value::String(s) => Value::Int(s
                                             .parse::<i32>()
                                             .expect(&format!("{} was not a valid int.", s)
                                                     )
                                             ),
            _ => unreachable!(),
        }
    }
}
