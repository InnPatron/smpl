use code_gen::interpreter::*;

pub struct Panic;

impl BuiltInFn for Panic {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        match a {
            Value::String(s) => panic!("{}", s),
            _ => unreachable!(),
        }
    }
}
