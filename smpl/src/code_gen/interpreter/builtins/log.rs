use code_gen::interpreter::*;

pub struct Print;

impl BuiltInFn for Print {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let args = args.expect("Print() expects at least one argument");

        for arg in args {
            print!("{}", arg);
        }

        Value::Unit
    }
}

pub struct Println;

impl BuiltInFn for Println {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {

        if let Some(args) = args {
            for arg in args {
                print!("{}", arg);
            }
        }

        print!("\n");

        Value::Unit
    }
}
