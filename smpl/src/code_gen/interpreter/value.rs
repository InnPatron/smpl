pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
    Function(Function),
    Struct(Struct),

    Builtin,
}

pub struct Function;

pub struct Struct(Vec<Value>);

impl Struct {
    pub fn new(v: Vec<Value>) -> Struct {
        Struct(v)
    }

    pub fn set_field(&mut self, nv: Value) {
    }
}
