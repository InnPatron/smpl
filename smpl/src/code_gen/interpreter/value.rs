use analysis::FnId;

#[derive(Debug, Clone, PartialOrd,PartialEq )]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
    Function(FnId),
    Struct(Struct),
    Unit,
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct Struct(Vec<Value>);

impl Struct {
    pub fn new(v: Vec<Value>) -> Struct {
        Struct(v)
    }

    pub fn set_field(&mut self, nv: Value) {
    }
}
