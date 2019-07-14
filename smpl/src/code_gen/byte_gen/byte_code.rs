use std::collections::HashMap;

pub struct Block {
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Store(Location, Arg),

    Add(Location, Arg, Arg),
    Sub(Location, Arg, Arg),
    Mul(Location, Arg, Arg),
    Div(Location, Arg, Arg),
    Mod(Location, Arg, Arg),

    And(Location, Arg, Arg),
    Or(Location, Arg, Arg),

    GEq(Location, Arg, Arg),
    LEq(Location, Arg, Arg),
    GE(Location, Arg, Arg),
    LE(Location, Arg, Arg),
    Eq(Location, Arg, Arg),
    InEq(Location, Arg, Arg),

    Negate(Location, Arg),
    Invert(Location, Arg),
    
    FnCall(Location, Location, Vec<Arg>),
    Return(Option<Arg>),

    Jump(JumpTarget),
    JumpCondition(JumpTarget, Arg),
    JumpE(JumpTarget, Arg, Arg),
    JumpNE(JumpTarget, Arg, Arg),
    JumpGE(JumpTarget, Arg, Arg),
    JumpLE(JumpTarget, Arg, Arg),
    JumpG(JumpTarget, Arg, Arg),
    JumpL(JumpTarget, Arg, Arg),
}

#[derive(Debug)]
pub struct JumpTarget;

#[derive(Debug)]
pub enum Location {
    Field {
        root: String,
        path: Vec<FieldAccess>,
    },
    Namespace(String),
    Tmp(String),
}

#[derive(Debug)]
pub enum FieldAccess {
    Field(String),
    FieldIndex {
        field: String,
        index_tmp: String
    }
}

#[derive(Debug)]
pub enum Arg {
    Location(Location),
    FieldAccess(Location, Vec<String>),
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Struct(Struct),
}

#[derive(Debug)]
pub struct Struct {
    field_map: HashMap<String, StructField>
}

#[derive(Debug)]
pub enum StructField {
    Int(i64),
    Float(i64),
    Bool(bool),
    String(String),
    Struct(Box<Struct>),
}
