use std::collections::HashMap;

pub type InstructionPointerType = u64;

#[derive(Debug, Clone)]
pub enum Instruction {
    Store(Location, Arg),
    StoreStructure(Location, HashMap<String, Arg>),
    StoreArray1(Location, Vec<Arg>),
    StoreArray2(Location, Arg, u64),

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
    JumpCondition(JumpTarget, Arg),                     // Jump when Arg is true
    JumpNegateCondition(JumpTarget, Arg),               // Jump when Arg is false
    JumpE(JumpTarget, Arg, Arg),
    JumpNE(JumpTarget, Arg, Arg),
    JumpGE(JumpTarget, Arg, Arg),
    JumpLE(JumpTarget, Arg, Arg),
    JumpG(JumpTarget, Arg, Arg),
    JumpL(JumpTarget, Arg, Arg),

    RelJump(RelJumpTarget),
    RelJumpCondition(RelJumpTarget, Arg),               // Jump when Arg is true
    RelJumpNegateCondition(RelJumpTarget, Arg),         // Jump when Arg is false
    RelJumpE(RelJumpTarget, Arg, Arg),
    RelJumpNE(RelJumpTarget, Arg, Arg),
    RelJumpGE(RelJumpTarget, Arg, Arg),
    RelJumpLE(RelJumpTarget, Arg, Arg),
    RelJumpG(RelJumpTarget, Arg, Arg),
    RelJumpL(RelJumpTarget, Arg, Arg),
}

#[derive(Debug, Clone, Copy)]
pub struct JumpTarget(InstructionPointerType);

impl JumpTarget {
    pub fn new(t: InstructionPointerType) -> JumpTarget {
        JumpTarget(t)
    }

    pub fn absolute_target(&self) -> InstructionPointerType {
        self.0
    }
}


#[derive(Debug, Clone, Copy)]
pub struct RelJumpTarget(i64);

impl RelJumpTarget {
    pub fn new(t: i64) -> RelJumpTarget {
        RelJumpTarget(t)
    }

    pub fn relative_target(&self) -> i64 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub enum Location {
    Compound {
        root: String,
        root_index: Option<String>,
        path: Vec<FieldAccess>,
    },
    Namespace(String),
    Tmp(String),
}

#[derive(Debug, Clone)]
pub enum FieldAccess {
    Field(String),
    FieldIndex {
        field: String,
        index_tmp: String
    }
}

#[derive(Debug, Clone)]
pub enum Arg {
    Location(Location),
    FieldAccess(Location, Vec<String>),
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Struct(Struct),
}

#[derive(Debug, Clone)]
pub struct Struct {
    field_map: HashMap<String, StructField>
}

#[derive(Debug, Clone)]
pub enum StructField {
    Int(i64),
    Float(i64),
    Bool(bool),
    String(String),
    Struct(Box<Struct>),
}
