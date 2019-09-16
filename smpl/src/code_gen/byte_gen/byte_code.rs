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
    
    FnCall(Location, Vec<Arg>),     // Function to call, args
    Return(Option<Arg>),
    TakeReturn(Location),           // Where to store return value

    Jump(JumpTarget),
    JumpCondition(JumpTarget, Arg),                     // Jump when Arg is true
    JumpNegateCondition(JumpTarget, Arg),               // Jump when Arg is false

    RelJump(RelJumpTarget),
    RelJumpCondition(RelJumpTarget, Arg),               // Jump when Arg is true
    RelJumpNegateCondition(RelJumpTarget, Arg),         // Jump when Arg is false
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
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}
