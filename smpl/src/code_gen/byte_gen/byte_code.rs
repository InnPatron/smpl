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
    Return(Arg),

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
    Namespace(String),
    Tmp(String),
}

#[derive(Debug)]
pub enum Arg {
    Location(Location),
    FieldAccess(Location, Vec<String>),
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}
