use std::collections::HashMap;
use std::fmt;

pub type InstructionPointerType = u64;

#[derive(Debug, Clone)]
pub enum Instruction {
    Store(Location, Arg),
    StoreStructure(Location, HashMap<String, Arg>),
    StoreArray1(Location, Vec<Arg>),
    StoreArray2(Location, Arg, u64),

    AddI(Location, Arg, Arg),
    SubI(Location, Arg, Arg),
    MulI(Location, Arg, Arg),
    DivI(Location, Arg, Arg),
    ModI(Location, Arg, Arg),

    AddF(Location, Arg, Arg),
    SubF(Location, Arg, Arg),
    MulF(Location, Arg, Arg),
    DivF(Location, Arg, Arg),
    ModF(Location, Arg, Arg),

    And(Location, Arg, Arg),
    Or(Location, Arg, Arg),

    GEqI(Location, Arg, Arg),
    LEqI(Location, Arg, Arg),
    GEI(Location, Arg, Arg),
    LEI(Location, Arg, Arg),

    GEqF(Location, Arg, Arg),
    LEqF(Location, Arg, Arg),
    GEF(Location, Arg, Arg),
    LEF(Location, Arg, Arg),

    Eq(Location, Arg, Arg),
    InEq(Location, Arg, Arg),

    Negate(Location, Arg),
    Invert(Location, Arg),

    FnCall(Location, Vec<Arg>), // Function to call, args
    Return(Option<Arg>),
    TakeReturn(Location), // Where to store return value

    Jump(JumpTarget),
    JumpCondition(JumpTarget, Arg), // Jump when Arg is true
    JumpNegateCondition(JumpTarget, Arg), // Jump when Arg is false

    RelJump(RelJumpTarget),
    RelJumpCondition(RelJumpTarget, Arg), // Jump when Arg is true
    RelJumpNegateCondition(RelJumpTarget, Arg), // Jump when Arg is false
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::Instruction::*;

        match *self {
            Store(ref location, ref arg) => {
                write!(f, "store {}, {}", location, arg)
            }

            StoreStructure(ref location, ref map) => {
                write!(f, "store_struct {}", location)?;

                for (key, arg) in map.iter() {
                    write!(f, ", ({}, {})", key, arg)?;
                }

                Ok(())
            }

            StoreArray1(ref location, ref args) => {
                write!(f, "store_array1 size={}, {} ", args.len(), location)?;

                for arg in args.iter() {
                    write!(f, ", {}", arg)?;
                }

                Ok(())
            }

            StoreArray2(ref location, ref element, ref size) => write!(
                f,
                "store_array2 size={}, {}, {}",
                size, location, element
            ),

            AddI(ref location, ref arg1, ref arg2) => {
                write!(f, "addi {}, {}, {}", location, arg1, arg2)
            }

            SubI(ref location, ref arg1, ref arg2) => {
                write!(f, "subi {}, {}, {}", location, arg1, arg2)
            }

            MulI(ref location, ref arg1, ref arg2) => {
                write!(f, "muli {}, {}, {}", location, arg1, arg2)
            }

            DivI(ref location, ref arg1, ref arg2) => {
                write!(f, "divi {}, {}, {}", location, arg1, arg2)
            }

            ModI(ref location, ref arg1, ref arg2) => {
                write!(f, "modi {}, {}, {}", location, arg1, arg2)
            }

            AddF(ref location, ref arg1, ref arg2) => {
                write!(f, "addf {}, {}, {}", location, arg1, arg2)
            }

            SubF(ref location, ref arg1, ref arg2) => {
                write!(f, "subf {}, {}, {}", location, arg1, arg2)
            }

            MulF(ref location, ref arg1, ref arg2) => {
                write!(f, "mulf {}, {}, {}", location, arg1, arg2)
            }

            DivF(ref location, ref arg1, ref arg2) => {
                write!(f, "divf {}, {}, {}", location, arg1, arg2)
            }

            ModF(ref location, ref arg1, ref arg2) => {
                write!(f, "modf {}, {}, {}", location, arg1, arg2)
            }

            And(ref location, ref arg1, ref arg2) => {
                write!(f, "and {}, {}, {}", location, arg1, arg2)
            }

            Or(ref location, ref arg1, ref arg2) => {
                write!(f, "or {}, {}, {}", location, arg1, arg2)
            }

            GEqI(ref location, ref arg1, ref arg2) => {
                write!(f, "geqi {}, {}, {}", location, arg1, arg2)
            }

            LEqI(ref location, ref arg1, ref arg2) => {
                write!(f, "leqi {}, {}, {}", location, arg1, arg2)
            }

            GEI(ref location, ref arg1, ref arg2) => {
                write!(f, "gei {}, {}, {}", location, arg1, arg2)
            }

            LEI(ref location, ref arg1, ref arg2) => {
                write!(f, "lei {}, {}, {}", location, arg1, arg2)
            }

            GEqF(ref location, ref arg1, ref arg2) => {
                write!(f, "geqf {}, {}, {}", location, arg1, arg2)
            }

            LEqF(ref location, ref arg1, ref arg2) => {
                write!(f, "leqf {}, {}, {}", location, arg1, arg2)
            }

            GEF(ref location, ref arg1, ref arg2) => {
                write!(f, "gef {}, {}, {}", location, arg1, arg2)
            }

            LEF(ref location, ref arg1, ref arg2) => {
                write!(f, "lef {}, {}, {}", location, arg1, arg2)
            }

            Eq(ref location, ref arg1, ref arg2) => {
                write!(f, "eq {}, {}, {}", location, arg1, arg2)
            }

            InEq(ref location, ref arg1, ref arg2) => {
                write!(f, "neq {}, {}, {}", location, arg1, arg2)
            }

            Negate(ref location, ref arg) => {
                write!(f, "negate {}, {}", location, arg)
            }

            Invert(ref location, ref arg) => {
                write!(f, "negate {}, {}", location, arg)
            }

            FnCall(ref location, ref args) => {
                write!(f, "call size={}, {} ", args.len(), location)?;

                for arg in args.iter() {
                    write!(f, ", {}", arg)?;
                }

                Ok(())
            }

            Return(ref arg) => match *arg {
                Some(ref to_return) => write!(f, "return {}", to_return),

                None => write!(f, "return <none>"),
            },

            TakeReturn(ref location) => write!(f, "take {}", location),

            Jump(ref target) => write!(f, "jump {}", target),

            JumpCondition(ref target, ref arg) => {
                write!(f, "jump {}, condition={}", target, arg)
            }

            JumpNegateCondition(ref target, ref arg) => {
                write!(f, "jump {}, invert-condition={}", target, arg)
            }

            RelJump(ref target) => write!(f, "rel_jump {}", target),

            RelJumpCondition(ref target, ref arg) => {
                write!(f, "rel_jump {}, condition={}", target, arg)
            }

            RelJumpNegateCondition(ref target, ref arg) => {
                write!(f, "rel_jump {}, invert-condition={}", target, arg)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Display)]
#[display(fmt = "{}", _0)]
pub struct JumpTarget(InstructionPointerType);

impl JumpTarget {
    pub fn new(t: InstructionPointerType) -> JumpTarget {
        JumpTarget(t)
    }

    pub fn absolute_target(&self) -> InstructionPointerType {
        self.0
    }
}

#[derive(Debug, Clone, Copy, Display)]
#[display(fmt = "{}", _0)]
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

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Location::Compound {
                ref root,
                ref root_index,
                ref path,
            } => {
                write!(f, "{}", root)?;

                if let Some(ref root_index) = *root_index {
                    write!(f, "[{}]", root_index)?;
                }

                for field_access in path.iter() {
                    write!(f, "{}", field_access)?;
                }

                Ok(())
            }

            Location::Namespace(ref n) | Location::Tmp(ref n) => {
                write!(f, "{}", n)
            }
        }
    }
}

#[derive(Debug, Clone, Display)]
pub enum FieldAccess {
    #[display(fmt = ".{}", _0)]
    Field(String),

    #[display(fmt = ".{}[{}]", field, index_tmp)]
    FieldIndex { field: String, index_tmp: String },
}

#[derive(Debug, Clone, Display)]
pub enum Arg {
    #[display(fmt = "{}", _0)]
    Location(Location),

    #[display(fmt = "{}", _0)]
    Int(i64),

    #[display(fmt = "{}", _0)]
    Float(f64),

    #[display(fmt = "{}", _0)]
    Bool(bool),

    #[display(fmt = "\"{}\"", _0)]
    String(String),
}
