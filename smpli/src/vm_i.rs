use failure::Error;

use smpl::{FnId, TypeId};

use super::value::Value;

pub type BuiltinFn = fn(args: Option<Vec<Value>>) -> Result<Value, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum ModQuery {
    FnHandle(FnHandle),
    TypeHandle(TypeHandle),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FnHandle(FnId);

impl FnHandle {
    pub(crate) fn id(&self) -> FnId {
        self.0.clone()
    }
}

impl From<FnId> for FnHandle {
    fn from(f: FnId) -> FnHandle {
        FnHandle(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeHandle(TypeId);

impl TypeHandle {
    pub(crate) fn id(&self) -> TypeId {
        self.0.clone()
    }
}

impl From<TypeId> for TypeHandle {
    fn from(t: TypeId) -> TypeHandle {
        TypeHandle(t)
    }
}
