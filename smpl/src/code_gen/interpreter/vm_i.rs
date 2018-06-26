use analysis::{FnId, TypeId};

use super::value::Value;

pub trait BuiltinFn {
    fn execute(&self, args: Option<Vec<Value>>) -> Value;
}

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
