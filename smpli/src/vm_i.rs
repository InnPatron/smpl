use failure::Error;

use smpl::{FnId, TypeId, ModuleId};

use super::value::Value;

pub type BuiltinFn = fn(args: Option<Vec<Value>>) -> Result<Value, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum ModQuery {
    FnHandle(FnHandle),
    TypeHandle(TypeHandle),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct FnHandle(ModuleId, FnId);

impl FnHandle {
    pub(crate) fn new(mod_id: ModuleId, fn_id: FnId) -> FnHandle {
        FnHandle(mod_id, fn_id)
    }

    pub(crate) fn fn_id(&self) -> FnId {
        self.1.clone()
    }

    pub(crate) fn mod_id(&self) -> ModuleId {
        self.0.clone()
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
