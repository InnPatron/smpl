use std::collections::HashMap;
use std::cell::Cell;

use analysis::{TypeId, VarId, FieldId};
use ast::Ident;

#[derive(Clone, Debug, PartialEq)]
pub enum SmplType {
    Function(FunctionType),
    Struct(StructType),
    Array(ArrayType),
    Int,
    Float,
    String,
    Bool,
    Unit,
}


#[derive(Clone, Debug, PartialEq)]
pub struct ArrayType {
    pub base_type: TypeId,
    pub size: u64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructType {
    pub name: Ident,
    pub fields: HashMap<FieldId, TypeId>,
    pub field_map: HashMap<Ident, FieldId>,
}

impl StructType {
    pub fn field_id(&self, name: &Ident) -> Option<FieldId> {
        self.field_map.get(name).map(|id| id.clone())
    }

    pub fn field_type(&self, id: FieldId) -> Option<TypeId> {
        self.fields.get(&id).map(|id| id.clone())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub params: Vec<FnParameter>,
    pub return_type: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnParameter {
    pub name: Ident,
    pub param_type: TypeId,
    var_id: Cell<Option<VarId>>,
}

impl FnParameter {
    pub fn new(name: Ident, param_type: TypeId) -> FnParameter {
        FnParameter {
            name: name,
            param_type: param_type,
            var_id: Cell::new(None),
        }
    }

    pub fn set_var_id(&self, id: VarId) {
        match self.var_id.get() {
            Some(previous) => panic!("Attempting to overwrite {} of parameter {} with {}", 
                                     previous, self.name, id),
            None => self.var_id.set(Some(id)),
        }
    }

    pub fn var_id(&self) -> Option<VarId> {
        self.var_id.get()
    }
}
