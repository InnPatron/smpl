use std::collections::HashMap;

use crate::analysis::{FieldId, TypeId};
use crate::ast::Ident;

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ConstructedType {
    Array(ArrayType),
    Function(FunctionType),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub params: ParamType,
    pub return_type: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ParamType {
    Unchecked,
    Checked(Vec<TypeId>),
}
