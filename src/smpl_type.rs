use std::collections::HashMap;

use semantic_ck::TypeId;
use ast::Ident;

#[derive(Clone, Debug, PartialEq)]
pub enum SmplType {
    Function(FunctionType),
    Struct(StructType),
    Int,
    Float,
    String,
    Bool,
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructType {
    pub name: Ident,
    pub fields: HashMap<Ident, TypeId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub args: Vec<TypeId>,
    pub return_type: TypeId,
}
