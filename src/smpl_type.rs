use std::collections::HashMap;
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
    pub fields: HashMap<Ident, SmplType>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub args: Vec<SmplType>,
    pub return_type: Box<SmplType>
}
