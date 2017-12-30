use std::collections::HashMap;
use std::rc::Rc;

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
    pub fields: HashMap<Ident, Rc<SmplType>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub args: Vec<Rc<SmplType>>,
    pub return_type: Rc<SmplType>,
}
