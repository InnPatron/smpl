use std::collections::HashMap;
use std::fmt;

use crate::span::Span;
use crate::ast_node::AstNode;
use crate::expr_ast::{ Expr, Block };
use crate::typable_ast::{Typed, Typable};

pub struct Module {
    pub ident: Option<AstNode<Ident>>,
    pub top_levels: Vec<DeclStmt>,
}

#[derive(Clone)]
pub enum DeclStmt {
    Use(AstNode<UseDecl>),
    Opaque(AstNode<Opaque>),
    Struct(AstNode<Struct>),
    Function(AstNode<Function>),
    BuiltinFunction(AstNode<BuiltinFunction>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl(pub AstNode<Ident>);

#[derive(Debug, Clone, PartialEq)]
pub struct BuiltinFunction {
    pub name: AstNode<Ident>,
    pub params: BuiltinFnParams,
    pub return_type: Option<Typable<AstNode<TypeAnn>>>,
    pub annotations: Vec<Annotation>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinFnParams {
    Unchecked,
    Checked(Vec<Typable<AstNode<FnParameter>>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: AstNode<Ident>,
    pub params: Vec<Typable<AstNode<FnParameter>>>,
    pub return_type: Option<Typable<AstNode<TypeAnn>>>,
    pub body: Typable<AstNode<Block>>,
    pub annotations: Vec<Annotation>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParameter {
    pub name: AstNode<Ident>,
    pub param_type: AstNode<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Opaque {
    pub name: AstNode<Ident>,
    pub annotations: Vec<Annotation>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: AstNode<Ident>,
    pub body: StructBody,
    pub annotations: Vec<Annotation>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhereClause(pub HashMap<AstNode<Ident>, Vec<AstNode<TypeAnn>>>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructBody(pub Option<Vec<StructField>>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: AstNode<Ident>,
    pub field_type: Typable<AstNode<TypeAnn>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAnn {
    Path(TypedPath),
    Array(Box<AstNode<TypeAnn>>, u64),
    FnType(
        Option<TypeParams>,
        Option<Vec<AstNode<TypeAnn>>>,
        Option<Box<AstNode<TypeAnn>>>,
    ),
    WidthConstraint(Vec<AstNode<WidthConstraint>>),
}

// TODO: May need to manually implement PartialEq
// Shouldn't matter b/c this is purely for syntactic comparison
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParams {
    pub params: Vec<AstNode<Ident>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum WidthConstraint {
    BaseStruct(AstNode<TypeAnn>),
    Anonymous(Vec<(AstNode<Ident>, AstNode<TypeAnn>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedPath {
    pub base: Typable<ModulePath>,
    pub params: Vec<Typable<AstNode<TypeAnn>>>,
}

impl TypedPath {
    pub fn nil_arity(base: Typable<ModulePath>) -> Self {
        TypedPath {
            base,
            params: Vec::with_capacity(0),
        }
    }

    pub fn n_arity(base: Typable<ModulePath>, params: Vec<Typable<AstNode<TypeAnn>>>) -> Self {
        TypedPath {
            base,
            params,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModulePath(pub Vec<AstNode<Ident>>);

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub keys: Vec<(Ident, Option<String>)>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl<T> From<T> for Ident where T: Into<String> {
    fn from(s: T) -> Ident {
        Ident(s.into())
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
