use std::collections::HashMap;
use std::fmt::{self, Debug};

use crate::span::Span;
use crate::ast_node::AstNode;
use crate::expr_ast::{ Expr, Block };
use crate::typable_ast::{Typed, Typable};

pub type TypedNode<T> = Typable<AstNode<T>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub ident: Option<AstNode<Ident>>,
    pub top_levels: Vec<DeclStmt<S, E>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclStmt<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    Use(AstNode<UseDecl>),
    Opaque(AstNode<Opaque>),
    Struct(AstNode<Struct>),
    Function(AstNode<Function<S, E>>),
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
pub struct Function<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub name: AstNode<Ident>,
    pub params: Vec<TypedNode<FnParameter>>,
    pub return_type: Option<TypedNode<TypeAnn>>,
    pub body: TypedNode<Block<S, E>>,
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
    pub body: Vec<StructField>,
    pub annotations: Vec<Annotation>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhereClause(pub HashMap<AstNode<Ident>, Vec<AstNode<TypeAnn>>>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: AstNode<Ident>,
    pub field_type: TypedNode<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAnn {
    ModulePath(TypedNode<ModulePath>),
    Path(TypedNode<TypedPath>),
    Array(Box<TypedNode<TypeAnn>>, u64),
    FnType(
        Option<TypeParams>,
        Vec<TypedNode<TypeAnn>>,
        Option<Box<TypedNode<TypeAnn>>>,
    ),
    WidthConstraints(Vec<WidthConstraint>),
}

// TODO: May need to manually implement PartialEq
// Shouldn't matter b/c this is purely for syntactic comparison
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParams {
    pub params: Vec<AstNode<Ident>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum WidthConstraint {
    BaseStruct(TypedNode<TypeAnn>),
    Anonymous(Vec<StructField>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedPath {
    pub base: TypedNode<ModulePath>,
    pub args: Vec<TypedNode<TypeAnn>>,
}

impl TypedPath {
    pub fn nil_arity(base: TypedNode<ModulePath>) -> Self {
        TypedPath {
            base,
            args: Vec::with_capacity(0),
        }
    }

    pub fn n_arity(base: TypedNode<ModulePath>, args: Vec<TypedNode<TypeAnn>>) -> Self {
        TypedPath {
            base,
            args,
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
