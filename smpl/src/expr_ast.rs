use crate::span::Span;
use crate::ast_node::{EmptyAstNode, AstNode};

use crate::new_ast::{Ident, TypedPath, TypeAnnotation, FnParameter};

use crate::analysis::{FieldId, VarId, FnId};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    ExprStmt(ExprStmt),
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprStmt {
    If(AstNode<If>),
    While(AstNode<While>),
    LocalVarDecl(AstNode<LocalVarDecl>),
    Assignment(AstNode<Assignment>),
    Return(AstNode<Option<Expr>>),
    Break(AstNode<Option<Expr>>),
    Continue(EmptyAstNode),
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub branches: Vec<Branch>,
    pub default_branch: Option<AstNode<Block>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub conditional: AstNode<Expr>,
    pub block: AstNode<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub conditional: AstNode<Expr>,
    pub body: AstNode<Block>,
    pub default_branch: Option<AstNode<Block>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

#[derive(Clone, Debug, PartialEq)]
pub struct LocalVarDecl {
    pub var_type: Option<AstNode<TypeAnnotation>>,
    pub var_name: AstNode<Ident>,
    pub var_init: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub name: Box<AstNode<Access>>,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    If(Box<AstNode<If>>),
    While(Box<AstNode<While>>),
    Bin(AstNode<BinExpr>),
    Uni(AstNode<UniExpr>),
    Literal(AstNode<Literal>),
    Binding(AstNode<Ident>),
    Access(Box<AstNode<Access>>),
    FnCall(AstNode<FnCall>),
    StructInit(AstNode<StructInit>),
    ArrayInit(AstNode<ArrayInit>),
    AnonymousFn(AstNode<AnonymousFn>),
    Path(AstNode<TypedPath>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnonymousFn {
    pub params: Option<Vec<AstNode<FnParameter>>>,
    pub return_type: Option<AstNode<TypeAnnotation>>,
    pub body: AstNode<Block>,
    pub fn_id: Option<FnId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Access {
    root_name: AstNode<Ident>,
    root_indexing: Option<Expr>,
    root_var: Option<VarId>,
    path: Vec<self::FASegment>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum FASegment {
    Ident(Field),
    Indexing(Field, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    name: AstNode<Ident>,
    field_id: Option<FieldId>,
}

impl Field {

    pub fn name(&self) -> &Ident {
        self.name.node()
    }

    pub fn field_id(&self) -> FieldId {

        self.field_id.as_ref().cloned().expect("No field id")
    }

    pub fn set_field_id(&mut self, id: FieldId) {
        if self.field_id.is_some() {
            panic!("Attempting to override field id.");
        }

        self.field_id = Some(id);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UniExpr {
    pub op: UniOp,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinExpr {
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub path: AstNode<TypedPath>,
    pub args: Option<Vec<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInit {
    pub struct_name: Option<TypedPath>,
    pub field_init: Vec<(AstNode<Ident>, Box<Expr>)>,
}

impl StructInit {
    pub fn is_anonymous(&self) -> bool {
        self.struct_name.is_none()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayInit {
    InitList(Vec<Expr>),
    Value(Box<Expr>, u64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Pipe,
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    LogicalAnd,
    LogicalOr,
    GreaterEq,
    LesserEq,
    Greater,
    Lesser,
    Eq,
    InEq,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UniOp {
    Negate,
    LogicalInvert,
}
