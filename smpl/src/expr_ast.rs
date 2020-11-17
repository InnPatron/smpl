use std::fmt::{self, Debug};
use std::marker::PhantomData;

use crate::ast::{Ident, Name, TypeAnn, TypedPath};
use crate::ast_node::{AstNode, Spanned};
use crate::span::Span;

#[derive(Clone, Debug)]
pub enum Stmt {
    If(AstNode<If>),
    While(AstNode<While>),
    Let(AstNode<LetStmt>),
    Return(AstNode<ReturnStmt>),
    Break(AstNode<BreakStmt>),
    Extract(AstNode<ExtractStmt>),
    Continue(AstNode<ContinueStmt>),
    Block(AstNode<Block>),
    ExprStmt(Expr),
}

#[derive(Clone, Debug)]
pub struct ContinueStmt;

#[derive(Clone, Debug)]
pub struct ExtractStmt {
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct BreakStmt {
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ReturnStmt {
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct If {
    pub branches: Vec<Branch>,
    pub default_branch: Option<AstNode<Block>>,
}

#[derive(Clone, Debug)]
pub struct Branch {
    pub condition: Expr,
    pub block: AstNode<Block>,
}

#[derive(Clone, Debug)]
pub struct While {
    pub branches: Vec<Branch>,
    pub default_branch: Option<AstNode<Block>>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub return_expr: Option<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct LetStmt {
    pub name: AstNode<Name>,
    pub type_ann: Option<AstNode<TypeAnn>>,
    pub init: Expr,
}

#[derive(Clone, Debug)]
pub struct DotAccess {
    pub base: Box<Expr>,
    pub field: AstNode<Name>,
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub params: Vec<AstNode<LambdaParam>>,
    pub return_type: Option<AstNode<TypeAnn>>,
    pub body: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct LambdaParam {
    pub name: AstNode<Name>,
    pub ann: Option<AstNode<TypeAnn>>,
}

#[derive(Clone, Debug)]
pub struct UniExpr {
    pub op: UniOp,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct BinExpr {
    pub op: AstNode<BinOp>,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct FnCall {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct StructInit {
    pub struct_name: Option<AstNode<TypedPath>>,
    pub field_init: Vec<(AstNode<Ident>, Box<Expr>)>,
}

impl StructInit {
    pub fn is_anonymous(&self) -> bool {
        self.struct_name.is_none()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(LiteralData<PhantomLitString>),
    Int(LiteralData<PhantomLitInt>),
    Float(LiteralData<PhantomLitFloat>),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Assign,
    Pipe,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UniOp {
    Ref,
    Deref,
    Not,
}

#[derive(Clone, Debug)]
pub enum Expr {
    If(Box<AstNode<If>>),
    While(Box<AstNode<While>>),
    Bin(AstNode<BinExpr>),
    Uni(AstNode<UniExpr>),
    Literal(AstNode<Literal>),
    DotAccess(AstNode<DotAccess>),
    FnCall(AstNode<FnCall>),
    StructInit(AstNode<StructInit>),
    Lambda(AstNode<Lambda>),
    Path(AstNode<TypedPath>),
    Block(AstNode<Block>),
    Underscore(AstNode<()>),
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match *self {
            Expr::If(ref spanned, ..) => spanned.span(),
            Expr::While(ref spanned, ..) => spanned.span(),
            Expr::Bin(ref spanned, ..) => spanned.span(),
            Expr::Uni(ref spanned, ..) => spanned.span(),
            Expr::Literal(ref spanned, ..) => spanned.span(),
            Expr::DotAccess(ref spanned, ..) => spanned.span(),
            Expr::FnCall(ref spanned, ..) => spanned.span(),
            Expr::StructInit(ref spanned, ..) => spanned.span(),
            Expr::Lambda(ref spanned, ..) => spanned.span(),
            Expr::Path(ref spanned, ..) => spanned.span(),
            Expr::Block(ref spanned, ..) => spanned.span(),
            Expr::Underscore(ref spanned, ..) => spanned.span(),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Assign => write!(f, "="),
            BinOp::Pipe => write!(f, "|>"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

impl fmt::Display for UniOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UniOp::Ref => write!(f, "&"),
            UniOp::Deref => write!(f, "@"),
            UniOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LiteralData<T> {
    pub data: String,
    pub suffix: LiteralSuffix,
    pub phantom: PhantomData<T>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralSuffix {
    Float(Option<String>),
    Int(Option<String>),
    String(Option<String>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PhantomLitString;
#[derive(Clone, Debug, PartialEq)]
pub struct PhantomLitFloat;
#[derive(Clone, Debug, PartialEq)]
pub struct PhantomLitInt;
