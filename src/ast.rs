use super::Span;
use ascii::AsciiString;

pub struct Program(pub Vec<Statement>);

pub struct Statement {
    span: Span
}

pub enum DeclStmt {
    Struct(Struct),
    Function(Function),
}

impl From<Struct> for DeclStmt {
    fn from(s: Struct) -> DeclStmt {
        DeclStmt::Struct(s)
    }
}

impl From<Function> for DeclStmt {
    fn from(f: Function) -> DeclStmt {
        DeclStmt::Function(f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<FnArg>,
    pub body: FnBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnBody;

#[derive(Debug, Clone, PartialEq)]
pub struct FnArg {
    pub name: Ident,
    pub arg_type: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: Ident,
    pub body: StructBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructBody(pub Vec<StructField>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Ident,
    pub field_type: Ident,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Exrp,
    ExprStmt(ExprStmt),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprStmt {
    If,
    While,
    LocalDecl,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub conditional: Expr,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assignment,
    Binary,
    Uni,
    Return,
    Break,
    Continue,
    Literal(Literal),
    Block,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(String),
    Number(String),
}

impl From<Literal> for Expr {
    fn from(literal: Literal) -> Expr {
        Expr::Literal(literal)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

#[derive(Clone, Debug, PartialEq)]
pub struct Ident(pub AsciiString);

#[derive(Clone, Debug, PartialEq)]
pub struct Path(pub Vec<Ident>);

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    If,
    While,
    Break,
    Return,
    Struct,
    Function,
}
