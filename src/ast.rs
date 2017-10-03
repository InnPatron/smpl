use super::Span;
use ascii::AsciiString;

pub struct Program(pub Vec<DeclStmt>);

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
    ExprStmt(ExprStmt),
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprStmt {
    If(If),
    While,
    LocalVarDecl,
    Assignment,
    Break,
    Continue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub conditional: Expr,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Bin(BinExpr),
    Uni(UniExpr),
    Literal(Literal),
    Ident(Ident),
    FnCall(FnCall),
    Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub name: Ident,
    pub args: Option<Vec<Expr>>
}

impl From<FnCall> for Expr {
    fn from(call: FnCall) -> Expr {
        Expr::FnCall(call)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinExpr {
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl From<BinExpr> for Expr {
    fn from(bin: BinExpr) -> Expr {
        Expr::Bin(bin)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UniExpr {
    pub op: UniOp,
    pub expr: Box<Expr>
}

#[derive(Clone, Debug, PartialEq)]
pub enum UniOp {
    Ref,
    Deref,
    Negate,
    LogicalInvert,
}

impl From<UniExpr> for Expr {
    fn from(uni: UniExpr) -> Expr {
        Expr::Uni(uni)
    }
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

impl From<Ident> for Expr {
    fn from(ident: Ident) -> Expr {
        Expr::Ident(ident)
    }
}

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
