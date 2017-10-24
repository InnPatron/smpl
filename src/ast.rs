use std::str::FromStr;
use super::Span;
use smpl_type::SmplType;
use ascii::AsciiString;

#[derive(Debug, Clone, PartialEq)]
pub struct AstNode<T: Clone + PartialEq + ::std::fmt::Debug> {
    pub data: T,
    pub d_type: Option<SmplType>,
}

impl<T> AstNode<T> where T: Clone + PartialEq + ::std::fmt::Debug {
    pub fn untyped(data: T) -> AstNode<T> {
        AstNode {
            data: data,
            d_type: None,
        }
    }

    pub fn typed(data: T, d_type: SmplType) -> AstNode<T> {
        AstNode {
            data: data,
            d_type: Some(d_type),
        }
    }
}

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
    pub args: Option<Vec<FnArg>>,
    pub return_type: Option<Path>,
    pub body: AstNode<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnArg {
    pub name: Ident,
    pub arg_type: Path,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: Ident,
    pub body: StructBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructBody(pub Option<Vec<StructField>>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Ident,
    pub field_type: Path,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    ExprStmt(ExprStmt),
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprStmt {
    If(If),
    While(While),
    LocalVarDecl(LocalVarDecl),
    Assignment(Assignment),
    Return(AstNode<Expr>),
    Break,
    Continue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub name: AstNode<Path>,
    pub value: AstNode<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocalVarDecl {
    pub var_type: Path,
    pub var_name: Ident,
    pub var_init: AstNode<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub conditional: AstNode<Expr>,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub conditional: AstNode<Expr>,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Bin(AstNode<BinExpr>),
    Uni(AstNode<UniExpr>),
    Literal(AstNode<Literal>),
    Ident(AstNode<Ident>),
    FnCall(AstNode<FnCall>),
    Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub name: Ident,
    pub args: Option<Vec<Expr>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinExpr {
    pub op: BinOp,
    pub lhs: AstNode<Box<Expr>>,
    pub rhs: AstNode<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
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
pub struct UniExpr {
    pub op: UniOp,
    pub expr: AstNode<Box<Expr>>
}

#[derive(Clone, Debug, PartialEq)]
pub enum UniOp {
    Ref,
    Deref,
    Negate,
    LogicalInvert,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(String),
    Number(String),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

#[derive(Clone, Debug, PartialEq)]
pub struct Ident(pub AsciiString);

impl Ident {
    pub fn new(str: &str) -> Ident {
        Ident(AsciiString::from_str(str).unwrap())
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
