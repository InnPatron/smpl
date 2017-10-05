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
    While(While),
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
pub struct While {
    pub conditional: Expr,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Bin(BinExpr),
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

    GreaterEq,
    LesserEq,
    Greater,
    Lesser,
    Eq,
    InEq,
}

impl From<BinExpr> for Expr {
    fn from(bin: BinExpr) -> Expr {
        Expr::Bin(bin)
    }
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
