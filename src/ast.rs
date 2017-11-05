use std::str::FromStr;
use std::fmt;
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
    fn_id: Option<u64>
}

impl Function {
    pub fn new(name: Ident, args: Option<Vec<FnArg>>, return_type: Option<Path>, body: Block) -> Function {
        Function {
            name: name,
            args: args,
            return_type: return_type,
            body: AstNode::untyped(body),
            fn_id: None,
        }
    }

    pub fn fn_id(&self) -> Option<u64> {
        self.fn_id
    }

    pub fn set_fn_id(&mut self, id: u64) {
        self.fn_id = Some(id);
    }
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
    Expr(AstNode<Expr>),
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
    base_ident_id: Option<u64>,
}

impl Assignment {
    pub fn new(name: Path, value: Expr) -> Assignment {
        Assignment {
            name: AstNode::untyped(name),
            value: AstNode::untyped(value),
            base_ident_id: None
        }
    }

    pub fn base_ident_id(&self) -> Option<u64> {
        self.base_ident_id
    }

    pub fn set_base_ident_id(&mut self, ident: u64) {
        self.base_ident_id = Some(ident);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocalVarDecl {
    pub var_type: Path,
    pub var_name: Ident,
    pub var_init: AstNode<Expr>,
    var_id: Option<u64>,
}

impl LocalVarDecl {
    pub fn new(var_type: Path, var_name: Ident, var_init: AstNode<Expr>) -> LocalVarDecl {
        LocalVarDecl {
            var_type: var_type,
            var_name: var_name,
            var_init: var_init,
            var_id: None
        }
    }

    pub fn var_id(&self) -> Option<u64> {
        self.var_id
    }

    pub fn set_var_id(&mut self, id: u64) {
        self.var_id = Some(id);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub branches: Vec<Branch>,
    pub default_block: Option<Block>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub name: Ident,
    pub args: Option<Vec<AstNode<Expr>>>,
    fn_id: Option<u64>
}

impl FnCall {
    pub fn new(name: Ident, args: Option<Vec<AstNode<Expr>>>) -> FnCall {
        FnCall {
            name: name,
            args: args, 
            fn_id: None,
        }
    }

    pub fn fn_id(&self) -> Option<u64> {
        self.fn_id
    }

    pub fn set_fn_id(&mut self, id: u64) {
        self.fn_id = Some(id);
    }
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub AsciiString);

impl Ident {
    pub fn new(str: &str) -> Ident {
        Ident(AsciiString::from_str(str).unwrap())
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Path(pub Vec<Ident>);

impl From<Ident> for Path {
    fn from(ident: Ident) -> Path {
        Path(vec![ident])
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let buffer = self.0.iter().fold(AsciiString::new(), 
                                        |mut buffer, ref item| {
                                            buffer.push_str(&item.0); 
                                            buffer
                                        });
        write!(f, "{}", buffer)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    If,
    While,
    Break,
    Return,
    Struct,
    Function,
}
