use std::str::FromStr;
use std::fmt;
use std::slice::Iter;
use std::borrow::Borrow;

use ascii::AsciiString;

use span::Span;

#[derive(Debug, Clone)]
pub struct AstNode<T: ::std::fmt::Debug + Clone + PartialEq> {
    data: T,
    span: Span,
}

impl<T> PartialEq for AstNode<T> where T: ::std::fmt::Debug + Clone + PartialEq{
    fn eq(&self, other: &AstNode<T>) -> bool {
        self.data == other.data
    }
}

impl<T> AstNode<T> where T: ::std::fmt::Debug + Clone + PartialEq {
    pub fn new(data: T, span: Span) -> AstNode<T> {
        AstNode {
            data: data,
            span: span
        }
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

pub struct Module(pub Option<Ident>, pub Vec<DeclStmt>);

impl Module {
    pub fn name(&self) -> Option<&Ident> {
        self.0.as_ref()
    }

    pub fn set_name(&mut self, new: AsciiString) {
        self.0 = Some(Ident(new));
    }
}

pub enum DeclStmt {
    Use(AstNode<UseDecl>),
    Struct(AstNode<Struct>),
    Function(AstNode<Function>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl(pub Ident);

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub params: Option<Vec<AstNode<FnParameter>>>,
    pub return_type: Option<AstNode<TypeAnnotation>>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParameter {
    pub name: Ident,
    pub param_type: AstNode<TypeAnnotation>,
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
    pub field_type: AstNode<TypeAnnotation>,
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
    Return(Option<Expr>),
    Break,
    Continue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInit {
    pub struct_name: ModulePath,
    pub field_init: Option<Vec<(Ident, Box<Expr>)>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub name: Path,
    pub value: Expr,
}

impl Assignment {
    pub fn new(name: Path, value: Expr) -> Assignment {
        Assignment {
            name: name,
            value: value,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocalVarDecl {
    pub var_type: AstNode<TypeAnnotation>,
    pub var_name: Ident,
    pub var_init: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub branches: Vec<Branch>,
    pub default_block: Option<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
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
    Uni(UniExpr),
    Literal(Literal),
    Binding(Ident),
    FieldAccess(Path),
    FnCall(FnCall),
    StructInit(StructInit),
    ArrayInit(ArrayInit),
    Indexing(Indexing),
    ModAccess(ModulePath),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Indexing {
    pub array: Box<Expr>,
    pub indexer: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayInit {
    InitList(Vec<Expr>),
    Value(Box<Expr>, u64),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub path: ModulePath,
    pub args: Option<Vec<Expr>>,
}

impl FnCall {
    pub fn new(path: ModulePath, args: Option<Vec<Expr>>) -> FnCall {
        FnCall {
            path: path,
            args: args,
        }
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
    pub expr: Box<Expr>,
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
    String(AsciiString),
    Int(i64),
    Float(f64),
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

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAnnotation {
    Path(ModulePath),
    Array(Box<AstNode<TypeAnnotation>>, u64),
    FnType(Option<Vec<AstNode<TypeAnnotation>>>, Option<Box<AstNode<TypeAnnotation>>>),
}

impl<'a> From<&'a TypeAnnotation> for TypeAnnotationRef<'a> {
    fn from(t: &TypeAnnotation) -> TypeAnnotationRef {
        match t {
            &TypeAnnotation::Path(ref p) => TypeAnnotationRef::Path(p),
            &TypeAnnotation::Array(ref t, ref s) => TypeAnnotationRef::Array(t, s),
            &TypeAnnotation::FnType(ref p, ref r) => TypeAnnotationRef::FnType(p.as_ref().map(|v| v.as_slice()), r.as_ref().map(|r| r.borrow())),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAnnotationRef<'a> {
    Path(&'a ModulePath),
    Array(&'a AstNode<TypeAnnotation>, &'a u64),
    FnType(Option<&'a [AstNode<TypeAnnotation>]>, Option<&'a AstNode<TypeAnnotation>>),
}

impl<'a> From<TypeAnnotationRef<'a>> for TypeAnnotation {
    fn from(tr: TypeAnnotationRef) -> TypeAnnotation {
        match tr {
            TypeAnnotationRef::Path(p) => TypeAnnotation::Path(p.clone()),
            TypeAnnotationRef::Array(t, s) => TypeAnnotation::Array(Box::new(t.clone()), s.clone()),
            TypeAnnotationRef::FnType(p, r) => TypeAnnotation::FnType(
                p.map(|params| params.iter().map(|param| param.clone()).collect()), r.map(|r| Box::new(r.clone())))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModulePath(pub Vec<Ident>);

impl ModulePath {
    pub fn iter(&self) -> Iter<Ident> {
        self.0.iter()
    }
}

impl From<Ident> for ModulePath {
    fn from(ident: Ident) -> ModulePath {
        ModulePath(vec![ident])
    }
}

impl<'a> From<&'a ModulePath> for TypeAnnotationRef<'a> {
    fn from(p: &ModulePath) -> TypeAnnotationRef {
        TypeAnnotationRef::Path(p)
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let buffer = self.0
            .iter()
            .fold(AsciiString::new(), |mut buffer, ref item| {
                buffer.push_str(&item.0);
                buffer
            });
        write!(f, "{}", buffer)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Path(pub Vec<PathSegment>);

impl Path {
    pub fn iter(&self) -> Iter<PathSegment> {
        self.0.iter()
    }
}

impl From<Ident> for Path {
    fn from(ident: Ident) -> Path {
        Path(vec![PathSegment::Ident(ident)])
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PathSegment {
    Ident(Ident),
    Indexing(Ident, Box<Expr>),
}
