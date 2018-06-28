use std::fmt;
use std::slice::Iter;
use std::borrow::Borrow;

use span::Span;

#[derive(Debug)]
pub struct AstNode<T: ::std::fmt::Debug> {
    data: T,
    span: Span,
}

impl<T> PartialEq for AstNode<T>
where
    T: ::std::fmt::Debug + PartialEq,
{
    fn eq(&self, other: &AstNode<T>) -> bool {
        self.data == other.data
    }
}

impl<T> Eq for AstNode<T>
where
    T: ::std::fmt::Debug + Eq,
{
}

impl<T> ::std::hash::Hash for AstNode<T>
where
    T: ::std::fmt::Debug + ::std::hash::Hash,
{
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

impl<T> Clone for AstNode<T>
where
    T: ::std::fmt::Debug + Clone,
{
    fn clone(&self) -> AstNode<T> {
        AstNode::new(self.data.clone(), self.span.clone())
    }
}

impl<T> AstNode<T>
where
    T: ::std::fmt::Debug,
{
    pub fn new(data: T, span: Span) -> AstNode<T> {
        AstNode {
            data: data,
            span: span,
        }
    }

    pub fn to_data(self) -> (T, Span) {
        (self.data, self.span)
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone)]
pub struct Module(pub Option<AstNode<Ident>>, pub Vec<DeclStmt>);

impl Module {
    pub fn name(&self) -> Option<&Ident> {
        match self.0 {
            Some(ref node) => Some(node.data()),
            None => None,
        }
    }

    pub fn name_if_none(&mut self, name: &str) {
        if self.0.is_none() {
            self.0 = Some(AstNode::new(Ident(name.to_string()), Span::new(0, 0)));
        }
    }
}

#[derive(Clone)]
pub enum DeclStmt {
    Use(AstNode<UseDecl>),
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
    pub return_type: Option<AstNode<TypeAnnotation>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinFnParams {
    Unchecked,
    Checked(Option<Vec<AstNode<FnParameter>>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: AstNode<Ident>,
    pub params: Option<Vec<AstNode<FnParameter>>>,
    pub return_type: Option<AstNode<TypeAnnotation>>,
    pub body: AstNode<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParameter {
    pub name: AstNode<Ident>,
    pub param_type: AstNode<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: AstNode<Ident>,
    pub body: StructBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructBody(pub Option<Vec<StructField>>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: AstNode<Ident>,
    pub field_type: AstNode<TypeAnnotation>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    ExprStmt(AstNode<ExprStmt>),
    Expr(AstNode<Expr>),
}

#[derive(Clone, Debug)]
pub enum ExprStmt {
    If(If),
    While(While),
    LocalVarDecl(LocalVarDecl),
    Assignment(Assignment),
    Return(Span, Option<Expr>),
    Break(Span),
    Continue(Span),
}

impl PartialEq for ExprStmt {
    fn eq(&self, other: &ExprStmt) -> bool {
        use self::ExprStmt::*;

        match (self, other) {
            (&If(ref lhs), &If(ref rhs)) => lhs == rhs,
            (&While(ref lhs), &While(ref rhs)) => lhs == rhs,
            (&LocalVarDecl(ref lhs), &LocalVarDecl(ref rhs)) => lhs == rhs,
            (&Assignment(ref lhs), &Assignment(ref rhs)) => lhs == rhs,
            (&Return(_, ref lhs), &Return(_, ref rhs)) => lhs == rhs,
            (&Break(..), &Break(..)) => true,
            (&Continue(..), &Continue(..)) => true,

            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInit {
    pub struct_name: ModulePath,
    pub field_init: Option<Vec<(AstNode<Ident>, Box<Expr>)>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub name: AstNode<Path>,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocalVarDecl {
    pub var_type: Option<AstNode<TypeAnnotation>>,
    pub var_name: AstNode<Ident>,
    pub var_init: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub branches: Vec<Branch>,
    pub default_block: Option<AstNode<Block>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub conditional: AstNode<Expr>,
    pub block: AstNode<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub conditional: AstNode<Expr>,
    pub block: AstNode<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Bin(AstNode<BinExpr>),
    Uni(AstNode<UniExpr>),
    Literal(AstNode<Literal>),
    Binding(AstNode<Ident>),
    FieldAccess(AstNode<Path>),
    FnCall(AstNode<FnCall>),
    StructInit(AstNode<StructInit>),
    ArrayInit(AstNode<ArrayInit>),
    Indexing(AstNode<Indexing>),
    ModAccess(AstNode<ModulePath>),
    AnonymousFn(AstNode<AnonymousFn>),
    FnCallChain(AstNode<FnCallChain>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnonymousFn {
    pub params: Option<Vec<AstNode<FnParameter>>>,
    pub return_type: Option<AstNode<TypeAnnotation>>,
    pub body: AstNode<Block>,
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
pub struct FnCallChain {
    pub base: AstNode<FnCall>,
    pub chain: Vec<AstNode<FnCall>>,
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
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub struct Block(pub Vec<Stmt>, pub Span, pub Span);

impl PartialEq for Block {
    fn eq(&self, other: &Block) -> bool {
        self.0 == other.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Ident {
    pub fn new(str: &str) -> Ident {
        Ident(str.to_string())
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
    FnType(
        Option<Vec<AstNode<TypeAnnotation>>>,
        Option<Box<AstNode<TypeAnnotation>>>,
    ),
}

impl<'a> From<&'a TypeAnnotation> for TypeAnnotationRef<'a> {
    fn from(t: &TypeAnnotation) -> TypeAnnotationRef {
        match t {
            &TypeAnnotation::Path(ref p) => TypeAnnotationRef::Path(p),
            &TypeAnnotation::Array(ref t, ref s) => TypeAnnotationRef::Array(t, s),
            &TypeAnnotation::FnType(ref p, ref r) => TypeAnnotationRef::FnType(
                p.as_ref().map(|v| v.as_slice()),
                r.as_ref().map(|r| r.borrow()),
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAnnotationRef<'a> {
    Path(&'a ModulePath),
    Array(&'a AstNode<TypeAnnotation>, &'a u64),
    FnType(
        Option<&'a [AstNode<TypeAnnotation>]>,
        Option<&'a AstNode<TypeAnnotation>>,
    ),
}

impl<'a> From<TypeAnnotationRef<'a>> for TypeAnnotation {
    fn from(tr: TypeAnnotationRef) -> TypeAnnotation {
        match tr {
            TypeAnnotationRef::Path(p) => TypeAnnotation::Path(p.clone()),
            TypeAnnotationRef::Array(t, s) => TypeAnnotation::Array(Box::new(t.clone()), s.clone()),
            TypeAnnotationRef::FnType(p, r) => TypeAnnotation::FnType(
                p.map(|params| params.iter().map(|param| param.clone()).collect()),
                r.map(|r| Box::new(r.clone())),
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModulePath(pub Vec<AstNode<Ident>>);

impl ModulePath {
    pub fn iter<'a>(&'a self) -> Box<Iterator<Item = &Ident> + 'a> {
        Box::new(self.0.iter().map(|node| &node.data))
    }
}

impl<'a> From<&'a ModulePath> for TypeAnnotationRef<'a> {
    fn from(p: &ModulePath) -> TypeAnnotationRef {
        TypeAnnotationRef::Path(p)
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let buffer = self.0.iter().fold(String::new(), |mut buffer, ref item| {
            buffer.push_str(&item.data().0);
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

#[derive(Clone, Debug, PartialEq)]
pub enum PathSegment {
    Ident(AstNode<Ident>),
    Indexing(AstNode<Ident>, Box<Expr>),
}
