use std::fmt::Debug;

use crate::span::Span;
use crate::ast_node::{EmptyAstNode, AstNode, Spanned};

use crate::new_ast::{Ident, TypedPath, ModulePath, TypeAnn, FnParameter, TypedNode};
use crate::typable_ast::{Typed, Typable};

use crate::analysis::abstract_type::AbstractType;
use crate::analysis::{FieldId, VarId, FnId};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    ExprStmt(ExprStmt<S, E>),
    Expr(Expr<S, E>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprStmt<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    If(AstNode<If<S, E>>, S),
    While(AstNode<While<S, E>>, S),
    Let(AstNode<LetStmt<S, E>>, S),
    Return(AstNode<Option<Expr<S, E>>>, S),
    Break(AstNode<Option<Expr<S, E>>>, S),
    Extract(AstNode<Option<Expr<S, E>>>, S),
    Continue(EmptyAstNode, S),
}

#[derive(Clone, Debug, PartialEq)]
pub struct If<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub branches: Vec<Branch<S, E>>,
    pub default_branch: Option<TypedNode<Block<S, E>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub conditional: Expr<S, E>,
    pub block: TypedNode<Block<S, E>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub conditional: Expr<S, E>,
    pub body: TypedNode<Block<S, E>>,
    pub branches: Vec<Branch<S, E>>,
    pub default_branch: Option<TypedNode<Block<S, E>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq>(pub Vec<Stmt<S, E>>);

#[derive(Clone, Debug, PartialEq)]
pub struct LetStmt<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub var_name: AstNode<Ident>,
    pub type_ann: Option<TypedNode<TypeAnn>>,
    pub init: Expr<S, E>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    If(Box<TypedNode<If<S, E>>>, E),
    While(Box<TypedNode<While<S, E>>>, E),
    Bin(TypedNode<BinExpr<S, E>>, E),
    Uni(TypedNode<UniExpr<S, E>>, E),
    Literal(TypedNode<Literal>, E),
    Binding(TypedNode<Ident>, E),
    DotAccess(TypedNode<DotAccess<S, E>>, E),
    FnCall(TypedNode<FnCall<S, E>>, E),
    StructInit(TypedNode<StructInit<S, E>>, E),
    ArrayInit(TypedNode<ArrayInit<S, E>>, E),
    IndexAccess(TypedNode<IndexAccess<S, E>>, E),
    AnonymousFn(TypedNode<AnonymousFn<S, E>>, E),
    ModulePath(TypedNode<ModulePath>, E),
    Path(TypedNode<TypedPath>, E),
    Block(TypedNode<Block<S, E>>, E),
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexAccess<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub base: Box<Expr<S, E>>,
    pub indexer: Box<Expr<S, E>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DotAccess<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub base: Box<Expr<S, E>>,
    pub field: TypedNode<Ident>,
    pub field_id: Option<FieldId>,
}


#[derive(Clone, Debug, PartialEq)]
pub struct AnonymousFn<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub params: Option<Vec<AstNode<FnParameter>>>,
    pub return_type: Option<AstNode<TypeAnn>>,
    pub body: Typable<AstNode<Block<S, E>>>,
    pub fn_id: Option<FnId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UniExpr<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub op: UniOp,
    pub expr: Box<Expr<S, E>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinExpr<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub op: BinOp,
    pub lhs: Box<Expr<S, E>>,
    pub rhs: Box<Expr<S, E>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub fn_value: Box<Expr<S, E>>,
    pub args: Vec<Expr<S, E>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInit<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub struct_name: Typable<Option<AstNode<TypedPath>>>,
    pub field_init: Vec<(AstNode<Ident>, Box<Expr<S, E>>)>,
}

impl<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> StructInit<S, E> {
    pub fn is_anonymous(&self) -> bool {
        self.struct_name.data().is_none()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayInit<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> {
    pub pattern: Vec<Expr<S, E>>,
    pub repetition_count: Option<Box<Expr<S, E>>>,
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

    LAnd,
    LOr,
    Gte,
    Lte,
    Gt,
    Lt,
    Eq,
    Neq,

    Assign,
    Dot,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UniOp {
    Negate,
    LogicalInvert,
}

impl<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> Typed for Expr<S, E> {
    fn typ(&self) -> &AbstractType {
        match *self {
            Expr::If(ref typed, ..) => typed.typ(),
            Expr::While(ref typed, ..) => typed.typ(),
            Expr::Bin(ref typed, ..) => typed.typ(),
            Expr::Uni(ref typed, ..) => typed.typ(),
            Expr::Literal(ref typed, ..) => typed.typ(),
            Expr::Binding(ref typed, ..) => typed.typ(),
            Expr::DotAccess(ref typed, ..) => typed.typ(),
            Expr::IndexAccess(ref typed, ..) => typed.typ(),
            Expr::FnCall(ref typed, ..) => typed.typ(),
            Expr::StructInit(ref typed, ..) => typed.typ(),
            Expr::ArrayInit(ref typed, ..) => typed.typ(),
            Expr::AnonymousFn(ref typed, ..) => typed.typ(),
            Expr::Path(ref typed, ..) => typed.typ(),
            Expr::Block(ref typed, ..) => typed.typ(),
            Expr::ModulePath(ref typed, ..) => typed.typ(),
        }
    }

    fn set_type(&mut self, t: AbstractType) {
        match *self {
            Expr::If(ref mut typed, ..) => typed.set_type(t),
            Expr::While(ref mut typed, ..) => typed.set_type(t),
            Expr::Bin(ref mut typed, ..) => typed.set_type(t),
            Expr::Uni(ref mut typed, ..) => typed.set_type(t),
            Expr::Literal(ref mut typed, ..) => typed.set_type(t),
            Expr::Binding(ref mut typed, ..) => typed.set_type(t),
            Expr::DotAccess(ref mut typed, ..) => typed.set_type(t),
            Expr::IndexAccess(ref mut typed, ..) => typed.set_type(t),
            Expr::FnCall(ref mut typed, ..) => typed.set_type(t),
            Expr::StructInit(ref mut typed, ..) => typed.set_type(t),
            Expr::ArrayInit(ref mut typed, ..) => typed.set_type(t),
            Expr::AnonymousFn(ref mut typed, ..) => typed.set_type(t),
            Expr::Path(ref mut typed, ..) => typed.set_type(t),
            Expr::Block(ref mut typed, ..) => typed.set_type(t),
            Expr::ModulePath(ref mut typed, ..) => typed.set_type(t),
        }
    }
}

impl<S: Clone + Debug + PartialEq, E: Clone + Debug + PartialEq> Spanned for Expr<S, E> {
    fn span(&self) -> Span {
        match *self {
            Expr::If(ref spanned, ..) => spanned.data().span(),
            Expr::While(ref spanned, ..) => spanned.data().span(),
            Expr::Bin(ref spanned, ..) => spanned.data().span(),
            Expr::Uni(ref spanned, ..) => spanned.data().span(),
            Expr::Literal(ref spanned, ..) => spanned.data().span(),
            Expr::Binding(ref spanned, ..) => spanned.data().span(),
            Expr::DotAccess(ref spanned, ..) => spanned.data().span(),
            Expr::IndexAccess(ref spanned, ..) => spanned.data().span(),
            Expr::FnCall(ref spanned, ..) => spanned.data().span(),
            Expr::StructInit(ref spanned, ..) => spanned.data().span(),
            Expr::ArrayInit(ref spanned, ..) => spanned.data().span(),
            Expr::AnonymousFn(ref spanned, ..) => spanned.data().span(),
            Expr::Path(ref spanned, ..) => spanned.data().span(),
            Expr::Block(ref spanned, ..) => spanned.data().span(),
            Expr::ModulePath(ref spanned, ..) => spanned.data().span(),
        }
    }
}
