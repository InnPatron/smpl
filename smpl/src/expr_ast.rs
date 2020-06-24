use crate::span::Span;
use crate::ast_node::{EmptyAstNode, AstNode, Spanned};

use crate::new_ast::{Ident, TypedPath, TypeAnnotation, FnParameter};
use crate::typable_ast::{Typed, Typable};

use crate::analysis::abstract_type::AbstractType;
use crate::analysis::{FieldId, VarId, FnId};

pub type TypedNode<T> = Typable<AstNode<T>>;

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
    Return(AstNode<Option<Expr>>),
    Break(AstNode<Option<Expr>>),
    Continue(EmptyAstNode),
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub branches: Vec<Branch>,
    pub default_branch: Option<TypedNode<Block>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub conditional: Expr,
    pub block: TypedNode<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub conditional: Expr,
    pub body: TypedNode<Block>,
    pub default_branch: Option<TypedNode<Block>>,
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
    pub name: Typable<Box<AstNode<Access>>>,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assignment(Box<TypedNode<Assignment>>),
    If(Box<TypedNode<If>>),
    While(Box<TypedNode<While>>),
    Bin(TypedNode<BinExpr>),
    Uni(TypedNode<UniExpr>),
    Literal(TypedNode<Literal>),
    Binding(TypedNode<Ident>),
    Access(Box<TypedNode<Access>>),
    FnCall(TypedNode<FnCall>),
    StructInit(TypedNode<StructInit>),
    ArrayInit(TypedNode<ArrayInit>),
    AnonymousFn(TypedNode<AnonymousFn>),
    Path(TypedNode<TypedPath>),
    Block(TypedNode<Block>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnonymousFn {
    pub params: Option<Vec<AstNode<FnParameter>>>,
    pub return_type: Option<AstNode<TypeAnnotation>>,
    pub body: Typable<AstNode<Block>>,
    pub fn_id: Option<FnId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Access {
    root_name: AstNode<Ident>,
    root_indexing: Option<Expr>,
    root_var: Option<Typable<VarId>>,
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
    field_id: Option<Typable<FieldId>>,
}

impl Field {

    pub fn name(&self) -> &Ident {
        self.name.node()
    }

    pub fn field_id(&self) -> &Typable<FieldId> {
        self.field_id.as_ref().expect("No field id")
    }

    pub fn field_id_mut(&mut self) -> &mut Typable<FieldId> {
        self.field_id.as_mut().expect("No field id")
    }

    pub fn set_field_id(&mut self, id: FieldId) {
        if self.field_id.is_some() {
            panic!("Attempting to override field id.");
        }

        self.field_id = Some(Typable::untyped(id));
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
    pub fn_id: Option<Typable<FnId>>,
    pub args: Option<Vec<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInit {
    pub struct_name: Typable<Option<TypedPath>>,
    pub field_init: Vec<(AstNode<Ident>, Box<Expr>)>,
}

impl StructInit {
    pub fn is_anonymous(&self) -> bool {
        self.struct_name.data().is_none()
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

    LAnd,
    LOr,
    Gte,
    Lte,
    Gt,
    Lt,
    Eq,
    Neq,
    Assign,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UniOp {
    Negate,
    LogicalInvert,
}

impl Typed for Expr {
    fn typ(&self) -> &AbstractType {
        match *self {
            Expr::Assignment(ref typed) => typed.typ(),
            Expr::If(ref typed) => typed.typ(),
            Expr::While(ref typed) => typed.typ(),
            Expr::Bin(ref typed) => typed.typ(),
            Expr::Uni(ref typed) => typed.typ(),
            Expr::Literal(ref typed) => typed.typ(),
            Expr::Binding(ref typed) => typed.typ(),
            Expr::Access(ref typed) => typed.typ(),
            Expr::FnCall(ref typed) => typed.typ(),
            Expr::StructInit(ref typed) => typed.typ(),
            Expr::ArrayInit(ref typed) => typed.typ(),
            Expr::AnonymousFn(ref typed) => typed.typ(),
            Expr::Path(ref typed) => typed.typ(),
            Expr::Block(ref typed) => typed.typ(),
        }
    }

    fn set_type(&mut self, t: AbstractType) {
        match *self {
            Expr::Assignment(ref mut typed) => typed.set_type(t),
            Expr::If(ref mut typed) => typed.set_type(t),
            Expr::While(ref mut typed) => typed.set_type(t),
            Expr::Bin(ref mut typed) => typed.set_type(t),
            Expr::Uni(ref mut typed) => typed.set_type(t),
            Expr::Literal(ref mut typed) => typed.set_type(t),
            Expr::Binding(ref mut typed) => typed.set_type(t),
            Expr::Access(ref mut typed) => typed.set_type(t),
            Expr::FnCall(ref mut typed) => typed.set_type(t),
            Expr::StructInit(ref mut typed) => typed.set_type(t),
            Expr::ArrayInit(ref mut typed) => typed.set_type(t),
            Expr::AnonymousFn(ref mut typed) => typed.set_type(t),
            Expr::Path(ref mut typed) => typed.set_type(t),
            Expr::Block(ref mut typed) => typed.set_type(t),
        }
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match *self {
            Expr::Assignment(ref spanned) => spanned.data().span(),
            Expr::If(ref spanned) => spanned.data().span(),
            Expr::While(ref spanned) => spanned.data().span(),
            Expr::Bin(ref spanned) => spanned.data().span(),
            Expr::Uni(ref spanned) => spanned.data().span(),
            Expr::Literal(ref spanned) => spanned.data().span(),
            Expr::Binding(ref spanned) => spanned.data().span(),
            Expr::Access(ref spanned) => spanned.data().span(),
            Expr::FnCall(ref spanned) => spanned.data().span(),
            Expr::StructInit(ref spanned) => spanned.data().span(),
            Expr::ArrayInit(ref spanned) => spanned.data().span(),
            Expr::AnonymousFn(ref spanned) => spanned.data().span(),
            Expr::Path(ref spanned) => spanned.data().span(),
            Expr::Block(ref spanned) => spanned.data().span(),
        }
    }
}
