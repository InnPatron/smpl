use crate::span::Span;
use crate::ast_node::{EmptyAstNode, AstNode};

use crate::new_ast::{Ident, TypedPath, TypeAnnotation, FnParameter};
use crate::typable_ast::{Typed, Typable};

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
    Assignment(AstNode<Assignment>),
    Return(AstNode<Option<TypedNode<Expr>>>),
    Break(AstNode<Option<TypedNode<Expr>>>),
    Continue(EmptyAstNode),
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub branches: Vec<Branch>,
    pub default_branch: Option<Typable<AstNode<Block>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub conditional: Typable<AstNode<Expr>>,
    pub block: Typable<AstNode<Block>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub conditional: Typable<AstNode<Expr>>,
    pub body: Typable<AstNode<Block>>,
    pub default_branch: Option<TypedNode<Block>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

#[derive(Clone, Debug, PartialEq)]
pub struct LocalVarDecl {
    pub var_type: Option<AstNode<TypeAnnotation>>,
    pub var_name: AstNode<Ident>,
    pub var_init: TypedNode<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub name: Typable<Box<AstNode<Access>>>,
    pub value: TypedNode<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    If(Box<Typable<AstNode<If>>>),
    While(Box<Typable<AstNode<While>>>),
    Bin(Typable<AstNode<BinExpr>>),
    Uni(Typable<AstNode<UniExpr>>),
    Literal(Typable<AstNode<Literal>>),
    Binding(Typable<AstNode<Ident>>),
    Access(Box<Typable<AstNode<Access>>>),
    FnCall(Typable<AstNode<FnCall>>),
    StructInit(Typable<AstNode<StructInit>>),
    ArrayInit(Typable<AstNode<ArrayInit>>),
    AnonymousFn(Typable<AstNode<AnonymousFn>>),
    Path(Typable<AstNode<TypedPath>>),
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
    root_indexing: Option<Typable<Expr>>,
    root_var: Option<Typable<VarId>>,
    path: Vec<self::FASegment>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum FASegment {
    Ident(Field),
    Indexing(Field, Typable<Expr>),
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
    pub expr: Box<Typable<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinExpr {
    pub op: BinOp,
    pub lhs: Box<Typable<Expr>>,
    pub rhs: Box<Typable<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub path: AstNode<TypedPath>,
    pub fn_id: Option<Typable<FnId>>,
    pub args: Option<Vec<Typable<Expr>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInit {
    pub struct_name: Typable<Option<TypedPath>>,
    pub field_init: Vec<(AstNode<Ident>, Box<Typable<Expr>>)>,
}

impl StructInit {
    pub fn is_anonymous(&self) -> bool {
        self.struct_name.data().is_none()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayInit {
    InitList(Vec<Typable<Expr>>),
    Value(Box<Typable<Expr>>, u64),
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
pub enum UniOp {
    Negate,
    LogicalInvert,
}
