use crate::span::Span;

use super::typed_ast;
use super::semantic_data::{BranchingId, LoopId};

#[derive(Clone, Debug)]
pub enum Node {
    Start,
    End,

    Expr(ExprData),

    BranchSplit(BranchingData),
    BranchMerge(BranchingData),

    Assignment(AssignmentData),
    LocalVarDecl(LocalVarDeclData),
    Condition(ExprData),

    LoopHead(LoopData),
    LoopFoot(LoopData),

    EnterScope,
    ExitScope,

    Return(ReturnData),
    Break(LoopData),
    Continue(LoopData),
}

#[derive(Clone, Debug)]
pub enum Edge {
    Normal,
    True,
    False,
    BackEdge,
}

#[derive(Debug, Clone)]
pub struct BranchingData {
    pub branch_id: BranchingId,
}

#[derive(Debug, Clone)]
pub struct ExprData {
    pub expr: typed_ast::Expr,
    pub span: Span,
}

impl PartialEq for ExprData {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

#[derive(Debug, Clone)]
pub struct AssignmentData {
    pub assignment: typed_ast::Assignment,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LocalVarDeclData {
    pub decl: typed_ast::LocalVarDecl,
    pub span: Span,
}

impl PartialEq for LocalVarDeclData {
    fn eq(&self, other: &Self) -> bool {
        self.decl == other.decl
    }
}

#[derive(Debug, Clone)]
pub struct LoopData {
    pub loop_id: LoopId,
    pub span: Span,
}

impl PartialEq for LoopData {
    fn eq(&self, other: &Self) -> bool {
        self.loop_id == other.loop_id
    }
}

#[derive(Debug, Clone)]
pub struct ReturnData {
    pub expr: Option<typed_ast::Expr>,
    pub span: Span,
}

impl PartialEq for ReturnData {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}
