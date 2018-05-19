use span::Span;

use super::expr_flow;
use super::typed_ast;
use super::semantic_data::{BranchingId, LoopId, Universe};


#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Edge {
    Normal,
    True,
    False,
    BackEdge,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BranchingData {
    pub branch_id: BranchingId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprData {
    pub expr: typed_ast::Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentData {
    pub assignment: typed_ast::Assignment,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalVarDeclData {
    pub decl: typed_ast::LocalVarDecl,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopData {
    pub loop_id: LoopId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnData {
    pub expr: Option<typed_ast::Expr>,
    pub span: Span,
}
