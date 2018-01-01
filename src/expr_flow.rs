use petgraph;
use petgraph::graph;
use petgraph::visit::EdgeRef;

use semantic_ck::{FnId, TypeId, VarId};
use ast::{Literal, UniOp, BinOp};

pub struct ExprGraph {
    graph: graph::Graph<Node, ()>,
}

impl ExprGraph {
    pub fn new() -> ExprGraph {
        let graph = graph::Graph::new();

        ExprGraph {
            graph
        }
    }
}

pub enum Node {
    BinExpr(BinOp),
    UniOp(UniOp),
    Literal(Literal),
}

pub struct BinExpr {
    pub expr_type: Option<TypeId>,
    pub op: BinOp,
}

pub struct UniExpr {
    expr_type: Option<TypeId>,
    op: UniOp,
}
