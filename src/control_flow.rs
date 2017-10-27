use std::collections::HashMap;
use petgraph;
use ast::*;

pub struct CFG {
    graph: petgraph::Graph<Node, ()>,
}

pub enum Node {
    Start,
    End,
    Block,
}

impl CFG {
    pub fn generate(fn_def: &Function) -> Result<(), ()> {
        unimplemented!()
    }
}
