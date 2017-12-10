use std::collections::HashMap;
use petgraph::graph;
use ast::*;

pub struct CFG {
    graph: graph::Graph<Node, ()>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Node {
    Start,
    End,
    Stmt(String),
    BranchSplit,
    BranchMerge,
}

macro_rules! append_node {
    ($CFG: expr, $previous: expr, $to_insert: expr) => {
        let temp = $CFG.graph.add_node($to_insert);
        if let Some(previous_node) = $previous {
            $CFG.graph.add_edge(previous_node, temp, ());
        }
        
        $previous = Some(temp);
    }
}

macro_rules! link_node_index {
    ($CFG: expr, $previous: expr, $to_insert: expr) => {
        if let Some(previous_node) = $previous {
            $CFG.graph.add_edge(previous_node, $to_insert, ());
        } else {
            panic!("Attempting to link to a non-existent node");
        }
    }
}

macro_rules! append_node_index {
    ($CFG: expr, $previous: expr, $to_insert: expr) => {
        if let Some(previous_node) = $previous {
            $CFG.graph.add_edge(previous_node, $to_insert, ());
        }
        
        $previous = Some($to_insert);
    }
}

macro_rules! branch {
    ($CFG: expr, $previous: expr, $to_insert: expr) => {
        let temp = $CFG.graph.add_node($to_insert);
        if let Some(previous_node) = $previous {
            $CFG.graph.add_edge(previous_node, temp, ());
        } else {
            panic!("Attempting to branch on a non-existent node");
        }

        temp
    }
}

impl CFG {
    pub fn generate(fn_def: &Function) -> Result<Self, ()> {
        let mut cfg = CFG {
            graph: graph::Graph::new()
        };

        let mut previous: Option<graph::NodeIndex> = None;
        previous = Some(cfg.graph.add_node(Node::Start));
        
        let instructions = &fn_def.body.data.0;
        let fn_graph = CFG::follow_branch(&mut cfg, instructions, previous);

        // TODO: Auto-insert Node::End if the return type is SmplType::Unit

        Ok(cfg)
    }

    /// 
    /// Returns the last node in a code branch.
    /// 
    fn follow_branch(cfg: &mut CFG, instructions: &[Stmt], mut previous: Option<graph::NodeIndex>) -> Option<graph::NodeIndex> {
        
        for stmt in instructions.iter() {
            if let &Stmt::ExprStmt(ref expr_stmt) = stmt {
                match (*expr_stmt) {
                    ExprStmt::If(ref if_data) => {

                        // Any potential branching starst off with at a Node::BranchSplit
                        append_node!(cfg, previous, Node::BranchSplit);


                        // Go through and generate the graph for code branches. Collect the ends.
                        let mut branch_ends = Vec::new();
                        let instructions = &if_data.block.0;
                        let branch_graph = CFG::follow_branch(cfg, instructions, previous);
                        if let Some(branch_end) = branch_graph { 
                            branch_ends.push(branch_end);
                        }


                        // All branches come back together at a Node::BranchMerge
                        let merge_node = cfg.graph.add_node(Node::BranchMerge);
                        for end in branch_ends.into_iter() {
                            cfg.graph.add_edge(end, merge_node, ());
                        }
                        previous = Some(merge_node);
                    }

                    ExprStmt::While(_) => {

                    }

                    ExprStmt::Return(_) => {

                    }

                    ref s @ _ => {
                        let to_insert = Node::Stmt(format!("{:?}", s));
                        let temp = cfg.graph.add_node(to_insert);
                        if let Some(previous_node) = previous {
                            cfg.graph.add_edge(previous_node, temp, ());
                        }
                        
                        previous = Some(temp);
                    }
                }
            }
        }

        return previous;
   }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use petgraph::dot::{Dot, Config};

    #[test]
    fn test_cfg_generation() {
        {
            let input =
"fn test(int arg) {
    int a = 2;
    int b = 3;
}";
            let fn_def = parse_FnDecl(input).unwrap();
            let cfg = CFG::generate(&fn_def).unwrap();

            println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));
        }

        {
            let input =
"fn test(int arg) {
    int a = 2;
    int b = 3;

    if (test) {
        int c = 4;
    }

    d = 5;
}";
            let fn_def = parse_FnDecl(input).unwrap();
            let cfg = CFG::generate(&fn_def).unwrap();

            println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));
        }
    }
}
