use std::collections::HashMap;
use petgraph;
use petgraph::graph;
use ast::*;
use smpl_type::{ SmplType, FunctionType };

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
    LoopHead,
    LoopFoot,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Err {
    MissingReturn,
}

impl CFG {
    pub fn generate(fn_def: &Function, fn_type: &FunctionType) -> Result<(Self, Option<graph::NodeIndex>), Err> {
        let mut cfg = CFG {
            graph: graph::Graph::new()
        };

        let mut previous: Option<graph::NodeIndex> = None;
        previous = Some(cfg.graph.add_node(Node::Start));
        
        let instructions = &fn_def.body.data.0;
        let fn_graph = CFG::follow_branch(&mut cfg, instructions, previous, None);
        previous = fn_graph;

        // Auto-insert Node::End if the return type is SmplType::Unit
        if *fn_type.return_type == SmplType::Unit {
            append_node!(cfg, previous, Node::End);
        }

        // previous is only None if return type != SmplType::Unit AND CFG::follow_branch returned
        // None (function was empty)
        Ok((cfg, previous))
    }

    pub fn validate(&self, previous: Option<graph::NodeIndex>) -> Result<(), Err> {
        
        let mut search_stack = Vec::new();
        if let Some(last_node) = previous {
            search_stack.push(last_node);
        } else {
            // Function was empty and note SmplType::Unit
            return Err(Err::MissingReturn);
        }

        // Node::End represents return statements. Start at the ends of the graph and scan for
        // Node::End's. If there are not enough Node::End's, there are not enough return
        // statements.
        while let Some(node_id) = search_stack.pop() {
            let node = self.graph.node_weight(node_id).unwrap();
            // Backtrack into the branches to search for Node::End's
            if let Node::BranchMerge = *node {
                let incoming = self.graph.neighbors_directed(node_id, 
                                                            petgraph::Direction::Incoming);
                search_stack.extend(incoming);
            } else if let Node::End = *node {
                // Pass
                continue;
            } else {
                // Fail
                return Err(Err::MissingReturn);
            }
        }

        Ok(())
    }

    /// 
    /// Returns the last node in a code branch.
    /// 
    fn follow_branch(cfg: &mut CFG, instructions: &[Stmt], mut previous: Option<graph::NodeIndex>, mut loop_data: Option<(graph::NodeIndex, graph::NodeIndex)>) -> Option<graph::NodeIndex> {
        
        for stmt in instructions.iter() {
            if let &Stmt::ExprStmt(ref expr_stmt) = stmt {
                match (*expr_stmt) {
                    ExprStmt::If(ref if_data) => {

                        // Any potential branching starst off with at a Node::BranchSplit
                        append_node!(cfg, previous, Node::BranchSplit);


                        // Go through and generate the graph for code branches. Collect the ends.
                        let mut branch_ends = Vec::new();

                        for branch in if_data.branches.iter() {
                            let instructions = &branch.block.0;
                            let branch_graph = CFG::follow_branch(cfg, instructions, previous, loop_data);
                            if let Some(branch_end) = branch_graph { 
                                branch_ends.push(branch_end);
                            }
                        }

                        let has_default_branch;
                        if let Some(ref block) = if_data.default_block.as_ref() {
                            has_default_branch = true;
                            
                            let instructions = &block.0;
                            let branch_graph = CFG::follow_branch(cfg, instructions, previous, loop_data);
                            if let Some(branch_end) = branch_graph {
                                branch_ends.push(branch_end);
                            }
                        } else {
                            has_default_branch = false;
                        }


                        // All branches come back together at a Node::BranchMerge
                        let merge_node = cfg.graph.add_node(Node::BranchMerge);
                        for end in branch_ends.into_iter() {
                            cfg.graph.add_edge(end, merge_node, ());
                        }


                        // No "else" branch to act as a default.
                        if has_default_branch == false {
                            // Create an edge directly linking pre-branch to post-branch.
                            cfg.graph.add_edge(previous.unwrap(), merge_node, ());
                        }

                        // All other nodes added after the branching.
                        previous = Some(merge_node);
                    }

                    ExprStmt::While(ref while_data) => {
                        let head = cfg.graph.add_node(Node::LoopHead);
                        let foot = cfg.graph.add_node(Node::LoopFoot);


                        append_node_index!(cfg, previous, head);

                        let instructions = &while_data.block.0;
                        let loop_body = CFG::follow_branch(cfg, instructions, 
                                                           previous, Some((head, foot)));
                        if let Some(body) = loop_body {
                            cfg.graph.add_edge(body, foot, ());
                        } else {
                            cfg.graph.add_edge(head, foot, ());
                        }

                        previous = Some(foot);
                    }

                    ExprStmt::Break => {
                        unimplemented!("Break statement CFG creation");
                    }

                    ExprStmt::Continue => {
                        unimplemented!("Continue statement CFG creation");
                    }

                    ExprStmt::Return(_) => {
                        append_node!(cfg, previous, Node::End);
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
    use smpl_type::*;

    #[test]
    fn test_cfg_generation() {
        {
            let input =
"fn test(int arg) {
    int a = 2;
    int b = 3;
}";
            let fn_type = FunctionType {
                args: vec![SmplType::Int],
                return_type: Box::new(SmplType::Unit)
            };
            let fn_def = parse_FnDecl(input).unwrap();
            let (cfg, _) = CFG::generate(&fn_def, &fn_type).unwrap();

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
            let fn_type = FunctionType {
                args: vec![SmplType::Int],
                return_type: Box::new(SmplType::Unit)
            };
            let fn_def = parse_FnDecl(input).unwrap();
            let (cfg, _) = CFG::generate(&fn_def, &fn_type).unwrap();

            println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));
        }
    }

    #[test]
    fn test_cfg_analysis() {
        {
            let input =
"fn test(int arg) {
    int a = 2;
    int b = 3;
}";
            let fn_type = FunctionType {
                args: vec![SmplType::Int],
                return_type: Box::new(SmplType::Unit)
            };
            let fn_def = parse_FnDecl(input).unwrap();
            let (cfg, previous) = CFG::generate(&fn_def, &fn_type).unwrap();
            let validation_result = cfg.validate(previous);
            assert_eq!(validation_result, Ok(()));
        }

        {
            let input =
"fn test(int arg) -> int {
    int a = 2;
    int b = 3;
}";
            let fn_type = FunctionType {
                args: vec![SmplType::Int],
                return_type: Box::new(SmplType::Int)
            };
            let fn_def = parse_FnDecl(input).unwrap();
            let (cfg, previous) = CFG::generate(&fn_def, &fn_type).unwrap();
            let validation_result = cfg.validate(previous);
            assert_eq!(validation_result, Err(Err::MissingReturn));
        }

        {
            let input =
"fn test(int arg) -> int {
    int a = 2;
    int b = 3;
    return a;
}";
            let fn_type = FunctionType {
                args: vec![SmplType::Int],
                return_type: Box::new(SmplType::Int)
            };
            let fn_def = parse_FnDecl(input).unwrap();
            let (cfg, previous) = CFG::generate(&fn_def, &fn_type).unwrap();
            let validation_result = cfg.validate(previous);
            assert_eq!(validation_result, Ok(()));
        }

        {
            let input =
"fn test(int arg) -> int {
    int a = 2;
    int b = 3;
    if true {
        if true {

        }
        return a;
    }
}";
            let fn_type = FunctionType {
                args: vec![SmplType::Int],
                return_type: Box::new(SmplType::Int)
            };
            let fn_def = parse_FnDecl(input).unwrap();
            let (cfg, previous) = CFG::generate(&fn_def, &fn_type).unwrap();
            let validation_result = cfg.validate(previous);
            assert_eq!(validation_result, Err(Err::MissingReturn));
        }

        {
            let input =
"fn test(int arg) -> int {
    int a = 2;
    int b = 3;
    if true {
        if true {
            return b;
        }
        return a;
    }
    return b;
}";
            let fn_type = FunctionType {
                args: vec![SmplType::Int],
                return_type: Box::new(SmplType::Int)
            };
            let fn_def = parse_FnDecl(input).unwrap();
            let (cfg, previous) = CFG::generate(&fn_def, &fn_type).unwrap();
            let validation_result = cfg.validate(previous);
            assert_eq!(validation_result, Ok(()));
        }

        {
            let input =
"fn test(int arg) -> int {
    int a = 2;
    int b = 3;
    int c = 4;
    if false {
        return a;
    } elif false {
        return b;
    } else {
        return c;
    }
}";
            let fn_type = FunctionType {
                args: vec![SmplType::Int],
                return_type: Box::new(SmplType::Int)
            };
            let fn_def = parse_FnDecl(input).unwrap();
            let (cfg, previous) = CFG::generate(&fn_def, &fn_type).unwrap();
            let validation_result = cfg.validate(previous);
            assert_eq!(validation_result, Ok(()));
        }

        {
            let input =
"fn test(int arg) -> int {
    int a = 2;
    int b = 3;
    if false {
        return b;
    } elif true {
        return a;
    } 
}";
            let fn_type = FunctionType {
                args: vec![SmplType::Int],
                return_type: Box::new(SmplType::Int)
            };
            let fn_def = parse_FnDecl(input).unwrap();
            let (cfg, previous) = CFG::generate(&fn_def, &fn_type).unwrap();
            let validation_result = cfg.validate(previous);
            assert_eq!(validation_result, Err(Err::MissingReturn));
        }
    }
}
