use std::collections::HashMap;
use petgraph;
use petgraph::graph;
use ast::*;
use smpl_type::{ SmplType, FunctionType };

macro_rules! append_node {

    ($CFG: expr, $previous: expr, $to_insert: expr) => {
        append_node!($CFG, $previous, $to_insert, Edge::Normal)
    };

    ($CFG: expr, $previous: expr, $to_insert: expr, $edge: expr) => {
        let temp = $CFG.graph.add_node($to_insert);
        if let Some(previous_node) = $previous {
            $CFG.graph.add_edge(previous_node, temp, $edge);
        }
        
        $previous = Some(temp);
    };
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
        append_node_index!($CFG, $previous, $to_insert, Edge::Normal)
    };

    ($CFG: expr, $previous: expr, $to_insert: expr, $edge: expr) => {
        if let Some(previous_node) = $previous {
            $CFG.graph.add_edge(previous_node, $to_insert, $edge);
        }
        
        $previous = Some($to_insert);
    };
}

pub struct CFG {
    graph: graph::Graph<Node, Edge>,
    start: graph::NodeIndex,
    end: graph::NodeIndex,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Start,
    End,

    Expr(AstNode<Expr>),

    BranchSplit,
    BranchMerge,

    Assignment(Assignment),
    LocalVarDecl(LocalVarDecl),

    LoopHead,
    LoopFoot,

    Return(Option<AstNode<Expr>>),
    Break,
    Continue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Edge {
    Normal,
    Conditional(AstNode<Expr>),
    FallbackCondition,
    BackEdge,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Err {
    MissingReturn,
}

impl CFG {
    pub fn generate(fn_def: Function, fn_type: &FunctionType) -> Result<Self, Err> {

        let mut previous: Option<graph::NodeIndex> = None;

        let mut cfg = {
            let mut graph = graph::Graph::new();
            let start = graph.add_node(Node::Start);
            let end = graph.add_node(Node::End);

            CFG {
                graph: graph,
                start: start,
                end: end,
            }
        };

        previous = Some(cfg.start);
        
        let instructions = fn_def.body.data.0;
        let fn_graph = CFG::follow_branch(&mut cfg, instructions, previous, None);
        previous = fn_graph;

        // Auto-insert Node::Return(None) if the return type is SmplType::Unit
        if *fn_type.return_type == SmplType::Unit {
            append_node!(cfg, previous, Node::Return(None));
        }

        append_node_index!(cfg, previous, cfg.end);

        Ok(cfg)
    }

    /// 
    /// Returns the last node in a code branch.
    /// 
    fn follow_branch(cfg: &mut CFG, instructions: Vec<Stmt>, mut previous: Option<graph::NodeIndex>, mut loop_data: Option<(graph::NodeIndex, graph::NodeIndex)>) -> Option<graph::NodeIndex> {
        
        for stmt in instructions.into_iter() {
            if let Stmt::ExprStmt(expr_stmt) = stmt {
                match (expr_stmt) {
                    ExprStmt::If(if_data) => {

                        // Any potential branching starst off with at a Node::BranchSplit
                        append_node!(cfg, previous, Node::BranchSplit);


                        // Go through and generate the graph for code branches. Collect the ends.
                        let mut branch_ends = Vec::new();

                        for branch in if_data.branches.into_iter() {
                            let instructions = branch.block.0;
                            let branch_graph = CFG::follow_branch(cfg, instructions, 
                                                                  previous, loop_data);
                            if let Some(branch_end) = branch_graph { 
                                branch_ends.push((Some(branch.conditional), branch_end));
                            }
                        }

                        let has_default_branch;
                        if let Some(block) = if_data.default_block {
                            has_default_branch = true;
                            
                            let instructions = block.0;
                            let branch_graph = CFG::follow_branch(cfg, instructions, 
                                                                  previous, loop_data);
                            if let Some(branch_end) = branch_graph {
                                branch_ends.push((None, branch_end));
                            }
                        } else {
                            has_default_branch = false;
                        }


                        // All branches come back together at a Node::BranchMerge
                        let merge_node = cfg.graph.add_node(Node::BranchMerge);
                        for (condition, end) in branch_ends.into_iter() {
                            let edge = if let Some(condition) = condition{
                                Edge::Conditional(condition)
                            } else {
                                Edge::FallbackCondition
                            };
                            cfg.graph.add_edge(end, merge_node, edge);
                        }


                        // No "else" branch to act as a default.
                        if has_default_branch == false {
                            // Create an edge directly linking pre-branch to post-branch.
                            cfg.graph.add_edge(previous.unwrap(), merge_node, Edge::FallbackCondition);
                        }

                        // All other nodes added after the branching.
                        previous = Some(merge_node);
                    }

                    ExprStmt::While(while_data) => {
                        let head = cfg.graph.add_node(Node::LoopHead);
                        let foot = cfg.graph.add_node(Node::LoopFoot);

                        cfg.graph.add_edge(head, foot, Edge::FallbackCondition);
                        cfg.graph.add_edge(foot, head, Edge::BackEdge);


                        append_node_index!(cfg, previous, head);

                        let instructions = while_data.block.0;
                        let loop_body = CFG::follow_branch(cfg, instructions, 
                                                           previous, Some((head, foot)));

                        // Go through the list of neighbors to the loop head and their edges.
                        // If the edge is NOT a FallbackCondition, then it is the loop body.
                        for neighbor in cfg.graph.neighbors_directed(head, Direction::Outgoing).detach() {
                            if let Some(edge) = cfg.graph.find_edge(head, neighbor) {
                                match edge {
                                    Edge::FallbackCondition => continue,
                                    
                                    _ => { 
                                        cfg.graph.update_edge(head, neighbor, 
                                                              Edge::Conditional(while_data.conditional));
                                        break;
                                    }
                                }
                            }
                        }

                        // Connect the end of the loop body to the loop foot.
                        if let Some(body) = loop_body {
                            cfg.graph.add_edge(body, foot, Edge::Normal);
                        }

                        previous = Some(foot);
                    }

                    ExprStmt::Break => {
                        let break_id = cfg.graph.add_node(Node::Break);
                        append_node_index!(cfg, previous, break_id);
                        
                        if let Some((_, foot)) = loop_data {
                            cfg.graph.add_edge(break_id, foot, Edge::Normal);
                        } else {
                            unimplemented!("Return error when no loop header data? Or save for validation.");
                        }
                    }

                    ExprStmt::Continue => {
                        let continue_id = cfg.graph.add_node(Node::Continue);
                        append_node_index!(cfg, previous, continue_id);
                        
                        if let Some((head, _)) = loop_data {
                            cfg.graph.add_edge(continue_id, head, Edge::BackEdge);
                        } else {
                            unimplemented!("Return error when no loop header data? Or save for validation.");
                        }
                    }

                    ExprStmt::Return(expr) => {
                        let ret = cfg.graph.add_node(Node::Return(Some(expr)));
                        append_node_index!(cfg, previous, ret);
                        cfg.graph.add_edge(ret, cfg.end, Edge::Normal);
                    }

                    ExprStmt::LocalVarDecl(decl) => {
                        append_node!(cfg, previous, Node::LocalVarDecl(decl));
                    }

                    ExprStmt::Assignment(assignment) => {
                        append_node!(cfg, previous, Node::Assignment(assignment));
                    }
                }
            } else if let Stmt::Expr(expr) = stmt {
                append_node!(cfg, previous, Node::Expr(expr));
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
