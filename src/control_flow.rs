use std::collections::HashMap;
use petgraph;
use petgraph::graph;
use ast::*;
use smpl_type::{ SmplType, FunctionType };

macro_rules! append_branch {

    ($CFG: expr, $head: expr, $previous: expr, $branch: expr, $edge: expr) => {
        match $head {
            None => {
                $head = $branch.head;
            }

            Some(head) => {
                if let Some(b_head) = $branch.head {
                    $CFG.graph.add_edge(head, b_head, $edge);
                }
            }
        }

        $previous = $branch.foot;
    }
}

macro_rules! append_node {

    ($CFG: expr, $head: expr, $previous: expr, $to_insert: expr) => {
        append_node!($CFG, $head, $previous, $to_insert, Edge::Normal)
    };

    ($CFG: expr, $head: expr, $previous: expr, $to_insert: expr, $edge: expr) => {
        let temp = $CFG.graph.add_node($to_insert);
        if let Some(previous_node) = $previous {
            $CFG.graph.add_edge(previous_node, temp, $edge);
        }

        if $head.is_none() {
            $head = Some(temp);
        }
        
        $previous = Some(temp);
    };
}

macro_rules! append_node_index {

    ($CFG: expr, $head: expr, $previous: expr, $to_insert: expr) => {
        append_node_index!($CFG, $head, $previous, $to_insert, Edge::Normal)
    };

    ($CFG: expr, $head: expr, $previous: expr, $to_insert: expr, $edge: expr) => {
        if let Some(previous_node) = $previous {
            $CFG.graph.add_edge(previous_node, $to_insert, $edge);
        }

        if $head.is_none() {
            $head = Some($to_insert);
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
    BadBreak,
    BadContinue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct BranchData {
    head: Option<graph::NodeIndex>,
    foot: Option<graph::NodeIndex>,
}

impl CFG {
    pub fn generate(fn_def: Function, fn_type: &FunctionType) -> Result<Self, Err> {

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

        let mut head = None;
        let mut previous = Some(cfg.start);
        
        let instructions = fn_def.body.data.0;
        let fn_graph = CFG::get_branch(&mut cfg, instructions, None)?;

        append_branch!(cfg, head, previous, fn_graph, Edge::Normal);
        
        // Auto-insert Node::Return(None) if the return type is SmplType::Unit
        if *fn_type.return_type == SmplType::Unit {
            append_node!(cfg, head, previous, Node::Return(None));
        }

        append_node_index!(cfg, head, previous, cfg.end);

        Ok(cfg)
    }

    /// 
    /// Returns the first and the last node in a code branch.
    /// 
    fn get_branch(cfg: &mut CFG, instructions: Vec<Stmt>, mut loop_data: Option<(graph::NodeIndex, graph::NodeIndex)>) -> Result<BranchData, Err> {
        
        let mut previous = None;
        let mut head = None;
        for stmt in instructions.into_iter() {
            if let Stmt::ExprStmt(expr_stmt) = stmt {
                match (expr_stmt) {
                    ExprStmt::If(if_data) => {

                        // Any potential branching starst off with at a Node::BranchSplit
                        let split_node = cfg.graph.add_node(Node::BranchSplit);
                        append_node_index!(cfg, head, previous, split_node);

                        // All branches come back together at a Node::BranchMerge
                        let merge_node = cfg.graph.add_node(Node::BranchMerge);


                        // Go through and generate the graph for code branches. Collect the ends.
                        let mut branches = Vec::new();

                        for branch in if_data.branches.into_iter() {
                            let instructions = branch.block.0;
                            let branch_graph = CFG::get_branch(cfg, instructions, loop_data)?;
                            branches.push((Some(branch.conditional), branch_graph));
                        }

                        let has_default_branch;
                        if let Some(block) = if_data.default_block {
                            has_default_branch = true;
                            
                            let instructions = block.0;
                            let branch_graph = CFG::get_branch(cfg, instructions, loop_data)?;

                            branches.push((None, branch_graph));
                        } else {
                            has_default_branch = false;
                        }

                        
                        for (condition, branch) in branches.into_iter() {
                            let start_edge = if let Some(condition) = condition {
                                Edge::Conditional(condition)
                            } else {
                                Edge::FallbackCondition
                            };

                            if let Some(branch_head) = branch.head {
                                cfg.graph.add_edge(branch_head, split_node, start_edge);
                            }

                            if let Some(branch_foot) = branch.foot {
                                cfg.graph.add_edge(branch_foot, merge_node, Edge::Normal);
                            }
                        }


                        // No "else" branch to act as a default.
                        if has_default_branch == false {
                            // Create an edge directly linking pre-branch to post-branch.
                            cfg.graph.add_edge(split_node, merge_node, Edge::FallbackCondition);
                        }

                        // All other nodes added after the branching.
                        previous = Some(merge_node);
                    }

                    ExprStmt::While(while_data) => {
                        let loop_head = cfg.graph.add_node(Node::LoopHead);
                        let loop_foot = cfg.graph.add_node(Node::LoopFoot);

                        cfg.graph.add_edge(loop_head, loop_foot, Edge::FallbackCondition);
                        cfg.graph.add_edge(loop_foot, loop_head, Edge::BackEdge);

                        append_node_index!(cfg, head, previous, loop_head);

                        let instructions = while_data.block.0;
                        let loop_body = CFG::get_branch(cfg, instructions, Some((loop_head, loop_foot)))?;

                        append_branch!(cfg, head, previous, loop_body, Edge::Conditional(while_data.conditional));

                        // Connect the end of the loop body to the loop loop_foot.
                        if let Some(body_foot) = loop_body.foot {
                            cfg.graph.add_edge(body_foot, loop_foot, Edge::Normal);
                        }

                        previous = Some(loop_foot);
                    }

                    ExprStmt::Break => {
                        let break_id = cfg.graph.add_node(Node::Break);
                        append_node_index!(cfg, head, previous, break_id);
                        
                        if let Some((_, foot)) = loop_data {
                            cfg.graph.add_edge(break_id, foot, Edge::Normal);
                        } else {
                            return Err(Err::BadBreak);
                        }
                    }

                    ExprStmt::Continue => {
                        let continue_id = cfg.graph.add_node(Node::Continue);
                        append_node_index!(cfg, head, previous, continue_id);
                        
                        if let Some((head, _)) = loop_data {
                            cfg.graph.add_edge(continue_id, head, Edge::BackEdge);
                        } else {
                            return Err(Err::BadContinue);
                        }
                    }

                    ExprStmt::Return(expr) => {
                        let ret = cfg.graph.add_node(Node::Return(Some(expr)));
                        append_node_index!(cfg, head, previous, ret);
                        cfg.graph.add_edge(ret, cfg.end, Edge::Normal);
                    }

                    ExprStmt::LocalVarDecl(decl) => {
                        append_node!(cfg, head, previous, Node::LocalVarDecl(decl));
                    }

                    ExprStmt::Assignment(assignment) => {
                        append_node!(cfg, head, previous, Node::Assignment(assignment));
                    }
                }
            } else if let Stmt::Expr(expr) = stmt {
                append_node!(cfg, head, previous, Node::Expr(expr));
            }
        }

        return Ok(BranchData {
            head: head,
            foot: previous 
        });
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
