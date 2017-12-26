use std::collections::HashMap;
use petgraph;
use petgraph::graph;
use petgraph::visit::EdgeRef;
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

        match $branch.foot {
            Some(foot) => $previous = Some(foot),

            None => $previous = $head,
        }
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
    Condition(AstNode<Expr>),

    LoopHead,
    LoopFoot,

    Return(Option<AstNode<Expr>>),
    Break,
    Continue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Edge {
    Normal,
    True,
    False,
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

        let mut previous = Some(cfg.start);
        let mut head = previous; 
        
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


                        let mut previous_condition = None;

                        for branch in if_data.branches.into_iter() {
                            let instructions = branch.block.0;
                            let branch_graph = CFG::get_branch(cfg, instructions, loop_data)?;
                            let condition_node = cfg.graph.add_node(Node::Condition(branch.conditional));

                            let edge = if previous_condition.is_none() {
                                Edge::Normal
                            } else {
                                // Means that there was a previous condition and the current branch
                                // should be connected along the false edge.
                                Edge::False
                            };

                            append_node_index!(cfg, head, previous, condition_node, edge);
                            if let Some(branch_head) = branch_graph.head {
                                cfg.graph.add_edge(previous.unwrap(), branch_head, Edge::True);
                                previous = branch_graph.foot;   // If branch_graph.head.is_some(), branch_graph.foot.is_some()
                            }

                            if let Some(previous) = previous {
                                cfg.graph.add_edge(previous, merge_node, Edge::Normal);
                            } else {
                                unreachable!();
                            }

                            previous = Some(condition_node);
                            previous_condition = Some(condition_node);
                        }

                        if let Some(block) = if_data.default_block {
                            
                            let instructions = block.0;
                            let branch_graph = CFG::get_branch(cfg, instructions, loop_data)?;

                            append_branch!(cfg, head, previous, branch_graph, Edge::False);

                        } else {
                            // No default branch ("else")
                            if let Some(previous) = previous {
                                cfg.graph.add_edge(previous, merge_node, Edge::False);
                            } else {
                                unreachable!();
                            }
                        }

                        // All other nodes added after the branching.
                        previous = Some(merge_node);
                    }

                    ExprStmt::While(while_data) => {
                        let loop_head = cfg.graph.add_node(Node::LoopHead);
                        let loop_foot = cfg.graph.add_node(Node::LoopFoot);

                        cfg.graph.add_edge(loop_foot, loop_head, Edge::BackEdge);

                        append_node_index!(cfg, head, previous, loop_head);

                        let instructions = while_data.block.0;
                        let loop_body = CFG::get_branch(cfg, instructions, Some((loop_head, loop_foot)))?;

                        let condition = cfg.graph.add_node(Node::Condition(while_data.conditional));
                        append_node_index!(cfg, head, previous, condition);
                        if loop_body.head.is_some() {
                            append_branch!(cfg, head, previous, loop_body, Edge::True);
                            cfg.graph.add_edge(previous.unwrap(), loop_foot, Edge::Normal);
                            cfg.graph.add_edge(condition, loop_foot, Edge::False);
                        } else {
                            cfg.graph.add_edge(previous.unwrap(), loop_foot, Edge::True);
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
    use petgraph::Direction;
    use smpl_type::*;
    use std::mem;

    #[test]
    fn linear_cfg_generation() {
    
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
        let cfg = CFG::generate(fn_def, &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            assert_eq!(*cfg.graph.node_weight(cfg.start).unwrap(), Node::Start);
            assert_eq!(*cfg.graph.node_weight(cfg.end).unwrap(), Node::End);
            // start -> var decl -> var decl -> implicit return -> end
            assert_eq!(cfg.graph.node_count(), 5);

            let var_dec_1 = cfg.graph.neighbors(cfg.start).next().expect("Looking for node after start");
            let var_dec_1_node = cfg.graph.node_weight(var_dec_1).unwrap();
            
            let var_dec_2 = cfg.graph.neighbors(var_dec_1).next().expect("Looking for node after the first var declaration");
            let var_dec_2_node = cfg.graph.node_weight(var_dec_2).unwrap();
            {
                assert!({
                    if let Node::LocalVarDecl(_) = *var_dec_1_node {
                        true
                    } else {
                        false
                    }
                });

                assert!({
                    if let Node::LocalVarDecl(_) = *var_dec_2_node {
                        true
                    } else {
                        false
                    }
                });
            }
        }
    }

    #[test]
    fn branching_cfg_generation() {
    
        let input =
"fn test(int arg) {
if (test) {
    int c = 4;
}
}";
        let fn_type = FunctionType {
            args: vec![SmplType::Int],
            return_type: Box::new(SmplType::Unit)
        };
        let fn_def = parse_FnDecl(input).unwrap();
        let cfg = CFG::generate(fn_def, &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            assert_eq!(*cfg.graph.node_weight(cfg.start).unwrap(), Node::Start);
            assert_eq!(*cfg.graph.node_weight(cfg.end).unwrap(), Node::End);

            // start -> branch_split -> condition 
            //      -[true]> {
            //          -> var decl
            //      } ->        >>___ branch_merge ->
            //        -[false]> >>
            //      implicit return -> end
            assert_eq!(cfg.graph.node_count(), 7);

            // Check split node
            let split = cfg.graph.neighbors(cfg.start).next().expect("Looking for node after start");
            let split_node = cfg.graph.node_weight(split).unwrap();
            {
                assert_eq!(mem::discriminant(split_node), mem::discriminant(&Node::BranchSplit));
                let mut edges = cfg.graph.edges_directed(split, Direction::Outgoing);
                assert_eq!(edges.clone().count(), 1);
            }

            let mut merge = None;
            // Check condition node
            let condition = cfg.graph.neighbors(split).next().expect("Looking for condition node");
            let condition_node = cfg.graph.node_weight(condition).unwrap();
            {
                if let Node::Condition(_) = *condition_node {
                    let mut edges = cfg.graph.edges_directed(condition, Direction::Outgoing);
                    assert_eq!(edges.clone().count(), 2);

                    let mut found_true_edge = false;
                    let mut found_false_edge = false;

                    // Look for True False edges and verify
                    for edge in edges {
                        if let Edge::True = *edge.weight() {

                            let target = edge.target();

                            let mut found_decl = false;
                            if let Node::LocalVarDecl(_) = *cfg.graph.node_weight(target).unwrap() {
                                found_decl = true;
                            }
                            assert!(found_decl);


                            found_true_edge = true;
                        } else if let Edge::False = *edge.weight() {
                            let target = edge.target();

                            if let Node::BranchMerge = *cfg.graph.node_weight(target).unwrap() {
                                merge = Some(target);
                            }

                            found_false_edge = true;
                        }
                    }

                    assert!(found_true_edge);
                    assert!(found_false_edge);
                } else {
                    panic!("Not a condition node");
                }
            }

            let merge = merge.unwrap();

            let end = cfg.graph.neighbors(merge).next().unwrap();
            let end_weight = cfg.graph.node_weight(end).unwrap();
            if let Node::End = *end_weight {
                // Should only be one (incoming) edge into Node::End
                let edges = cfg.graph.edges(end);
                assert_eq!(edges.clone().count(), 1);
            } else {
                panic!("After branch merge should be Node::End");
            }
        }
    }   
}
