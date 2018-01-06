use std::collections::HashMap;
use std::cell::Cell;

use petgraph;
use petgraph::graph;
use petgraph::visit::EdgeRef;
use typed_ast;
use ast;
use expr_flow;
use semantic_ck::{Universe, TypeId};
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

#[derive(Clone, Debug)]
pub struct CFG {
    graph: graph::Graph<Node, Edge>,
    start: graph::NodeIndex,
    end: graph::NodeIndex,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Start,
    End,

    Expr(typed_ast::Expr),

    BranchSplit,
    BranchMerge,

    Assignment(typed_ast::Assignment),
    LocalVarDecl(typed_ast::LocalVarDecl),
    Condition(typed_ast::Expr),

    LoopHead,
    LoopFoot,

    EnterScope,
    ExitScope,

    Return(Option<typed_ast::Expr>),
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

    ///
    /// Generate the control flow graph.
    /// Only performs continue/break statement checking (necessary for CFG generation).
    ///
    pub fn generate(universe: &Universe, fn_def: ast::Function, fn_type: &FunctionType) -> Result<Self, Err> {

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

        // Start with Node::Start
        let mut previous = Some(cfg.start);
        let mut head = previous; 

        append_node!(cfg, head, previous, Node::EnterScope);
        
        let instructions = fn_def.body.0;
        let fn_graph = CFG::get_branch(universe, &mut cfg, instructions, None)?;

        // Append the function body.
        append_branch!(cfg, head, previous, fn_graph, Edge::Normal);
        

        // Auto-insert Node::Return(None) if the return type is SmplType::Unit
        if *fn_type.return_type == SmplType::Unit {
            append_node!(cfg, head, previous, Node::Return(None));
        }

        append_node!(cfg, head, previous, Node::ExitScope);

        // Connect the last node to the Node::End
        append_node_index!(cfg, head, previous, cfg.end);

        Ok(cfg)
    }

    /// 
    /// Returns the first and the last node in a code branch.
    /// 
    fn get_branch(universe: &Universe, cfg: &mut CFG, instructions: Vec<ast::Stmt>, mut loop_data: Option<(graph::NodeIndex, graph::NodeIndex)>) -> Result<BranchData, Err> {
        use ast::*;
        use typed_ast::Expr as ExprFlow;
        
        let mut previous = None;
        let mut head = None;

        // Go through all the instructions
        for stmt in instructions.into_iter() {
            if let Stmt::ExprStmt(expr_stmt) = stmt {
                match (expr_stmt) {

                    // All if statements begin and and with Node::BranchSplit and Node::BranchMerge
                    ExprStmt::If(if_data) => {

                        // Any potential branching starst off with at a Node::BranchSplit
                        let split_node = cfg.graph.add_node(Node::BranchSplit);
                        append_node_index!(cfg, head, previous, split_node);

                        // All branches come back together at a Node::BranchMerge
                        let merge_node = cfg.graph.add_node(Node::BranchMerge);


                        let mut previous_condition = None;

                        for branch in if_data.branches.into_iter() {
                            let instructions = branch.block.0;
                            let branch_graph = CFG::get_branch(universe, cfg, instructions, loop_data)?;
                            let condition_node = {
                                let expr = expr_flow::flatten(universe, branch.conditional);
                                cfg.graph.add_node(Node::Condition(expr))
                            };

                            // Check if there was a previous condition / branch
                            let edge = if previous_condition.is_none() {
                                Edge::Normal
                            } else {
                                // Means that there was a previous condition and the current branch
                                // should be connected to the previous by a false edge.
                                Edge::False
                            };

                            // Append the condition node
                            append_node_index!(cfg, head, previous, condition_node, edge);
                            if let Some(branch_head) = branch_graph.head {
                                let scope_enter = cfg.graph.add_node(Node::EnterScope);
                                let scope_exit = cfg.graph.add_node(Node::ExitScope);

                                cfg.graph.add_edge(condition_node, scope_enter, Edge::True);
                                cfg.graph.add_edge(scope_enter, branch_head, Edge::Normal);
                                cfg.graph.add_edge(branch_graph.foot.unwrap(), scope_exit, Edge::Normal);
                                cfg.graph.add_edge(scope_exit, merge_node, Edge::Normal);
                            } else {
                                cfg.graph.add_edge(condition_node, merge_node, Edge::True);
                            }

                            // Backtrack to this branch's condition node
                            previous = Some(condition_node);
                            previous_condition = Some(condition_node);
                        }

                        // Run out of conditional branches.
                        // Check for a default branch.
                        if let Some(block) = if_data.default_block {
                            // Found default branch ("else")
                            // Connect branch via false edge 
                    
                            let instructions = block.0;
                            let branch_graph = CFG::get_branch(universe, cfg, instructions, loop_data)?;

                            if let Some(branch_head) = branch_graph.head {
                                let scope_enter = cfg.graph.add_node(Node::EnterScope);
                                let scope_exit = cfg.graph.add_node(Node::ExitScope);

                                cfg.graph.add_edge(previous.unwrap(), scope_enter, Edge::False);
                                cfg.graph.add_edge(scope_enter, branch_head, Edge::Normal);
                                cfg.graph.add_edge(branch_graph.foot.unwrap(), scope_exit, Edge::Normal);
                                cfg.graph.add_edge(scope_exit, merge_node, Edge::Normal);
                            } else {
                                cfg.graph.add_edge(previous.unwrap(), merge_node, Edge::False);
                            }
                        } else {
                            // No default branch ("else"). Connect the last condition node to the
                            // merge node with a false edge.
                            if let Some(previous) = previous {
                                cfg.graph.add_edge(previous, merge_node, Edge::False);
                            } else {
                                unreachable!();
                            }
                        }

                        // All other nodes added after the branching.
                        previous = Some(merge_node);
                    }

                    // All loops being and end with Node::LoopHead and Node::LoopFoot
                    ExprStmt::While(while_data) => {
                        let loop_head = cfg.graph.add_node(Node::LoopHead);
                        let loop_foot = cfg.graph.add_node(Node::LoopFoot);

                        cfg.graph.add_edge(loop_foot, loop_head, Edge::BackEdge);

                        append_node_index!(cfg, head, previous, loop_head);

                        let instructions = while_data.block.0;
                        let loop_body = CFG::get_branch(universe, cfg, instructions, Some((loop_head, loop_foot)))?;
                        let condition = {
                            let expr = expr_flow::flatten(universe, while_data.conditional);
                            cfg.graph.add_node(Node::Condition(expr))
                        };

                        append_node_index!(cfg, head, previous, condition);

                        if let Some(branch_head) = loop_body.head {
                            let scope_enter = cfg.graph.add_node(Node::EnterScope);
                            let scope_exit = cfg.graph.add_node(Node::ExitScope);

                            cfg.graph.add_edge(condition, scope_enter, Edge::True);
                            cfg.graph.add_edge(scope_enter, branch_head, Edge::Normal);
                            cfg.graph.add_edge(loop_body.foot.unwrap(), scope_exit, Edge::Normal);
                            cfg.graph.add_edge(scope_exit, loop_foot, Edge::Normal);

                            cfg.graph.add_edge(condition, loop_foot, Edge::False);
                        } else {
                            cfg.graph.add_edge(condition, loop_foot, Edge::True);
                            cfg.graph.add_edge(condition, loop_foot, Edge::False);
                        }

                        previous = Some(loop_foot);
                    }

                    ExprStmt::Break => {
                        let break_id = cfg.graph.add_node(Node::Break);
                        append_node_index!(cfg, head, previous, break_id);
                        
                        if let Some((_, foot)) = loop_data {
                            // Add an edge to the foot of the loop.
                            cfg.graph.add_edge(break_id, foot, Edge::Normal);
                        } else {
                            // Found a break statement not inside a loop.
                            return Err(Err::BadBreak);
                        }
                    }

                    ExprStmt::Continue => {
                        let continue_id = cfg.graph.add_node(Node::Continue);
                        append_node_index!(cfg, head, previous, continue_id);
                        
                        if let Some((head, _)) = loop_data {
                            // Add a backedge to the head of the loop.
                            cfg.graph.add_edge(continue_id, head, Edge::BackEdge);
                        } else {
                            // Found a continue statement not inside a loop.
                            return Err(Err::BadContinue);
                        }
                    }

                    ExprStmt::Return(expr) => {
                        let expr = expr_flow::flatten(universe, expr);
                        let ret = cfg.graph.add_node(Node::Return(Some(expr)));
                        append_node_index!(cfg, head, previous, ret);

                        // Add an edge to the Node::End of the function
                        cfg.graph.add_edge(ret, cfg.end, Edge::Normal);
                    }

                    ExprStmt::LocalVarDecl(decl) => {
                        append_node!(cfg, head, previous, Node::LocalVarDecl(typed_ast::LocalVarDecl::new(universe, decl)));
                    }

                    ExprStmt::Assignment(assignment) => {
                        append_node!(cfg, head, previous, Node::Assignment(typed_ast::Assignment::new(universe, assignment)));
                    }
                }
            } else if let Stmt::Expr(expr) = stmt {
                append_node!(cfg, head, previous, Node::Expr(expr_flow::flatten(universe, expr)));
            }
        }

        // 'previous' represents the last node in the branch
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
    use std::rc::Rc;

    use semantic_ck::Universe;

    macro_rules! neighbors {
        ($CFG: expr, $node: expr) => {
            $CFG.graph.neighbors_directed($node, Direction::Outgoing)
        }
    }

    macro_rules! edges {
        ($CFG: expr, $node: expr) => {
            $CFG.graph.edges_directed($node, Direction::Outgoing)
        }
    }

    macro_rules! node_w {
        ($CFG: expr, $node: expr) => {
            $CFG.graph.node_weight($node).unwrap()
        }
    }

    #[test]
    fn linear_cfg_generation() {
    
        let input =
"fn test(int arg) {
int a = 2;
int b = 3;
}";
        let universe = Universe::std();
        let fn_type = FunctionType {
            args: vec![Rc::new(SmplType::Int)],
            return_type: Rc::new(SmplType::Unit)
        };
        let fn_def = parse_FnDecl(input).unwrap();
        let cfg = CFG::generate(&universe, fn_def, &fn_type).unwrap();

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
        let universe = Universe::std();
        let fn_type = FunctionType {
            args: vec![Rc::new(SmplType::Int)],
            return_type: Rc::new(SmplType::Unit)
        };
        let fn_def = parse_FnDecl(input).unwrap();
        let cfg = CFG::generate(&universe, fn_def, &fn_type).unwrap();

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

            let return_n = cfg.graph.neighbors(merge).next().unwrap();
            let return_weight = cfg.graph.node_weight(return_n).unwrap();
            if let Node::Return(_) = *return_weight {
                let edges = cfg.graph.edges_directed(return_n, Direction::Outgoing);
                assert_eq!(edges.clone().count(), 1);
            } else {
                panic!("After Node::Merge should be Node::Return");
            }

            let end = cfg.graph.neighbors(return_n).next().unwrap();
            let end_weight = cfg.graph.node_weight(end).unwrap();
            if let Node::End = *end_weight {
                let edges = cfg.graph.edges_directed(end, Direction::Outgoing);
                assert_eq!(edges.clone().count(), 0);
            } else {
                panic!("After Node::Return should be Node::End");
            }
        }
    }

    #[test]
    fn complex_branching_cfg_generation() {
let input =
"fn test(int arg) {
    if (false) {
        int c = 4;
    } elif (true) {

    } else {

    }
}";
        let universe = Universe::std();
        let fn_type = FunctionType {
            args: vec![Rc::new(SmplType::Int)],
            return_type: Rc::new(SmplType::Unit)
        };
        let fn_def = parse_FnDecl(input).unwrap();
        let cfg = CFG::generate(&universe, fn_def, &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            // start -> branch_split(A) -> condition(B)
            //      -[true]> {
            //          local_var_decl
            //      } -> branch_merge(A)
            //
            //      -[false]> condition(C)
            //           -[true]> branch_merge(A)
            //
            //           -[false]> branch_merge(A) 
            //
            // branch_merge(A) -> implicit_return -> end
            //
            
            assert_eq!(cfg.graph.node_count(), 8);

            let mut start_neighbors = neighbors!(cfg, cfg.start);
            assert_eq!(start_neighbors.clone().count(), 1);

            let split = start_neighbors.next().unwrap();
            match *node_w!(cfg, split) {
                Node::BranchSplit => (),    // Success
                _ => panic!("Expected Node::BranchSplit"),
            }

            let mut split_neighbors = neighbors!(cfg, split);
            assert_eq!(split_neighbors.clone().count(), 1);

            let condition_b = split_neighbors.next().unwrap();
            match *node_w!(cfg, condition_b) {
                Node::Condition(_) => (), // Success

                ref n @ _ => panic!("Expected a condition node. Found {:?}", n),

            }

            let condition_b_edges = edges!(cfg, condition_b);
            let mut condition_c = None;
            let mut var_decl = None;

            assert_eq!(condition_b_edges.clone().count(), 2);
            for edge in condition_b_edges {
                match *edge.weight() {
                    Edge::True => var_decl = Some(edge.target()),
                    Edge::False => condition_c = Some(edge.target()),

                    ref e @ _ => panic!("Expected true or false edge. Found {:?}", e),
                }
            }


            // condition b TRUE branch
            let var_decl = var_decl.expect("Missing true edge connecting to variable declaration");
            match *node_w!(cfg, var_decl) {
                Node::LocalVarDecl(_) => (), 

                ref n @ _ => panic!("Expected local variable declartion. Found {:?}", n),
            }

            let mut var_decl_neighbors = neighbors!(cfg, var_decl);
            assert_eq!(var_decl_neighbors.clone().count(), 1);
            match *node_w!(cfg, var_decl_neighbors.next().unwrap()) {
                Node::BranchMerge => (),

                ref n @ _ => panic!("Expected Node::BranchMerge. Found {:?}", n),
            }


            // condition b FALSE branch (condition c)
            let condition_c = condition_c.expect("Missing false edge connecting to Condition C");
            let condition_c_edges = edges!(cfg, condition_c);
            let mut truth_target = None;
            let mut false_target = None;

            assert_eq!(condition_c_edges.clone().count(), 2);
            for edge in condition_c_edges {
                match *edge.weight() {
                    Edge::True => truth_target = Some(edge.target()),
                    Edge::False => false_target = Some(edge.target()),

                    ref e @ _ => panic!("Expected true or false edge. Found {:?}", e),
                }
            }

            let truth_target = truth_target.unwrap();
            let false_target = false_target.unwrap();

            assert_eq!(truth_target, false_target);
            assert_eq!(*node_w!(cfg, truth_target), Node::BranchMerge);
            assert_eq!(*node_w!(cfg, false_target), Node::BranchMerge);

            let branch_merge = truth_target;
            let mut branch_merge_neighbors = neighbors!(cfg, branch_merge);
            assert_eq!(branch_merge_neighbors.clone().count(), 1);

            let implicit_return = branch_merge_neighbors.next().unwrap();
            match *node_w!(cfg, implicit_return) {
                Node::Return(_) => (),
                ref n @ _ => println!("Expected return node. Found {:?}", n),
            }

            let mut implicit_return_neighbors = neighbors!(cfg, implicit_return);
            assert_eq!(implicit_return_neighbors.clone().count(), 1);

            let end = implicit_return_neighbors.next().unwrap();
            assert_eq!(*node_w!(cfg, end), Node::End);
        }
    }

    #[test]
    fn while_loop_generation() {
        let input =
"fn test(int arg) {
    while (true) {
        
    }
}";
        let universe = Universe::std();
        let fn_type = FunctionType {
            args: vec![Rc::new(SmplType::Int)],
            return_type: Rc::new(SmplType::Unit)
        };
        let fn_def = parse_FnDecl(input).unwrap();
        let cfg = CFG::generate(&universe, fn_def, &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        // start -> loop_head(A) -> condition(B)
        //       -[true]> loop_foot(A)
        //       -[false]> loop_foot(A)
        // loop_foot(A) -> implicit_return -> end
        // loop_head(A) << loop_foot(A)
        //

        assert_eq!(cfg.graph.node_count(), 6);

        let mut start_neighbors = neighbors!(cfg, cfg.start);
        assert_eq!(start_neighbors.clone().count(), 1);

        let loop_head = start_neighbors.next().unwrap();
        assert_eq!(*node_w!(cfg, loop_head), Node::LoopHead);

        let mut head_neighbors = neighbors!(cfg, loop_head);
        assert_eq!(head_neighbors.clone().count(), 1);

        let condition = head_neighbors.next().unwrap();
        match *node_w!(cfg, condition) {
            Node::Condition(_) => (),
            ref n @ _ => panic!("Expected condition node. Found {:?}", n),
        }        
        
        let mut condition_edges = edges!(cfg, condition);
        assert_eq!(condition_edges.clone().count(), 2);

        let mut truth_target = None;
        let mut false_target = None;
        for edge in condition_edges {
            match *edge.weight() {
                Edge::True => truth_target = Some(edge.target()),
                Edge::False => false_target = Some(edge.target()),

                ref e @ _ => panic!("Expected true or false edge. Found {:?}", e),
            }
        }

        let truth_target = truth_target.unwrap();
        let false_target = false_target.unwrap();
        assert_eq!(*node_w!(cfg, truth_target), Node::LoopFoot);
        assert_eq!(truth_target, false_target);

        let foot = truth_target;
        let mut foot_neighbors = neighbors!(cfg, foot);
        assert_eq!(foot_neighbors.clone().count(), 2);

        let implicit_return = foot_neighbors.next().unwrap();
        match *node_w!(cfg, implicit_return) {
            Node::Return(_) => (),
            ref n @ _ => panic!("Expected return node. Found {:?}", n),
        }
        
        let mut return_neighbors = neighbors!(cfg, implicit_return);
        assert_eq!(return_neighbors.clone().count(), 1);

        let end = return_neighbors.next().unwrap();
        assert_eq!(*node_w!(cfg, end), Node::End);
    }
}
