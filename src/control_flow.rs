use std::collections::HashMap;
use std::cell::Cell;

use petgraph;
use petgraph::graph;
use petgraph::Direction;
use petgraph::visit::EdgeRef;
use typed_ast;
use ast;
use expr_flow;
use err::ControlFlowErr;
use semantic_ck::{Universe, TypeId};
use smpl_type::{ SmplType, FunctionType };

macro_rules! node_w {
    ($CFG: expr, $node: expr) => {
        $CFG.graph().node_weight($node).unwrap()
    }
}

macro_rules! neighbors {
    ($CFG: expr, $node: expr) => {
        $CFG.graph().neighbors_directed($node, Direction::Outgoing)
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct BranchData {
    head: Option<graph::NodeIndex>,
    foot: Option<graph::NodeIndex>,
}

impl CFG {

    pub fn graph(&self) -> &graph::Graph<Node, Edge> {
        &self.graph
    }

    pub fn after_loop_foot(&self, id: graph::NodeIndex) -> Result<graph::NodeIndex, ()> {

        match *node_w!(self, id) {
            Node::LoopFoot => (),
            _ => return Err(()),
        }

        let neighbors = neighbors!(self, id);
        assert_eq!(neighbors.clone().count(), 2);
         
        for n in neighbors {
            match *node_w!(self, n) {
                Node::LoopHead => (),
                _ => return Ok(n),
            }
        }
        Err(())
    }

    ///
    /// Returns (TRUE, FALSE) branch heads.
    ///
    pub fn after_condition(&self, id: graph::NodeIndex) -> Result<(graph::NodeIndex, graph::NodeIndex), ()> {

        match *node_w!(self, id) {
            Node::Condition(_) => (),
            _ => return Err(()),
        }

        let edges = self.graph.edges_directed(id, Direction::Outgoing);
        assert_eq!(edges.clone().count(), 2);
         
        let mut true_branch = None;
        let mut false_branch = None;
        for e in edges {
            match *e.weight() {
                Edge::True => true_branch = Some(e.target()),
                Edge::False => false_branch = Some(e.target()),
                ref e @ _ => panic!("Unexpected edge {:?} coming out of a condition node.", e),
            }
        }

        Ok((true_branch.unwrap(), false_branch.unwrap()))
    }

    pub fn after_return(&self, id: graph::NodeIndex) -> Result<graph::NodeIndex, ()> {

        match *node_w!(self, id) {
            Node::Return(_) => (),
            _ => return Err(()),
        }

        let neighbors = neighbors!(self, id);
        assert_eq!(neighbors.clone().count(), 2);
         
        for n in neighbors {
            match *node_w!(self, n) {
                Node::End => (),
                _ => return Ok(n),
            }
        }
        Err(())
        
    }

    pub fn after_continue(&self, id: graph::NodeIndex) -> Result<graph::NodeIndex, ()> {

        match *node_w!(self, id) {
            Node::Continue => (),
            _ => return Err(()),
        }

        let neighbors = neighbors!(self, id);
        assert_eq!(neighbors.clone().count(), 2);
         
        for n in neighbors {
            match *node_w!(self, n) {
                Node::LoopHead => (),
                _ => return Ok(n),
            }
        }
        Err(())
        
    }

    pub fn after_break(&self, id: graph::NodeIndex) -> Result<graph::NodeIndex, ()> {

        match *node_w!(self, id) {
            Node::Break => (),
            _ => return Err(()),
        }

        let neighbors = neighbors!(self, id);
        assert_eq!(neighbors.clone().count(), 2);
        
        for n in neighbors {
            match *node_w!(self, n) {
                Node::LoopFoot => (),
                _ => return Ok(n),
            }
        }
        Err(())
    }

    pub fn after_start(&self) -> Result<graph::NodeIndex, ()> {
        self.next(self.start)
    }

    ///
    /// Convenience function to get the next node in a linear sequence. If the current node has
    /// multiple outgoing edge (such as Node::Condition, Node::Return, Node::Break, and
    /// Node::Continue) or none (Node::End), return an error.
    ///
    pub fn next(&self, id: graph::NodeIndex) -> Result<graph::NodeIndex, ()> {
        let mut neighbors = self.graph.neighbors_directed(id, Direction::Outgoing);
        if neighbors.clone().count() != 1 {
            Err(())
        } else {
            Ok(neighbors.next().unwrap())
        }
    }

    ///
    /// Get the branch heads of the branches of a conditional node in the format (TRUE, FALSE). If
    /// the given node is not Node::Conditional, return an error.
    ///
    pub fn get_branches(&self, id: graph::NodeIndex) -> Result<(graph::NodeIndex, graph::NodeIndex), ()> {
        match *self.graph.node_weight(id).unwrap() {
            Node::Condition(_) => {
                unimplemented!()
            }

            _ => Err(()),
        }
    }

    ///
    /// Generate the control flow graph.
    /// Only performs continue/break statement checking (necessary for CFG generation).
    ///
    pub fn generate(universe: &Universe, fn_def: ast::Function, fn_type: &FunctionType) -> Result<Self, ControlFlowErr> {

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
        if let Some(branch_head) = fn_graph.head {
            cfg.graph.add_edge(previous.unwrap(), branch_head, Edge::Normal);
            previous = Some(fn_graph.foot.unwrap());
        }
        

        // Auto-insert Node::Return(None) if the return type is SmplType::Unit
        if *universe.get_type(fn_type.return_type)== SmplType::Unit {
            append_node!(cfg, head, previous, Node::Return(None));
        }

        append_node!(cfg, head, previous, Node::ExitScope);
        append_node_index!(cfg, head, previous, cfg.end);

        Ok(cfg)
    }

    /// 
    /// Returns the first and the last node in a code branch.
    /// 
    fn get_branch(universe: &Universe, cfg: &mut CFG, instructions: Vec<ast::Stmt>, mut loop_data: Option<(graph::NodeIndex, graph::NodeIndex)>) -> Result<BranchData, ControlFlowErr> {
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
                            return Err(ControlFlowErr::BadBreak);
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
                            return Err(ControlFlowErr::BadContinue);
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
            args: vec![universe.int()],
            return_type: universe.unit()
        };
        let fn_def = parse_FnDecl(input).unwrap();
        let cfg = CFG::generate(&universe, fn_def, &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            assert_eq!(*cfg.graph.node_weight(cfg.start).unwrap(), Node::Start);
            assert_eq!(*cfg.graph.node_weight(cfg.end).unwrap(), Node::End);
            // start -> enter_scope -> var decl -> var decl -> implicit return -> exit_scope -> end
            assert_eq!(cfg.graph.node_count(), 7);

            let mut start_neighbors = neighbors!(cfg, cfg.start);
            
            let enter = start_neighbors.next().unwrap();
            let mut enter_neighbors = neighbors!(cfg, enter);
            match *node_w!(cfg, enter) {
                Node::EnterScope => (),
                ref n @ _ => panic!("Expected to find Node::EnterScope. Found {:?}", n),
            }

            let var_decl_1 = enter_neighbors.next().unwrap();
            let mut var_decl_1_neighbors = neighbors!(cfg, var_decl_1);
            match *node_w!(cfg, var_decl_1) {
                Node::LocalVarDecl(_) => (),
                ref n @ _ => panic!("Expected to find Node::LocalVarDecl. Found {:?}", n),
            }

            let var_decl_2 = var_decl_1_neighbors.next().unwrap();
            let mut var_decl_2_neighbors = neighbors!(cfg, var_decl_2);
            match *node_w!(cfg, var_decl_2) {
                Node::LocalVarDecl(_) => (),
                ref n @ _ => panic!("Expected to find Node::LocalVarDecl. Found {:?}", n),
            }

            let ret = var_decl_2_neighbors.next().unwrap();
            let mut ret_neighbors = neighbors!(cfg, ret);
            match *node_w!(cfg, ret) {
                Node::Return(_) => (),
                ref n @ _ => panic!("Expected to find Node::Return. Found {:?}", n),
            }

            let exit = ret_neighbors.next().unwrap();
            let mut exit_neighbors = neighbors!(cfg, exit);
            match *node_w!(cfg, exit) {
                Node::ExitScope => (),
                ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
            }

            let end = exit_neighbors.next().unwrap();
            let mut end_neighbors = neighbors!(cfg, end);
            assert_eq!(end_neighbors.count(), 0);
            match *node_w!(cfg, end) {
                Node::End => (),
                ref n @ _ => panic!("Expected to find Node::End. Found {:?}", n),
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
            args: vec![universe.int()],
            return_type: universe.unit(), 
        };
        let fn_def = parse_FnDecl(input).unwrap();
        let cfg = CFG::generate(&universe, fn_def, &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            assert_eq!(*cfg.graph.node_weight(cfg.start).unwrap(), Node::Start);
            assert_eq!(*cfg.graph.node_weight(cfg.end).unwrap(), Node::End);

            // start -> enter_scope -> branch_split -> condition 
            //      -[true]> {
            //          -> enter_scope
            //          -> var decl
            //          -> exit_scope
            //      } ->        >>___ branch_merge ->
            //        -[false]> >>
            //      implicit return -> exit_scope -> end
            assert_eq!(cfg.graph.node_count(), 11);

            let mut start_neighbors = neighbors!(cfg, cfg.start);

            let enter = start_neighbors.next().unwrap();
            let mut enter_neighbors = neighbors!(cfg, enter);
            match *node_w!(cfg, enter) {
                Node::EnterScope => (),
                ref n @ _ => panic!("Expected to find Node::EnterScope. Found {:?}", n),
            }

            // Check split node
            let split = enter_neighbors.next().unwrap();
            match *node_w!(cfg, split) {
                Node::BranchSplit => (),
                ref n @ _ => panic!("Expected to find Node::BranchSplit. Found {:?}", n),
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

                            match *cfg.graph.node_weight(target).unwrap() {
                                Node::EnterScope => {
                                    let mut neighbors = cfg.graph.neighbors_directed(target, Direction::Outgoing);
                                    let decl = neighbors.next().unwrap();
                                    
                                    match *cfg.graph.node_weight(decl).unwrap() {
                                        Node::LocalVarDecl(_) => (),
                                        ref n @ _ => panic!("Expected to find Node::LocalVarDecl. Found {:?}", n),
                                    }

                                    let mut neighbors = cfg.graph.neighbors_directed(decl, Direction::Outgoing);
                                    let exit_scope = neighbors.next().unwrap();

                                    match *cfg.graph.node_weight(exit_scope).unwrap() {
                                        Node::ExitScope => (),

                                        ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
                                    }
                                }

                                ref n @ _ => panic!("Expected to find Node::EnterScope. Found {:?}", n),
                            }
                            

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
            let mut merge_neighbors = neighbors!(cfg, merge);

            let return_n = cfg.graph.neighbors(merge).next().unwrap();
            let mut return_neighbors = neighbors!(cfg, return_n);
            match *node_w!(cfg, return_n) {
                Node::Return(_) => (),
                ref n @ _ => panic!("Expected to find Node::Return. Found {:?}", n),
            }

            let exit = return_neighbors.next().unwrap();
            let mut exit_neighbors = neighbors!(cfg, exit);
            match *node_w!(cfg, exit) {
                Node::ExitScope => (),
                ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
            }

            let end = exit_neighbors.next().unwrap();
            let mut end_neighbors = neighbors!(cfg, end);
            match *node_w!(cfg, end) {
                Node::End => {
                    assert_eq!(end_neighbors.count(), 0);
                },
                ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
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
            args: vec![universe.int()],
            return_type: universe.unit(),
        };
        let fn_def = parse_FnDecl(input).unwrap();
        let cfg = CFG::generate(&universe, fn_def, &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            // start -> enter_scope -> branch_split(A) -> condition(B)
            //      -[true]> {
            //          enter_scope ->
            //          local_var_decl ->
            //          exit_scope ->
            //      } -> branch_merge(A)
            //
            //      -[false]> condition(C)
            //           -[true]> branch_merge(A)
            //
            //           -[false]> branch_merge(A) 
            //
            // branch_merge(A) -> implicit_return -> exit_scope -> end
            //
            
            assert_eq!(cfg.graph.node_count(), 12);

            let mut start_neighbors = neighbors!(cfg, cfg.start);
            assert_eq!(start_neighbors.clone().count(), 1);

            let enter = start_neighbors.next().unwrap();
            let mut enter_neighbors = neighbors!(cfg, enter);
            match *node_w!(cfg, enter) {
                Node::EnterScope => (),
                ref n @ _ => panic!("Expected to find Node::Enter. Found {:?}", n),
            }

            let split = enter_neighbors.next().unwrap();
            match *node_w!(cfg, split) {
                Node::BranchSplit => (),    // Success
                ref n @ _ => panic!("Expected to find Node::BranchSplit. Found {:?}", n),
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
            let mut condition_b_true = None;

            assert_eq!(condition_b_edges.clone().count(), 2);
            for edge in condition_b_edges {
                match *edge.weight() {
                    Edge::True => condition_b_true = Some(edge.target()),
                    Edge::False => condition_c = Some(edge.target()),

                    ref e @ _ => panic!("Expected true or false edge. Found {:?}", e),
                }
            }


            // condition b TRUE branch

            let enter = condition_b_true.expect("Missing true edge connecting to variable declaration");
            let mut enter_neighbors = neighbors!(cfg, enter);
            match *node_w!(cfg, enter) {
                Node::EnterScope => (),
                ref n @ _ => panic!("Expected Node::EnterScope. Found {:?}", n),
            }

            let var_decl = enter_neighbors.next().unwrap();
            let mut var_decl_neighbors = neighbors!(cfg, var_decl);
            assert_eq!(var_decl_neighbors.clone().count(), 1);
            match *node_w!(cfg, var_decl) {
                Node::LocalVarDecl(_) => (), 

                ref n @ _ => panic!("Expected local variable declartion. Found {:?}", n),
            }


            let exit = var_decl_neighbors.next().unwrap();
            let mut exit_neighbors = neighbors!(cfg, exit);
            match *node_w!(cfg, exit) {
                Node::ExitScope => (),
                ref n @ _ => panic!("Expected Node::ExitScope. Found {:?}", n),
            }

            let merge = exit_neighbors.next().unwrap();
            match *node_w!(cfg, merge) {
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
            let mut implicit_return_neighbors = neighbors!(cfg, implicit_return);
            assert_eq!(implicit_return_neighbors.clone().count(), 1);
            match *node_w!(cfg, implicit_return) {
                Node::Return(_) => (),
                ref n @ _ => println!("Expected return node. Found {:?}", n),
            }

            let exit = implicit_return_neighbors.next().unwrap();
            let mut exit_neighbors = neighbors!(cfg, exit);
            match *node_w!(cfg, exit) {
                Node::ExitScope => (),
                ref n @ _ => panic!("Expected to find Node::Exit. Found {:?}", n),
            }

            let end = exit_neighbors.next().unwrap();
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
            args: vec![universe.int()],
            return_type: universe.unit(), 
        };
        let fn_def = parse_FnDecl(input).unwrap();
        let cfg = CFG::generate(&universe, fn_def, &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        // start -> enter_scope -> loop_head(A) -> condition(B)
        //       -[true]> loop_foot(A)
        //       -[false]> loop_foot(A)
        // loop_foot(A) -> implicit_return -> exit_scope -> end
        // loop_head(A) << loop_foot(A)
        //

        assert_eq!(cfg.graph.node_count(), 8);

        let mut start_neighbors = neighbors!(cfg, cfg.start);
        assert_eq!(start_neighbors.clone().count(), 1);

        let enter = start_neighbors.next().unwrap();
        let mut enter_neighbors = neighbors!(cfg, enter);
        match *node_w!(cfg, enter) {
            Node::EnterScope => (),
            ref n @ _ => panic!("Expected to find Node::Enter. Found {:?}", n),
        }

        let loop_head = enter_neighbors.next().unwrap();
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
        let mut return_neighbors = neighbors!(cfg, implicit_return);
        assert_eq!(return_neighbors.clone().count(), 1);
        match *node_w!(cfg, implicit_return) {
            Node::Return(_) => (),
            ref n @ _ => panic!("Expected return node. Found {:?}", n),
        }

        let exit = return_neighbors.next().unwrap();
        let mut exit_neighbors = neighbors!(cfg, exit);
        match *node_w!(cfg, exit) {
            Node::ExitScope => (),
            ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
        }

        let end = exit_neighbors.next().unwrap();
        let mut end_neighbors = neighbors!(cfg, end);
        assert_eq!(end_neighbors.count(), 0);
        match *node_w!(cfg, end) {
            Node::End => (),
            ref n @ _ => panic!("Expected to find Node::End. Found {:?}", n),
        }
    }
}
