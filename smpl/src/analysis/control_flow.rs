use petgraph::graph;
use petgraph::Direction;
use petgraph::visit::EdgeRef;

use ast;
use err::ControlFlowErr;

use span::Span;

use super::smpl_type::{FunctionType, SmplType};
use super::expr_flow;
use super::typed_ast;
use super::semantic_data::{LoopId, Universe};

use super::control_data::*;

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


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct BranchData {
    head: Option<graph::NodeIndex>,
    foot: Option<graph::NodeIndex>,
}

#[derive(Clone, Debug)]
pub struct CFG {
    graph: graph::Graph<Node, Edge>,
    start: graph::NodeIndex,
    end: graph::NodeIndex,
}

impl CFG {
    pub fn graph(&self) -> &graph::Graph<Node, Edge> {
        &self.graph
    }

    pub fn node_weight(&self, node: graph::NodeIndex) -> &Node {
        self.graph.node_weight(node).unwrap()
    }

    pub fn neighbors_out(&self, node: graph::NodeIndex) -> graph::Neighbors<Edge> {
        self.graph.neighbors_directed(node, Direction::Outgoing)
    }

    pub fn neighbors_in(&self, node: graph::NodeIndex) -> graph::Neighbors<Edge> {
        self.graph.neighbors_directed(node, Direction::Incoming)
    }

    pub fn after_loop_foot(&self, id: graph::NodeIndex) -> graph::NodeIndex {
        let loop_id;
        match *node_w!(self, id) {
            Node::LoopFoot(ref data) => loop_id = data.loop_id,
            _ => panic!("Should only be given a Node::LoopFoot"),
        }

        let neighbors = neighbors!(self, id);
        let neighbor_count = neighbors.clone().count();

        if neighbor_count != 2 {
            panic!("Loop foot should always be pointing to LoopHead and the next Node. Need two directed neighbors, found {}", neighbor_count);
        }

        for n in neighbors {
            match *node_w!(self, n) {
                Node::LoopHead(ref data) => {
                    if loop_id != data.loop_id {
                        return n;
                    }
                }
                _ => return n,
            }
        }
        unreachable!();
    }

    ///
    /// Returns (TRUE, FALSE) branch heads.
    ///
    pub fn after_condition(&self, id: graph::NodeIndex) -> (graph::NodeIndex, graph::NodeIndex) {
        match *node_w!(self, id) {
            Node::Condition(_) => (),
            _ => panic!("Should only be given a Node::Condition"),
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

        (true_branch.unwrap(), false_branch.unwrap())
    }

    pub fn after_return(&self, id: graph::NodeIndex) -> graph::NodeIndex {
        match *node_w!(self, id) {
            Node::Return(..) => (),
            _ => panic!("Should only be given a Node::Return"),
        }

        let mut neighbors = neighbors!(self, id);
        let neighbor_count = neighbors.clone().count();

        if neighbor_count == 2 {
            let mut found_first_end = false;
            for n in neighbors {
                match *node_w!(self, n) {
                    Node::End => {
                        if found_first_end {
                            return n;
                        } else {
                            found_first_end = true;
                        }
                    }
                    _ => return n,
                }
            }
        } else if neighbor_count == 1 {
            return neighbors.next().unwrap();
        } else {
            panic!("Node::Return points to {} neighbors. Nodes should never point towards more than 2 neighbors but at least 1 (except Node::End).", neighbor_count);
        }

        unreachable!();
    }

    pub fn after_continue(&self, id: graph::NodeIndex) -> graph::NodeIndex {
        match *node_w!(self, id) {
            Node::Continue(_) => (),
            _ => panic!("Should only be given a Node::Continue"),
        }

        let mut neighbors = neighbors!(self, id);
        let neighbor_count = neighbors.clone().count();

        if neighbor_count == 2 {
            let mut found_first = false;
            for n in neighbors {
                match *node_w!(self, n) {
                    Node::LoopHead(_) => {
                        if found_first {
                            return n;
                        } else {
                            found_first = true;
                        }
                    }
                    _ => return n,
                }
            }
        } else if neighbor_count == 1 {
            return neighbors.next().unwrap();
        } else {
            panic!("Node::Continue points to {} neighbors. Nodes should never point towards more than 2 neighbors but at least 1 (except Node::End).", neighbor_count);
        }

        unreachable!();
    }

    pub fn after_break(&self, id: graph::NodeIndex) -> graph::NodeIndex {
        match *node_w!(self, id) {
            Node::Break(_) => (),
            _ => panic!("Should only be given a Node::Break"),
        }

        let neighbors = neighbors!(self, id);
        let neighbor_count = neighbors.clone().count();

        if neighbor_count == 2 {
            let mut found_first = false;
            for n in neighbors {
                match *node_w!(self, n) {
                    Node::LoopFoot(_) => {
                        if found_first {
                            return n;
                        } else {
                            found_first = true;
                        }
                    }
                    _ => return n,
                }
            }
        } else if neighbor_count == 1 {

        } else {
            panic!("Node::Continue points to {} neighbors. Nodes should never point towards more than 2 neighbors but at least 1 (except Node::End).", neighbor_count);
        }

        unreachable!();
    }

    pub fn start(&self) -> graph::NodeIndex {
        self.start
    }

    pub fn end(&self) -> graph::NodeIndex {
        self.end
    }

    pub fn after_start(&self) -> graph::NodeIndex {
        self.next(self.start)
    }

    ///
    /// Convenience function to get the next node in a linear sequence. If the current node has
    /// multiple outgoing edge (such as Node::Condition, Node::Return, Node::Break, and
    /// Node::Continue) or none (Node::End), return an error.
    ///
    pub fn next(&self, id: graph::NodeIndex) -> graph::NodeIndex {
        let mut neighbors = self.graph.neighbors_directed(id, Direction::Outgoing);
        if neighbors.clone().count() != 1 {
            panic!("CFG::next() only works when a Node has 1 neighbor");
        } else {
            neighbors.next().unwrap()
        }
    }

    pub fn previous(&self, id: graph::NodeIndex) -> graph::NodeIndex {
        let mut neighbors = self.neighbors_in(id);
        if neighbors.clone().count() != 1 {
            panic!("CFG::previous() only works when a Node has 1 neighbor");
        } else {
            neighbors.next().unwrap()
        }
    }

    pub fn before_branch_merge(&self, id: graph::NodeIndex) -> Vec<graph::NodeIndex> {
        match *self.node_weight(id) {
            Node::BranchMerge(_) => {
                self.neighbors_in(id).collect()
            }

            ref n @ _ => panic!("CFG::before_branch_merge() only works with Node::BranchMerge. Found {:?}", n),
        }
    }

    pub fn before_loop_foot(&self, id: graph::NodeIndex) -> graph::NodeIndex {
        match *self.node_weight(id) {
            Node::LoopFoot(ref loop_data) => {
                let mut found_loop_break = false;
                for n in self.neighbors_in(id) {
                    match *self.node_weight(n) {
                        Node::Break(ref break_data) => {
                            if break_data.loop_id != loop_data.loop_id {
                                return n;
                            } else if found_loop_break {
                                return n;
                            } else {
                                found_loop_break = true;
                            }
                        },
                        _ => return n,
                    }
                }

                unreachable!();
            }

            ref n @ _ => panic!("CFG::before_loop_foot() only works with Node::LoopFoot. Found {:?}", n),
        }
    }

    pub fn before_loop_head(&self, id: graph::NodeIndex) -> graph::NodeIndex {
        match *self.node_weight(id) {
            Node::LoopHead(ref loop_data) => {
                for n in self.neighbors_in(id) {
                    match *self.node_weight(n) {
                        Node::Continue(ref cont_data) => {
                            if cont_data.loop_id != loop_data.loop_id {
                                return n;
                            }
                        }

                        _ => return n,
                    }
                }

                unreachable!();
            }

            ref n @ _ => panic!("CFG::before_loop_head() only works with Node::LoopHead. Found {:?}", n),
        }
    }



    #[allow(unused_assignments)]
    ///
    /// Generate the control flow graph.
    /// Only performs continue/break statement checking (necessary for CFG generation).
    ///
    pub fn generate(
        universe: &Universe,
        body: ast::AstNode<ast::Block>,
        fn_type: &FunctionType,
    ) -> Result<Self, ControlFlowErr> {
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
    
        let (body, _) = body.to_data();
        let instructions = body.0;
        let fn_graph = CFG::get_branch(universe, &mut cfg, instructions, None)?;

        // Append the function body.
        if let Some(branch_head) = fn_graph.head {
            cfg.graph
                .add_edge(previous.unwrap(), branch_head, Edge::Normal);
            previous = Some(fn_graph.foot.unwrap());
        }

        // Auto-insert Node::Return(None) if the return type is SmplType::Unit
        if *universe.get_type(fn_type.return_type) == SmplType::Unit {
            // TODO: Figure out how to get last line of function
            append_node!(cfg, head, previous, Node::Return(ReturnData {
                expr: None,
                span: Span::new(0, 0),
            }));
        }

        append_node!(cfg, head, previous, Node::ExitScope);
        append_node_index!(cfg, head, previous, cfg.end);

        Ok(cfg)
    }

    #[allow(unused_assignments)]
    ///
    /// Returns the first and the last node in a code branch.
    ///
    fn get_branch(
        universe: &Universe,
        cfg: &mut CFG,
        instructions: Vec<ast::Stmt>,
        loop_data: Option<(graph::NodeIndex, graph::NodeIndex, LoopId)>,
    ) -> Result<BranchData, ControlFlowErr> {
        use ast::*;

        let mut previous = None;
        let mut head = None;

        // Go through all the instructions
        for stmt in instructions.into_iter() {
            if let Stmt::ExprStmt(expr_stmt) = stmt {
                let (expr_stmt, expr_stmt_span) = expr_stmt.to_data();
                match expr_stmt {
                    // All if statements begin and and with Node::BranchSplit and Node::BranchMerge
                    ExprStmt::If(if_data) => {
                        let id = universe.new_branching_id();
                        append_node!(cfg, head, previous, Node::BranchSplit(BranchingData {
                            branch_id: id,
                        }), Edge::Normal);

                        // All branches come back together at a Node::BranchMerge
                        let merge_node = cfg.graph.add_node(Node::BranchMerge(BranchingData {
                            branch_id: id,
                        }));

                        let mut previous_condition = None;

                        for branch in if_data.branches.into_iter() {
                            let (block, _) = branch.block.to_data();
                            let instructions = block.0;
                            let branch_graph =
                                CFG::get_branch(universe, cfg, instructions, loop_data)?;
                            let condition_node = {
                                let (conditional, con_span) = branch.conditional.to_data();
                                let expr = expr_flow::flatten(universe, conditional);
                                cfg.graph.add_node(Node::Condition(ExprData {
                                    expr: expr,
                                     span: con_span,
                                }))
                            };

                            // Check if there was a previous condition / branch
                            let edge = if previous_condition.is_none() {
                                // First condition, connected normally to Branch split
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
                                cfg.graph.add_edge(
                                    branch_graph.foot.unwrap(),
                                    scope_exit,
                                    Edge::Normal,
                                );
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
                            let (block, _) = block.to_data();
                            let instructions = block.0;
                            let branch_graph =
                                CFG::get_branch(universe, cfg, instructions, loop_data)?;

                            if let Some(branch_head) = branch_graph.head {
                                let scope_enter = cfg.graph.add_node(Node::EnterScope);
                                let scope_exit = cfg.graph.add_node(Node::ExitScope);

                                cfg.graph
                                    .add_edge(previous.unwrap(), scope_enter, Edge::False);
                                cfg.graph.add_edge(scope_enter, branch_head, Edge::Normal);
                                cfg.graph.add_edge(
                                    branch_graph.foot.unwrap(),
                                    scope_exit,
                                    Edge::Normal,
                                );
                                cfg.graph.add_edge(scope_exit, merge_node, Edge::Normal);
                            } else {
                                cfg.graph
                                    .add_edge(previous.unwrap(), merge_node, Edge::False);
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
                        let (block, _) = while_data.block.to_data();

                        let loop_id = universe.new_loop_id();
                        let loop_head = cfg.graph.add_node(Node::LoopHead(LoopData {
                            loop_id: loop_id,
                            span: block.1,
                        }));
                        let loop_foot = cfg.graph.add_node(Node::LoopFoot(LoopData {
                            loop_id: loop_id,
                            span: block.2,
                        }));

                        cfg.graph.add_edge(loop_foot, loop_head, Edge::BackEdge);

                        append_node_index!(cfg, head, previous, loop_head);
                        let instructions = block.0;
                        let loop_body = CFG::get_branch(
                            universe,
                            cfg,
                            instructions,
                            Some((loop_head, loop_foot, loop_id)),
                        )?;
                        let condition = {
                            let (conditional, con_span) = while_data.conditional.to_data();
                            let expr = expr_flow::flatten(universe, conditional);
                            cfg.graph.add_node(Node::Condition(ExprData {
                                expr: expr,
                                span: con_span,
                            }))
                        };

                        append_node_index!(cfg, head, previous, condition);

                        let scope_enter = cfg.graph.add_node(Node::EnterScope);
                        let scope_exit = cfg.graph.add_node(Node::ExitScope);

                        cfg.graph.add_edge(condition, scope_enter, Edge::True);
                        cfg.graph.add_edge(scope_exit, loop_foot, Edge::Normal);
                        cfg.graph.add_edge(condition, loop_foot, Edge::False);

                        if let Some(branch_head) = loop_body.head {
                            cfg.graph.add_edge(scope_enter, branch_head, Edge::Normal);
                            cfg.graph
                                .add_edge(loop_body.foot.unwrap(), scope_exit, Edge::Normal);
                        } else {
                            cfg.graph.add_edge(scope_enter, scope_exit, Edge::Normal);
                        }

                        previous = Some(loop_foot);
                    }

                    ExprStmt::Break(span) => {
                        if let Some((_, foot, loop_id)) = loop_data {
                            let break_id = cfg.graph.add_node(Node::Break(LoopData {
                                loop_id: loop_id,
                                span: span,
                            }));
                            append_node_index!(cfg, head, previous, break_id);

                            // Add an edge to the foot of the loop.
                            cfg.graph.add_edge(break_id, foot, Edge::Normal);
                        } else {
                            // Found a break statement not inside a loop.
                            return Err(ControlFlowErr::BadBreak(span));
                        }
                    }

                    ExprStmt::Continue(span) => {
                        if let Some((loop_head, _, loop_id)) = loop_data {
                            let continue_id = cfg.graph.add_node(Node::Continue(LoopData {
                                loop_id: loop_id,
                                span: span,
                            }));
                            append_node_index!(cfg, head, previous, continue_id);

                            // Add a backedge to the head of the loop.
                            cfg.graph.add_edge(continue_id, loop_head, Edge::BackEdge);
                        } else {
                            // Found a continue statement not inside a loop.
                            return Err(ControlFlowErr::BadContinue(span));
                        }
                    }

                    ExprStmt::Return(span, expr) => {
                        let expr = expr.map(|expr| expr_flow::flatten(universe, expr));
                        let ret = cfg.graph.add_node(Node::Return(ReturnData {
                            expr: expr,
                            span: span,
                        }));
                        append_node_index!(cfg, head, previous, ret);
                    }

                    ExprStmt::LocalVarDecl(decl) => {
                        let decl = typed_ast::LocalVarDecl::new(universe, decl, expr_stmt_span);
                        append_node!(
                            cfg,
                            head,
                            previous,
                            Node::LocalVarDecl(LocalVarDeclData {
                                decl: decl,
                                span: expr_stmt_span,
                            })
                        );
                    }

                    ExprStmt::Assignment(assignment) => {
                        let assignment = typed_ast::Assignment::new(universe, assignment);
                        append_node!(
                            cfg,
                            head,
                            previous,
                            Node::Assignment(AssignmentData {
                                assignment: assignment,
                                span: expr_stmt_span,
                            })
                        );
                    }
                }
            } else if let Stmt::Expr(expr) = stmt {
                let (expr, span) = expr.to_data();
                let expr = expr_flow::flatten(universe, expr);
                append_node!(
                    cfg,
                    head,
                    previous,
                    Node::Expr(ExprData {
                        expr: expr,
                        span: span,
                    })
                );
            }
        }

        // 'previous' represents the last node in the branch
        return Ok(BranchData {
            head: head,
            foot: previous,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use petgraph::dot::{Config, Dot};
    use petgraph::Direction;

    use super::super::smpl_type::*;
    use super::super::semantic_data::Universe;

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
        let input = "fn test(arg: int) {
let a: int = 2;
let b: int = 3;
}";
        let input = wrap_input(input);
        let universe = Universe::std();
        let fn_type = FunctionType {
            params: ParamType::Checked(vec![universe.int()]),
            return_type: universe.unit(),
        };
        let parser = FnDeclParser::new();
        let fn_def = parser.parse(input).unwrap();
        let fn_def = fn_def.data();
        let cfg = CFG::generate(&universe, fn_def.body.clone(), &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            irmatch!(*cfg.graph.node_weight(cfg.start).unwrap(); Node::Start => ());
            irmatch!(*cfg.graph.node_weight(cfg.end).unwrap(); Node::End => ());
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
                Node::Return(..) => (),
                ref n @ _ => panic!("Expected to find Node::Return. Found {:?}", n),
            }

            let exit = ret_neighbors.next().unwrap();
            let mut exit_neighbors = neighbors!(cfg, exit);
            match *node_w!(cfg, exit) {
                Node::ExitScope => (),
                ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
            }

            let end = exit_neighbors.next().unwrap();
            let end_neighbors = neighbors!(cfg, end);
            assert_eq!(end_neighbors.count(), 0);
            match *node_w!(cfg, end) {
                Node::End => (),
                ref n @ _ => panic!("Expected to find Node::End. Found {:?}", n),
            }
        }
    }

    #[test]
    fn branching_cfg_generation() {
        let input = "fn test(arg: int) {
if (test) {
    let c: int = 4;
}
}";
        let input = wrap_input(input);

        let universe = Universe::std();
        let fn_type = FunctionType {
            params: ParamType::Checked(vec![universe.int()]),
            return_type: universe.unit(),
        };
        let parser = FnDeclParser::new();
        let fn_def = parser.parse(input).unwrap();
        let fn_def = fn_def.data();
        let cfg = CFG::generate(&universe, fn_def.body.clone(), &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            irmatch!(*cfg.graph.node_weight(cfg.start).unwrap(); Node::Start => ());
            irmatch!(*cfg.graph.node_weight(cfg.end).unwrap(); Node::End => ());

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

            let mut merge = None;

            let branch_split = enter_neighbors.next().expect("Looking for BranchSplit");
            let mut branch_split_neighbors = neighbors!(cfg, branch_split);

            match *node_w!(cfg, branch_split) {
                Node::BranchSplit(_) => (),
                ref n @ _ => panic!("Expected BranchSplit node. Found {:?}", n),
            }

            // Check condition node
            let condition = branch_split_neighbors.next().expect("Looking for condition node");
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
                                    let mut neighbors =
                                        cfg.graph.neighbors_directed(target, Direction::Outgoing);
                                    let decl = neighbors.next().unwrap();

                                    match *cfg.graph.node_weight(decl).unwrap() {
                                        Node::LocalVarDecl(_) => (),
                                        ref n @ _ => panic!(
                                            "Expected to find Node::LocalVarDecl. Found {:?}",
                                            n
                                        ),
                                    }

                                    let mut neighbors =
                                        cfg.graph.neighbors_directed(decl, Direction::Outgoing);
                                    let exit_scope = neighbors.next().unwrap();

                                    match *cfg.graph.node_weight(exit_scope).unwrap() {
                                        Node::ExitScope => (),

                                        ref n @ _ => panic!(
                                            "Expected to find Node::ExitScope. Found {:?}",
                                            n
                                        ),
                                    }
                                }

                                ref n @ _ => {
                                    panic!("Expected to find Node::EnterScope. Found {:?}", n)
                                }
                            }

                            found_true_edge = true;
                        } else if let Edge::False = *edge.weight() {
                            let target = edge.target();

                            if let Node::BranchMerge(_) = *cfg.graph.node_weight(target).unwrap() {
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
            let mut return_neighbors = neighbors!(cfg, return_n);
            match *node_w!(cfg, return_n) {
                Node::Return(..) => (),
                ref n @ _ => panic!("Expected to find Node::Return. Found {:?}", n),
            }

            let exit = return_neighbors.next().unwrap();
            let mut exit_neighbors = neighbors!(cfg, exit);
            match *node_w!(cfg, exit) {
                Node::ExitScope => (),
                ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
            }

            let end = exit_neighbors.next().unwrap();
            let end_neighbors = neighbors!(cfg, end);
            match *node_w!(cfg, end) {
                Node::End => {
                    assert_eq!(end_neighbors.count(), 0);
                }
                ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
            }
        }
    }

    #[test]
    fn complex_branching_cfg_generation() {
        let input = "fn test(arg: int) {
    if (false) {
        let c: int = 4;
    } elif (true) {

    } else {

    }
}";
        let input = wrap_input(input);
        let universe = Universe::std();
        let fn_type = FunctionType {
            params: ParamType::Checked(vec![universe.int()]),
            return_type: universe.unit(),
        };
        let parser = FnDeclParser::new();
        let fn_def = parser.parse(input).unwrap();
        let fn_def = fn_def.data();
        let cfg = CFG::generate(&universe, fn_def.body.clone(), &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            // start -> enter_scope -> branch_split -> condition(B)
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

            let branch_split = enter_neighbors.next().unwrap();
            let mut branch_split_neighbors = neighbors!(cfg, branch_split);
            match *node_w!(cfg, branch_split) {
                Node::BranchSplit(_) => (), // Success

                ref n @ _ => panic!("Expected a condition node. Found {:?}", n),
            }

            let condition_b = branch_split_neighbors.next().unwrap();
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

            let enter =
                condition_b_true.expect("Missing true edge connecting to variable declaration");
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
                Node::BranchMerge(_) => (),

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
            match *node_w!(cfg, truth_target) {
                Node::BranchMerge(_) => (),
                ref n @ _ => panic!("Expected BranchMerge. Found {:?}", n),
            }

            match *node_w!(cfg, false_target) {
                Node::BranchMerge(_) => (),
                ref n @ _ => panic!("Expected BranchMerge. Found {:?}", n),
            }

            let branch_merge = truth_target;
            let mut branch_merge_neighbors = neighbors!(cfg, branch_merge);
            assert_eq!(branch_merge_neighbors.clone().count(), 1);

            let implicit_return = branch_merge_neighbors.next().unwrap();
            let mut implicit_return_neighbors = neighbors!(cfg, implicit_return);
            assert_eq!(implicit_return_neighbors.clone().count(), 1);
            match *node_w!(cfg, implicit_return) {
                Node::Return(..) => (),
                ref n @ _ => println!("Expected return node. Found {:?}", n),
            }

            let exit = implicit_return_neighbors.next().unwrap();
            let mut exit_neighbors = neighbors!(cfg, exit);
            match *node_w!(cfg, exit) {
                Node::ExitScope => (),
                ref n @ _ => panic!("Expected to find Node::Exit. Found {:?}", n),
            }

            let end = exit_neighbors.next().unwrap();
            irmatch!(*node_w!(cfg, end); Node::End => ());
        }
    }

    #[test]
    fn while_loop_generation() {
        let input = "fn test(arg: int) {
    while (true) {
        
    }
}";
        let input = wrap_input(input);
        let universe = Universe::std();
        let fn_type = FunctionType {
            params: ParamType::Checked(vec![universe.int()]),
            return_type: universe.unit(),
        };
        let parser = FnDeclParser::new();
        let fn_def = parser.parse(input).unwrap();
        let fn_def = fn_def.data();
        let cfg = CFG::generate(&universe, fn_def.body.clone(), &fn_type).unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        // start -> enter_scope -> loop_head(A) -> condition(B)
        //       -[true]> enter_scope exit_scope loop_foot(A)
        //       -[false]> loop_foot(A)
        // loop_foot(A) -> implicit_return -> exit_scope -> end
        // loop_head(A) << loop_foot(A)
        //

        assert_eq!(cfg.graph.node_count(), 10);

        let mut start_neighbors = neighbors!(cfg, cfg.start);
        assert_eq!(start_neighbors.clone().count(), 1);

        let enter = start_neighbors.next().unwrap();
        let mut enter_neighbors = neighbors!(cfg, enter);
        match *node_w!(cfg, enter) {
            Node::EnterScope => (),
            ref n @ _ => panic!("Expected to find Node::Enter. Found {:?}", n),
        }

        let loop_id;
        let loop_head = enter_neighbors.next().unwrap();
        match *node_w!(cfg, loop_head) {
            Node::LoopHead(ref loop_data) => loop_id = loop_data.loop_id,
            ref n @ _ => panic!("Expected to find Node::LoopHead. Found {:?}", n),
        }

        let mut head_neighbors = neighbors!(cfg, loop_head);
        assert_eq!(head_neighbors.clone().count(), 1);

        let condition = head_neighbors.next().unwrap();
        match *node_w!(cfg, condition) {
            Node::Condition(_) => (),
            ref n @ _ => panic!("Expected condition node. Found {:?}", n),
        }

        let condition_edges = edges!(cfg, condition);
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
        match *node_w!(cfg, truth_target) {
            Node::EnterScope => (),
            ref n @ _ => panic!("Expected to find Node::EnterScope. Found {:?}", n),
        }

        let mut enter_neighbors = neighbors!(cfg, truth_target);
        let exit = enter_neighbors.next().unwrap();
        let mut exit_neighbors = neighbors!(cfg, exit);
        match *node_w!(cfg, exit) {
            Node::ExitScope => (),
            ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
        }

        let foot = exit_neighbors.next().unwrap();
        let mut foot_neighbors = neighbors!(cfg, foot);
        match *node_w!(cfg, foot) {
            Node::LoopFoot(ref loop_data) => assert_eq!(loop_data.loop_id, loop_id),
            ref n @ _ => panic!("Expected to find Node::LoopFoot. Found {:?}", n),
        }

        assert_eq!(foot, false_target);

        assert_eq!(foot_neighbors.clone().count(), 2);

        let implicit_return = foot_neighbors.next().unwrap();
        let mut return_neighbors = neighbors!(cfg, implicit_return);
        assert_eq!(return_neighbors.clone().count(), 1);
        match *node_w!(cfg, implicit_return) {
            Node::Return(..) => (),
            ref n @ _ => panic!("Expected return node. Found {:?}", n),
        }

        let exit = return_neighbors.next().unwrap();
        let mut exit_neighbors = neighbors!(cfg, exit);
        match *node_w!(cfg, exit) {
            Node::ExitScope => (),
            ref n @ _ => panic!("Expected to find Node::ExitScope. Found {:?}", n),
        }

        let end = exit_neighbors.next().unwrap();
        let end_neighbors = neighbors!(cfg, end);
        assert_eq!(end_neighbors.count(), 0);
        match *node_w!(cfg, end) {
            Node::End => (),
            ref n @ _ => panic!("Expected to find Node::End. Found {:?}", n),
        }
    }
}
