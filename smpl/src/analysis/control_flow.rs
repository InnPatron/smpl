use petgraph::graph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;

use crate::ast;

use crate::span::Span;

use super::error::{AnalysisError, ControlFlowError};
use super::expr_flow;

use super::semantic_data::{AnalysisContext, LoopId, Universe};
use super::type_cons::TypeCons;
use super::abstract_type::AbstractType;

use super::type_checker::TypingContext;
use super::typed_ast;

use super::control_data::*;

type InternalLoopData = Option<(graph::NodeIndex, graph::NodeIndex, LoopId)>;

macro_rules! node_w {
    ($CFG: expr, $node: expr) => {
        $CFG.graph().node_weight($node).unwrap()
    };
}

macro_rules! neighbors {
    ($CFG: expr, $node: expr) => {
        $CFG.graph().neighbors_directed($node, Direction::Outgoing)
    };
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

    pub fn start(&self) -> graph::NodeIndex {
        self.start
    }

    pub fn after_start(&self) -> graph::NodeIndex {
        self.next(self.start)
    }

    pub fn end(&self) -> graph::NodeIndex {
        self.end
    }

    ///
    /// Convenience function to get the next node in a linear sequence. If the current node has
    /// multiple outgoing edge (such as Node::Return, Node::Break, and
    /// Node::Continue) or none (Node::End), return an error.
    ///
    pub fn next(&self, id: graph::NodeIndex) -> graph::NodeIndex {
        let mut neighbors =
            self.graph.neighbors_directed(id, Direction::Outgoing);
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

    pub fn before_branch_merge(
        &self,
        id: graph::NodeIndex,
    ) -> Vec<graph::NodeIndex> {
        match *self.node_weight(id) {
            Node::BranchMerge(_) => self.neighbors_in(id).collect(),

            ref n @ _ => panic!(
                "CFG::before_branch_merge() only works with Node::BranchMerge. Found {:?}",
                n
            ),
        }
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
                Node::LoopHead(ref data, _) => {
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
    /// Expects id of LoopHead or BranchSplit
    /// Returns (TRUE, FALSE) branch heads.
    ///
    pub fn after_conditional(
        &self,
        id: graph::NodeIndex,
    ) -> (graph::NodeIndex, graph::NodeIndex) {
        match *node_w!(self, id) {
            Node::LoopHead(..) => (),
            Node::BranchSplit(..) => (),
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
                ref e @ _ => panic!(
                    "Unexpected edge {:?} coming out of a condition node.",
                    e
                ),
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
                    Node::LoopHead(..) => {
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

    pub fn node_weight(&self, node: graph::NodeIndex) -> &Node {
        self.graph.node_weight(node).unwrap()
    }

    pub fn node_weight_mut(&mut self, node: graph::NodeIndex) -> &mut Node {
        self.graph.node_weight_mut(node).unwrap()
    }

    pub fn neighbors_out(
        &self,
        node: graph::NodeIndex,
    ) -> graph::Neighbors<Edge> {
        self.graph.neighbors_directed(node, Direction::Outgoing)
    }

    pub fn neighbors_in(
        &self,
        node: graph::NodeIndex,
    ) -> graph::Neighbors<Edge> {
        self.graph.neighbors_directed(node, Direction::Incoming)
    }

    #[allow(unused_assignments)]
    ///
    /// Generate the control flow graph.
    /// Only performs continue/break statement checking (necessary for CFG generation).
    ///
    pub fn generate(
        universe: &mut Universe,
        body: ast::AstNode<ast::Block>,
        fn_type: &TypeCons,
        _analysis_context: &AnalysisContext,
    ) -> Result<Self, AnalysisError> {
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

        let (body, _) = body.to_data();
        let instructions = body.0;
        let function_body = cfg.generate_scoped_block(
            universe,
            instructions.into_iter(),
            None,
        )?;

        let mut previous = Some(cfg.start);
        let mut head = previous;
        append_node!(cfg, head, previous, Node::EnterScope);

        // Append the function body.
        if let Some(branch_head) = function_body.head {
            cfg.graph
                .add_edge(previous.unwrap(), branch_head, Edge::Normal);
            previous = Some(function_body.foot.unwrap());
        }

        if let TypeCons::Function {
            ref return_type, ..
        } = fn_type
        {
            let outer_scope = universe.std_scope();
            let outer_context = TypingContext::empty();
            // TODO: Should this be the function scope/context?
            let return_type = return_type.substitute(
                universe,
                &outer_scope,
                &outer_context,
            )?;
            if let AbstractType::Unit(_) = return_type {
                append_node!(
                    cfg,
                    head,
                    previous,
                    Node::Return(ReturnData {
                        expr: None,
                        span: Span::dummy(),
                    })
                );
            }
        }

        append_node!(cfg, head, previous, Node::ExitScope);
        append_node_index!(cfg, head, previous, cfg.end);

        Ok(cfg)
    }

    ///
    /// Generates a block of code encapsualted by ScopeEnter and ScopeExit.
    ///
    /// Expression statements (assignment, local declaration), expressions ('1 + 2;') are placed
    /// into a basic block. Whenever a non-basic block structure is encountered, the current basic
    /// block is appended to the graph.
    ///
    fn generate_scoped_block<'a, 'b, T>(
        &'a mut self,
        universe: &'b mut Universe,
        mut instructions: T,
        loop_data: InternalLoopData,
    ) -> Result<BranchData, ControlFlowError>
    where
        T: Iterator<Item = ast::Stmt>,
    {
        use crate::ast::*;

        let mut previous: Option<graph::NodeIndex> = None;
        let mut head: Option<graph::NodeIndex> = None;

        let mut current_block = BasicBlock::new();

        while let Some(next) = instructions.next() {
            match next {
                // Added to current basic block
                Stmt::Expr(expr) => {
                    let (ast_expr, span) = expr.to_data();
                    let expr = expr_flow::flatten(universe, ast_expr);
                    current_block.append(BlockNode::Expr(ExprData {
                        expr: expr,
                        span: span,
                    }));
                }

                Stmt::ExprStmt(expr_stmt) => {
                    let (expr_stmt, expr_stmt_span) = expr_stmt.to_data();
                    match expr_stmt {
                        // Append assignment node to current basic block
                        ExprStmt::Assignment(assignment) => {
                            let assignment = typed_ast::Assignment::new(
                                universe, assignment,
                            );
                            current_block.append(BlockNode::Assignment(
                                AssignmentData {
                                    assignment: assignment,
                                    span: expr_stmt_span,
                                },
                            ));
                        }

                        // Append local variable declaration node to current basic block
                        ExprStmt::LocalVarDecl(decl) => {
                            let decl = typed_ast::LocalVarDecl::new(
                                universe,
                                decl,
                                expr_stmt_span.clone(),
                            );
                            current_block.append(BlockNode::LocalVarDecl(
                                LocalVarDeclData {
                                    decl: decl,
                                    span: expr_stmt_span,
                                },
                            ));
                        }

                        // Append return node to current basic block
                        ExprStmt::Return(span, expr) => {
                            if current_block.is_empty() == false {
                                append_node!(
                                    self,
                                    head,
                                    previous,
                                    Node::Block(current_block)
                                );
                                current_block = BasicBlock::new();
                            }
                            let expr = expr
                                .map(|expr| expr_flow::flatten(universe, expr));
                            append_node!(
                                self,
                                head,
                                previous,
                                Node::Return(ReturnData {
                                    expr: expr,
                                    span: span,
                                })
                            );
                        }

                        // Append break node to current basic block
                        ExprStmt::Break(span) => match loop_data {
                            Some((_, _foot, loop_id)) => {
                                if current_block.is_empty() == false {
                                    append_node!(
                                        self,
                                        head,
                                        previous,
                                        Node::Block(current_block)
                                    );
                                    current_block = BasicBlock::new();
                                }
                                append_node!(
                                    self,
                                    head,
                                    previous,
                                    Node::Break(LoopData {
                                        loop_id: loop_id,
                                        span: span,
                                    })
                                );
                            }

                            None => {
                                return Err(ControlFlowError::BadBreak(span))
                            }
                        },

                        // Append continue node to current basic block
                        ExprStmt::Continue(span) => match loop_data {
                            Some((_loop_head, _, loop_id)) => {
                                if current_block.is_empty() == false {
                                    append_node!(
                                        self,
                                        head,
                                        previous,
                                        Node::Block(current_block)
                                    );
                                    current_block = BasicBlock::new();
                                }

                                append_node!(
                                    self,
                                    head,
                                    previous,
                                    Node::Continue(LoopData {
                                        loop_id: loop_id,
                                        span: span,
                                    })
                                );
                            }

                            None => {
                                return Err(ControlFlowError::BadContinue(span))
                            }
                        },

                        ExprStmt::While(while_data) => {
                            // Generate a fragment of a CFG beginning with LoopHead and ending with
                            //   LoopFoot.
                            // LoopHead connects to the loop body via a TRUE edge (or to the LoopFoot
                            //   in the case of an empty body)
                            // LoopHead connects to the LoopFoot via a FALSE edge

                            // Append current basic block if not empty
                            if current_block.is_empty() == false {
                                append_node!(
                                    self,
                                    head,
                                    previous,
                                    Node::Block(current_block)
                                );
                                current_block = BasicBlock::new();
                            }

                            let (block, _) = while_data.block.to_data();

                            let loop_id = universe.new_loop_id();

                            let expr_data = {
                                let (conditional, con_span) =
                                    while_data.conditional.to_data();
                                let expr =
                                    expr_flow::flatten(universe, conditional);
                                ExprData {
                                    expr: expr,
                                    span: con_span,
                                }
                            };

                            let loop_data = LoopData {
                                loop_id: loop_id,
                                span: expr_stmt_span.clone(),
                            };

                            let loop_head = self.graph.add_node(
                                Node::LoopHead(loop_data.clone(), expr_data),
                            );
                            let loop_foot =
                                self.graph.add_node(Node::LoopFoot(loop_data));

                            // Connect loop foot to loop head with a backedge
                            self.graph.add_edge(
                                loop_foot,
                                loop_head,
                                Edge::BackEdge,
                            );

                            // Append the loop head to the graph
                            append_node_index!(self, head, previous, loop_head);
                            let instructions = block.0;
                            let loop_body = self.generate_scoped_block(
                                universe,
                                instructions.into_iter(),
                                Some((loop_head, loop_foot, loop_id)),
                            )?;

                            // Connect the condition node to the loop foot by the FALSE path
                            self.graph.add_edge(
                                loop_head,
                                loop_foot,
                                Edge::False,
                            );

                            if let Some(loop_body_head) = loop_body.head {
                                let scope_enter =
                                    self.graph.add_node(Node::EnterScope);
                                let scope_exit =
                                    self.graph.add_node(Node::ExitScope);

                                // Connect the scope enter/exit to the loop body by the TRUE path
                                self.graph.add_edge(
                                    loop_head,
                                    scope_enter,
                                    Edge::True,
                                );

                                // Connect the loop body to the scope enter and exit
                                self.graph.add_edge(
                                    scope_enter,
                                    loop_body_head,
                                    Edge::Normal,
                                );
                                self.graph.add_edge(
                                    loop_body.foot.unwrap(),
                                    scope_exit,
                                    Edge::Normal,
                                );

                                // Connect scope exit to loop foot
                                self.graph.add_edge(
                                    scope_exit,
                                    loop_foot,
                                    Edge::Normal,
                                );
                            } else {
                                // Empty loop body
                                // Connect the condition node to the loop foot by the TRUE path
                                self.graph.add_edge(
                                    loop_head,
                                    loop_foot,
                                    Edge::True,
                                );
                            }

                            previous = Some(loop_foot);
                        }

                        ExprStmt::If(if_data) => {
                            // If statements are broken down into "stacked branches"
                            // 1) Each BranchSplit represents a conditional split
                            // 2) Each True path is ended by a BranchMerge
                            // 3) The next branch's BranchSplit is connected to the previous BranchSplit via the
                            //    False path
                            // 4) The default branch's head connects directly to the previous
                            //    BranchSplit via the False path
                            // 5) The end of the current False path connects to the previous' BranchMerge

                            // Generates a scoped fragment of the CFG suitable for easy branching.
                            // If it is a conditional branch, generate the branch with a BranchSplit
                            //   and BranchMerge at the heads.
                            // If it is the default branch (i.e. no condition), do not generate a
                            //   BranchSplit or BranchMerge (keep ScopeEnter, ScopeExit)
                            fn generate_branch(
                                cfg: &mut CFG,
                                universe: &mut Universe,
                                body: AstNode<Block>,
                                condition: Option<AstNode<Expr>>,
                                loop_data: InternalLoopData,
                            ) -> Result<BranchData, ControlFlowError>
                            {
                                let (block, _) = body.to_data();
                                let instructions = block.0;
                                // Generate the branch subgraph
                                let branch_graph = cfg.generate_scoped_block(
                                    universe,
                                    instructions.into_iter(),
                                    loop_data,
                                )?;

                                let scope_enter =
                                    cfg.graph.add_node(Node::EnterScope);
                                let scope_exit =
                                    cfg.graph.add_node(Node::ExitScope);

                                match (branch_graph.head, branch_graph.foot) {
                                    (Some(head), Some(foot)) => {
                                        cfg.graph.add_edge(
                                            scope_enter,
                                            head,
                                            Edge::Normal,
                                        );
                                        cfg.graph.add_edge(
                                            foot,
                                            scope_exit,
                                            Edge::Normal,
                                        );
                                    }

                                    (Some(head), None) => {
                                        cfg.graph.add_edge(
                                            scope_enter,
                                            head,
                                            Edge::Normal,
                                        );
                                        cfg.graph.add_edge(
                                            head,
                                            scope_exit,
                                            Edge::Normal,
                                        );
                                    }

                                    (None, None) => {
                                        // Empty block
                                        // Currently guarenteeing generate_branch() always returns
                                        // head = Some, foot = Some
                                        cfg.graph.add_edge(
                                            scope_enter,
                                            scope_exit,
                                            Edge::Normal,
                                        );
                                    }

                                    (None, Some(_)) => unreachable!(),
                                }

                                // Generate the BranchSplit and BranchMerge
                                // Make those the new head and foot of the body, respectively
                                match condition {
                                    Some(ast_condition) => {
                                        let branch_id =
                                            universe.new_branching_id();

                                        let (conditional, con_span) =
                                            ast_condition.to_data();
                                        let expr = expr_flow::flatten(
                                            universe,
                                            conditional,
                                        );
                                        let expr_data = ExprData {
                                            expr: expr,
                                            span: con_span,
                                        };

                                        let branching_data = BranchingData {
                                            branch_id: branch_id,
                                        };

                                        let split_node = cfg.graph.add_node(
                                            Node::BranchSplit(
                                                branching_data.clone(),
                                                expr_data,
                                            ),
                                        );
                                        let merge_node = cfg.graph.add_node(
                                            Node::BranchMerge(
                                                branching_data.clone(),
                                            ),
                                        );

                                        cfg.graph.add_edge(
                                            split_node,
                                            scope_enter,
                                            Edge::True,
                                        );
                                        cfg.graph.add_edge(
                                            scope_exit,
                                            merge_node,
                                            Edge::Normal,
                                        );

                                        Ok(BranchData {
                                            head: Some(split_node),
                                            foot: Some(merge_node),
                                        })
                                    }

                                    None => Ok(BranchData {
                                        head: Some(scope_enter),
                                        foot: Some(scope_exit),
                                    }),
                                }
                            }

                            // Append current basic block if not empty
                            if current_block.is_empty() == false {
                                append_node!(
                                    self,
                                    head,
                                    previous,
                                    Node::Block(current_block)
                                );
                                current_block = BasicBlock::new();
                            }

                            let mut branches = if_data.branches.into_iter();

                            // Generate the first branch
                            let first_branch = branches.next().unwrap();
                            let first_branch = generate_branch(
                                self,
                                universe,
                                first_branch.block,
                                Some(first_branch.conditional),
                                loop_data,
                            )?;

                            // Append the first branch to the overall CFG
                            append_node_index!(
                                self,
                                head,
                                previous,
                                first_branch.head
                                    .expect("generate_branch() head should always be Some"),
                                Edge::Normal
                            );

                            let first_branch_foot = first_branch.foot.expect(
                                "generate_branch() foot should always be Some",
                            );

                            let mut previous_branch: BranchData = first_branch;
                            // Stack the branches
                            for branch in branches {
                                let branch = generate_branch(
                                    self,
                                    universe,
                                    branch.block,
                                    Some(branch.conditional),
                                    loop_data,
                                )?;

                                let branch_head = branch.head
                                    .expect("generate_branch() head should always be Some");
                                let branch_foot = branch.foot
                                    .expect("generate_branch() foot should always be Some");

                                let previous_head = previous_branch.head
                                    .expect("generate_branch() head should always be Some");
                                let previous_foot = previous_branch.foot
                                    .expect("generate_branch() foot should always be Some");

                                // Connect false edge of previous BranchSplit to current
                                //   BranchSplit
                                self.graph.add_edge(
                                    previous_head,
                                    branch_head,
                                    Edge::False,
                                );
                                // Connect current BranchMerge to previous BranchMerge ("stacking")
                                self.graph.add_edge(
                                    branch_foot,
                                    previous_foot,
                                    Edge::Normal,
                                );

                                previous_branch = BranchData {
                                    head: Some(branch_head),
                                    foot: Some(branch_foot),
                                };
                            }

                            let previous_head = previous_branch.head.expect(
                                "generate_branch() head should always be Some",
                            );
                            let previous_foot = previous_branch.foot.expect(
                                "generate_branch() foot should always be Some",
                            );

                            // No more conditional branches.
                            // Check for the "else" branch.
                            match if_data.default_block {
                                Some(block) => {
                                    // Found an "else" branch
                                    let else_branch = generate_branch(
                                        self, universe, block, None, loop_data,
                                    )?;
                                    let else_head = else_branch.head
                                        .expect("generate_branch() head should always be Some");
                                    let else_foot = else_branch.foot
                                        .expect("generate_branch() foot should always be Some");

                                    // Connect false edge of previous BranchMerge to head of
                                    // the else branch
                                    self.graph.add_edge(
                                        previous_head,
                                        else_head,
                                        Edge::False,
                                    );
                                    self.graph.add_edge(
                                        else_foot,
                                        previous_foot,
                                        Edge::Normal,
                                    );
                                }

                                None => {
                                    // No default branch ("else"). Connect the previous BranchSplit
                                    //   to the previous BranchMerge with a FALSE edge
                                    self.graph.add_edge(
                                        previous_head,
                                        previous_foot,
                                        Edge::False,
                                    );
                                }
                            }

                            // All other nodes are added after any branching.
                            previous = Some(first_branch_foot);
                        }
                    }
                }
            }
        }

        if current_block.is_empty() == false {
            append_node!(self, head, previous, Node::Block(current_block));
        }

        Ok(BranchData {
            head: head,
            foot: previous,
        })
    }
}

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod tests {
    use super::*;
    use super::super::type_checker::TypingContext;
    use super::super::analysis_helpers::*;
    use crate::parser::*;
    use crate::parser::parser::*;
    use crate::module::ModuleSource;
    use crate::span::Span;
    use petgraph::dot::{Config, Dot};
    use petgraph::Direction;

    use super::super::semantic_data::{TypeId, Universe};
    use super::super::type_cons::*;

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

    fn expected_app(tc: TypeId) -> AbstractType {
        AbstractType::App {
            data: Span::dummy(),
            type_cons: tc,
            args: Vec::new(),
        }
    }

    fn fn_type_cons(params: Vec<AbstractType>, return_type: AbstractType) -> TypeCons {
        let tc = TypeCons::Function {
            parameters: params,
            return_type: return_type,
            type_params: TypeParams::empty(),
        };

        tc
    }

    #[test]
    fn linear_cfg_generation() {
        let input = "fn test(arg: int) {
let a: int = 2;
let b: int = 3;
}";
        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        let mut universe = Universe::std();
        let fn_type = fn_type_cons(vec![expected_app(universe.int())], expected_app(universe.unit()));
        let fn_def = testfn_decl(&mut input).unwrap();
        let analysis_context = 
            generate_fn_analysis_data(&universe, 
                &universe.std_scope(), 
                &TypingContext::empty(),
                &fn_type,
                &fn_def
                ).unwrap();
        let cfg = CFG::generate(&mut universe, 
                fn_def.body.clone(), &fn_type, &analysis_context)
            .unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            irmatch!(*cfg.graph.node_weight(cfg.start).unwrap(); Node::Start => ());
            irmatch!(*cfg.graph.node_weight(cfg.end).unwrap(); Node::End => ());
            // start -> enter_scope -> block -> implicit return -> exit_scope -> end
            assert_eq!(cfg.graph.node_count(), 6);

            let mut start_neighbors = neighbors!(cfg, cfg.start);

            let enter = start_neighbors.next().unwrap();
            let mut enter_neighbors = neighbors!(cfg, enter);
            match *node_w!(cfg, enter) {
                Node::EnterScope => (),
                ref n @ _ => panic!("Expected to find Node::EnterScope. Found {:?}", n),
            }

            let block_1 = enter_neighbors.next().unwrap();
            let mut block_1_neighbors = neighbors!(cfg, block_1);
            match *node_w!(cfg, block_1) {
                Node::Block(_) => (),
                ref n @ _ => panic!("Expected to find Node::LocalVarDecl. Found {:?}", n),
            }

            let ret = block_1_neighbors.next().unwrap();
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
        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);

        let mut universe = Universe::std();
        let fn_type = fn_type_cons(vec![expected_app(universe.int())], expected_app(universe.unit()));
        let fn_def = testfn_decl(&mut input).unwrap();
        let analysis_context = 
            generate_fn_analysis_data(&universe, 
                &universe.std_scope(), 
                &TypingContext::empty(),
                &fn_type,
                &fn_def
                ).unwrap();
        let cfg = CFG::generate(
                &mut universe, fn_def.body.clone(), &fn_type, &analysis_context,
            )
            .unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            irmatch!(*cfg.graph.node_weight(cfg.start).unwrap(); Node::Start => ());
            irmatch!(*cfg.graph.node_weight(cfg.end).unwrap(); Node::End => ());

            // start -> enter_scope -> branch_split
            //      -[true]> {
            //          -> enter_scope
            //          -> block
            //          -> exit_scope
            //      } ->        >>___ branch_merge ->
            //        -[false]> >>
            //      implicit return -> exit_scope -> end
            assert_eq!(cfg.graph.node_count(), 10);

            let mut start_neighbors = neighbors!(cfg, cfg.start);

            let enter = start_neighbors.next().unwrap();
            let mut enter_neighbors = neighbors!(cfg, enter);
            match *node_w!(cfg, enter) {
                Node::EnterScope => (),
                ref n @ _ => panic!("Expected to find Node::EnterScope. Found {:?}", n),
            }

            let mut merge = None;

            let branch_split = enter_neighbors.next().expect("Looking for BranchSplit");

            match *node_w!(cfg, branch_split) {
                Node::BranchSplit(..) => (),
                ref n @ _ => panic!("Expected BranchSplit node. Found {:?}", n),
            }

            // Check condition node
            let condition = branch_split;
            let condition_node = cfg.graph.node_weight(condition).unwrap();
            {
                if let Node::BranchSplit(..) = *condition_node {
                    let edges = cfg.graph.edges_directed(condition, Direction::Outgoing);
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
                                        Node::Block(_) => (),
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
        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        let mut universe = Universe::std();
        let fn_type = fn_type_cons(vec![expected_app(universe.int())], expected_app(universe.unit()));
        
        let fn_def = testfn_decl(&mut input).unwrap();
        let analysis_context = 
            generate_fn_analysis_data(&universe, 
                &universe.std_scope(), 
                &TypingContext::empty(),
                &fn_type,
                &fn_def
                ).unwrap();
        let cfg = CFG::generate(
                &mut universe, fn_def.body.clone(), &fn_type, &analysis_context,
            )
            .unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        {
            // start -> enter_scope
            //   branch_split(A)
            //      -[true]> {
            //          enter_scope ->
            //          local_var_decl ->
            //          exit_scope
            //      }
            //
            //      -[false]> {
            //          branch_split(C)
            //          -[true]> {
            //              scope_enter ->
            //              scope_exit
            //          }
            //
            //          -[false]> {
            //              scope_enter ->
            //              scope_exit
            //          }
            //
            //          branch_merge(C) ->
            //      }
            //    branch_merge(A) -> implicit_return -> exit_scope ->
            // end


            assert_eq!(cfg.graph.node_count(), 16);

            let mut start_neighbors = neighbors!(cfg, cfg.start);
            assert_eq!(start_neighbors.clone().count(), 1);

            let enter = start_neighbors.next().unwrap();
            let mut enter_neighbors = neighbors!(cfg, enter);
            match *node_w!(cfg, enter) {
                Node::EnterScope => (),
                ref n @ _ => panic!("Expected to find Node::Enter. Found {:?}", n),
            }

            let branch_split_A = enter_neighbors.next().unwrap();
            let _branch_split_neighbors_A = neighbors!(cfg, branch_split_A);
            match *node_w!(cfg, branch_split_A) {
                Node::BranchSplit(..) => (), // Success

                ref n @ _ => panic!("Expected a condition node. Found {:?}", n),
            }

            let branch_split_A_edges = edges!(cfg, branch_split_A);
            let mut branch_split_c = None;
            let mut branch_split_A_true = None;

            assert_eq!(branch_split_A_edges.clone().count(), 2);
            for edge in branch_split_A_edges {
                match *edge.weight() {
                    Edge::True => branch_split_A_true = Some(edge.target()),
                    Edge::False => branch_split_c = Some(edge.target()),

                    ref e @ _ => panic!("Expected true or false edge. Found {:?}", e),
                }
            }

            // branch split a TRUE branch
            let enter =
                branch_split_A_true.expect("Missing true edge connecting to variable declaration");
            let mut enter_neighbors = neighbors!(cfg, enter);
            match *node_w!(cfg, enter) {
                Node::EnterScope => (),
                ref n @ _ => panic!("Expected Node::EnterScope. Found {:?}", n),
            }

            let var_decl = enter_neighbors.next().unwrap();
            let mut var_decl_neighbors = neighbors!(cfg, var_decl);
            assert_eq!(var_decl_neighbors.clone().count(), 1);
            match *node_w!(cfg, var_decl) {
                Node::Block(_) => (),

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

            // condition b FALSE branch (branch_split_c)
            let branch_split_c = branch_split_c.expect("Missing false edge connecting to branch split C");
            
            let branch_split_c_edges = edges!(cfg, branch_split_c);
            let mut truth_target = None;
            let mut false_target = None;

            assert_eq!(branch_split_c_edges.clone().count(), 2);
            for edge in branch_split_c_edges {
                match *edge.weight() {
                    Edge::True => truth_target = Some(edge.target()),
                    Edge::False => false_target = Some(edge.target()),

                    ref e @ _ => panic!("Expected true or false edge. Found {:?}", e),
                }
            }

            {
                let enter =
                    truth_target.expect("Missing true edge connecting to empty block");
                let mut enter_neighbors = neighbors!(cfg, enter);
                match *node_w!(cfg, enter) {
                    Node::EnterScope => (),
                    ref n @ _ => panic!("Expected Node::EnterScope. Found {:?}", n),
                }

                let exit = enter_neighbors.next().unwrap();
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
            }

            let merge_c = {
                let enter =
                    false_target.expect("Missing true edge connecting to empty block");
                let mut enter_neighbors = neighbors!(cfg, enter);
                match *node_w!(cfg, enter) {
                    Node::EnterScope => (),
                    ref n @ _ => panic!("Expected Node::EnterScope. Found {:?}", n),
                }

                let exit = enter_neighbors.next().unwrap();
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

                merge
            };

            let mut merge_c_neighbors = neighbors!(cfg, merge_c);
            let merge_a = merge_c_neighbors.next().unwrap();
            let mut merge_a_neighbors = neighbors!(cfg, merge_a);
            match *node_w!(cfg, merge_a) {
                Node::BranchMerge(_) => (),
                ref n @ _ => panic!("Expected BranchMerge. Found {:?}", n),
            }

            let implicit_return = merge_a_neighbors.next().unwrap();
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
        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        let mut universe = Universe::std();
        let fn_type = fn_type_cons(vec![expected_app(universe.int())], expected_app(universe.unit()));
        
        let fn_def = testfn_decl(&mut input).unwrap();
        let analysis_context = 
            generate_fn_analysis_data(&universe, 
                &universe.std_scope(), 
                &TypingContext::empty(),
                &fn_type,
                &fn_def
                ).unwrap();

        let cfg = CFG::generate(
                &mut universe, fn_def.body.clone(), &fn_type, &analysis_context
            )
            .unwrap();

        println!("{:?}", Dot::with_config(&cfg.graph, &[Config::EdgeNoLabel]));

        // start -> enter_scope -> loop_head(A)
        //       -[true]> loop_foot(A)
        //       -[false]> loop_foot(A)
        // loop_foot(A) -> implicit_return -> exit_scope -> end
        // loop_head(A) << loop_foot(A)
        //

        assert_eq!(cfg.graph.node_count(), 7);

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
            Node::LoopHead(ref loop_data, _) => loop_id = loop_data.loop_id,
            ref n @ _ => panic!("Expected to find Node::LoopHead. Found {:?}", n),
        }

        let head_neighbors = neighbors!(cfg, loop_head);
        assert_eq!(head_neighbors.clone().count(), 2);
        
        let head_edges = edges!(cfg, loop_head);
        assert_eq!(head_edges.clone().count(), 2);

        let mut truth_target = None;
        let mut false_target = None;
        for edge in head_edges {
            match *edge.weight() {
                Edge::True => truth_target = Some(edge.target()),
                Edge::False => false_target = Some(edge.target()),

                ref e @ _ => panic!("Expected true or false edge. Found {:?}", e),
            }
        }

        let truth_target = truth_target.unwrap();
        let false_target = false_target.unwrap();
        match *node_w!(cfg, truth_target) {
            Node::LoopFoot(ref loop_data) => assert_eq!(loop_data.loop_id, loop_id),
            ref n @ _ => panic!("Expected to find Node::LoopFoot. Found {:?}", n),
        }

        let mut foot_neighbors = neighbors!(cfg, truth_target);

        assert_eq!(truth_target, false_target);

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
