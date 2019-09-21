use petgraph::graph::NodeIndex;

use super::control_data::*;
use super::control_flow::*;

pub trait Passenger<E> {
    fn start(&mut self, id: NodeIndex) -> Result<(), E>;
    fn end(&mut self, id: NodeIndex) -> Result<(), E>;
    fn loop_head(&mut self, id: NodeIndex, ld: &LoopData, expr: &ExprData) -> Result<(), E>;
    fn loop_foot(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), E>;
    fn cont(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), E>;
    fn br(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), E>;
    fn enter_scope(&mut self, id: NodeIndex) -> Result<(), E>;
    fn exit_scope(&mut self, id: NodeIndex) -> Result<(), E>;
    fn local_var_decl(&mut self, id: NodeIndex, decl: &LocalVarDeclData) -> Result<(), E>;
    fn assignment(&mut self, id: NodeIndex, assign: &AssignmentData) -> Result<(), E>;
    fn expr(&mut self, id: NodeIndex, expr: &ExprData) -> Result<(), E>;
    fn ret(&mut self, id: NodeIndex, rdata: &ReturnData) -> Result<(), E>;

    fn loop_start_true_path(&mut self, id: NodeIndex) -> Result<(), E>;
    fn loop_end_true_path(&mut self, id: NodeIndex) -> Result<(), E>;

    fn branch_split(&mut self, id: NodeIndex, b: &BranchingData, e: &ExprData) -> Result<(), E>;
    fn branch_merge(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), E>;
    fn branch_start_true_path(&mut self, id: NodeIndex) -> Result<(), E>;
    fn branch_start_false_path(&mut self, id: NodeIndex) -> Result<(), E>;
    fn branch_end_true_path(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), E>;
    fn branch_end_false_path(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), E>;
}

pub struct Traverser<'a, 'b, E: 'b> {
    graph: &'a CFG,
    passenger: &'b mut Passenger<E>,
    node_count: usize,
}

impl<'a, 'b, E> Traverser<'a, 'b, E> {
    pub fn new(graph: &'a CFG, passenger: &'b mut Passenger<E>) -> Traverser<'a, 'b, E> {
        Traverser {
            graph: graph,
            passenger: passenger,
            node_count: graph.graph().node_count(),
        }
    }

    pub fn traverse(mut self) -> Result<(), E> {
        let mut current = Some(self.graph.start());

        // Traverser::visit_node should be called AT MAX the number of nodes in the graph
        for _ in 0..self.node_count {
            match current {
                Some(to_visit) => current = self.visit_node(to_visit)?,
                None => break,
            }
        }

        if current.is_some() {
            panic!("Graph traversal error. Node::End should have returned None. If Node::End was reached, this panic should not be triggered.")
        }

        Ok(())
    }

    fn visit_node(&mut self, current: NodeIndex) -> Result<Option<NodeIndex>, E> {
        match *self.graph.node_weight(current) {
            Node::End => {
                self.passenger.end(current)?;
                Ok(None)
            }

            Node::Start => {
                self.passenger.start(current)?;
                Ok(Some(self.graph.next(current)))
            }

            Node::BranchSplit(ref branch_data, ref expr_data) => {
                self.passenger.branch_split(current, branch_data, expr_data)?;

                let (true_path, false_path) = self.graph.after_conditional(current);

                self.passenger.branch_start_true_path(true_path)?;

                let mut merge = None;

                // True path
                let mut current_node = true_path;
                for _ in 0..self.node_count {
                    match *self.graph.node_weight(current_node) {
                        Node::BranchMerge(ref branch_data) => {
                            self.passenger
                                .branch_end_true_path(current_node, branch_data)?;
                            merge = Some(current_node);
                            break;
                        }

                        _ => (),
                    }

                    match self.visit_node(current_node)? {
                        Some(next) => current_node = next,
                        None => break,
                    }
                }

                if merge.is_none() {
                    panic!("Traversed entire graph and did not find Condition::BranchMerge");
                }

                self.passenger.branch_start_false_path(false_path)?;

                // False path
                let mut current_node = false_path;
                let mut merge = None;
                for _ in 0..self.node_count {
                    match *self.graph.node_weight(current_node) {
                        Node::BranchMerge(ref branch_data) => {
                            self.passenger
                                .branch_end_false_path(current_node, branch_data)?;
                            self.passenger.branch_merge(current_node, branch_data)?;
                            merge = Some(current_node);
                            break;
                        }

                        _ => (),
                    }

                    match self.visit_node(current_node)? {
                        Some(next) => current_node = next,
                        None => panic!(),
                    }
                }


                if merge.is_none() {
                    panic!("Traversed entire graph and did not find Condition::BranchMerge");
                }

                Ok(Some(self.graph.next(merge.unwrap())))
            }

            Node::BranchMerge(ref branch_data) => {
                unreachable!();
            }

            Node::LoopHead(ref branch_data, ref expr_data) => {
                self.passenger.loop_head(current, branch_data, expr_data)?;
                let (true_path, false_path) = self.graph.after_conditional(current);
                self.passenger.loop_start_true_path(true_path)?;

                let mut current_node = true_path;
                let mut found_foot = false;
                for _ in 0..self.node_count {
                    match *self.graph.node_weight(current_node) {
                        Node::LoopFoot(ref loop_data) => {
                            self.passenger.loop_end_true_path(current_node)?;
                            self.passenger.loop_foot(current_node, loop_data);
                            found_foot = true;
                            break;
                        }

                        _ => (),
                    }

                    match self.visit_node(current_node)? {
                        Some(next) => current_node = next,
                        None => return Ok(None),
                    }
                }

                if found_foot == false {
                    panic!(
                        "Traversed the rest of the graph but did not find a Node::LoopFoot."
                    );
                }

                match *self.graph.node_weight(false_path) {
                    Node::LoopFoot(_) => (),
                    ref n @ _ => println!("Loop condition should be connected to Node::LoopFoot along the false path. Found {:?}.", n),
                }

                Ok(Some(self.graph.after_loop_foot(false_path)))
            }

            Node::LoopFoot(ref data) => {
                unreachable!();
            }

            Node::Continue(ref data) => {
                self.passenger.cont(current, data)?;
                Ok(Some(self.graph.after_continue(current)))
            }

            Node::Break(ref data) => {
                self.passenger.br(current, data)?;
                Ok(Some(self.graph.after_break(current)))
            }

            Node::EnterScope => {
                self.passenger.enter_scope(current)?;
                Ok(Some(self.graph.next(current)))
            }

            Node::ExitScope => {
                self.passenger.exit_scope(current)?;
                Ok(Some(self.graph.next(current)))
            }

            Node::Block(ref basic_block) => {
                for n in basic_block.graph() {
                    match *n {
                        BlockNode::LocalVarDecl(ref decl) => {
                            self.passenger.local_var_decl(current, decl)?;
                        },

                        BlockNode::Assignment(ref assign) => {
                            self.passenger.assignment(current, assign)?;
                        },

                        BlockNode::Expr(ref expr) => {
                            self.passenger.expr(current, expr)?;
                        }
                    }
                }

                Ok(Some(self.graph.next(current)))
            }

            Node::Return(ref rdata) => {
                self.passenger.ret(current, rdata)?;
                Ok(Some(self.graph.after_return(current)))
            }
        }
    }
}
