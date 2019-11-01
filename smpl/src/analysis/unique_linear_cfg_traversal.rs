use petgraph::graph::NodeIndex;

use super::control_data::*;
use super::control_flow::*;

pub trait UniquePassenger<E> {
    fn start(&mut self, id: NodeIndex) -> Result<(), E>;
    fn end(&mut self, id: NodeIndex) -> Result<(), E>;
    fn loop_head(
        &mut self,
        id: NodeIndex,
        ld: &mut LoopData,
        expr: &mut ExprData,
    ) -> Result<(), E>;
    fn loop_foot(&mut self, id: NodeIndex, ld: &mut LoopData) -> Result<(), E>;
    fn cont(&mut self, id: NodeIndex, ld: &mut LoopData) -> Result<(), E>;
    fn br(&mut self, id: NodeIndex, ld: &mut LoopData) -> Result<(), E>;
    fn enter_scope(&mut self, id: NodeIndex) -> Result<(), E>;
    fn exit_scope(&mut self, id: NodeIndex) -> Result<(), E>;
    fn local_var_decl(
        &mut self,
        id: NodeIndex,
        decl: &mut LocalVarDeclData,
    ) -> Result<(), E>;
    fn assignment(
        &mut self,
        id: NodeIndex,
        assign: &mut AssignmentData,
    ) -> Result<(), E>;
    fn expr(&mut self, id: NodeIndex, expr: &mut ExprData) -> Result<(), E>;
    fn ret(&mut self, id: NodeIndex, rdata: &mut ReturnData) -> Result<(), E>;

    fn loop_start_true_path(&mut self, id: NodeIndex) -> Result<(), E>;
    fn loop_end_true_path(&mut self, id: NodeIndex) -> Result<(), E>;

    fn branch_split(
        &mut self,
        id: NodeIndex,
        b: &mut BranchingData,
        e: &mut ExprData,
    ) -> Result<(), E>;
    fn branch_merge(
        &mut self,
        id: NodeIndex,
        b: &mut BranchingData,
    ) -> Result<(), E>;
    fn branch_start_true_path(&mut self, id: NodeIndex) -> Result<(), E>;
    fn branch_start_false_path(&mut self, id: NodeIndex) -> Result<(), E>;
    fn branch_end_true_path(
        &mut self,
        id: NodeIndex,
        b: &mut BranchingData,
    ) -> Result<(), E>;
    fn branch_end_false_path(
        &mut self,
        id: NodeIndex,
        b: &mut BranchingData,
    ) -> Result<(), E>;
}

pub fn traverse<E>(
    graph: &mut CFG,
    passenger: &mut dyn UniquePassenger<E>,
) -> Result<(), E> {
    let mut current = Some(graph.start());
    let node_count = graph.graph().node_count();

    // Traverser::visit_node should be called AT MAX the number of nodes in the graph
    for _ in 0..node_count {
        match current {
            Some(to_visit) => current = visit_node(graph, to_visit, passenger)?,
            None => break,
        }
    }

    if current.is_some() {
        panic!("Graph traversal error. Node::End should have returned None. If Node::End was reached, this panic should not be triggered.")
    }

    Ok(())
}

fn visit_node<E>(
    graph: &mut CFG,
    current: NodeIndex,
    passenger: &mut dyn UniquePassenger<E>,
) -> Result<Option<NodeIndex>, E> {
    let node_count = graph.graph().node_count();
    match graph.node_weight_mut(current) {
        Node::End => {
            passenger.end(current)?;
            Ok(None)
        }

        Node::Start => {
            passenger.start(current)?;
            Ok(Some(graph.next(current)))
        }

        Node::BranchSplit(ref mut branch_data, ref mut expr_data) => {
            passenger.branch_split(current, branch_data, expr_data)?;

            let (true_path, false_path) = graph.after_conditional(current);

            passenger.branch_start_true_path(true_path)?;

            let mut merge = None;

            // True path
            let mut current_node = true_path;
            for _ in 0..node_count {
                match graph.node_weight_mut(current_node) {
                    Node::BranchMerge(ref mut branch_data) => {
                        passenger
                            .branch_end_true_path(current_node, branch_data)?;
                        merge = Some(current_node);
                        break;
                    }

                    _ => (),
                }

                match visit_node(graph, current_node, passenger)? {
                    Some(next) => current_node = next,
                    None => break,
                }
            }

            if merge.is_none() {
                panic!("Traversed entire graph and did not find Condition::BranchMerge");
            }

            passenger.branch_start_false_path(false_path)?;

            // False path
            let mut current_node = false_path;
            let mut merge = None;
            for _ in 0..node_count {
                match graph.node_weight_mut(current_node) {
                    Node::BranchMerge(ref mut branch_data) => {
                        passenger
                            .branch_end_false_path(current_node, branch_data)?;
                        passenger.branch_merge(current_node, branch_data)?;
                        merge = Some(current_node);
                        break;
                    }

                    _ => (),
                }

                match visit_node(graph, current_node, passenger)? {
                    Some(next) => current_node = next,
                    None => panic!(),
                }
            }

            if merge.is_none() {
                panic!("Traversed entire graph and did not find Condition::BranchMerge");
            }

            Ok(Some(graph.next(merge.unwrap())))
        }

        Node::BranchMerge(ref mut _branch_data) => {
            unreachable!();
        }

        Node::LoopHead(ref mut branch_data, ref mut expr_data) => {
            passenger.loop_head(current, branch_data, expr_data)?;
            let (true_path, false_path) = graph.after_conditional(current);
            passenger.loop_start_true_path(true_path)?;

            let mut current_node = true_path;
            let mut found_foot = false;
            for _ in 0..node_count {
                match graph.node_weight_mut(current_node) {
                    Node::LoopFoot(ref mut loop_data) => {
                        passenger.loop_end_true_path(current_node)?;
                        passenger.loop_foot(current_node, loop_data)?;
                        found_foot = true;
                        break;
                    }

                    _ => (),
                }

                match visit_node(graph, current_node, passenger)? {
                    Some(next) => current_node = next,
                    None => return Ok(None),
                }
            }

            if found_foot == false {
                panic!(
                    "Traversed the rest of the graph but did not find a Node::LoopFoot."
                );
            }

            match graph.node_weight_mut(false_path) {
                Node::LoopFoot(_) => (),
                ref mut n @ _ => println!("Loop condition should be connected to Node::LoopFoot along the false path. Found {:?}.", n),
            }

            Ok(Some(graph.after_loop_foot(false_path)))
        }

        Node::LoopFoot(ref mut _data) => {
            unreachable!();
        }

        Node::Continue(ref mut data) => {
            passenger.cont(current, data)?;
            Ok(Some(graph.after_continue(current)))
        }

        Node::Break(ref mut data) => {
            passenger.br(current, data)?;
            Ok(Some(graph.after_break(current)))
        }

        Node::EnterScope => {
            passenger.enter_scope(current)?;
            Ok(Some(graph.next(current)))
        }

        Node::ExitScope => {
            passenger.exit_scope(current)?;
            Ok(Some(graph.next(current)))
        }

        Node::Block(ref mut basic_block) => {
            for n in basic_block.graph_mut() {
                match *n {
                    BlockNode::LocalVarDecl(ref mut decl) => {
                        passenger.local_var_decl(current, decl)?;
                    }

                    BlockNode::Assignment(ref mut assign) => {
                        passenger.assignment(current, assign)?;
                    }

                    BlockNode::Expr(ref mut expr) => {
                        passenger.expr(current, expr)?;
                    }
                }
            }

            Ok(Some(graph.next(current)))
        }

        Node::Return(ref mut rdata) => {
            passenger.ret(current, rdata)?;
            Ok(Some(graph.after_return(current)))
        }
    }
}
