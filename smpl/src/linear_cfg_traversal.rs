use petgraph::graph::NodeIndex;
use petgraph::Direction;
use petgraph::visit::EdgeRef;

use control_flow::*;
use typed_ast::*;

pub trait Passenger<E> {
    fn start(&mut self, id: NodeIndex) -> Result<(), E>;
    fn end(&mut self, id: NodeIndex) -> Result<(), E>;
    fn branch_merge(&mut self, id: NodeIndex) -> Result<(), E>;
    fn loop_head(&mut self, id: NodeIndex) -> Result<(), E>;
    fn loop_foot(&mut self, id: NodeIndex) -> Result<(), E>;
    fn cont(&mut self, id: NodeIndex) -> Result<(), E>;
    fn br(&mut self, id: NodeIndex) -> Result<(), E>;
    fn enter_scope(&mut self, id: NodeIndex) -> Result<(), E>;
    fn exit_scope(&mut self, id: NodeIndex) -> Result<(), E>; 
    fn local_var_decl(&mut self, id: NodeIndex, decl: &LocalVarDecl) -> Result<(), E>;
    fn assignment(&mut self, id: NodeIndex, assign: &Assignment) -> Result<(), E>;
    fn expr(&mut self, id: NodeIndex, expr: &Expr) -> Result<(), E>;
    fn ret(&mut self, id: NodeIndex, expr: Option<&Expr>) -> Result<(), E>;

    fn loop_condition(&mut self, id: NodeIndex, e: &Expr) -> Result<(), E>;
    fn loop_start_true_path(&mut self) -> Result<(), E>;
    fn loop_end_true_path(&mut self) -> Result<(), E>;

    fn branch_condition(&mut self, id: NodeIndex, e: &Expr) -> Result<(), E>;
    fn branch_start_true_path(&mut self) -> Result<(), E>;
    fn branch_start_false_path(&mut self) -> Result<(), E>;
    fn branch_end_true_path(&mut self) -> Result<(), E>;
    fn branch_end_false_path(&mut self) -> Result<(), E>;
}

struct Traverser<'a, 'b, E: 'b> {
    graph: &'a CFG,
    passenger: &'b mut Passenger<E>,
    previous_is_loop_head: bool,
}

impl<'a, 'b, E> Traverser<'a, 'b, E> {

    pub fn traverse(&mut self) {
        let mut current = Some(self.graph.start());

        // Traverser::visit_node should be called AT MAX the number of nodes in the graph
        for _ in 0..self.graph.graph().node_count() {
            match current {
                Some(current) => current = self.visit_node(current),
                None => break,
            }
        }

        if current.is_some() {
            panic!("Graph traversal error. Node::End should have returned None. If Node::End was reached, this panic should not be triggered.")
        }
    }

    ///
    /// Convenience function to get the next node in a linear sequence. If the current node has
    /// multiple outgoing edge (such as Node::Condition, Node::Return, Node::Break, and
    /// Node::Continue) or none (Node::End), return an error.
    ///
    fn next(&self, current: NodeIndex) -> NodeIndex {
        let mut neighbors = self.graph.neighbors_out(current);

        if neighbors.clone().count() != 1 {
            panic!("CFG::next() only works when a Node has 1 neighbor");
        } else {
            neighbors.next().unwrap()
        }
    }

    fn after_condition(&self, id: NodeIndex) -> (NodeIndex, NodeIndex) {

        match *self.graph.node_weight(id) {
            Node::Condition(_) => (),
            _ => panic!("Should only be given a Node::Condition"),
        }

        let edges = self.graph.graph().edges_directed(id, Direction::Outgoing);
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

    fn after_break(&self, id: NodeIndex) -> NodeIndex {

        match *self.graph.node_weight(id) {
            Node::Break(_) => (),
            _ => panic!("Should only be given a Node::Break"),
        }

        let neighbors = self.graph.neighbors_out(id);
        let neighbor_count = neighbors.clone().count();
        
        if neighbor_count == 2 {
            let mut found_first = false;
            for n in neighbors {
                match *self.graph.node_weight(n) {
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

    fn after_continue(&self, id: NodeIndex) -> NodeIndex {

        match *self.graph.node_weight(id) {
            Node::Continue(_) => (),
            _ => panic!("Should only be given a Node::Continue"),
        }

        let mut neighbors = self.graph.neighbors_out(id);
        let neighbor_count = neighbors.clone().count();

        if neighbor_count == 2 {
            let mut found_first = false;
            for n in neighbors {
                match *self.graph.node_weight(n) {
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

    fn after_loop_foot(&self, id: NodeIndex) -> NodeIndex {

        let loop_id;
        match *self.graph.node_weight(id) {
            Node::LoopFoot(id) => loop_id = id,
            _ => panic!("Should only be given a Node::LoopFoot"),
        }

        let neighbors = self.graph.neighbors_out(id);
        let neighbor_count = neighbors.clone().count();
        
        if neighbor_count != 2 {
            panic!("Loop foot should always be pointing to LoopHead and the next Node. Need two directed neighbors, found {}", neighbor_count);
        }
         
        for n in neighbors {
            match *self.graph.node_weight(n) {
                Node::LoopHead(id) => {
                    if loop_id != id {
                        return n;
                    }
                }
                _ => return n,
            }
        }
        unreachable!();
    }

    fn visit_node(&mut self, current: NodeIndex) -> Result<Option<NodeIndex>, E> {
        match *self.graph.node_weight(current) {
            Node::End => {
                self.passenger.end(current)?;
                self.previous_is_loop_head = false;
                Ok(None)
            },

            Node::Start => {
                self.passenger.start(current)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.next(current)))
            }

            Node::BranchMerge => {
                self.passenger.branch_merge(current)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.next(current)))
            }

            Node::LoopHead(_) => {
                self.passenger.loop_head(current)?;
                self.previous_is_loop_head = true;
                Ok(Some(self.next(current)))
            }

            Node::LoopFoot(_) => {
                self.passenger.loop_foot(current)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.after_loop_foot(current)))
            }

            Node::Continue(_) => {
                self.passenger.cont(current)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.after_continue(current)))
            }

            Node::Break(_) => {
                self.passenger.br(current)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.after_break(current)))
            }

            Node::EnterScope => {
                self.passenger.enter_scope(current)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.next(current)))
            }

            Node::ExitScope => {
                self.passenger.exit_scope(current)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.next(current)))
            }

            Node::LocalVarDecl(ref decl) => {
                self.passenger.local_var_decl(current, decl)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.next(current)))
            }

            Node::Assignment(ref assign) => {
                self.passenger.assignment(current, assign)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.next(current)))
            }

            Node::Expr(ref expr) => {
                self.passenger.expr(current, expr)?;
                self.previous_is_loop_head = false;
                Ok(Some(self.next(current)))
            }

            Node::Return(ref ret_expr) => {
                self.passenger.ret(current, ret_expr.as_ref())?;
                self.previous_is_loop_head = false;
                Ok(Some(self.next(current)))
            }

            Node::Condition(ref condition) => {
                if self.previous_is_loop_head {
                    // Loop condition
                    self.previous_is_loop_head = false;
                    self.passenger.loop_condition(current, condition)?;
                    
                    let (true_path, false_path) = self.after_condition(current);
                    self.passenger.loop_start_true_path()?;

                    let mut current_node = true_path;
                    loop {
                        match *self.graph.node_weight(current_node) {
                            Node::LoopFoot(_) => {
                                self.passenger.loop_end_true_path()?;
                                break;
                            }

                            _ => (),
                        }

                        match self.visit_node(current_node)? {
                            Some(next) => current_node = next,
                            None => return Ok(None),
                        }
                    }

                    match *self.graph.node_weight(false_path) {
                        Node::LoopFoot(_) => (),
                        ref n @ _ => println!("Loop condition should be connected to Node::LoopFoot along the false path. Found {:?}.", n),
                    }

                    Ok(Some(false_path))
                } else {
                    // Branch condition
                    self.passenger.branch_condition(current, condition)?;

                    let (true_path, false_path) = self.after_condition(current);
                    
                    self.passenger.branch_start_true_path()?;

                    let mut merge = None;

                    // True path
                    let mut current_node = true_path;
                    loop {
                        match *self.graph.node_weight(current_node) {
                            Node::BranchMerge => {
                                self.passenger.branch_end_true_path()?;
                                merge = Some(current_node);
                                break;
                            }

                            _ => (),
                        }

                        match self.visit_node(current_node)? {
                            Some(next) => current_node = next,
                            None => return Ok(None),
                        }
                    }

                    self.passenger.branch_start_false_path()?;

                    // False path
                    let mut current_node = false_path;
                    loop {
                        match *self.graph.node_weight(current_node) {
                            Node::BranchMerge => {
                                self.passenger.branch_end_false_path()?;
                                merge = Some(current_node);
                                break;
                            }

                            _ => (),
                        }

                        match self.visit_node(current_node)? {
                            Some(next) => current_node = next,
                            None => return Ok(None),
                        }
                    }

                    Ok(Some(merge.unwrap()))
                }
            }
        }
    }
}
