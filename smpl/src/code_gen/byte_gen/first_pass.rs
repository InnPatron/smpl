use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::analysis::*;
use crate::analysis::metadata::*;

use super::byte_code::*;

type FirstPassError = ();

enum PartialInstruction {
    Instruction(Instruction),
    LoopBody(LoopId),
    BranchBodies(BranchingId),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum State {
    Branching(BranchingId),
    Loop(LoopId),
    Normal,
}

struct LoopFrame {
    condition: Option<Vec<PartialInstruction>>,
    body: Option<Vec<PartialInstruction>>,
}

impl LoopFrame {
    fn new() -> LoopFrame {
        LoopFrame {
            condition: None,
            body: None
        }
    }

    fn set_condition(&mut self, condition: Vec<PartialInstruction>) {
        if self.condition.is_some() {
            panic!("Attempting to double set the loop condition.");
        } 

        self.condition = Some(condition);
    }

    fn set_body(&mut self, body: Vec<PartialInstruction>) {
        if self.body.is_some() {
            panic!("Attempting to double set the loop body.");
        } 

        self.body = Some(body);
    }
}

struct BranchFrame {
    conditions: Vec<(Vec<PartialInstruction>, Option<Vec<PartialInstruction>>)>,
    default: Option<Vec<PartialInstruction>>
}

impl BranchFrame {
    fn new() -> BranchFrame {
        BranchFrame {
            conditions: Vec::new(),
            default: None
        }
    }

    fn push_condition(&mut self, condition: Vec<PartialInstruction>) {
        self.conditions.push((condition, None));
    }

    fn push_true_body(&mut self, true_body: Vec<PartialInstruction>) {
        let (condition, body) = self.conditions.last_mut().unwrap();

        if body.is_some() {
            panic!("Attempting to double set the true body");
        } 

        *body = Some(true_body);
    }

    fn set_default(&mut self, default_body: Vec<PartialInstruction>) {
        if self.default.is_some() {
            panic!("Attempting to double set the default branch.");
        }

        self.default = Some(default_body);
    }
}

struct FirstPass<'a> {
    cfg: &'a CFG,
    instructions: Vec<PartialInstruction>,
    loops: HashMap<LoopId, LoopFrame>,
    branches: HashMap<BranchingId, BranchFrame>,
    frames: Vec<Vec<PartialInstruction>>,
    states: Vec<State>,
}

impl<'a> FirstPass<'a> {
    fn current_index(&self) -> usize {
        self.instructions.len()
    }

    fn current_state(&self) -> State {
        self.states
            .last()
            .map(|state| state.clone())
            .expect("Expected a state. Found none.")
    }

    fn push_state(&mut self, state: State) { 
        self.states.push(state);
    }

    fn pop_state(&mut self) -> State {
        self.states.pop().unwrap()
    }

    fn new_branch_frame(&mut self, id: BranchingId) {
        if self.branches.insert(id, BranchFrame::new()).is_some() {
            panic!("Attempting to override branch frames.");
        }
    }

    fn new_loop_frame(&mut self, id: LoopId) {
        if self.loops.insert(id, LoopFrame::new()).is_some() {
            panic!("Attempting to override loop frames.");
        }
    }

    fn get_branch_frame_mut(&mut self, id: BranchingId) -> &mut BranchFrame {
        self.branches.get_mut(&id).expect("Expected a branch frame. Found none.")
    }

    fn get_loop_frame_mut(&mut self, id: LoopId) -> &mut LoopFrame {
        self.loops.get_mut(&id).expect("Expected a loop frame. Found none.")
    }

    fn push_to_current_frame(&mut self, instr: PartialInstruction) {
        let current_frame = self.frames.last_mut().expect("Expected a frame. Found none.");
        current_frame.push(instr);
    }

    fn new_frame(&mut self) {
        self.frames.push(Vec::new());
    }

    fn consume_current_frame(&mut self) -> Vec<PartialInstruction> {
        let current_frame = self.frames.pop().expect("Expected a frame. Found none.");
        self.frames.push(Vec::new());

        current_frame
    }
}

impl<'a> BlockyPassenger<FirstPassError> for FirstPass<'a> {
    fn start(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn end(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn loop_head(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn loop_foot(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn cont(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn br(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn enter_scope(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn exit_scope(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn block(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    } 

    fn ret(&mut self, id: NodeIndex, rdata: &ReturnData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn loop_condition(&mut self, id: NodeIndex, e: &ExprData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn loop_start_true_path(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn loop_end_true_path(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn branch_split(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn branch_merge(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn branch_condition(&mut self, id: NodeIndex, e: &ExprData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn branch_start_true_path(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn branch_start_false_path(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn branch_end_true_path(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn branch_end_false_path(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), FirstPassError> {
        Ok(())
    }
}
