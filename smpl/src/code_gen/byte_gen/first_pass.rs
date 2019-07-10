use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::analysis::*;
use crate::analysis::metadata::*;

use super::byte_code::*;
use super::byte_expr;

type FirstPassError = ();

enum PartialInstruction {
    Instruction(Instruction),
    LoopBody(LoopId),
    BranchBodies(BranchingId),
    Continue(LoopId),
    Break(LoopId),
}

impl From<Instruction> for PartialInstruction {
    fn from(instr: Instruction) -> PartialInstruction {
        PartialInstruction::Instruction(instr)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum State {
    Start,
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

    fn current_frame_mut(&mut self) -> &mut Vec<PartialInstruction> {
        self.frames.last_mut().expect("Expected a frame. Found none.")
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

    fn push_to_current_frame<T>(&mut self, instr: T) 
    where T: Into<PartialInstruction> {
        let current_frame = self.frames.last_mut().expect("Expected a frame. Found none.");
        current_frame.push(instr.into());
    }

    fn new_frame(&mut self) {
        self.frames.push(Vec::new());
    }

    fn pop_current_frame(&mut self) -> Vec<PartialInstruction> {
        let current_frame = self.frames.pop().expect("Expected a frame. Found none.");

        current_frame
    }
}

impl<'a> Passenger<FirstPassError> for FirstPass<'a> {
    fn start(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        self.push_state(State::Start);
        Ok(())
    }

    fn end(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        
        // Sanity check. Should always end on the "Start" state
        let old_state = self.pop_state();
        assert_eq!(old_state, State::Start);

        Ok(())
    }

    fn loop_head(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), FirstPassError> {
        // Mark in the current frame that there should be a loop with LoopId(#) inserted here
        self.push_to_current_frame(PartialInstruction::LoopBody(ld.loop_id));

        // Make a new frame for the loop body
        self.new_frame();

        // Change the current state
        self.push_state(State::Loop(ld.loop_id));

        // Setup loop frame
        self.new_loop_frame(ld.loop_id);

        Ok(())
    }

    fn loop_foot(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), FirstPassError> {
        // Exiting the loop

        // Change the state to old state
        let old_state = self.pop_state();

        // Sanity check to make sure the generated code is for the correct loop
        assert_eq!(old_state, State::Loop(ld.loop_id));

        // Set the loop frame's loop body to the current frame
        let loop_body = self.pop_current_frame();
        let loop_frame = self.get_loop_frame_mut(ld.loop_id);
        loop_frame.set_body(loop_body);

        Ok(())
    }

    fn cont(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), FirstPassError> {
        self.push_to_current_frame(PartialInstruction::Continue(ld.loop_id));
        Ok(())
    }

    fn br(&mut self, id: NodeIndex, ld: &LoopData) -> Result<(), FirstPassError> {
        self.push_to_current_frame(PartialInstruction::Break(ld.loop_id));
        Ok(())
    }

    fn enter_scope(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn exit_scope(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn local_var_decl(&mut self, id: NodeIndex, decl: &LocalVarDeclData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn assignment(&mut self, id: NodeIndex, assign: &AssignmentData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn expr(&mut self, id: NodeIndex, expr: &ExprData) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn ret(&mut self, id: NodeIndex, rdata: &ReturnData) -> Result<(), FirstPassError> {
        if let Some(ref return_expr) = rdata.expr {
            // Return expression

            // Append return expression instructions to current frame
            let return_instructions = byte_expr::translate_expr(return_expr);
            self.current_frame_mut().extend(
                return_instructions.into_iter().map(|instr| PartialInstruction::Instruction(instr))
            );

            // Append return instruction
            let return_value = byte_expr::tmp_id(return_expr.last());
            let return_value_location = Arg::Location(Location::Tmp(return_value));
            self.push_to_current_frame(Instruction::Return(Some(return_value_location)));

        } else {
            // No return expression
            self.push_to_current_frame(Instruction::Return(None));
        }
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
