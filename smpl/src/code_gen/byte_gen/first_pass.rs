use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::analysis::*;

use super::byte_code::*;
use super::byte_expr;

pub(super) type FirstPassError = ();

pub(super) enum PartialInstruction {
    Instruction(Instruction),
    Loop(LoopId),
    Branch(BranchingId),
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

pub(super) struct LoopFrame {
    result_location: Option<Arg>,
    condition: Option<Vec<PartialInstruction>>,
    body: Option<Vec<PartialInstruction>>,
}

impl LoopFrame {
    fn new() -> LoopFrame {
        LoopFrame {
            result_location: None,
            condition: None,
            body: None
        }
    }

    fn set_result_location(&mut self, loc: Arg) {
        if self.result_location.is_some() {
            panic!("Attempting to double set the result location");
        }

        self.result_location = Some(loc);
    }

    fn set_condition<I: Iterator<Item=PartialInstruction>>(&mut self, condition: I) {
        if self.condition.is_some() {
            panic!("Attempting to double set the loop condition.");
        } 

        self.condition = Some(condition.collect());
    }

    fn set_body<I: Iterator<Item=PartialInstruction>>(&mut self, body: I) {
        if self.body.is_some() {
            panic!("Attempting to double set the loop body.");
        } 

        self.body = Some(body.collect());
    }

    pub(super) fn get_condition(&self) -> &[PartialInstruction] {
        self.condition.as_ref().expect("Condition none")
    }

    pub(super) fn get_body(&self) -> &[PartialInstruction] {
        self.body.as_ref().expect("Branch none")
    }

    pub(super) fn get_result_location(&self) -> &Arg {
        self.result_location.as_ref().expect("Result location none")
    }
}

pub(super) struct BranchFrame {
    result_location: Option<Arg>,
    condition: Option<Vec<PartialInstruction>>,
    true_branch: Option<Vec<PartialInstruction>>,
    false_branch: Option<Vec<PartialInstruction>>,
}

impl BranchFrame {
    fn new() -> BranchFrame {
        BranchFrame {
            result_location: None,
            condition: None,
            true_branch: None,
            false_branch: None,
        }
    }

    fn set_result_location(&mut self, loc: Arg) {
        if self.result_location.is_some() {
            panic!("Attempting to double set the result location");
        }

        self.result_location = Some(loc);
    }

    fn set_condition<I: Iterator<Item=PartialInstruction>>(&mut self, condition: I) {
        if self.condition.is_some() {
            panic!("Attempting to double set the condition.");
        } 

        self.condition = Some(condition.collect());
    }

    fn set_true_branch<I: Iterator<Item=PartialInstruction>>(&mut self, body: I) {
        if self.true_branch.is_some() {
            panic!("Attempting to double set the true body.");
        } 

        self.true_branch = Some(body.collect());
    }

    fn set_false_branch<I: Iterator<Item=PartialInstruction>>(&mut self, body: I) {
        if self.false_branch.is_some() {
            panic!("Attempting to double set the false body.");
        } 

        self.false_branch = Some(body.collect());
    }

    pub(super) fn get_condition(&self) -> &[PartialInstruction] {
        self.condition.as_ref().expect("Condition none")
    }

    pub(super) fn get_true_branch(&self) -> &[PartialInstruction] {
        self.true_branch.as_ref().expect("True branch none")
    }

    pub(super) fn get_false_branch(&self) -> &[PartialInstruction] {
        self.false_branch.as_ref().expect("False branch none")
    }

    pub(super) fn get_result_location(&self) -> &Arg {
        self.result_location.as_ref().expect("Result location none")
    }
}

pub(super) struct FirstPass<'a> {
    cfg: &'a CFG,
    instructions: Option<Vec<PartialInstruction>>,
    loops: HashMap<LoopId, LoopFrame>,
    branches: HashMap<BranchingId, BranchFrame>,
    frames: Vec<Vec<PartialInstruction>>,
    states: Vec<State>,
}

impl<'a> From<FirstPass<'a>> for super::second_pass::SecondPass {
    fn from(fp: FirstPass) -> super::second_pass::SecondPass {
        super::second_pass::SecondPass::new(
            fp.instructions.expect("FirstPass.instructions should be set to some"),
            fp.loops,
            fp.branches
        )
    }
}

impl<'a> FirstPass<'a> {

    pub(super) fn new(cfg: &CFG) -> FirstPass {
        FirstPass {
            cfg: cfg,
            instructions: None,
            loops: HashMap::new(),
            branches: HashMap::new(),
            frames: Vec::new(),
            states: Vec::new(),
        }
    }

    fn current_frame_mut(&mut self) -> &mut Vec<PartialInstruction> {
        self.frames.last_mut().expect("Expected a frame. Found none.")
    }

    fn extend_current_frame<I, Item>(&mut self, i: I)
    where I: Iterator<Item=Item>,
          Item: Into<PartialInstruction> {

        self.current_frame_mut().extend(i.map(|item| item.into()));
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

    fn peek_state(&self) -> State {
        self.states.last()
            .map(|s| s.clone())
            .unwrap()
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

        // This frame represents the entire function
        self.new_frame();

        Ok(())
    }

    fn end(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        
        // Sanity check. Should always end on the "Start" state
        let old_state = self.pop_state();
        assert_eq!(old_state, State::Start);

        // Only one frame remaining (the one pushed in start())
        // This should be the frame for the entire CFG
        let function_body = self.pop_current_frame();

        if self.instructions.is_none() {
            self.instructions = Some(function_body)
        } else {
            panic!("FirstPass.instructions should only be set in end()");
        }

        Ok(())
    }

    fn loop_head(&mut self, id: NodeIndex, ld: &LoopData, condition: &ExprData) -> Result<(), FirstPassError> {
        // Mark in the current frame that there should be a loop with LoopId(#) inserted here
        self.push_to_current_frame(PartialInstruction::Loop(ld.loop_id));

        // Make a new frame for the loop body
        self.new_frame();

        // Setup loop frame
        self.new_loop_frame(ld.loop_id);

        let frame = self.get_loop_frame_mut(ld.loop_id);
        let condition_instructions = byte_expr::translate_expr(&condition.expr)
            .into_iter()
            .map(|instr| PartialInstruction::Instruction(instr));
        frame.set_condition(condition_instructions);

        let result_location = Arg::Location(Location::Tmp(byte_expr::tmp_id(condition.expr.last())));
        frame.set_result_location(result_location);
        

        // Change the current state
        self.push_state(State::Loop(ld.loop_id));

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
        loop_frame.set_body(loop_body.into_iter());

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

        let init_instructions = byte_expr::translate_expr(decl.decl.init_expr());
        self.extend_current_frame(init_instructions.into_iter());

        let key = decl.decl.var_name().as_str().to_owned();
        let store_location = Location::Namespace(key);
        let value = Arg::Location(Location::Tmp(byte_expr::tmp_id(decl.decl.init_expr().last())));
        self.push_to_current_frame(Instruction::Store(store_location, value));

        Ok(())
    }

    fn assignment(&mut self, id: NodeIndex, assign: &AssignmentData) -> Result<(), FirstPassError> {
        let assignment = &assign.assignment;
        let assignee = assignment.assignee();

        // Generate lhs tmps
        let access_tmps = byte_expr::translate_expr(assignment.access());

        // Generate rhs tmps
        let value_tmps = byte_expr::translate_expr(assignment.value());
        let value = Arg::Location(Location::Tmp(byte_expr::tmp_id(assignment.value().last())));

        // Create location to store rhs value
        let internal_path = assignee.path();
        let assign_root_var = 
            internal_path.root_name().data().as_str().to_owned();
        let assign_root_indexing_expr = internal_path.root_indexing_expr()
            .map(|tmp_id| byte_expr::tmp_id(tmp_id));
        let path: Vec<_> = internal_path
            .path()
            .iter()
            .map(|path_segment| {
                match path_segment {
                    PathSegment::Ident(ref field) => {
                        super::byte_code::FieldAccess::Field(field.name().to_string())
                    }

                    PathSegment::Indexing(ref field, ref index_tmp) => {
                        super::byte_code::FieldAccess::FieldIndex {
                            field: field.name().to_string(),
                            index_tmp: byte_expr::tmp_id(*index_tmp),
                        }
                    }
                }
            }).collect();

        // If assignment has field accesses or indexing, location is Location::Compound
        let assign_location = if path.len() > 0 || assign_root_indexing_expr.is_some() {
            Location::Compound {
                root: assign_root_var,
                root_index: assign_root_indexing_expr,
                path: path,
            }
        } else {
            Location::Namespace(assign_root_var)
        };

        // Emit access expressions before evaluating the assignment
        // Emit storage instruction last
        self.extend_current_frame(access_tmps.into_iter());
        self.extend_current_frame(value_tmps.into_iter());
        self.push_to_current_frame(Instruction::Store(assign_location, value));
        Ok(())
    }

    fn expr(&mut self, id: NodeIndex, expr: &ExprData) -> Result<(), FirstPassError> {
        let expr_instructions = byte_expr::translate_expr(&expr.expr);
        self.extend_current_frame(expr_instructions.into_iter());
        Ok(())
    }

    fn ret(&mut self, id: NodeIndex, rdata: &ReturnData) -> Result<(), FirstPassError> {
        if let Some(ref return_expr) = rdata.expr {
            // Return expression

            // Append return expression instructions to current frame
            let return_instructions = byte_expr::translate_expr(return_expr);
            self.extend_current_frame(return_instructions.into_iter());

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

    fn loop_start_true_path(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn loop_end_true_path(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        Ok(())
    }

    fn branch_split(&mut self, id: NodeIndex, b: &BranchingData, condition: &ExprData) 
        -> Result<(), FirstPassError> {
        // Mark in the current frame that there should be a branch with Branch(#) inserted here
        self.push_to_current_frame(PartialInstruction::Branch(b.branch_id));

        // Setup branch frame
        self.new_branch_frame(b.branch_id);

        let frame = self.get_branch_frame_mut(b.branch_id);
        let condition_instructions = byte_expr::translate_expr(&condition.expr)
            .into_iter()
            .map(|instr| PartialInstruction::Instruction(instr));
        frame.set_condition(condition_instructions);

        let result_location = Arg::Location(Location::Tmp(byte_expr::tmp_id(condition.expr.last())));
        frame.set_result_location(result_location);


        // Change the current state
        self.push_state(State::Branching(b.branch_id));

        Ok(())
    }

    fn branch_merge(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), FirstPassError> {
        // Exiting a branch

        // Change the state to old state
        let old_state = self.pop_state();

        // Sanity check to make sure the generated code is for the correct loop
        assert_eq!(old_state, State::Branching(b.branch_id));

        // Branch frame should already be set in branch_end_true_path() and branch_end_false_path()
        Ok(())
    }

    fn branch_start_true_path(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        // Make a new frame for the true body
        self.new_frame();

        Ok(())
    }

    fn branch_start_false_path(&mut self, id: NodeIndex) -> Result<(), FirstPassError> {
        // Make a new frame for the false body
        self.new_frame();
        
        Ok(())
    }

    fn branch_end_true_path(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), FirstPassError> {
        // Pop the current frame as the body of the true path
        let true_instructions = self.pop_current_frame();

        let state = self.peek_state();
        let branch_id = match state {
            State::Branching(id) => id,

            state => panic!("branch_end_true_path() encountered invalid state: {:?}", self.states),
        };
        // Set the true path in the frame
        let frame = self.get_branch_frame_mut(branch_id);
        frame.set_true_branch(true_instructions.into_iter());

        Ok(())
    }

    fn branch_end_false_path(&mut self, id: NodeIndex, b: &BranchingData) -> Result<(), FirstPassError> {
        // Pop the current frame as the body of the false path
        let false_instructions = self.pop_current_frame();

        let state = self.peek_state();
        let branch_id = match state {
            State::Branching(id) => id,

            state => panic!("branch_end_true_path() encountered invalid state: {:?}", self.states),
        };

        // Set the false path in the frame
        let frame = self.get_branch_frame_mut(branch_id);
        frame.set_false_branch(false_instructions.into_iter());
        Ok(())
    }
}
