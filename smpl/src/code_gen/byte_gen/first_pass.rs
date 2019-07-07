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

#[derive(Debug, Clone, Copy)]
enum State {
    Branching(BranchingId),
    Loop(LoopId),
    Normal,
}

struct BranchFrame {
    conditions: Vec<(Vec<PartialInstruction>, Vec<PartialInstruction>)>,
    default: Vec<PartialInstruction>
}

struct FirstPass<'a> {
    cfg: &'a CFG,
    instructions: Vec<PartialInstruction>,
    loops: HashMap<LoopId, Vec<PartialInstruction>>,
    branches: HashMap<BranchingId, BranchFrame>,
    current_frame: Vec<PartialInstruction>,
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
