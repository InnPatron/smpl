use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::analysis::*;
use crate::analysis::metadata::*;

use super::byte_code::*;

type FirstPassError = ();

enum PartialInstruction {
    Instruction(Instruction),
    UnresolvedLoopJump(LoopId),
    UnresolvedBranchJump(BranchingId),
}

struct FirstPass<'a> {
    cfg: &'a CFG,
    instructions: Vec<PartialInstruction>,
    loop_jump_heads: HashMap<LoopId, usize>,
    loop_jump_feet: HashMap<LoopId, usize>,
    branch_jump_merge: HashMap<BranchingId, usize>,
}

impl<'a> FirstPass<'a> {
    fn current_index(&self) -> usize {
        self.instructions.len()
    }
}

impl<'a> Passenger<FirstPassError> for FirstPass<'a> {
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
