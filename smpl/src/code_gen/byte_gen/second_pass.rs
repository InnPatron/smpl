use std::collections::HashMap;

use crate::analysis::*;
use crate::analysis::metadata::*;
use super::first_pass::*;
use super::first_pass::PartialInstruction as PartialInstructionFP;
use super::byte_code::*;

#[derive(Debug, Clone)]
pub(super) enum PartialInstruction {
    Instruction(Instruction),
    LoopBegin(LoopId),              // Marker instruction, points to before loop condition
    LoopEnd(LoopId),                // Marker instruction, points to after looper instruction
    Continue(LoopId),
    Break(LoopId),
}

impl From<Instruction> for PartialInstruction {
    fn from(instr: Instruction) -> PartialInstruction {
        PartialInstruction::Instruction(instr)
    }
}

pub(super) struct SecondPass {
    main_body: Vec<PartialInstructionFP>,
    loops: HashMap<LoopId, LoopFrame>,
    branches: HashMap<BranchingId, BranchFrame>,
}

impl SecondPass {
    pub(super) fn new(main_body: Vec<PartialInstructionFP>,
                loops: HashMap<LoopId, LoopFrame>,
                branches: HashMap<BranchingId, BranchFrame>) -> SecondPass {

        SecondPass {
            main_body: main_body,
            loops: loops,
            branches: branches,
        }
    }

    pub(super) fn pass(self) -> Vec<PartialInstruction> {
        self.flatten(&self.main_body)
    }

    /// Goes over a slice of first pass partial instructions and inlines branches and loops
    fn flatten(&self, partial_instrs: &[PartialInstructionFP]) -> Vec<PartialInstruction> {
        let mut instructions: Vec<PartialInstruction> = Vec::new();

        for instr in partial_instrs {
            match instr  {
                PartialInstructionFP::Instruction(ref instr) => {
                    instructions.push((*instr).clone().into());
                }

                PartialInstructionFP::Loop(ref loop_id) => {
                    let loop_frame = self.loops
                        .get(loop_id)
                        .expect(&format!("Could not find: {:?}", loop_id));
                    let condition = loop_frame.get_condition();
                    let body = loop_frame.get_body();
                    let result_arg = loop_frame.get_result_location();

                    instructions.push(Instruction::Meta(
                            format!("Begin loop: {}", loop_id)).into());

                    // Append marker instruction for start of loop
                    instructions.push(PartialInstruction::LoopBegin(loop_id.clone()));

                    instructions.push(Instruction::Meta(
                            format!("Begin loop condition for {}", loop_id)).into());
                    // Append the condition instructions
                    instructions.append(&mut self.flatten(condition));

                    // Append the loop skip instruction
                    // Skips the body if the condition results in FALSE
                    // TODO(alex): Add check to ensure body length within u64 size?
                    let skip_loop_rel_target: i64 = (body.len() as i64) + 2;
                    // body-size + 2 to skip over the looper jump
                    let skip_loop_instr = Instruction::RelJumpNegateCondition(
                        RelJumpTarget::new(skip_loop_rel_target),
                        result_arg.clone()
                    );
                    instructions.push(skip_loop_instr.into());

                    instructions.push(Instruction::Meta(
                            format!("Begin loop body for {}", loop_id)).into());
                    // Append the body instructions
                    instructions.append(&mut self.flatten(body));
                    instructions.push(Instruction::Meta(
                            format!("End loop body for {}", loop_id)).into());
                    
                    // Append the looper instruction
                    // Unconditionally jumps to start of condition instructions
                    // Loop skip instruction should jump to directly AFTER this instruction
                    let looper_rel_target: i64 = 
                        -((body.len() as i64) + (condition.len() as i64)) - 2;
                    let loop_instr = Instruction::RelJump(
                        RelJumpTarget::new(looper_rel_target)
                    );
                    instructions.push(loop_instr.into());

                    // Append marker instruction for end of loop
                    instructions.push(PartialInstruction::LoopEnd(loop_id.clone()));
                }

                PartialInstructionFP::Branch(branch_id) => {
                    let branch_frame = self.branches
                        .get(branch_id)
                        .expect(&format!("Missing: {:?}", branch_id));

                    let condition = branch_frame.get_condition();
                    let result_arg = branch_frame.get_result_location();
                    let true_branch = branch_frame.get_true_branch();
                    let false_branch = branch_frame.get_false_branch();

                    // Marked as 'mut' for append purposes
                    let mut condition = self.flatten(condition);
                    let mut true_branch = self.flatten(true_branch);
                    let mut false_branch = self.flatten(false_branch);

                    // Append condition instructions
                    instructions.append(&mut condition);

                    // Append instruction to jump to the succeed branch
                    // Emit false branch first in order to chain any conditions 
                    //   while minimizing the number of jump instructions
                    // +2 to go after false branch and true branch skip
                    let true_rel_jump_target: i64 =
                        (false_branch.len() as i64) + 2;
                    let true_rel_jump_instr = Instruction::RelJumpCondition(
                        RelJumpTarget::new(true_rel_jump_target),
                        result_arg.clone()
                    );
                    instructions.push(true_rel_jump_instr.into());

                    // Append the false branch
                    instructions.append(&mut false_branch);

                    // Append instruction to jump over the succeed branch
                    // +1 to go to instruction just after the true branch
                    let true_skip_rel_jump_target: i64 =
                        (true_branch.len() as i64) + 2;
                    let true_skip_rel_jump_instr = Instruction::RelJump(
                        RelJumpTarget::new(true_skip_rel_jump_target)
                    );
                    instructions.push(true_skip_rel_jump_instr.into());

                    // Append the true branch
                    instructions.append(&mut true_branch);
                }

                PartialInstructionFP::Continue(loop_id) => {
                    instructions.push(PartialInstruction::Continue(loop_id.clone()));
                }
                
                PartialInstructionFP::Break(loop_id) => {
                    instructions.push(PartialInstruction::Break(loop_id.clone()));
                }
            }
        }

        instructions
    }
}
