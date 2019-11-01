use std::collections::HashMap;

use super::byte_code::*;
use super::first_pass::PartialInstruction as PartialInstructionFP;
use super::first_pass::*;
use crate::analysis::*;

#[derive(Debug, Clone)]
pub(super) enum PartialInstruction {
    Instruction(Instruction),
    LoopBegin(LoopId), // Marker instruction, points to before loop condition
    LoopEnd(LoopId),   // Marker instruction, points to after looper instruction
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
    pub(super) fn new(
        main_body: Vec<PartialInstructionFP>,
        loops: HashMap<LoopId, LoopFrame>,
        branches: HashMap<BranchingId, BranchFrame>,
    ) -> SecondPass {
        SecondPass {
            main_body: main_body,
            loops: loops,
            branches: branches,
        }
    }

    pub(super) fn pass(self) -> Vec<PartialInstruction> {
        self.flatten(&self.main_body).0
    }

    /// Goes over a slice of first pass partial instructions and inlines branches and loops
    fn flatten(&self, partial_instrs: &[PartialInstructionFP]) -> Flattened {
        let mut instructions: Flattened = Flattened::new();

        for instr in partial_instrs {
            match instr {
                PartialInstructionFP::Instruction(ref instr) => {
                    instructions.push((*instr).clone().into());
                }

                PartialInstructionFP::Loop(ref loop_id) => {
                    let loop_frame = self
                        .loops
                        .get(loop_id)
                        .expect(&format!("Could not find: {:?}", loop_id));

                    let result_arg = loop_frame.get_result_location();

                    let mut condition =
                        self.flatten(loop_frame.get_condition());
                    let condition_len = condition.len();

                    let mut body = self.flatten(loop_frame.get_body());
                    let body_len = body.len();

                    // Append marker instruction for start of loop
                    instructions
                        .push(PartialInstruction::LoopBegin(loop_id.clone()));

                    // Append the condition instructions
                    instructions.append(&mut condition);

                    let after_looper = 2;
                    let skip_amount = (body_len as i64) + after_looper;

                    // Append the loop skip instruction
                    // Skips the body if the condition results in FALSE
                    // TODO(alex): Add check to ensure body length within u64 size?
                    let skip_loop_rel_target: i64 = skip_amount;
                    // body-size + 2 to skip over the looper jump
                    let skip_loop_instr = Instruction::RelJumpNegateCondition(
                        RelJumpTarget::new(skip_loop_rel_target),
                        result_arg.clone(),
                    );
                    instructions.push(skip_loop_instr.into());

                    // Append the body instructions
                    instructions.append(&mut body);

                    // Append the looper instruction
                    // Unconditionally jumps to start of condition instructions
                    // Loop skip instruction should jump to directly AFTER this instruction
                    let looper_rel_target: i64 =
                        -((body_len as i64) + (condition_len as i64)) - 1;
                    let loop_instr = Instruction::RelJump(RelJumpTarget::new(
                        looper_rel_target,
                    ));
                    instructions.push(loop_instr.into());

                    // Append marker instruction for end of loop
                    instructions
                        .push(PartialInstruction::LoopEnd(loop_id.clone()));
                }

                PartialInstructionFP::Branch(branch_id) => {
                    let branch_frame = self
                        .branches
                        .get(branch_id)
                        .expect(&format!("Missing: {:?}", branch_id));

                    let condition = branch_frame.get_condition();
                    let result_arg = branch_frame.get_result_location();
                    let true_branch = branch_frame.get_true_branch();
                    let false_branch = branch_frame.get_false_branch();

                    // Marked as 'mut' for append purposes
                    let mut condition = self.flatten(condition);
                    let mut true_branch = self.flatten(true_branch);
                    let true_branch_len = true_branch.len();
                    let mut false_branch = self.flatten(false_branch);
                    let false_branch_len = false_branch.len();

                    // Append condition instructions
                    instructions.append(&mut condition);

                    let after_false_branch = 2;
                    let true_rel_jump_amount =
                        (false_branch_len as i64) + after_false_branch;

                    // Append instruction to jump to the succeed branch
                    // Emit false branch first in order to chain any conditions
                    //   while minimizing the number of jump instructions
                    // +2 to go after false branch and true branch skip
                    let true_rel_jump_target: i64 = true_rel_jump_amount;
                    let true_rel_jump_instr = Instruction::RelJumpCondition(
                        RelJumpTarget::new(true_rel_jump_target),
                        result_arg.clone(),
                    );
                    instructions.push(true_rel_jump_instr.into());

                    // Append the false branch
                    instructions.append(&mut false_branch);

                    // Append instruction to jump over the succeed branch
                    // +1 to go to instruction just after the true branch
                    let after_true_branch = 1;
                    let true_skip_rel_jump_target: i64 =
                        (true_branch_len as i64) + after_true_branch;
                    let true_skip_rel_jump_instr = Instruction::RelJump(
                        RelJumpTarget::new(true_skip_rel_jump_target),
                    );
                    instructions.push(true_skip_rel_jump_instr.into());

                    // Append the true branch
                    instructions.append(&mut true_branch);
                }

                PartialInstructionFP::Continue(loop_id) => {
                    instructions
                        .push(PartialInstruction::Continue(loop_id.clone()));
                }

                PartialInstructionFP::Break(loop_id) => {
                    instructions
                        .push(PartialInstruction::Break(loop_id.clone()));
                }
            }
        }

        instructions
    }
}

struct Flattened(Vec<PartialInstruction>);

impl Flattened {
    fn new() -> Flattened {
        Flattened(Vec::new())
    }

    /// Custom len() implementation needed in order to not count LoopBegin and LoopEnd as
    /// instructions to jump over
    fn len(&self) -> usize {
        let mut size = 0;

        for i in self.0.iter() {
            match *i {
                PartialInstruction::LoopBegin(..)
                | PartialInstruction::LoopEnd(..) => (),

                _ => size += 1,
            }
        }

        size
    }

    pub fn push(&mut self, i: PartialInstruction) {
        self.0.push(i);
    }

    pub fn append(&mut self, other: &mut Flattened) {
        self.0.append(&mut other.0);
    }
}
