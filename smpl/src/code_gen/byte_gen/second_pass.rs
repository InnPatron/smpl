use std::collections::HashMap;

use crate::analysis::*;
use crate::analysis::metadata::*;
use super::first_pass::*;
use super::byte_code::*;

pub(super) struct SecondPass {
    main_body: Vec<PartialInstruction>,
    loops: HashMap<LoopId, LoopFrame>,
    branches: HashMap<BranchingId, BranchFrame>,
}

impl SecondPass {
    pub(super) fn new(main_body: Vec<PartialInstruction>,
                loops: HashMap<LoopId, LoopFrame>,
                branches: HashMap<BranchingId, BranchFrame>) -> SecondPass {

        SecondPass {
            main_body: main_body,
            loops: loops,
            branches: branches,
        }
    }

    fn flatten(&self, partial_instrs: &[PartialInstruction]) -> Vec<Instruction> {
        let mut instructions: Vec<Instruction> = Vec::new();

        for instr in partial_instrs {
            match instr  {
                PartialInstruction::Instruction(ref instr) => {
                    instructions.push((*instr).clone());
                }

                PartialInstruction::Loop(ref loop_id) => {
                    let loop_frame = self.loops
                        .get(loop_id)
                        .expect(&format!("Could not find: {:?}", loop_id));
                    let condition = loop_frame.get_condition();
                    let body = loop_frame.get_condition();
                    let result_arg = loop_frame.get_result_location();

                    // Append the condition instructions
                    instructions.append(&mut self.flatten(condition));

                    // Append the loop skip instruction
                    // Skips the body if the condition results in FALSE
                    // TODO(alex): Add check to ensure body length within u64 size?
                    let skip_loop_rel_target: i64 = (body.len() as i64) + 1;
                    // body-size + 1 to skip over the looper jump
                    let skip_loop_instr = Instruction::RelJumpNegateCondition(
                        RelJumpTarget::new(skip_loop_rel_target),
                        result_arg.clone()
                    );
                    instructions.push(skip_loop_instr);

                    // Append the body instructions
                    instructions.append(&mut self.flatten(body));
                    
                    // Append the looper instruction
                    // Unconditionally jumps to start of condition instructions
                    // Loop skip instruction should jump to directly AFTER this instruction
                    let looper_rel_target: i64 = 
                        -((body.len() as i64) + (condition.len() as i64)) - 1;
                    let loop_instr = Instruction::RelJump(
                        RelJumpTarget::new(looper_rel_target)
                    );
                    instructions.push(loop_instr);
                }

                PartialInstruction::Branch(branch_id) => unimplemented!(),
                PartialInstruction::Continue(loop_id) => unimplemented!(), 
                PartialInstruction::Break(loop_id) => unimplemented!(),
            }
        }

        instructions
    }
}
