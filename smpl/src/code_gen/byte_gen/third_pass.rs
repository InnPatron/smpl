use std::collections::HashMap;

use crate::analysis::*;
use crate::analysis::metadata::*;
use super::second_pass::PartialInstruction as PartialInstructionSP;
use super::byte_code::*;

pub(super) struct ThirdPass {
    main_body: Vec<PartialInstructionSP>,
    loop_begin_indexes: HashMap<LoopId, usize>,
    loop_end_indexes: HashMap<LoopId, usize>,
}

impl ThirdPass {
    pub(super) fn new(mb: Vec<PartialInstructionSP>) -> ThirdPass {
        ThirdPass {
            main_body: mb,
            loop_begin_indexes: HashMap::new(),
            loop_end_indexes: HashMap::new(),
        }
    }

    pub(super) fn pass(mut self) -> Vec<Instruction> {
        self.gather_loop_indexes();

        let mut instructions: Vec<Instruction> = Vec::new();

        // Replace continue/breaks with the correct jump instructions
        for (index, p_instr) in self.main_body.into_iter().enumerate() {
            match p_instr {
                PartialInstructionSP::Instruction(instr) => {
                    instructions.push(instr);
                }

                PartialInstructionSP::Continue(loop_id) => {
                    let loop_begin_index = self.loop_begin_indexes
                        .get(&loop_id)
                        .expect(&format!("Could not find start index for loop id: {:?}", loop_id));

                    if *loop_begin_index > index {
                        panic!("Attempting to continue to a loop start {} after the continue instruction {}",
                               loop_begin_index,
                               index
                        );
                    }

                    let rel_jump_target = (index - loop_begin_index) as i64;
                    let instr = Instruction::RelJump(RelJumpTarget::new(rel_jump_target));
                    instructions.push(instr);
                }

                PartialInstructionSP::Break(loop_id) => {
                    let loop_end_index = self.loop_begin_indexes
                        .get(&loop_id)
                        .expect(&format!("Could not find end index for loop id: {:?}", loop_id));

                    if *loop_end_index < index {
                        panic!("Attempting to break to a loop end {} before the continue instruction {}",
                               loop_end_index,
                               index
                        );
                    }

                    let rel_jump_target = (loop_end_index - index) as i64;
                    let instr = Instruction::RelJump(RelJumpTarget::new(rel_jump_target));
                    instructions.push(instr);
                }

                PartialInstructionSP::LoopBegin(..) | PartialInstructionSP::LoopEnd(..) => (),
            }
        }

        instructions
    }

    fn gather_loop_indexes(&mut self) {
        for (index, p_instr) in self.main_body.iter().enumerate() {
            match p_instr {
                PartialInstructionSP::Instruction(..) => (),

                PartialInstructionSP::LoopBegin(loop_id) => {
                    if self.loop_begin_indexes
                        .insert(loop_id.clone(), index)
                        .is_some() {
                    
                        panic!("Found duplicate of loop id begin ({:?}) at {}",
                            loop_id,
                            index
                        );
                    }
                }

                PartialInstructionSP::LoopEnd(loop_id) => {
                    if self.loop_end_indexes
                        .insert(loop_id.clone(), index)
                        .is_some() {
                    
                        panic!("Found duplicate of loop id end ({:?}) at {}",
                            loop_id,
                            index
                        );
                    }
                }

                PartialInstructionSP::Continue(..) | PartialInstructionSP::Break(..) => (),
            }
        }
    }
}
