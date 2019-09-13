use std::collections::HashMap;

use crate::analysis::*;
use crate::analysis::metadata::*;
use super::second_pass::PartialInstruction as PartialInstructionSP;
use super::byte_code::*;

pub(super) struct ThirdPass {
    main_body: Vec<PartialInstructionSP>,
    loop_begin_indexes: HashMap<LoopId, usize>
    loop_end_indexes: HashMap<LoopId, usize>
}

impl ThirdPass {
    pub(super) fn new(mb: Vec<PartialInstructionSP>) -> ThirdPass {
        ThirdPass {
            main_body: mb,
            loop_begin_indexes: HashMap::new(),
            loop_end_indexes: HashMap::new(),
        }
    }
}
