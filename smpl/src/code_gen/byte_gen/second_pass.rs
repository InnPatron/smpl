use std::collections::HashMap;

use crate::analysis::*;
use crate::analysis::metadata::*;
use super::first_pass::*;

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
}
