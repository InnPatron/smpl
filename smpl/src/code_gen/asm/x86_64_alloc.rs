use std::collections::{VecDeque, HashMap};

use analysis::DataId;

use super::DataLocation;
use super::x86_64_gen::Register;


pub struct LocalAllocator {
    storage: VecDeque<Block>,
    map: HashMap<DataId, Vec<Block>>,
    deepest_offset: usize,               // Used to track how much TOTAL stack space to allocate
    block_size: usize,
}

impl LocalAllocator {
    pub fn new(block_size: usize, blocks: usize) -> LocalAllocator {
        let mut s = VecDeque::new();

        // Begin at 1; stack grows DOWN
        // Locals/temps are placed top-down but read down-top
        // First variable will be at RBP-8
        for i in 1..blocks+1 {
            s.push_back(Block::new(i));
        }

        LocalAllocator {
            storage: s,
            map: HashMap::new(),
            deepest_offset: 0,
            block_size: block_size,
        }
    }

    pub fn deepest_offset(&self) -> usize {
        self.deepest_offset
    }

    pub fn dealloc<T: Into<DataId>>(&mut self, id: T) {
        let memory = self.map.remove(&id.into())
            .expect("Attempting to deallocate nonexistant local.");

        // Insert memory blocks to the head in reverse order.
        // Memory block closest to RBP should be closer to head.
        // Storage should probably be a different data type (like a BST),
        // but a Queue is fine b/c most memory will allocated and deallocated
        // from the front.
        for b in memory.into_iter().rev() {
            self.storage.push_front(b);
        }
    }

    pub fn alloc<T: Into<DataId>>(&mut self, id: T, size: usize) -> DataLocation<Register> {
        let block_min = size / self.block_size;
        let excess = size % self.block_size;

        let mut blocks_required = block_min;
        if excess > 0 {
            blocks_required += 1;
        }

        let mut start = None;
        let mut to_allocate = 0;
        let mut deepest_offset = 0;
        for (current, ref current_block) in self.storage.iter().enumerate() {
            if start.is_none() {
                start = Some(current);
                to_allocate = 1;
                deepest_offset = current_block.offset;
            } else if start.is_some() {
                if to_allocate == blocks_required {
                    break;
                }

                let previous_block = self.storage.get(current - 1).unwrap();
                if previous_block.offset + 1 == current_block.offset {
                    to_allocate += 1;
                } else {
                    start = Some(current);
                    to_allocate = 1;
                }

                deepest_offset = current_block.offset;
            }
        }

        if to_allocate != blocks_required {
            panic!("Unable to allocate enough blocks (of size {}) to fit {}. Allocate more blocks?",
            self.block_size, size);
        }

        if self.deepest_offset < deepest_offset {
            self.deepest_offset = deepest_offset;
        }

        let start_block_index = start.unwrap();

        // Start of variable in memory
        let start_stack_offset = {
            self.storage.get(start_block_index).unwrap().offset
        };

        // Remove memory blocks
        let allocated = self.storage
            .drain(start_block_index..start_block_index + blocks_required)
            .collect::<Vec<_>>();

        // Map allocated memory to a local / temporary
        self.map.insert(id.into(), allocated);

        DataLocation::Local(start_stack_offset * self.block_size)
    }
}

struct Block {
    offset: usize
}

impl Block {
    fn new(offset: usize) -> Block {
        Block {
            offset: offset
        }
    }

    fn offset(&self) -> usize {
        self.offset
    }
}

pub struct RegisterAllocator {
    registers: Vec<Register>,
}

impl RegisterAllocator {
    pub fn new() -> RegisterAllocator {
        RegisterAllocator {
            registers: vec![
                            Register::R8,
                            Register::R9,
                            Register::R10,
                            Register::R11,
                            Register::R12,
                            Register::R13,
                            Register::R14,
                            Register::R15,
            ]
        }
    }

    pub fn alloc(&mut self) -> Option<DataLocation<Register>> {
        self.registers.pop().map(|r| DataLocation::Register(r))
    }

    pub fn dealloc(&mut self, r: Register) {
        if self.registers.contains(&r) {
            panic!("Attempting to deallocate register {} when it is already available", r);
        }

        self.registers.push(r);
    }
}

pub struct ParamAllocator {
    param_total: usize,
    param_tracker: usize,
}

impl ParamAllocator {

    pub fn new(start_offset: usize) -> ParamAllocator {
        ParamAllocator {
            param_total: 0,
            param_tracker: start_offset,
        }
    }

    pub fn param_total(&self) -> usize {
        self.param_total
    }

    pub fn alloc(&mut self, size: usize) -> DataLocation<Register> {
        self.param_total += size;
        let loc = DataLocation::Local(self.param_tracker);
        self.param_tracker += size;

        loc
    }
}
