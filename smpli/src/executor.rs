use std::sync::Arc;

use smpl::byte_gen;

#[derive(Debug, Clone)]
pub struct Executor {
    byte_code: Arc<byte_gen::ByteCodeFunction>,
    instruction_pointer: byte_gen::InstructionPointerType,
 
}

impl Executor {

}
