mod byte_code;
mod byte_expr;
mod first_pass;
mod second_pass;
mod third_pass;

use std::fmt;

use crate::analysis::Function;

pub use byte_code::{
    Instruction,
    JumpTarget,
    RelJumpTarget,
    Location,
    FieldAccess,
    Arg,
    InstructionPointerType,
};

pub use byte_expr::{
    fn_id as to_fn_id,
};

use crate::analysis::{Traverser, CFG};

#[derive(Debug, Clone)] 
pub struct ByteCodeFunction {
    instructions: Vec<Instruction>,
    validated_flag: bool,
}

impl ByteCodeFunction {

    pub fn new_not_validated(instructions: Vec<Instruction>) -> ByteCodeFunction {
        ByteCodeFunction {
            instructions: instructions,
            validated_flag: false,
        }
    }

    pub fn new_pre_validated(instructions: Vec<Instruction>) -> ByteCodeFunction {
        ByteCodeFunction {
            instructions: instructions,
            validated_flag: true,
        }
    }

    pub fn validate(mut self) -> ByteCodeFunction {
        self.validated_flag = true;
        self
    }

    pub fn is_validated(&self) -> bool {
        self.validated_flag
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
}

impl fmt::Display for ByteCodeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for instr in self.instructions.iter() {
            writeln!(f, "{}", instr)?;
        }

        Ok(())
    }
}

/// Takes a CFG and transforms it into valid and executable bytecode
pub fn compile_to_byte_code(function: &Function) -> ByteCodeFunction {

    let cfg = function.cfg();

    // Goes through the CFG and collects the main function body, loops, and branches
    //   into organized groups of instructions with metadata
    let mut first_pass = first_pass::FirstPass::new(&*cfg);
    {
        let traverser = Traverser::new(&*cfg, &mut first_pass);

        traverser.traverse().unwrap();
    }

    // Takes the data from the first pass and combines all branches and loops together
    //   into a single chunk of instructions
    // This pass leaves break/continue meta instructions to be resolved later
    let second_pass: second_pass::SecondPass = first_pass.into();

    // Takes the data from the second pass and resolves break/continue statements
    //   to jump to the correct index
    let third_pass = third_pass::ThirdPass::new(
        second_pass.pass()
    );

    ByteCodeFunction::new_pre_validated(third_pass.pass())
}
