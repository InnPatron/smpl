mod byte_code;
mod byte_expr;
mod first_pass;
mod second_pass;
mod third_pass;

pub use byte_code::{
    Instruction,
    JumpTarget,
    RelJumpTarget,
    Location,
    FieldAccess,
    Arg,
    Struct,
    StructField,
};

use crate::analysis::{Traverser, CFG};

/// Takes a CFG and transforms it into valid and executable bytecode
pub fn compile_to_byte_code(cfg: &CFG) -> Vec<Instruction> {

    // Goes through the CFG and collects the main function body, loops, and branches
    //   into organized groups of instructions with metadata
    let mut first_pass = first_pass::FirstPass::new(cfg);
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

    third_pass.pass()
}
