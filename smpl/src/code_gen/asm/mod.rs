pub mod x86_64_gen;

#[macro_use]
mod nasm_macros;
mod nasm_const;
mod x86_64_fn_gen;
mod layout;

use analysis::{TypeId, FnId};

pub trait ASMBackend {
    fn layout(&self, id: TypeId) -> &self::layout::Layout;

    fn byte_alignment(&self) -> usize;
}

pub fn fn_id(id: FnId) -> String {
    format!("fn_{}", id.raw())
}

#[derive(Clone, Copy, PartialEq)]
pub enum DataLocation<R: PartialEq> {
    Local(usize), 
    Param(usize),
    Register(R),
}

impl<R> DataLocation<R> {
    fn is_register(&self) -> bool {
        if let DataLocation::Register(_) = *self {
            true
        } else {
            false
        }
    }
}
