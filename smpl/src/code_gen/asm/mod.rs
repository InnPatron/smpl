pub mod x86_64_gen;

#[macro_use]
mod nasm_macros;
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

#[derive(Clone, Copy)]
pub enum DataLocation<R> {
    Local(usize), 
    Param(usize),
    Register(R),
}
