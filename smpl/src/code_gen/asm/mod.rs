pub mod x86_64_gen;

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
