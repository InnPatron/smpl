pub mod x86_64_gen;

mod x86_64_fn_gen;
mod layout;

use analysis::TypeId;

pub trait ASMBackend {
    fn layout(&self, id: TypeId) -> &self::layout::Layout;

    fn byte_alignment(&self) -> usize;
}
