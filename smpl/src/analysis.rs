#[macro_use]
mod macros;

mod error;
mod wf_checker;

use std::collections::HashMap;

use crate::ast::TypedPath;
use crate::expr_ast::LiteralSuffix;
use crate::Source;

// TODO: Literal suffix mapper module or program-wide?
#[derive(Debug, Clone)]
pub struct StaticModData<'a> {
    pub source: Source,
    pub lit_sfx_map: &'a LiteralSuffixMap,
}

#[derive(Debug, Clone)]
pub struct LiteralSuffixMap {
    type_map: HashMap<LiteralSuffix, TypedPath>,
}

impl LiteralSuffixMap {
    pub fn new() -> Self {
        LiteralSuffixMap {
            type_map: HashMap::new(),
        }
    }

    // TODO: Add an error type for invalid literal suffix map
    pub fn insert(
        &mut self,
        sfx: LiteralSuffix,
        tp: TypedPath,
    ) -> Result<(), ()> {
        match self.type_map.insert(sfx.clone(), tp) {
            Some(_) => todo!("Error: duplicate literal suffix: {:?}", sfx),
            None => Ok(()),
        }
    }

    pub fn get_typed_path(&self, sfx: &LiteralSuffix) -> Option<&TypedPath> {
        self.type_map.get(sfx)
    }
}
