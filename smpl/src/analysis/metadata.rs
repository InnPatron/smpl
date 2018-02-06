use std::collections::HashMap;

use err::Err;
use analysis::semantic_data::{TypeId, ModuleId, FnId, Universe};

#[derive(Clone, Debug)]
pub struct Metadata {
    fn_meta: HashMap<FnId, FnMetadata>,
    main: Option<(FnId, ModuleId)>
}

impl Metadata {

    pub fn new() -> Metadata {
        Metadata {
            fn_meta: HashMap::new(),
            main: None,
        }
    }

    pub fn find_main(&mut self, universe: &Universe) -> Result<(), Err> {
        use ast::Path;

        for (_, mod_id) in universe.all_modules().into_iter() {
            let module = universe.get_module(*mod_id);
            if let Ok(id) = module.module_scope().get_fn(&path!("main")) {
                if self.main.is_none() {
                    self.main = Some((id, *mod_id))
                } else {
                    return Err(Err::MultipleMainFns);
                }
            }
        }

        Ok(())
    }

    pub fn main(&self) -> Option<(FnId, ModuleId)> {
        self.main
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnMetadata {
    local_vars: Vec<TypeId>,
}
