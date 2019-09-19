use std::collections::HashMap;
use crate::analysis::semantic_data::ModuleId;
use crate::ast::Ident;
use crate::module::ModuleSource;

#[derive(Clone, Debug)]
pub struct ModuleMetadata {
    module_map: HashMap<Ident, ModuleId>,
    module_reverse_map: HashMap<ModuleId, String>,
    module_sources: HashMap<ModuleId, ModuleSource>,
}

impl ModuleMetadata {
    pub fn new() -> ModuleMetadata {
        ModuleMetadata {
            module_map: HashMap::new(),
            module_reverse_map: HashMap::new(),
            module_sources: HashMap::new(),
        }
    }

    pub(crate) fn insert_mod_source(&mut self, id: ModuleId, source: ModuleSource) {
        map_unique_set!(self.module_sources, 
            id, 
            source, 
            format!("Modules should only have one source")
        );
    }

    pub fn mod_source(&self, id: ModuleId) -> &ModuleSource {
        self.module_sources
            .get(&id)
            .expect("module should have a source")
    }

    pub(crate) fn map_module(&mut self, name: Ident, mod_id: ModuleId) {
        map_unique_set!(self.module_map, 
            name.clone(), 
            mod_id, 
            format!("Overriding {:?}", mod_id)
        );

        map_unique_set!(self.module_reverse_map, 
            mod_id, 
            name.to_string(), 
            format!("Overriding {:?}", mod_id)
        );
    }

    pub fn get_module<T: Into<Ident>>(&self, name: T) -> Option<ModuleId> {
       self.module_map.get(&name.into()).map(|id| id.clone())
    }

    pub fn get_module_by_id(&self, id: ModuleId) -> Option<String> {
        self.module_reverse_map
            .get(&id)
            .clone()
            .map(|name| name.to_string())
    } 
}
