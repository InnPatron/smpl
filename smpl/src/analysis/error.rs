use crate::module::ModuleSource;
use crate::ast::{Name, Ident};

#[derive(Debug, Clone)]
pub enum AnalysisError {
    MissingModuleName(ModuleSource),
    ConflictingModules(Ident, ModuleSource, ModuleSource),
    MissingDependency {
        original_module: Ident,
        original_source: ModuleSource,
        dependency: Ident,
    },
    CyclicDependencies(Vec<(Ident, ModuleSource)>),
    ConflictingModItems {
        c1_item: Name,
        c1_module: Ident,
        c1_module_source: ModuleSource,
        c2_item: Name,
        c2_module: Ident,
        c2_module_source: ModuleSource,
    },
    UnknownModItem {
        module: Ident,
        item: Ident,
    }

}
