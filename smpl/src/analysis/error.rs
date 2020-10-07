use crate::module::ModuleSource;
use crate::ast::Ident;

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

}
