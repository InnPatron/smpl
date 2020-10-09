pub mod type_structs;
pub mod error;
mod id_structs;
mod dep_graph;
mod mod_interface;

pub use id_structs::{VarId, FnId};

use crate::module::ParsedModule;

pub struct AnalyzedProgram;

pub fn analyze_program<T>(program: T) -> Result<AnalyzedProgram, Vec<error::AnalysisError>>
    where T: IntoIterator<Item=ParsedModule> {

    let mut dep_graph = dep_graph::build_dependency_graph(program)?;
    let mod_interfaces = mod_interface::module_interfaces(&mut dep_graph)
        .map_err(|e| vec![e]);
    todo!();

}
