use std::collections::{HashSet, HashMap};
use std::iter::IntoIterator;

use super::error::AnalysisError;
use crate::module::ParsedModule;
use crate::ast::Ident;

pub struct DepGraph {
    graph: HashMap<Ident, Vec<Ident>>,
    modules: HashMap<Ident, ParsedModule>,
}

pub type DepGraphResult<T> = Result<T, Vec<AnalysisError>>;

///
/// 1) Build a dependency graph based on exports/imports
/// 2) Validate all depedencies are present
/// 3) Check for cycles
///
/// After this function, the SMPL program is guarenteed to have all the module dependencies
///   it needs to compile
///
pub fn build_dependency_graph<T>(parsed_modules: T) -> DepGraphResult<DepGraph>
    where T: IntoIterator<Item=ParsedModule> {

    // Step 1
    let dep_graph = generate_graph(parsed_modules)?;

    // Step 2
    validate_dependencies(&dep_graph)?;

    // Step 3
    cyclic_check(&dep_graph)?;

    Ok(dep_graph)
}

/// 3)
fn cyclic_check(dep_graph: &DepGraph) -> DepGraphResult<()> {

    let mut global_visited: HashSet<&Ident> = HashSet::new();

    // NOTE: graph may be disjoint
    // Loop through all nodes and perform a DFS check unless already visited
    //   (tracked by global_visited)
    for (mod_name, _) in dep_graph.modules.iter() {
        if global_visited.contains(mod_name) {
            continue;
        }

        let mut local_visited_set = HashSet::new();
        detect_cycle(&mut global_visited, &mut local_visited_set, dep_graph, mod_name)
            .map_err(|_| {
            let cycle: Vec<(Ident, crate::module::ModuleSource)> = local_visited_set
                .into_iter()
                .map(|ident| {
                    let source = dep_graph.modules.get(ident).unwrap().source.clone();
                    (ident.clone(), source)
                })
            .collect();

            vec![AnalysisError::CyclicDependencies(cycle)]
        })?;
    }

    Ok(())
}

fn detect_cycle<'a, 'b>(
    global_visited_set: &'a mut HashSet<&'b Ident>,
    local_visited_set: &'a mut HashSet<&'b Ident>,
    dep_graph: &'b DepGraph,
    start_node: &'b Ident,
) -> Result<(), ()> {

    global_visited_set.insert(start_node);
    local_visited_set.insert(start_node);

    for d in dep_graph.graph.get(start_node).unwrap() {
        if local_visited_set.contains(d) {
            return Err(());
        }

        detect_cycle(global_visited_set, local_visited_set, dep_graph, d)?;
    }

    Ok(())
}


/// 2) Validates all dependencies are present in the graph
fn validate_dependencies(dep_graph: &DepGraph) -> DepGraphResult<()> {
    let mut errors = Vec::new();
    for (current_module, deps) in dep_graph.graph.iter() {
        for d in deps {
            if !dep_graph.modules.contains_key(d) {
                errors.push(AnalysisError::MissingDependency {
                    original_module: current_module.clone(),
                    original_source: dep_graph.modules.get(current_module).expect("module missing from the DepGraph's modules cache").source.clone(),
                    dependency: d.clone(),
                });
            }

        }
    }

    if errors.len() > 0 {
        return Err(errors);
    }

    Ok(())
}

/// 1) Build a dependency graph based on exports/imports
fn generate_graph<T>(parsed_modules: T) -> DepGraphResult<DepGraph>
    where T: IntoIterator<Item=ParsedModule> {

    let mut errors = Vec::new();

    let mut dependency_graph = DepGraph {
        graph: HashMap::new(),
        modules: HashMap::new(),
    };

    for parsed_module in parsed_modules.into_iter() {
        let current_module = &parsed_module.module;
        let current_id = if let Some(ref ident_node) = current_module.ident {
            ident_node.node()
        } else {
            errors.push(AnalysisError::MissingModuleName(parsed_module.source.clone()));
            continue;
        };

        if let Some(conflicting_module) = dependency_graph.modules.get(current_id) {
            errors.push(AnalysisError::ConflictingModules(current_id.clone(), conflicting_module.source.clone(), parsed_module.source.clone()));
            continue;
        }

        let dependencies = scan_dependencies(current_module);
        dependency_graph.graph.insert(current_id.clone(), dependencies);


        dependency_graph.modules.insert(current_id.clone(), parsed_module);
    }

    if errors.len() > 0 {
        return Err(errors);
    }

    Ok(dependency_graph)
}

fn scan_dependencies(current_module: &crate::ast::Module<(), ()>) -> Vec<Ident> {
    use crate::ast::*;

    let mut current_dependencies = Vec::new();
    for decl_stmts in current_module.top_levels.iter() {
        let dependent_module = match decl_stmts {
            DeclStmt::Import(ref import) => {
                match import.node() {
                    ImportDecl::ImportItems {
                        ref module,
                        ..
                    } => {
                        module.node().clone()
                    }

                    ImportDecl::ImportModule {
                        ref module,
                        ..
                    } => {
                        module.node().clone()
                    }

                    ImportDecl::ImportAll  {
                        ref module,
                        ..
                    } => {
                        module.node().clone()
                    }
                }
            }

            DeclStmt::Export(ref export) => {
                match export.node() {
                    ExportDecl::ExportItems {
                        from_module: Some(ref from_module),
                        ..
                    } => {
                        from_module.node().clone()
                    }

                    ExportDecl::ExportAll  {
                        from_module: Some(ref from_module),
                        ..
                    } => {
                        from_module.node().clone()
                    }

                    _ => continue,
                }
            }
            _ => continue,
        };

        current_dependencies.push(dependent_module);
    }

    current_dependencies
}
