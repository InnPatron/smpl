use super::StaticModData;
use crate::ast::*;
use crate::expr_ast::*;
use std::collections::{hash_map::Entry, HashMap, HashSet};

#[derive(Debug, Clone)]
pub enum Error {}

pub struct ExportResolved;

pub type DepResult<T> = Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemLocation {
    pub original_name: ItemName,
    pub module: ModName,
}

type ItemName = Name;
/// Module where item was originally defined
type ModName = Ident;

type LocalInterface = HashMap<ItemName, ItemLocation>;
type SccInterface = HashMap<ItemName, ModName>;
type ExportAllGraph = HashMap<ModName, HashSet<ModName>>;

pub fn resolve_dependencies<'a, I: Iterator<Item = &'a Module> + Clone>(
    sdm: &StaticModData,
    modules: I,
) -> DepResult<ExportResolved> {
    todo!();
}

///
/// Terminology:
///   Module interface/interface: module's public API
///   Local interface: module's exported local definitions
///   Local SCC interface: the union of all the local interfaces
///     of the modules in the SCC
///
/// 1) Calculate the local interface for each module
/// 2) Generate the graph of "export all from D" edges
///    A -> D: A reexports all from module D
/// 3) Identify all strongly connected components
/// 4) For each SCC S:
///    1) Calculate the local SCC interface by unioning
///       the local interface sts of all modules in the SCC
///    2) Check that each local item is uniquely named within the SCC
/// 5)
///
fn export_all_resolution<'a, I: Iterator<Item = &'a Module> + Clone>(
    sdm: &StaticModData,
    modules: I,
) -> DepResult<ExportResolved> {
    let local_interfaces = modules
        .clone()
        .map(|m| {
            let local_interface = local_set(m)
                .and_then(|local_set| local_interface(m, local_set));

            local_interface
                .map(|li| (m.name().expect("No mod name: module not wf"), li))
        })
        .collect::<DepResult<HashMap<_, _>>>()?;
    todo!();
}

fn export_all_graph() -> DepResult<ExportAllGraph> {
    let mut graph: ExportAllGraph = HashMap::new();
    for (mod_name, mod_decls) in modules.map(|m| (m.name().unwrap(), &m.decls))
    {
        for decl in mod_decls {
            match decl {
                Decl::Export(ref decl) => match decl.data() {
                    ExportDecl::ExportAll {
                        from_module: Some(ref dep_inst),
                        ..
                    } => {
                        for dep_name in dep_inst.data().get_deps() {
                            match graph.entry(mod_name.clone()) {
                                Entry::Occupied(ref mut e) => {
                                    e.get_mut().insert(dep_name.clone());
                                }

                                Entry::Vacant(mut e) => {
                                    let mut s = HashSet::new();
                                    s.insert(dep_name.clone());
                                    e.insert(s);
                                }
                            }
                        }
                    }

                    _ => continue,
                },

                _ => continue,
            }
        }
    }
}

/// Builds a LocalInterface containing ALL locally declared items
fn local_set(module: &Module) -> DepResult<LocalInterface> {
    let mut li = LocalInterface::new();
    let module_name = module.mod_decl.as_ref().unwrap().data().mod_name.data();
    for d in module.decls.iter() {
        match d {
            Decl::Export(..) | Decl::Import(..) => continue,
            Decl::Local(ref node_local) => {
                let item_name = node_local.name().data();
                li.insert(
                    item_name.clone(),
                    ItemLocation {
                        original_name: item_name.clone(),
                        module: module_name.clone(),
                    },
                );
            }
        }
    }

    Ok(li)
}

///
/// Builds the module's local interface
///
/// * Checks that all exported local items actually exist
/// * Duplicate export names actually refer to the same item i.e:
///
///  ```smpl
///     export all;
///     export { Foo };
///
///     type Foo = ...
///  ```
///
fn local_interface(
    module: &Module,
    local_set: LocalInterface,
) -> DepResult<LocalInterface> {
    let mut export_all = false;
    let mut li = LocalInterface::new();
    let module_name = module.mod_decl.as_ref().unwrap().data().mod_name.data();

    // Go through all exports of local items
    // Handle local export-all after handling named exports
    for d in module.decls.iter() {
        match d {
            Decl::Export(ref node_export_decl) => {
                match node_export_decl.data() {
                    // Exporting all locally declared items
                    // Sets the `export_all` flag to add all local items
                    //   AFTER handling all named exports
                    ExportDecl::ExportAll {
                        from_module: None, ..
                    } => export_all = true,

                    // Handle named exports
                    ExportDecl::ExportItems {
                        from_module: None,
                        ref items,
                    } => {
                        for mod_item_data in items {
                            let mod_item_data = mod_item_data.data();
                            // Make sure local items exist on the module
                            match local_set.get(mod_item_data.original_name.data()) {
                                Some(item_location) => {
                                    let export_name = mod_item_data
                                        .name_override
                                        .as_ref()
                                        .unwrap_or(&mod_item_data.original_name)
                                        .data()
                                        .clone();

                                    // Check if overriding an exported name
                                    if let Some(export_override) = li.insert(
                                        export_name,
                                        item_location.clone(),
                                    ) {
                                        if export_override.original_name != item_location.original_name {
                                            // Found a named export that overrides another named
                                            // export pointing towards a different local item
                                            // ```
                                            // export Foo as Bar
                                            // export Bar
                                            // ```
                                            todo!("Attempting to export different local items with the same name: {}", item_location.original_name);

                                        }
                                    }
                                }

                                None => todo!("Attempting to export a non-existant local item"),
                            }
                        }
                    }

                    _ => continue,
                }
            }
            Decl::Local(..) | Decl::Import(..) => continue,
        }
    }

    if export_all {
        for (export_name, local_item) in local_set.into_iter() {
            let local_item_name = local_item.original_name.clone();
            if let Some(export_override) = li.insert(export_name, local_item) {
                if export_override.original_name != local_item_name {
                    todo!("Attempting to export different local items with teh same name: {}", local_item_name);
                }
            }
        }
    }

    Ok(li)
}
