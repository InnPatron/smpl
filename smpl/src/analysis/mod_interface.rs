use std::collections::HashMap;
use std::mem;

use crate::module::ParsedModule;
use crate::ast::*;

use super::error::AnalysisError;
use super::dep_graph::DepGraph;

macro_rules! mod_item {
    ($module: expr => $ident: expr) => {
        Name::ModuleItem {
            module: $module.clone(),
            item: $ident.into_ident(),
        }
    }
}

macro_rules! try_insert {
    ($mi: expr; {
        $original: expr,
        $swap: expr,
    }; [LOCAL $cm_name: expr]) => {
        let mut name_store = mod_item!($cm_name => $original);
        let new_name = name_store.clone();
        mem::swap(&mut name_store, $swap);

        if let Some(conflict) = $mi.insert(name_store.into_ident(), new_name) {
            todo!("Conflicting local top-levels: {}", conflict);
        }
    }
}

enum ExportState {
    Export(Option<Ident>),
    NoExport,
}


pub type InterfaceResult<T> = Result<T, AnalysisError>;

pub struct ModInterfaces {
    pub map: HashMap<Ident, ModInterface>,
}

pub struct ModInterface(pub HashMap<Ident, Name>);

///
/// Dependency graph guaranteed to have NO cycles
///
///
pub fn module_interfaces(dep_graph: &mut DepGraph) -> InterfaceResult<ModInterfaces> {
    let mi = ModInterfaces {
        map: HashMap::new(),
    };

    Ok(mi)
}

fn module_interface<'a, 'b: 'c, 'c>(
    dep_graph: &'a mut DepGraph,
    mis: &'b mut ModInterfaces,
    to_retrieve: &'b Ident) -> InterfaceResult<()> {

    if let Some(my_mi) = mis.map.get(to_retrieve) {
        return Ok(());
    }


    let my_mi = make_mod_interface(dep_graph, mis, to_retrieve)?;
    mis.map.insert(to_retrieve.clone(), my_mi);

    Ok(())
}

fn make_mod_interface(
    dep_graph: &mut DepGraph,
    mi: &mut ModInterfaces,
    to_make: &Ident) -> InterfaceResult<ModInterface> {

    let current_module = dep_graph.modules.get_mut(to_make).unwrap();

    let mut local_interface = make_local_interface(to_make, current_module)?;
    todo!();
}

fn generate_foreign_interfaces<T>(current_mi: &mut ModInterface, deps: T, dep_graph: &mut DepGraph, mis: &mut ModInterfaces) -> InterfaceResult<()>
    where T: IntoIterator<Item=Ident> {

    for dep in deps.into_iter() {
        if mis.map.contains_key(&dep) {
            continue;
        }

        let foreign_mi = module_interface(dep_graph, mis, &dep)?;
        merge_foreign_interface(current_mi, mis.map.get(&dep).unwrap())?;
    }

    Ok(())
}

fn merge_foreign_interface(current_mit: &mut ModInterface, foreign: &ModInterface) -> InterfaceResult<()> {
    todo!();
}

///
/// Scans local type/function declarations ONLY
/// Replaces ALL local type/function declaration names with Name::ModuleItem
///
/// Imports are NOT considered part of a module's top-level scope for exporting purposes
///   To export from a module, either wrap with a locally declared item or directly
///   export with `export [all] from` syntax
///
fn make_local_interface(current_mod_name: &Ident, current_module: &mut ParsedModule) -> InterfaceResult<ModInterface> {

    let mut top_levels: HashMap<Ident, Name> = HashMap::new();
    let mut all = false;
    let mut local_exports_state: HashMap<Ident, ExportState> = HashMap::new();

    // Generate a map of the top level local scope and the local export state
    //   Checks for top-level local conflicts
    for decl in current_module.module.top_levels.iter_mut() {
        match decl {
            DeclStmt::Struct(ref mut decl_node) => {
                //let mut name_store = mod_item!(current_mod_name => decl_node.node().name.node().clone());
                //let new_name = name_store.clone();
                //mem::swap(&mut name_store, decl_node.node_mut().name.node_mut());

                //if let Some(conflict) = current_mi.0.insert(name_store.into_ident(), new_name) {
                //    todo!("Conflicting local top-levels");
                //}

                try_insert!(top_levels; {
                    decl_node.node().name.node().clone(),
                    decl_node.node_mut().name.node_mut(),
                }; [LOCAL current_mod_name]);
            }

            DeclStmt::Opaque(ref mut decl_node) => {
                try_insert!(top_levels; {
                    decl_node.node().name.node().clone(),
                    decl_node.node_mut().name.node_mut(),
                }; [LOCAL current_mod_name]);
            }

            DeclStmt::Function(ref mut decl_node) => {
                try_insert!(top_levels; {
                    decl_node.node().name.node().clone(),
                    decl_node.node_mut().name.node_mut(),
                }; [LOCAL current_mod_name]);
            }

            DeclStmt::BuiltinFunction(ref mut decl_node) => {
                try_insert!(top_levels; {
                    decl_node.node().name.node().clone(),
                    decl_node.node_mut().name.node_mut(),
                }; [LOCAL current_mod_name]);
            }

            DeclStmt::Export(ref export) => {
                match export.node() {
                    ExportDecl::ExportAll {
                        from_module: None,
                        ref except,
                    } => {
                        all = true;
                        populate_excepts(&mut local_exports_state,
                            except.iter().map(|node| &node.node().0))?;
                    }

                    ExportDecl::ExportItems {
                        from_module: None,
                        ref items
                    } => populate_additions(&mut local_exports_state,
                            items.iter().map(|node| &node.node().0))?,

                    _ => continue,
                }
            }

            _ => continue,
        }
    }


    let initial_local_exports: HashMap<Ident, Name> = if all {
        top_levels
    } else {
        HashMap::new()
    };

    // Prune the final exports according to the item export states
    let mut final_local_exports = initial_local_exports;
    for (item, export_state) in local_exports_state.into_iter() {
        match export_state {
            ExportState::Export(alias) => {
                let export_item = final_local_exports.get_mut(&item);

                match export_item {
                    Some(export_item) => {
                        // Export under the alias if it exists
                        // Otherwise, do nothing
                        if let Some(alias) = alias {
                            *export_item = Name::Name(alias);
                        }
                    }

                    None => todo!("Trying to export unknown local item '{}'", item),
                }
            }

            ExportState::NoExport => {
                if let None = final_local_exports.remove(&item) {
                    todo!("Trying to un-export local item '{}' but it does not exist", item);
                }
            }
        }
    }
    Ok(ModInterface(final_local_exports))
}

fn populate_excepts<'a, T>(state: &mut HashMap<Ident, ExportState>, except_list: T) -> InterfaceResult<()>
    where T: IntoIterator<Item=&'a ModuleItemData> {

    for item_exception in except_list {
        match item_exception.name_override {
            // An exception to the "all" export
            //   Export instead with the override
            Some(ref item_override) => {
                let override_value = ExportState::Export(Some(item_override.node().clone()));
                if let Some(item_state) = state.get_mut(item_exception.original_name.node()) {
                    *item_state = override_value;
                    continue;
                }

                state.insert(item_exception.original_name.node().clone(), override_value);

            }

            // An exception to the "all" export
            //   Do NOT export at all
            None => {

                let override_value = ExportState::NoExport;
                if let Some(item_state) = state.get_mut(item_exception.original_name.node()) {
                    *item_state = override_value;
                    continue;
                }

                state.insert(item_exception.original_name.node().clone(), override_value);
            }
        }
    }

    Ok(())
}

fn populate_additions<'a, T>(state: &mut HashMap<Ident, ExportState>, list: T) -> InterfaceResult<()>
    where T: IntoIterator<Item=&'a ModuleItemData> {

    for item_addition in list {
        match item_addition.name_override {
            // An exception to the "all" export
            //   Export instead with the override
            Some(ref item_override) => {
                let override_value = ExportState::Export(Some(item_override.node().clone()));
                if let Some(item_state) = state.get_mut(item_addition.original_name.node()) {
                    *item_state = override_value;
                    continue;
                }

                state.insert(item_addition.original_name.node().clone(), override_value);

            }

            // An exception to the "all" export
            //   Do NOT export at all
            None => {

                let override_value = ExportState::Export(None);
                if let Some(item_state) = state.get_mut(item_addition.original_name.node()) {
                    *item_state = override_value;
                    continue;
                }

                state.insert(item_addition.original_name.node().clone(), override_value);
            }
        }
    }

    Ok(())
}
