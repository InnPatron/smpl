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
    Export(Option<Name>),
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

///
/// Merge a foreign module's interface based on the export statements of the current module
///
fn merge_foreign_interface(foreign_mod_name: &Ident, current_module: &ParsedModule, current_mi: &mut ModInterface, foreign_mi: &ModInterface) -> InterfaceResult<()> {

    let mut all = false;
    // Keys are the foreign keys
    // ExportStates are the local re-export states
    let mut local_reexport_states: HashMap<Ident, ExportState> = HashMap::new();
    for decl in current_module.module.top_levels.iter() {
        match decl {
            DeclStmt::Export(ref export) => {
                match export.node() {
                    ExportDecl::ExportAll {
                        from_module: Some(ref from_module),
                        ref except,
                    } => {
                        if from_module.node() != foreign_mod_name {
                            continue;
                        }

                        all = true;
                        populate_excepts(&mut local_reexport_states,
                            except.iter().map(|node| &node.node().0))?;
                    }

                    ExportDecl::ExportItems {
                        from_module: Some(ref from_module),
                        ref items
                    } => {
                        if from_module.node() != foreign_mod_name {
                            continue;
                        }

                        populate_additions(&mut local_reexport_states,
                            items.iter().map(|node| &node.node().0))?;
                    }

                    _ => continue,
                }
            }

            _ => continue,
        }
    }

    let foreign_keys_to_search = if all  {
        Box::new(foreign_mi.0.iter().map(|(i, _)| i)) as Box<dyn Iterator<Item=_>>
    } else {
        Box::new(local_reexport_states.iter().map(|(i, _)| i)) as Box<dyn Iterator<Item=_>>
    };

    // Loop through all the foreign keys to search
    //   Add to the current module interface depending on the all flag and the export state
    //   The new items are foreign Name::ModuleItem's
    let current_mi: &mut HashMap<Ident, Name> = &mut current_mi.0;
    for foreign_key in foreign_keys_to_search {

        let foreign_item = match foreign_mi.0.get(foreign_key) {
            Some(foreign_item) => foreign_item,
            None => todo!("Foreign module does not have '{}' in its interface", foreign_key),
        };
        match local_reexport_states.get(foreign_key) {

            // Not re-exporting all symbols from the foreign module and
            //   no export state was found
            None if !all => continue,


            // Explicitly not exporting this foreign item
            Some(ExportState::NoExport) => continue,


            // No export state but re-exporting all anyways
            // NOTE: order matters
            None => {
                if let Some(_conflict) = current_mi.insert(foreign_key.clone(), foreign_item.clone()) {
                    todo!("Conflict with key '{}'", foreign_key);
                }
            }

            Some(ExportState::Export(ref alias)) => {

                let key = match alias {
                    Some(alias) => get_item(alias),
                    None => foreign_key,
                };
                if let Some(_conflict) = current_mi.insert(key.clone(), foreign_item.clone()) {
                    todo!("Conflict with key '{}'", key);
                }
            }
        }
    }

    Ok(())
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

    let mut initial_local_exports: HashMap<Ident, Name> = if all {
        top_levels
    } else {
        HashMap::new()
    };

    export_state_merge(&mut initial_local_exports, local_exports_state)?;

    Ok(ModInterface(initial_local_exports))
}

fn export_state_merge(initial_exports: &mut HashMap<Ident, Name>, export_states: HashMap<Ident, ExportState>) -> InterfaceResult<()> {

    // Prune the final exports according to the item export states
    let mut final_local_exports = initial_exports;
    for (item, export_state) in export_states.into_iter() {
        match export_state {
            ExportState::Export(alias) => {
                let export_item = final_local_exports.get_mut(&item);

                match export_item {
                    Some(export_item) => {
                        // Export under the alias if it exists
                        // Otherwise, do nothing
                        if let Some(alias) = alias {
                            *export_item = alias;
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

    Ok(())
}

fn populate_excepts<'a, T>(state: &mut HashMap<Ident, ExportState>, except_list: T) -> InterfaceResult<()>
    where T: IntoIterator<Item=&'a ModuleItemData> {

    for item_exception in except_list {
        match item_exception.name_override {
            // An exception to the "all" export
            //   Export instead with the override
            Some(ref item_override) => {
                let override_value = ExportState::Export(Some(item_override.node().clone().into()));
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
                let override_value = ExportState::Export(Some(item_override.node().clone().into()));
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

fn get_item(n: &Name) -> &Ident {
    match n {
        Name::Name(ref i) => i,
        Name::ModuleItem {
            ref item,
            ..
        } => item,
        _ => panic!("No item for {}", n),
    }
}
