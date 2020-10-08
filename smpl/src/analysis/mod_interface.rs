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

        if let Some(conflict) = $mi.0.insert(name_store.into_ident(), new_name) {
            todo!("Conflicting local top-levels: {}", conflict);
        }
    }
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
fn make_local_interface(current_mod_name: &Ident, current_module: &mut ParsedModule) -> InterfaceResult<ModInterface> {
    let mut current_mi = ModInterface(HashMap::new());

    let mut all = false;
    let mut export_state: HashMap<Ident, Name> = HashMap::new();
    for decl in current_module.module.top_levels.iter_mut() {
        match decl {
            DeclStmt::Struct(ref mut decl_node) => {
                //let mut name_store = mod_item!(current_mod_name => decl_node.node().name.node().clone());
                //let new_name = name_store.clone();
                //mem::swap(&mut name_store, decl_node.node_mut().name.node_mut());

                //if let Some(conflict) = current_mi.0.insert(name_store.into_ident(), new_name) {
                //    todo!("Conflicting local top-levels");
                //}

                try_insert!(current_mi; {
                    decl_node.node().name.node().clone(),
                    decl_node.node_mut().name.node_mut(),
                }; [LOCAL current_mod_name]);
            }

            DeclStmt::Opaque(ref mut decl_node) => {
                try_insert!(current_mi; {
                    decl_node.node().name.node().clone(),
                    decl_node.node_mut().name.node_mut(),
                }; [LOCAL current_mod_name]);
            }

            DeclStmt::Function(ref mut decl_node) => {
                try_insert!(current_mi; {
                    decl_node.node().name.node().clone(),
                    decl_node.node_mut().name.node_mut(),
                }; [LOCAL current_mod_name]);
            }

            DeclStmt::BuiltinFunction(ref mut decl_node) => {
                try_insert!(current_mi; {
                    decl_node.node().name.node().clone(),
                    decl_node.node_mut().name.node_mut(),
                }; [LOCAL current_mod_name]);
            }

            DeclStmt::Export(ref export) => {
                match export.node() {

                }
            }

            _ => continue,
        }
    }

    Ok(current_mi)
}
