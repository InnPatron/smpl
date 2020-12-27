use std::collections::{HashMap, HashSet};

use crate::ast::{
    self, Decl as AstDecl, ExportItem, Ident, ImportItem,
    LocalDecl as AstLocalDecl, Module as AstModule,
    ModuleInst as AstModuleInst, ModuleItemData, Name,
};
use crate::ast_node::AstNode;
use crate::span::Span;

use super::error::AnalysisError;
use super::UnanalyzedProgram;

#[derive(Debug, Clone)]
pub enum DepKind {
    Import(ImportSrc, Import),
    Export(ExportSrc, Export),
}

pub type ImportSrc = AstNode<AstModuleInst>;

#[derive(Debug, Clone)]
pub enum ExportSrc {
    Module(AstNode<AstModuleInst>),
    SelfMod,
}

#[derive(Debug, Clone)]
pub enum Export {
    All { except: Vec<AstNode<ImportItem>> },
    Items { items: Vec<AstNode<ExportItem>> },
}

#[derive(Debug, Clone)]
pub enum Import {
    All { except: Vec<AstNode<ImportItem>> },
    Items { items: Vec<AstNode<ImportItem>> },
    Module { alias: Option<AstNode<Ident>> },
}

#[derive(Debug, Clone)]
pub struct ModBuilders {
    map: HashMap<Ident, ModBuilder>,
}

impl ModBuilders {
    fn insert(
        &mut self,
        i: AstNode<Ident>,
        mb: ModBuilder,
    ) -> Result<(), AnalysisError> {
        let (i, span) = i.into_data();
        match self.map.insert(i.clone(), mb) {
            Some(_) => panic!("Duplicate module {} at {:?}", i, span),
            None => Ok(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModBuilder {
    name: AstNode<Ident>,

    deps: Vec<DepKind>,

    /// Local-items WITHOUT exports/imports
    total_local_items: HashMap<Ident, AstLocalDecl>,
}

#[derive(Debug, Clone)]
pub struct ModuleInst {
    /// Exported items
    items: HashMap<Ident, Name>,
}

// Only used to check for cylcic module dependencies
struct DepGraph {
    graph: HashMap<Ident, Vec<AstNode<Ident>>>,
}

impl DepGraph {
    fn insert(&mut self, m: Ident, dep: AstNode<Ident>) {
        use std::collections::hash_map::Entry;
        match self.graph.entry(m) {
            Entry::Occupied(ref mut o) => {
                o.get_mut().push(dep);
            }

            Entry::Vacant(v) => {
                v.insert(vec![dep]);
            }
        }
    }
}

///
/// 1) Transforms all top-level local module items `Name`s into `Name::ModuleItem`
///
/// 2) Scan through the program and for each module, create a `ModBuilder`
///    for each module
///
///    `ModBuilder` stores:
///      1) Local declarations/items
///      2) Local dependencies
///      3) Mod signatures
///      4) Mod parameters
///
///     From this, the `ModBuilder` can be instantiated
///
///
/// X) Generates each module's top-level scope based on the import/export graph
///
pub fn gen_mod_graph(p: UnanalyzedProgram) -> Result<(), AnalysisError> {
    let (dep_graph, mod_builders) = process_analyzed_program(p)?;
    todo!();
}

///
/// * Transforms all top-level local module items `Name`s into `Name::ModuleItem`
///
/// * Scan through the program and for each module, create a ModBuilder
///     to use for instantiation
///
///     * While traversing the program, also create a dependency graph (taking into
///         account module uses, including inner instantiations)
fn process_analyzed_program(
    mut p: UnanalyzedProgram,
) -> Result<(DepGraph, ModBuilders), AnalysisError> {
    let mut dep_graph = DepGraph {
        graph: HashMap::new(),
    };

    let mut builders = ModBuilders {
        map: HashMap::new(),
    };

    for (_source, m) in p.modules.into_iter() {
        // TODO: Store mod parameters + declared signatures
        let (mod_decl, _) = m
            .mod_decl
            .expect("No mod-decl. WF should not have passed")
            .into_data();

        let curr_mod_name = mod_decl.mod_name.clone();

        let mut builder = ModBuilder {
            name: curr_mod_name.clone(),
            total_local_items: HashMap::new(),
            deps: Vec::new(),
        };

        for mut d in m.decls.into_iter() {
            use ast::Decl;
            match d {
                Decl::Import(i) => {
                    // TODO: store export Span?
                    let import = ast_import_to_internal(
                        curr_mod_name.data(),
                        i.into_data().0,
                        &mut dep_graph,
                    );
                    builder.deps.push(DepKind::Import(import.0, import.1));
                }

                Decl::Export(e) => {
                    // TODO: store export Span?
                    let export = ast_export_to_internal(
                        curr_mod_name.data(),
                        e.into_data().0,
                        &mut dep_graph,
                    );
                    builder.deps.push(DepKind::Export(export.0, export.1));
                }

                Decl::Local(mut l) => {
                    let local_name = l.name().data().clone().into_ident();
                    l.set_mod(curr_mod_name.data().clone());
                    builder.total_local_items.insert(local_name, l);
                }
            }
        }

        builders.insert(curr_mod_name, builder)?;
    }

    Ok((dep_graph, builders))
}

/// ast::ImportDecl to internal import repr (Import)
/// Also collects the dependency module names into the DepGraph
fn ast_import_to_internal(
    curr: &Ident,
    i: ast::ImportDecl,
    deps: &mut DepGraph,
) -> (AstNode<AstModuleInst>, self::Import) {
    use ast::ImportDecl;
    match i {
        ImportDecl::ImportItems { module, items } => {
            deps_from_mod_inst(curr, deps, &module);
            (module, Import::Items { items })
        }

        ImportDecl::ImportModule { module, alias } => {
            deps_from_mod_inst(curr, deps, &module);
            (module, Import::Module { alias })
        }

        ImportDecl::ImportAll { module, except } => {
            deps_from_mod_inst(curr, deps, &module);
            (module, Import::All { except })
        }
    }
}

/// ast::ExportDecl to internal export repr (Export)
/// Also collects the dependency module names into the DepGraph
fn ast_export_to_internal(
    curr: &Ident,
    e: ast::ExportDecl,
    deps: &mut DepGraph,
) -> (ExportSrc, self::Export) {
    use ast::ExportDecl;

    match e {
        ExportDecl::ExportItems { from_module, items } => (
            from_module
                .map(|m| {
                    deps_from_mod_inst(curr, deps, &m);
                    ExportSrc::Module(m)
                })
                .unwrap_or(ExportSrc::SelfMod),
            Export::Items { items },
        ),

        ExportDecl::ExportAll {
            from_module,
            except,
        } => (
            from_module
                .map(|m| {
                    deps_from_mod_inst(curr, deps, &m);
                    ExportSrc::Module(m)
                })
                .unwrap_or(ExportSrc::SelfMod),
            Export::All { except },
        ),
    }
}

fn deps_from_mod_inst(
    curr: &Ident,
    deps: &mut DepGraph,
    node_inst: &AstNode<AstModuleInst>,
) {
    let inst = node_inst.data();

    deps.insert(curr.clone(), inst.module.clone());

    for a in inst.args.iter() {
        deps_from_mod_inst(curr, deps, a);
    }
}
