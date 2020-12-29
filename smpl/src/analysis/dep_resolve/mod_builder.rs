use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{
    self, ExportDecl, ExportItem, Ident, ImportDecl, ImportItem, ModParam,
    Module, ModulePath, Name, SigConstraint,
};
use crate::ast_node::{AstNode, Spanned};
use crate::ast_visitor::*;
use crate::expr_ast::*;
use crate::span::Span;

use super::{DepResult, Error as DepError};

///
/// Representation of an n-ary functor/module (including n=0)
///
#[derive(Debug)]
pub struct ModBuilder {
    data: Rc<ModData>,
    ast: Rc<Module>,
}

#[derive(Debug)]
pub struct ModData {
    path: ModulePath,
    params: Vec<AstNode<ModParam>>,
    declared_sigs: Vec<AstNode<Name>>,

    // Unapplied module dependencies (i.e. `self::module_param` keys are still in the maps)
    import_module: HashMap<ModInstKey, Vec<Option<AstNode<Ident>>>>,
    import_all: HashMap<ModInstKey, Vec<AstNode<ImportItem>>>,
    import_items: HashMap<ModInstKey, Vec<AstNode<ImportItem>>>,
    export_all: HashMap<ModInstKey, Vec<AstNode<ExportItem>>>,
    export_items: HashMap<ModInstKey, Vec<AstNode<ExportItem>>>,
}

impl ModBuilder {
    /// Assumes all modules have already been flattened
    ///
    /// Extracts the module declaration and exports/imports
    ///   Expects exports/imports to be normalized
    ///
    /// NOTES:
    ///  * To access module paramaters, you need to use `self::PARAM_NAME`
    ///  * Does NOT check for conflicting import/export names
    ///  * Does NOT check that modules instances/functors actually exist
    pub fn new(path: ModulePath, mut ast: Module) -> Self {
        let (mod_decl, _span) = ast
            .mod_decl
            .take()
            .expect("No mod decl: not WF")
            .into_data();

        let mut data = ModData {
            path,
            params: mod_decl.mod_params,
            declared_sigs: mod_decl.declared_sigs,

            import_module: HashMap::new(),
            import_all: HashMap::new(),
            import_items: HashMap::new(),

            export_all: HashMap::new(),
            export_items: HashMap::new(),
        };

        // Map out all of the module's dependencies (even inner-scope ones)
        // ModInstKey => DepData
        walk_module(&mut data, &ast)
            .expect("Collecting imports/exports should not have an error");

        ModBuilder {
            data: Rc::new(data),
            ast: Rc::new(ast),
        }
    }

    /// Applies the given module instantiations to this ModBuilder
    ///
    /// NOTES:
    ///  * Only checks arity
    ///  * Does NOT check that modules instances/functors actually exist
    ///  * Module signature conformance handled later (when we have type info/resolved
    ///    dependencies)
    pub fn apply(&self, args: Vec<ModInstKey>) -> DepResult<ModInst> {
        if args.len() != self.data.params.len() {
            todo!("Attempting to apply incorrect number of modules ({}) to module {} (expecting {})",
                args.len(), self.data.path, self.data.params.len());
        }

        let mut import_module = self.data.import_module.clone();
        let mut import_all = self.data.import_all.clone();
        let mut import_items = self.data.import_items.clone();
        let mut export_all = self.data.export_all.clone();
        let mut export_items = self.data.export_items.clone();

        // Replace all `self::mod_param` with the corresponding argument
        for (param_key, arg) in self.data.params.iter().zip(args.iter()) {
            let inst = ast::ModuleInst {
                module: AstNode::new(
                    ModulePath::mod_param_path(
                        param_key.data().name.data().clone(),
                        param_key.span(),
                    ),
                    param_key.span(),
                ),
                args: vec![],
            };

            let param_key = ast_mod_inst_to_key(&inst);
            // Use `apply_map` b/c some arguments may be the same module
            apply_map(&mut import_module, &param_key, arg.clone())?;
            apply_map(&mut import_all, &param_key, arg.clone())?;
            apply_map(&mut import_items, &param_key, arg.clone())?;
            apply_map(&mut export_all, &param_key, arg.clone())?;
            apply_map(&mut export_items, &param_key, arg.clone())?;
        }

        Ok(ModInst {
            data: self.data.clone(),
            ast: self.ast.clone(),
            args,
            import_module,
            import_all,
            import_items,
            export_all,
            export_items,
        })
    }
}

fn ast_mod_inst_to_key(mi: &ast::ModuleInst) -> ModInstKey {
    ModInstKey {
        module: mi.module.data().clone(),
        args: mi
            .args
            .iter()
            .map(|arg| ast_mod_inst_to_key(arg.data()))
            .collect(),
    }
}

/// A module instance
/// Uniquely ID's by the functor path and its ModInst arguments
pub struct ModInst {
    data: Rc<ModData>,
    ast: Rc<Module>,
    args: Vec<ModInstKey>,

    // Applied module dependencies (i.e. `self::module_param` keys have been replaced with their
    //  corresponding arguments)
    import_module: HashMap<ModInstKey, Vec<Option<AstNode<Ident>>>>,
    import_all: HashMap<ModInstKey, Vec<AstNode<ImportItem>>>,
    import_items: HashMap<ModInstKey, Vec<AstNode<ImportItem>>>,
    export_all: HashMap<ModInstKey, Vec<AstNode<ExportItem>>>,
    export_items: HashMap<ModInstKey, Vec<AstNode<ExportItem>>>,
}

impl Visitor for ModData {
    type E = DepError;

    fn visit_export(
        &mut self,
        e: &AstNode<ExportDecl>,
    ) -> VisitorResult<Self::E> {
        match e.data() {
            ExportDecl::ExportAll {
                from_module: Some(ref from_module),
                ref except,
            } => {
                union_map(
                    &mut self.export_all,
                    ast_mod_inst_to_key(from_module.data()),
                    except.clone().into_iter(),
                );
            }

            ExportDecl::ExportItems {
                from_module: Some(ref from_module),
                ref items,
            } => {
                union_map(
                    &mut self.export_items,
                    ast_mod_inst_to_key(from_module.data()),
                    items.clone().into_iter(),
                );
            }

            _ => unreachable!("from_module should already be normalized"),
        }

        Ok(())
    }

    fn visit_import(
        &mut self,
        i: &AstNode<ImportDecl>,
    ) -> VisitorResult<Self::E> {
        match i.data() {
            ImportDecl::ImportModule {
                ref module,
                ref alias,
            } => {
                union_map(
                    &mut self.import_module,
                    ast_mod_inst_to_key(module.data()),
                    std::iter::once(alias.clone()),
                );
            }

            ImportDecl::ImportAll {
                ref module,
                ref except,
            } => {
                union_map(
                    &mut self.import_all,
                    ast_mod_inst_to_key(module.data()),
                    except.clone().into_iter(),
                );
            }

            ImportDecl::ImportItems {
                ref module,
                ref items,
            } => {
                union_map(
                    &mut self.import_items,
                    ast_mod_inst_to_key(module.data()),
                    items.clone().into_iter(),
                );
            }
        }

        Ok(())
    }
}

/// Module instances are uniquely ID's by the functor's path and the module instance arguments
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModInstKey {
    module: ModulePath,
    args: Vec<ModInstKey>,
}

impl std::fmt::Display for ModInstKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}(", self.module)?;

        for arg in self.args.iter() {
            write!(f, "{},", arg)?;
        }

        Ok(())
    }
}

/// Inserts data at a given key
/// If the key already exists, extend its data with the new data
fn union_map<V, I: Iterator<Item = V>>(
    map: &mut HashMap<ModInstKey, Vec<V>>,
    key: ModInstKey,
    mut v: I,
) {
    use std::collections::hash_map::Entry;
    match map.entry(key) {
        Entry::Occupied(ref mut occupied) => {
            occupied.get_mut().extend(v);
        }

        Entry::Vacant(vacant) => {
            vacant.insert(v.collect());
        }
    }
}

/// Replaces the `param_key` with the `replacement` key
/// If the `replacement` key already is in the map, union the data of both
///   the `replacement` and `param_key`
fn apply_map<V>(
    map: &mut HashMap<ModInstKey, Vec<V>>,
    param_key: &ModInstKey,
    replacement: ModInstKey,
) -> DepResult<()> {
    match map.remove(param_key) {
        Some(data) => {
            union_map(map, replacement, data.into_iter());
            Ok(())
        }

        None => todo!("Mod param {} not used in the module", param_key),
    }
}
