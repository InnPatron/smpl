use std::collections::HashMap;

use crate::ast::{AstNode, BuiltinFunction as AstBuiltinFunction, Ident, UseDecl};
use crate::ast::{DeclStmt, Function as AstFunction, Struct};
use crate::module::{ModuleSource, ParsedModule};

use super::control_flow::CFG;
use super::cyclic_type_ck::cyclic_type_check;
use super::error::AnalysisError;
use super::fn_analyzer::analyze_fn;
use super::metadata::*;
use super::semantic_data::Module;
use super::semantic_data::*;
use super::type_cons_gen::*;

use crate::feature::*;

struct RawProgram {
    scopes: HashMap<ModuleId, ScopedData>,
    dependencies: HashMap<ModuleId, Vec<ModuleId>>,
    raw_map: HashMap<Ident, ModuleId>,
}

struct RawModData {
    name: AstNode<Ident>,
    id: ModuleId,
    reserved_structs: HashMap<Ident, ReservedType>,
    reserved_fns: HashMap<Ident, ReservedFn>,
    reserved_builtins: HashMap<Ident, ReservedBuiltinFn>,
    uses: Vec<AstNode<UseDecl>>,
}

struct ReservedType(TypeId, AstNode<Struct>);
struct ReservedFn(FnId, AstNode<AstFunction>);
struct ReservedBuiltinFn(FnId, AstNode<AstBuiltinFunction>);

pub fn check_modules(
    program: &mut Program,
    modules: Vec<ParsedModule>,
) -> Result<(), AnalysisError> {
    let (mut raw_data, sources) = raw_mod_data(program, modules)?;

    let mut mapped_raw = HashMap::new();
    let mut scopes = HashMap::new();

    // Map module IDs to sources
    for (mod_id, source) in sources.into_iter() {
        program
            .metadata_mut()
            .insert_mod_source(mod_id.clone(), source);
    }

    // Map reserved data
    for (mod_id, raw) in raw_data.iter() {
        // Map reserved data
        let mut scope = program.universe().std_scope();
        map_internal_data(&mut scope, raw);

        mapped_raw.insert(raw.name.data().clone(), mod_id.clone());
        scopes.insert(mod_id.clone(), scope);
    }

    let mut raw_program = RawProgram {
        scopes: scopes,
        raw_map: mapped_raw,
        dependencies: HashMap::new(),
    };

    map_usings(&raw_data, &mut raw_program)?;

    // Map ALL structs into the universe before generating functions
    for (mod_id, raw_mod) in raw_data.iter() {
        for (_, reserved_type) in raw_mod.reserved_structs.iter() {
            let type_id = reserved_type.0;
            let (struct_type, field_ordering) = generate_struct_type_cons(
                program,
                type_id,
                raw_program.scopes.get(mod_id).unwrap(),
                reserved_type.1.data(),
            )?;

            program
                .universe_mut()
                .manual_insert_type_cons(type_id, struct_type);

            let field_ordering = FieldOrdering::new(type_id, field_ordering);
            program
                .metadata_mut()
                .insert_field_ordering(type_id, field_ordering);
            program
                .metadata_mut()
                .set_struct_annotations(type_id, &reserved_type.1.data().annotations);
        }
    }

    for (mod_id, raw_mod) in raw_data.iter() {
        for (_, reserved_fn) in raw_mod.reserved_fns.iter() {
            let fn_id = reserved_fn.0;
            let fn_decl = reserved_fn.1.data();
            // TODO: Store new function scope storing the type parameters
            let (fn_scope, fn_type) = generate_fn_type(
                program,
                raw_program.scopes.get(mod_id).unwrap(),
                fn_id,
                reserved_fn.1.data(),
            )?;

            let cfg = CFG::generate(
                program.universe_mut(),
                fn_decl.body.clone(),
                &fn_type,
                &fn_scope,
            )?;

            let fn_type_id = program.universe_mut().insert_type_cons(fn_type);

            program
                .universe_mut()
                .insert_fn(fn_id, fn_type_id, fn_scope, cfg);
            program.metadata_mut().insert_module_fn(
                mod_id.clone(),
                fn_decl.name.data().clone(),
                fn_id,
            );
            program
                .metadata_mut()
                .set_fn_annotations(fn_id, &reserved_fn.1.data().annotations);
        }

        for (_, reserved_builtin) in raw_mod.reserved_builtins.iter() {
            let fn_id = reserved_builtin.0;
            let fn_decl = reserved_builtin.1.data();
            let fn_type = generate_builtin_fn_type(
                program,
                raw_program.scopes.get(mod_id).unwrap(),
                fn_id,
                reserved_builtin.1.data(),
            )?;

            let fn_type_id = program.universe_mut().insert_type_cons(fn_type);

            program.features_mut().add_feature(BUILTIN_FN);

            program.universe_mut().insert_builtin_fn(fn_id, fn_type_id);

            program.metadata_mut().insert_builtin(fn_id);
            program.metadata_mut().insert_module_fn(
                mod_id.clone(),
                fn_decl.name.data().clone(),
                fn_id,
            );
            program
                .metadata_mut()
                .set_fn_annotations(fn_id, &reserved_builtin.1.data().annotations);
        }
    }

    cyclic_type_check(program)?;

    for (mod_id, raw_mod) in raw_data.iter() {
        for (_, reserved_fn) in raw_mod.reserved_fns.iter() {
            let fn_id = reserved_fn.0;
            analyze_fn(program, fn_id, mod_id.clone())?;
        }
    }

    for (name, mod_id) in raw_program.raw_map.into_iter() {
        let module_data = raw_data.remove(&mod_id).unwrap();

        let owned_structs = module_data
            .reserved_structs
            .into_iter()
            .map(|(_, r)| r.0)
            .collect::<Vec<_>>();
        let owned_fns = module_data
            .reserved_fns
            .into_iter()
            .map(|(_, r)| r.0)
            .chain(module_data.reserved_builtins.into_iter().map(|(_, r)| r.0))
            .collect::<Vec<_>>();

        let module_scope = raw_program.scopes.remove(&mod_id).unwrap();

        let dependencies = raw_program.dependencies.remove(&mod_id).unwrap();

        let module = Module::new(module_scope, owned_structs, owned_fns, dependencies, mod_id);

        program.universe_mut().map_module(mod_id, name, module);
    }

    Ok(())
}

fn map_usings(
    raw_modules: &HashMap<ModuleId, RawModData>,
    raw_prog: &mut RawProgram,
) -> Result<(), AnalysisError> {
    for (id, raw_mod) in raw_modules {
        let mut dependencies = Vec::new();
        for use_decl in raw_mod.uses.iter() {
            let import_name = use_decl.data().0.data();
            let import_id = raw_prog
                .raw_map
                .get(import_name)
                .ok_or(AnalysisError::UnresolvedUses(vec![use_decl.clone()]))?;

            dependencies.push(import_id.clone());
            // Get imported module's types and functions
            let (all_types, all_fns) = {
                let imported_scope = raw_prog.scopes.get(import_id).unwrap();
                let all_types = imported_scope
                    .all_types()
                    .into_iter()
                    .map(|(path, id)| {
                        let mut path = path.clone();
                        path.0.insert(0, import_name.clone());

                        (path, id.clone())
                    })
                    .collect::<HashMap<_, _>>();
                let all_fns = imported_scope
                    .all_fns()
                    .into_iter()
                    .map(|(path, id)| {
                        let mut path = path.clone();
                        path.0.insert(0, import_name.clone());

                        (path, id.clone())
                    })
                    .collect::<HashMap<_, _>>();

                (all_types, all_fns)
            };

            let current_module_scope = raw_prog.scopes.get_mut(id).unwrap();

            // Bring imported types into scope
            for (path, imported) in all_types.into_iter() {
                if current_module_scope
                    .insert_type_cons(path.clone().into(), imported)
                    .is_some()
                {
                    panic!("Should not have overrwritten {}. Paths should be unique by prefixing with the originating module.", path);
                }
            }

            // Bring imported functions into scope
            for (path, imported) in all_fns.into_iter() {
                current_module_scope.insert_fn(path, imported);
            }
        }

        raw_prog.dependencies.insert(id.clone(), dependencies);
    }

    Ok(())
}

fn map_internal_data(scope: &mut ScopedData, raw: &RawModData) {
    for (_ident, r) in raw.reserved_structs.iter() {
        scope.insert_type_cons(r.1.data().name.data().clone().into(), r.0.clone());
    }

    for (_ident, r) in raw.reserved_fns.iter() {
        scope.insert_fn(r.1.data().name.data().clone().into(), r.0.clone());
    }

    for (_ident, r) in raw.reserved_builtins.iter() {
        scope.insert_fn(r.1.data().name.data().clone().into(), r.0.clone());
    }
}

fn raw_mod_data(
    program: &mut Program,
    modules: Vec<ParsedModule>,
) -> Result<(HashMap<ModuleId, RawModData>, Vec<(ModuleId, ModuleSource)>), AnalysisError> {
    let mut mod_map = HashMap::new();
    let mut source_map = Vec::new();

    for module in modules { 

        let mut struct_reserve = HashMap::new();
        let mut fn_reserve = HashMap::new();
        let mut builtin_fn_reserve = HashMap::new();
        let mut uses = Vec::new();

        let ast_module = module.module;
        for decl_stmt in ast_module.1.into_iter() {
            match decl_stmt {
                DeclStmt::Struct(d) => {
                    struct_reserve.insert(
                        d.data().name.data().clone().clone(),
                        ReservedType(program.universe_mut().new_type_id(), d),
                    );
                }

                DeclStmt::Function(d) => {
                    fn_reserve.insert(
                        d.data().name.data().clone(),
                        ReservedFn(program.universe_mut().new_fn_id(), d),
                    );
                }

                DeclStmt::BuiltinFunction(d) => {
                    builtin_fn_reserve.insert(
                        d.data().name.data().clone(),
                        ReservedBuiltinFn(program.universe_mut().new_fn_id(), d),
                    );
                }

                DeclStmt::Use(u) => {
                    uses.push(u);
                }
            }
        }

        let raw = RawModData {
            name: ast_module.0.ok_or(AnalysisError::MissingModName)?,
            id: module.id,
            reserved_structs: struct_reserve,
            reserved_fns: fn_reserve,
            reserved_builtins: builtin_fn_reserve,
            uses: uses,
        };

        // Map module name to id
        program
            .metadata_mut()
            .map_module(raw.name.data().clone(), module.id);

        let id = raw.id;
        mod_map.insert(id, raw);
        source_map.push((id, module.source));
    }

    Ok((mod_map, source_map))
}
