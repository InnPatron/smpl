use std::collections::HashMap;

use crate::ast::{
    AstNode, BuiltinFunction as AstBuiltinFunction, Ident, UseDecl,
};
use crate::ast::{DeclStmt, Function as AstFunction, Opaque, Struct};
use crate::module::{ModuleSource, ParsedModule};

use super::analysis_helpers;
use super::control_flow::CFG;
use super::error::AnalysisError;
use super::metadata::*;
use super::resolve_scope::ScopedData;
use super::semantic_data::Module;
use super::semantic_data::*;
use super::type_checker::TypingContext;
use super::type_cons_gen;

use crate::feature::*;

struct RawProgram {
    scopes: HashMap<ModuleId, ScopedData>,
    dependencies: HashMap<ModuleId, Vec<ModuleId>>,
    raw_map: HashMap<Ident, ModuleId>,
}

struct RawModData {
    source: ModuleSource,
    name: AstNode<Ident>,
    id: ModuleId,
    reserved_opaque: HashMap<Ident, ReservedOpaque>,
    reserved_structs: HashMap<Ident, ReservedStruct>,
    reserved_fns: HashMap<Ident, ReservedFn>,
    reserved_builtins: HashMap<Ident, ReservedBuiltinFn>,
    uses: Vec<AstNode<UseDecl>>,
}

struct ReservedOpaque(TypeId, AstNode<Opaque>);
struct ReservedStruct(TypeId, AstNode<Struct>);
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
            .mod_metadata_mut()
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
        for (_, reserved_struct) in raw_mod.reserved_structs.iter() {
            let type_id = reserved_struct.0;
            let (struct_type, field_ordering) =
                type_cons_gen::generate_struct_type_cons(
                    program,
                    type_id,
                    raw_program.scopes.get(mod_id).unwrap(),
                    &TypingContext::empty(),
                    reserved_struct.1.data(),
                )?;

            program
                .universe_mut()
                .manual_insert_type_cons(type_id, struct_type);

            let field_ordering = FieldOrdering::new(type_id, field_ordering);
            program
                .metadata_mut()
                .insert_field_ordering(type_id, field_ordering);
            program.metadata_mut().set_struct_annotations(
                type_id,
                &reserved_struct.1.data().annotations,
            );
        }

        for (_, reserved_opaque) in raw_mod.reserved_opaque.iter() {
            let type_id = reserved_opaque.0;
            let opaque_type_cons = type_cons_gen::generate_opaque_type_cons(
                program,
                type_id,
                raw_program.scopes.get(mod_id).unwrap(),
                &TypingContext::empty(),
                reserved_opaque.1.data(),
            )?;

            program
                .universe_mut()
                .manual_insert_type_cons(type_id, opaque_type_cons);
        }
    }

    for (mod_id, raw_mod) in raw_data.iter() {
        for (_, reserved_fn) in raw_mod.reserved_fns.iter() {
            let fn_id = reserved_fn.0;
            let fn_span = reserved_fn.1.span();
            let fn_decl = reserved_fn.1.data();
            let fn_name = fn_decl.name.data();
            // TODO: Store new function scope storing the type parameters

            let (universe, metadata, _) = program.analysis_context();

            let fn_type_cons = type_cons_gen::generate_fn_type_cons(
                universe,
                metadata,
                raw_program.scopes.get(mod_id).unwrap(),
                &TypingContext::empty(),
                fn_id,
                reserved_fn.1.data(),
            )?;

            let analysis_context = analysis_helpers::generate_fn_analysis_data(
                universe,
                raw_program.scopes.get(mod_id).unwrap(),
                &TypingContext::empty(),
                &fn_type_cons,
                reserved_fn.1.data(),
            )?;

            let cfg = CFG::generate(
                program.universe_mut(),
                fn_decl.body.clone(),
                &fn_type_cons,
                &analysis_context,
            )?;

            // TODO: Insert fn typing context
            let fn_type_id =
                program.universe_mut().insert_type_cons(fn_type_cons);

            program.universe_mut().insert_fn(
                fn_id,
                fn_name.clone(),
                fn_type_id,
                analysis_context,
                cfg,
                fn_span,
            );
            program.metadata_mut().insert_module_fn(
                mod_id.clone(),
                fn_name.clone(),
                fn_id,
            );
            program
                .metadata_mut()
                .set_fn_annotations(fn_id, &reserved_fn.1.data().annotations);
        }

        for (_, reserved_builtin) in raw_mod.reserved_builtins.iter() {
            let fn_id = reserved_builtin.0;
            let fn_decl = reserved_builtin.1.data();
            let fn_name = fn_decl.name.data();

            let fn_type = type_cons_gen::generate_builtin_fn_type(
                program,
                raw_program.scopes.get(mod_id).unwrap(),
                &TypingContext::empty(),
                fn_id,
                reserved_builtin.1.data(),
            )?;

            let fn_type_id = program.universe_mut().insert_type_cons(fn_type);

            program.features_mut().add_feature(BUILTIN_FN);

            program.universe_mut().insert_builtin_fn(
                fn_id,
                fn_name.clone(),
                fn_type_id,
            );

            program.metadata_mut().insert_builtin(fn_id);
            program.metadata_mut().insert_module_fn(
                mod_id.clone(),
                fn_name.clone(),
                fn_id,
            );
            program.metadata_mut().set_fn_annotations(
                fn_id,
                &reserved_builtin.1.data().annotations,
            );
        }
    }

    for (_mod_id, raw_mod) in raw_data.iter() {
        let (universe, metadata, _) = program.analysis_context();
        for (_, reserved_fn) in raw_mod.reserved_fns.iter() {
            let fn_id = reserved_fn.0;

            analysis_helpers::analyze_fn(universe, metadata, fn_id)?;
        }
    }

    for (name, mod_id) in raw_program.raw_map.into_iter() {
        let module_data = raw_data.remove(&mod_id).unwrap();

        let owned_structs = module_data
            .reserved_structs
            .into_iter()
            .map(|(_, r)| r.0)
            .chain(module_data.reserved_opaque.iter().map(|(_, r)| r.0))
            .collect::<Vec<_>>();
        let owned_fns = module_data
            .reserved_fns
            .into_iter()
            .map(|(_, r)| r.0)
            .chain(module_data.reserved_builtins.into_iter().map(|(_, r)| r.0))
            .collect::<Vec<_>>();

        let module_scope = raw_program.scopes.remove(&mod_id).unwrap();

        // Insert module scope metadata
        let module_scope_meta = super::metadata::ModuleScope {
            funcs: module_scope
                .all_fns()
                .map(|(_, fn_id)| fn_id.clone())
                .collect(),
        };
        program
            .metadata_mut()
            .mod_metadata_mut()
            .insert_module_scope(mod_id, module_scope_meta);

        let dependencies = raw_program.dependencies.remove(&mod_id).unwrap();

        let module = Module::new(
            name.clone(),
            module_data.source,
            module_scope,
            owned_structs,
            owned_fns,
            dependencies,
            mod_id,
        );

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
                .ok_or({
                    let (ident, span) = use_decl.data().0.clone().to_data();
                    AnalysisError::UnresolvedUses(vec![(ident, span)])
                })?;

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
    // TODO: Perform name collision check here?
    for (_ident, r) in raw.reserved_structs.iter() {
        scope.insert_type_cons(
            r.1.data().name.data().clone().into(),
            r.0.clone(),
        );
    }

    // TODO: Perform name collision check here?
    for (_ident, r) in raw.reserved_opaque.iter() {
        scope.insert_type_cons(
            r.1.data().name.data().clone().into(),
            r.0.clone(),
        );
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
) -> Result<
    (HashMap<ModuleId, RawModData>, Vec<(ModuleId, ModuleSource)>),
    AnalysisError,
> {
    use super::error::TopLevelError;

    let mut mod_map = HashMap::new();
    let mut source_map = Vec::new();

    for module in modules {
        let mut opaque_reserve = HashMap::new();
        let mut struct_reserve = HashMap::new();
        let mut fn_reserve = HashMap::new();
        let mut builtin_fn_reserve = HashMap::new();
        let mut uses = Vec::new();

        let ast_module = module.module;
        for decl_stmt in ast_module.1.into_iter() {
            match decl_stmt {
                DeclStmt::Struct(d) => {
                    let span = d.data().name.span();
                    let name = d.data().name.data().clone();
                    if struct_reserve
                        .insert(
                            name.clone(),
                            ReservedStruct(
                                program.universe_mut().new_type_id(),
                                d,
                            ),
                        )
                        .is_some()
                        || opaque_reserve.contains_key(&name)
                    {
                        return Err(TopLevelError::DuplicateTypes(name, span).into());
                    }
                }

                DeclStmt::Function(d) => {
                    let span = d.data().name.span();
                    let name = d.data().name.data().clone();
                    if fn_reserve
                        .insert(
                            name.clone(),
                            ReservedFn(program.universe_mut().new_fn_id(), d),
                        )
                        .is_some()
                        || builtin_fn_reserve.contains_key(&name)
                    {
                        return Err(TopLevelError::DuplicateFns(name, span).into());
                    }
                }

                DeclStmt::BuiltinFunction(d) => {
                    let span = d.data().name.span();
                    let name = d.data().name.data().clone();
                    if builtin_fn_reserve
                        .insert(
                            name.clone(),
                            ReservedBuiltinFn(
                                program.universe_mut().new_fn_id(),
                                d,
                            ),
                        )
                        .is_some()
                        || fn_reserve.contains_key(&name)
                    {
                        return Err(TopLevelError::DuplicateFns(name, span).into());
                    }
                }

                DeclStmt::Use(u) => {
                    uses.push(u);
                }

                DeclStmt::Opaque(o) => {
                    let span = o.data().name.span();
                    let name = o.data().name.data().clone();
                    if opaque_reserve
                        .insert(
                            name.clone(),
                            ReservedOpaque(
                                program.universe_mut().new_type_id(),
                                o,
                            ),
                        )
                        .is_some()
                        || struct_reserve.contains_key(&name)
                    {
                        return Err(TopLevelError::DuplicateTypes(name, span).into());
                    }
                }
            }
        }

        let raw = RawModData {
            source: module.source.clone(),
            name: ast_module.0.ok_or(AnalysisError::MissingModName)?,
            id: module.id,
            reserved_opaque: opaque_reserve,
            reserved_structs: struct_reserve,
            reserved_fns: fn_reserve,
            reserved_builtins: builtin_fn_reserve,
            uses: uses,
        };

        // Map module name to id
        program
            .metadata_mut()
            .mod_metadata_mut()
            .map_module(raw.name.data().clone(), module.id);

        let id = raw.id;
        mod_map.insert(id, raw);
        source_map.push((id, module.source));
    }

    Ok((mod_map, source_map))
}
