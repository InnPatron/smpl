use std::collections::{HashMap, HashSet};

use crate::ast::{
    AstNode, BuiltinFunction as AstBuiltinFunction, Ident, UseDecl,
};
use crate::ast::{DeclStmt, Function as AstFunction, Opaque, Struct};
use crate::module::{ModuleSource, ParsedModule};
use crate::span::Span;

use super::analysis_helpers;
use super::control_flow::CFG;
use super::error::AnalysisError;
use super::metadata::*;
use super::resolve_scope::ScopedData;
use super::semantic_data::Module;
use super::semantic_data::*;
use super::type_checker::TypingContext;
use super::type_cons::TypeCons;
use super::type_cons_gen;
use super::analysis_context::*;
use super::analysis_context::SMPLFunction;

use crate::feature::*;

struct UnscopedRawProgram {
    map: HashMap<ModuleId, RawModData>,
}

struct ScopedRawProgram {
    module_map: HashMap<ModuleId, RawModData>,
    scope_map: HashMap<ModuleId, ScopedData>,
}

struct DependentRawProgram {
    module_map: HashMap<ModuleId, RawModData>,
    scope_map: HashMap<ModuleId, ScopedData>,
    dependency_map: HashMap<ModuleId, HashSet<ModuleId>>,
}

struct TypableRawProgram {
    module_map: HashMap<ModuleId, RawModData>,
    scope_map: HashMap<ModuleId, ScopedData>,
    dependency_map: HashMap<ModuleId, HashSet<ModuleId>>,
    type_map: HashMap<TypeId, TypeCons>,
}

struct AnalyzableRawProgram {
    module_map: HashMap<ModuleId, RawModData>,
    scope_map: HashMap<ModuleId, ScopedData>,
    dependency_map: HashMap<ModuleId, HashSet<ModuleId>>,
    type_map: HashMap<TypeId, TypeCons>,
    fn_map: HashMap<FnId, Function>,
}

struct RawProgram {
    scopes: HashMap<ModuleId, ScopedData>,
    dependencies: HashMap<ModuleId, HashSet<ModuleId>>,
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
    modules: Vec<ParsedModule>,
) -> Result<Program, AnalysisError> {

    let mut global_data = GlobalData::new();
    let mut program_origin = {
        let metadata = Metadata::new();
        let universe = Universe::std(&mut global_data);
        let features = PresentFeatures::new();

        Program::new(universe, metadata, features)
    };
    
    let program = &mut program_origin;
    let unscoped_raw_program = raw_mod_data(&mut global_data, modules)?;

    let internally_scoped_raw_program = 
        scope_raw_data_internal(program.universe(), unscoped_raw_program);

    let dependent_raw_program = map_usings(internally_scoped_raw_program)?;

    // Insert modules BEFORE static analysis
    // Anonymous functions are marked as owned by a module during analysis
    // for (name, mod_id) in raw_program.raw_map.iter() {
    //     let module_data = raw_data.get(mod_id).unwrap();

    //     let owned_structs = module_data
    //         .reserved_structs
    //         .iter()
    //         .map(|(_, r)| r.0)
    //         .chain(module_data.reserved_opaque.iter().map(|(_, r)| r.0))
    //         .collect::<HashSet<_>>();
    //     let owned_fns = module_data
    //         .reserved_fns
    //         .iter()
    //         .map(|(_, r)| r.0)
    //         .chain(module_data.reserved_builtins.iter().map(|(_, r)| r.0))
    //         .collect::<HashSet<_>>();

    //     let module_scope = raw_program.scopes.get(&mod_id).unwrap();

    //     // Insert module scope metadata
    //     let module_scope_meta = super::metadata::ModuleScope {
    //         funcs: module_scope
    //             .all_fns()
    //             .map(|(_, fn_id)| fn_id.clone())
    //             .collect(),
    //     };
    //     program
    //         .metadata_mut()
    //         .mod_metadata_mut()
    //         .insert_module_scope(mod_id.clone(), module_scope_meta);

    //     let dependencies = raw_program.dependencies.get(&mod_id).unwrap();

    //     let module = Module {
    //         name: name.clone(),
    //         source: module_data.source.clone(),
    //         id: mod_id.clone(),
    //         module_scope: module_scope.clone(),
    //         owned_types: owned_structs,
    //         owned_fns: owned_fns,
    //         dependencies: dependencies.clone(),
    //     };

    //     program.universe_mut().map_module(mod_id.clone(), name.clone(), module);
    // } 
    
    let typable_raw_program = 
        map_types(program, &mut global_data, dependent_raw_program)?; 
    let analyzable_raw_program = 
        generate_analyzable_fns(&mut global_data, program, typable_raw_program)?;

    for (mod_id, raw_mod) in analyzable_raw_program.module_map.iter() {
        let (universe, metadata, _) = program.analysis_context();
        for (_, reserved_fn) in raw_mod.reserved_fns.iter() {
            let fn_id = reserved_fn.0;

            analysis_helpers::analyze_fn(universe, metadata, &mut global_data, mod_id.clone(), fn_id)?;
        }
    } 

    Ok(program_origin)
}

fn generate_analyzable_fns(
    global_data: &mut GlobalData, 
    program: &mut Program, 
    raw_program: TypableRawProgram)
    -> Result<AnalyzableRawProgram, AnalysisError> {

    let mut fn_map = HashMap::new();
    for (mod_id, raw_mod) in raw_program.module_map.iter() {
        for (_, reserved_fn) in raw_mod.reserved_fns.iter() {
            let mut local_data = LocalData::new();
            let fn_id = reserved_fn.0;
            let fn_span = reserved_fn.1.span();
            let fn_decl = reserved_fn.1.data();
            let fn_name = fn_decl.name.data();
            // TODO: Store new function scope storing the type parameters

            let (universe, metadata, _) = program.analysis_context();

            let fn_type_cons = type_cons_gen::generate_fn_type_cons(
                universe,
                metadata,
                global_data,
                raw_program.scope_map.get(mod_id).unwrap(),
                &TypingContext::empty(),
                fn_id,
                reserved_fn.1.data(),
            )?;

            let analysis_context = analysis_helpers::generate_fn_analysis_data(
                universe,
                global_data,
                &mut local_data,
                raw_program.scope_map.get(mod_id).unwrap(),
                &TypingContext::empty(),
                &fn_type_cons,
                reserved_fn.1.data(),
            )?;

            // TODO: Store local_data? Is it even necessary?
            // TODO: Use anonymous functions
            let (anon_fns, cfg) = CFG::generate(
                program.universe_mut(),
                global_data,
                &mut local_data,
                fn_decl.body.clone(),
                &fn_type_cons,
                &analysis_context,
            )?;

            let fn_type_id = global_data.new_type_id();
            // TODO: Insert fn typing context
            program.universe_mut().manual_insert_type_cons(fn_type_id, fn_type_cons);

            // TODO: Only insert into fn_map, not universe
            assert!(fn_map.insert(fn_id.clone(), Function::SMPL(SMPLFunction {
                fn_id: fn_id.clone(),
                name: fn_name.clone(),
                type_id: fn_type_id.clone(),
                cfg: cfg.clone(),
                analysis_context: analysis_context.clone(),
                span: fn_span.clone(),
            })).is_none());

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

            let (universe, metadata, features) = program.analysis_context();
            let fn_type = type_cons_gen::generate_builtin_fn_type(
                universe,
                metadata,
                features,
                global_data,
                raw_program.scope_map.get(mod_id).unwrap(),
                &TypingContext::empty(),
                fn_id,
                reserved_builtin.1.data(),
            )?;

            let fn_type_id = global_data.new_type_id();
            universe
                .manual_insert_type_cons(fn_type_id, fn_type);

            features.add_feature(BUILTIN_FN);

            // TODO: Only insert into fn_map, not universe
            assert!(fn_map.insert(fn_id.clone(), Function::Builtin(BuiltinFunction {
                fn_id: fn_id.clone(),
                name: fn_name.clone(),
                type_id: fn_type_id.clone(),
            })).is_none());

            universe.insert_builtin_fn(
                fn_id,
                fn_name.clone(),
                fn_type_id,
            ); 

            metadata.insert_builtin(fn_id);
            metadata.insert_module_fn(
                mod_id.clone(),
                fn_name.clone(),
                fn_id,
            );
            metadata.set_fn_annotations(
                fn_id,
                &reserved_builtin.1.data().annotations,
            );
        }
    }

    Ok(AnalyzableRawProgram {
        module_map: raw_program.module_map,
        scope_map: raw_program.scope_map,
        dependency_map: raw_program.dependency_map,
        type_map: raw_program.type_map,
        fn_map: fn_map,
    })
}

fn map_usings(
    internally_scoped: ScopedRawProgram,
) -> Result<DependentRawProgram, AnalysisError> {

    let module_map = internally_scoped.module_map;
    let internally_scoped_map = internally_scoped.scope_map;
    let mut fully_scoped_map = internally_scoped_map.clone();

    let module_name_map: HashMap<Ident, ModuleId> = module_map
        .iter()
        .map(|(id, raw_module)| {

            (raw_module.name.data().clone(), id.clone())
        })
    .collect();

    let mut dependency_map: HashMap<ModuleId, HashSet<ModuleId>> = HashMap::new();
    for (id, raw_mod) in module_map.iter() {

        let mut dependencies = HashSet::new();

        for use_decl in raw_mod.uses.iter() {
            let import_name = use_decl.data().0.data();
            let import_id = module_name_map
                .get(import_name)
                .ok_or({
                    let (ident, span) = use_decl.data().0.clone().to_data();
                    AnalysisError::UnresolvedUses(vec![(ident, span)])
                })?;

            dependencies.insert(import_id.clone());
            // Get imported module's types and functions
            let (all_types, all_fns) = {
                let imported_scope = internally_scoped_map
                    .get(import_id)
                    .unwrap();

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

            let current_module_scope = fully_scoped_map
                .get_mut(&id)
                .unwrap();

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

        dependency_map.insert(id.clone(), dependencies);
    }

    Ok(DependentRawProgram {
        module_map: module_map,
        scope_map: fully_scoped_map,
        dependency_map: dependency_map,
    })
}

fn raw_mod_data(
    global_data: &mut GlobalData,
    modules: Vec<ParsedModule>,
) -> Result<UnscopedRawProgram, AnalysisError> {

    use super::error::TopLevelError;

    let mut mod_map = HashMap::new();
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
                                global_data.new_type_id(),
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
                            ReservedFn(global_data.new_fn_id(), d),
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
                                global_data.new_fn_id(),
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
                                global_data.new_type_id(),
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

        // TODO Map module name to id
        // program
        //     .metadata_mut()
        //     .mod_metadata_mut()
        //     .map_module(raw.name.data().clone(), module.id);

        let id = raw.id;
        mod_map.insert(id, raw);
    }


    Ok(UnscopedRawProgram {
        map: mod_map    
    })
}

fn scope_raw_data_internal(universe: &Universe, unscoped_raw_program: UnscopedRawProgram) 
    -> ScopedRawProgram {

    let scope_map = unscoped_raw_program.map.iter()
        .map(|(id, raw_mod_data)| {
            let mut scope = universe.std_scope();
            map_internal_data(&mut scope, raw_mod_data);

            (id.clone(), scope)
        })
    .collect();

    ScopedRawProgram {
       module_map: unscoped_raw_program.map,
       scope_map: scope_map
    }
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

/// Insert types into the Universe and a separate type map
fn map_types(program: &mut Program, 
    global_data: &mut GlobalData,
    raw_program: DependentRawProgram) 

    -> Result<TypableRawProgram, AnalysisError> {

    let mut type_map = HashMap::new();
    // Map ALL structs into the universe before generating functions
    for (mod_id, raw_mod) in raw_program.module_map.iter() {
        for (_, reserved_struct) in raw_mod.reserved_structs.iter() {
            let type_id = reserved_struct.0;
            let (struct_type, field_ordering) =
                type_cons_gen::generate_struct_type_cons(
                    program.universe(),
                    global_data,
                    type_id,
                    raw_program.scope_map.get(mod_id).unwrap(),
                    &TypingContext::empty(),
                    reserved_struct.1.data(),
                )?;

            // TODO: Insert type constructors into program? Or just type map
            assert!(type_map.insert(type_id, struct_type.clone()).is_none());
            program.universe_mut().manual_insert_type_cons(type_id, struct_type);

            let field_ordering = FieldOrdering::new(type_id, field_ordering);
            program.metadata_mut()
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
                global_data,
                type_id,
                raw_program.scope_map.get(mod_id).unwrap(),
                &TypingContext::empty(),
                reserved_opaque.1.data(),
            )?;

            assert!(type_map.insert(type_id, opaque_type_cons.clone()).is_none());
            program.universe_mut().manual_insert_type_cons(type_id, opaque_type_cons);
        }
    }

    // TODO: Use type map outside of map types?
    Ok(TypableRawProgram {
        module_map: raw_program.module_map,
        scope_map: raw_program.scope_map,
        dependency_map: raw_program.dependency_map,
        type_map: type_map,
    })

}
