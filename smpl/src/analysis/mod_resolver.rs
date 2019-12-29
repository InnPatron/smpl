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
use super::semantic_data::{ AnonymousFn as ResolvedAnonymousFn, BuiltinFunction, Function};
use super::type_checker::TypingContext;
use super::type_cons::TypeCons;
use super::type_cons_gen;
use super::analysis_context::*;
use super::anon_storage::AnonStorage;

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

pub(super) struct AnalyzableRawProgram {
    pub(super) module_map: HashMap<ModuleId, RawModData>,
    pub(super) scope_map: HashMap<ModuleId, ScopedData>,
    pub(super) dependency_map: HashMap<ModuleId, HashSet<ModuleId>>,
    pub(super) type_map: HashMap<TypeId, TypeCons>,
    pub(super) fn_map: HashMap<FnId, AnalyzableFn>,
    pub(super) anon_fns: AnonStorage<ReservedAnonymousFn>,
    pub(super) anon_fn_parents: AnonStorage<FnId>,
    pub(super) local_data_map: HashMap<FnId, LocalData>,
}

struct RawProgram {
    scopes: HashMap<ModuleId, ScopedData>,
    dependencies: HashMap<ModuleId, HashSet<ModuleId>>,
    raw_map: HashMap<Ident, ModuleId>,
}

pub(super) struct RawModData {
    pub(super) source: ModuleSource,
    pub(super) name: AstNode<Ident>,
    pub(super) id: ModuleId,
    pub(super) reserved_opaque: HashMap<Ident, ReservedOpaque>,
    pub(super) reserved_structs: HashMap<Ident, ReservedStruct>,
    pub(super) reserved_fns: HashMap<Ident, ReservedFn>,
    pub(super) reserved_builtins: HashMap<Ident, ReservedBuiltinFn>,
    pub(super) uses: Vec<AstNode<UseDecl>>,
}

pub(super) struct ReservedOpaque(pub(super) TypeId, pub(super) AstNode<Opaque>);
pub(super) struct ReservedStruct(pub(super) TypeId, pub(super) AstNode<Struct>);
pub(super) struct ReservedFn(pub(super) FnId, pub(super) AstNode<AstFunction>, pub(super) TypeId,);
pub(super) struct ReservedBuiltinFn(pub(super) FnId, pub(super) AstNode<AstBuiltinFunction>, pub(super)TypeId);

///
/// Perform static analysis on a collection of SMPL modules.
///
/// 1. Convert the parsed modules into raw module data
/// 2. Generate module-level scopes for each module.
///    Abstractly map top-level type and function declarations into scope.
/// 3. Generate module dependencies and bring external module symbols
///    into scope.
/// 4. Generate type constructors from type declarations.
///    Insert type constructors into the universe.
/// 5. Based off of the module-level scope and universe, process top-level function
///    declarations by:
///      1. Creating and inserting their type constructors into the universe.
///      2. Converting the function body into a CFG with pseudo-ANF expressions.
///         This conversion captures the AST of nested anonymous functions.
/// 6. Perform program-wide static analysis:
///      1. Perform scope resolution, type checking, and branch tracing on
///         all top-level functions.
///      2. Capture a snapshot of the scope and typing context whenever an anonymous
///         function is created.
///      3. Mark these anonymous functions as reserved in the universe.
///      4. Initialize a worklist with these reserved anonymous functions.
///      5. Until the worklist is empty:
///         1. Perform scope resolution, type checking, and branch tracing on
///            the next anonymous function to resolve.
///         2. Capture a snapshot of the scope and typing context whenever an anonymous
///            function is created.
///         3. Mark nested anonymous functions as reserved in the universe.
///         4. Add these nested anonymous functions to the worklist.
///
pub fn check_modules(
    modules: Vec<ParsedModule>,
) -> Result<Program, AnalysisError> {

    let mut global_data = GlobalData::new();
    let mut universe = AnalysisUniverse::std(&mut global_data);
    let mut features = PresentFeatures::new();
    let mut metadata = Metadata::new();

    // Gather raw mod data
    let unscoped_raw_program = raw_mod_data(&mut global_data, modules)?;

    // Abstractly map top-level functions and type names
    let internally_scoped_raw_program =
        scope_raw_data_internal(&universe, unscoped_raw_program);

    // Calculate dependencies and bring other modules into scope
    let dependent_raw_program = map_usings(internally_scoped_raw_program)?;

    // Read raw type declarations and insert their type constructors into the universe
    let typable_raw_program =
        map_types(&mut universe, &mut metadata, &mut global_data, dependent_raw_program)?;

    // Convert top-level functions into an analyzable form
    let analyzable_raw_program =
        generate_analyzable_fns(&mut universe,
            &mut metadata,
            &mut features,
            &mut global_data,
            typable_raw_program)?;

    // Perform program-wide static analysis
    let universe = analyze_program(
        universe,
        &mut metadata,
        global_data,
        analyzable_raw_program
    )?;

    let _ =
        super::metadata_collectors::collect_metadata_post(&mut metadata, &universe)
        .expect("Unhandled metadata errors");

    Ok(Program::new(universe, metadata, features))
}

fn analyze_program(
    mut universe: AnalysisUniverse,
    metadata: &mut Metadata,
    mut global_data: GlobalData,
    analyzable_raw_program: AnalyzableRawProgram,
    ) -> Result<Universe, AnalysisError> {

        let module_map =
            module_ownership(&analyzable_raw_program);

        let _ =
            super::metadata_collectors::collect_metadata(metadata, &analyzable_raw_program)
            .expect("Unhandled metadata errors");

        let (fn_map, type_map, anon_ownership) =
            analyze_fns(
                &mut universe,
                metadata,
                &mut global_data,
                analyzable_raw_program)?;

        let module_map = {
            let mut module_map = module_map;
            for (fn_id, mod_id) in anon_ownership.into_iter() {
                let module = module_map.get_mut(&mod_id).unwrap();
                module.owned_fns.insert(fn_id);
            }

            module_map
        };

        let fn_map = fn_map
            .into_iter()
            .map(|(fn_id, func)| {
                let func = match func {
                    AnalyzableFn::Anonymous(AnalyzableAnonymousFn::Reserved(..)) => {
                        panic!("Expected all anonymous functions to be resolved after analysis");
                    }

                    AnalyzableFn::Anonymous(AnalyzableAnonymousFn::Resolved(resolved)) => {
                        Function::Anonymous(resolved)
                    }

                    AnalyzableFn::SMPL(s) => Function::SMPL(s),

                    AnalyzableFn::Builtin(b) => Function::Builtin(b),
                };

                (fn_id, func)
            })
        .collect::<HashMap<FnId, Function>>();

        let builtin_fn_set = fn_map
            .iter()
            .filter_map(|(fn_id, func)| {
                match func {
                    Function::Builtin(..)   => Some(fn_id.clone()),
                    _                       => None,
                }
            })
        .collect::<HashSet<FnId>>();

        let module_name_map = module_map
            .iter()
            .map(|(mod_id, module)| {
                (module.name.clone(), mod_id.clone())
            })
        .collect::<HashMap<Ident, ModuleId>>();

        let int = universe.int();
        let float = universe.float();
        let string = universe.string();
        let boolean = universe.boolean();
        let unit = universe.unit();

        Ok(Universe {
            module_map,
            fn_map,
            type_map,
            builtin_fn_set,
            module_name_map,
            int,
            float,
            string,
            boolean,
            unit,
        })
}

fn module_ownership(
    raw_program: &AnalyzableRawProgram,
    ) -> HashMap<ModuleId, Module> {

    // Insert modules BEFORE static analysis
    // Anonymous functions are marked as owned by a module during analysis
    let mut module_map = HashMap::new();
    for (mod_id, module_data) in raw_program.module_map.iter() {

        let name = module_data.name.data();
        let owned_structs = module_data
            .reserved_structs
            .iter()
            .map(|(_, r)| r.0)
            .chain(module_data.reserved_opaque.iter().map(|(_, r)| r.0))
            .collect::<HashSet<_>>();
        let owned_fns = module_data
            .reserved_fns
            .iter()
            .map(|(_, r)| r.0)
            .chain(module_data.reserved_builtins.iter().map(|(_, r)| r.0))
            .collect::<HashSet<_>>();

        let module_scope = raw_program.scope_map
            .get(&mod_id)
            .unwrap();

        let dependencies = raw_program.dependency_map
            .get(&mod_id)
            .unwrap();

        let module = Module {
            name: name.clone(),
            source: module_data.source.clone(),
            id: mod_id.clone(),
            module_scope: module_scope.clone(),
            owned_types: owned_structs,
            owned_fns: owned_fns,
            dependencies: dependencies.clone(),
        };

        module_map.insert(mod_id.clone(), module);
    }

    module_map
}

fn analyze_fns(
    universe: &mut AnalysisUniverse,
    metadata: &mut Metadata,
    global_data: &mut GlobalData,
    mut analyzable_raw_program: AnalyzableRawProgram,
    ) -> Result<(HashMap<FnId, AnalyzableFn>, HashMap<TypeId, TypeCons>, Vec<(FnId, ModuleId)>), AnalysisError> {

    let mut finished: HashMap<FnId, AnalyzableFn> = HashMap::new();

    let mut reserved_anon_fns: AnonStorage<ReservedAnonymousFn> =
        analyzable_raw_program.anon_fns;

    // Insert reserved anonymous functions into the Universe
    // Required to generate type constructors during type checking for parent functions
    for (fn_id, reserved_anon_fn) in reserved_anon_fns.data() {
        universe.insert_anon_fn(fn_id, AnalyzableAnonymousFn::Reserved(reserved_anon_fn));
    }
    reserved_anon_fns = AnonStorage::new();

    let mut unresolved_anon_fns: AnonStorage<(AnalysisContext, TypeCons, ModuleId)> =
        AnonStorage::new();

    for (mod_id, raw_mod) in analyzable_raw_program.module_map.into_iter() {
        for (_, reserved_fn) in raw_mod.reserved_fns.into_iter() {
            let fn_id = reserved_fn.0;

            let mut fn_to_analyze = analyzable_raw_program
                .fn_map
                .remove(&fn_id)
                .expect(&format!("Missing analyzable function for {}", fn_id));

            let local_data = analyzable_raw_program
                .local_data_map
                .get_mut(&fn_id)
                .expect(&format!("Missing local data function for {}", fn_id));


            let mut this_anon_fns =
                analysis_helpers::analyze_fn(
                    &mut fn_to_analyze,
                    universe,
                    metadata,
                    global_data,
                    local_data,
                    mod_id.clone(),
                )?;

            unresolved_anon_fns.append(&mut this_anon_fns);

            finished.insert(fn_id, fn_to_analyze);
        }

        // Add builtin functions to function map
        for (_, reserved_builtin_fn) in raw_mod.reserved_builtins.into_iter() {
            let fn_id = reserved_builtin_fn.0;
            let type_id = reserved_builtin_fn.2;

            let name: Ident = reserved_builtin_fn.1
                .data()
                .name
                .data()
                .clone();


            let func = AnalyzableFn::Builtin(BuiltinFunction {
                fn_id,
                type_id,
                name
            });

            finished.insert(fn_id, func);
        }
    }

    let anon_ownership = resolve_anonymous_fns(
        universe,
        metadata,
        global_data,
        &mut finished,
        analyzable_raw_program.local_data_map,
        analyzable_raw_program.anon_fn_parents,
        unresolved_anon_fns,
        reserved_anon_fns)?;

    Ok((finished, analyzable_raw_program.type_map, anon_ownership))
}

fn resolve_anonymous_fns(
    universe: &mut AnalysisUniverse,
    metadata: &mut Metadata,
    global_data: &mut GlobalData,
    finished: &mut HashMap<FnId, AnalyzableFn>,
    mut local_data_map: HashMap<FnId, LocalData>,
    mut anon_fn_parents: AnonStorage<FnId>,
    mut unresolved_anon_fns: AnonStorage<(AnalysisContext, TypeCons, ModuleId)>,
    mut reserved_anon_fns: AnonStorage<ReservedAnonymousFn>,
    ) -> Result<Vec<(FnId, ModuleId)>, AnalysisError> {

    // TODO: Return parent map?

    let mut anon_ownership: Vec<(FnId, ModuleId)> = Vec::new();

    // Insert reserved anonymous functions into the Universe
    // Required to generate type constructors during type checking for parent functions
    for (fn_id, reserved_anon_fn) in reserved_anon_fns.data() {
        universe.insert_anon_fn(fn_id, AnalyzableAnonymousFn::Reserved(reserved_anon_fn));
    }

    // Analyze all currently unresolved anonymous functions.
    //   Any nested anonymous functions are analyzed on the next iteration
    //   and so on until there are no more unresolved anonymous functions.
    // Guarenteed to terminate because parsed anonymous functions cannot be
    //   infinitely nested.
    loop {

        if unresolved_anon_fns.len() == 0 {
            break;
        }

        let mut anon_fns_to_resolve = unresolved_anon_fns.data();

        unresolved_anon_fns = AnonStorage::new();

        for (to_resolve_fn_id, (analysis_context, type_cons, mod_id)) in anon_fns_to_resolve {

            // Track which module owns the anonymous function
            anon_ownership.push((to_resolve_fn_id, mod_id));

            let reserved_anon_fn = universe.get_reserved_anon_fn(to_resolve_fn_id);

            let span = reserved_anon_fn
                .span();

            let anon_fn_decl = reserved_anon_fn
                .ast();

            let type_id = global_data.new_type_id();
            let parent_fn_id = anon_fn_parents
                .get(to_resolve_fn_id)
                .clone();
            let local_data = local_data_map
                .get_mut(&parent_fn_id)
                .expect("Missing local data for function");

            let (mut nested_unresolved_anon_fns, cfg) = CFG::generate(
                universe,
                global_data,
                local_data,
                anon_fn_decl.data().body.clone(),
                &type_cons,
                &analysis_context,
            )?;

            // Reserve nested unresolved anonymous functions
            // Insert reserved anonymous functions into the Universe
            //   Required to generate type constructors during type checking for parent functions
            for (nested_anon_fn_id, reserved_anon_fn) in nested_unresolved_anon_fns.data() {
                anon_fn_parents
                    .insert(nested_anon_fn_id.clone(), parent_fn_id);

                universe
                    .insert_anon_fn(nested_anon_fn_id,
                        AnalyzableAnonymousFn::Reserved(reserved_anon_fn));
            }

            // Insert anonymous function type constructors into the universe
            universe
                .manual_insert_type_cons(type_id, type_cons);

            let mut to_analyze = AnalyzableFn::Anonymous(
                AnalyzableAnonymousFn::Resolved(
                    ResolvedAnonymousFn {
                        span,
                        type_id,
                        cfg,
                        analysis_context,
                    }));

            let mut this_unresolved_anon_fns =
                analysis_helpers::analyze_fn(
                    &mut to_analyze,
                    universe,
                    metadata,
                    global_data,
                    local_data,
                    mod_id.clone(),
                )?;

            // TODO: Need to change the 'reserved' anonymous function
            //   'resolved' within AnalysisUniverse?
            finished
                .insert(to_resolve_fn_id, to_analyze);

            // Mark nested anonymous functions as unresolved
            unresolved_anon_fns.append(&mut this_unresolved_anon_fns);
        }
    }

    Ok(anon_ownership)
}

///
/// Creates an AnalyzableRawProgram
///
/// Also inserts type constructors for SMPL functions (including builtins)
///
fn generate_analyzable_fns(
    universe: &mut AnalysisUniverse,
    metadata: &mut Metadata,
    features: &mut PresentFeatures,
    global_data: &mut GlobalData,
    raw_program: TypableRawProgram)
    -> Result<AnalyzableRawProgram, AnalysisError> {

    let mut local_data_map = HashMap::new();
    let mut anon_fn_parent_buff = AnonStorage::new();
    let mut anon_fn_buff = AnonStorage::new();
    let mut fn_map = HashMap::new();
    for (mod_id, raw_mod) in raw_program.module_map.iter() {
        for (_, reserved_fn) in raw_mod.reserved_fns.iter() {
            let mut local_data = LocalData::new();
            let fn_id = reserved_fn.0;
            let fn_span = reserved_fn.1.span();
            let fn_decl = reserved_fn.1.data();
            let fn_name = fn_decl.name.data();
            // TODO: Store new function scope storing the type parameters

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
            let (mut anon_fns, cfg) = CFG::generate(
                universe,
                global_data,
                &mut local_data,
                fn_decl.body.clone(),
                &fn_type_cons,
                &analysis_context,
            )?;

            // Get reference to anonymous function's enclosing function for access
            //   to its LocalData
            let parent_fn_id = fn_id;
            for (anon_fn_id, _) in anon_fns.ref_data() {
                anon_fn_parent_buff.insert(anon_fn_id, parent_fn_id);
            }

            anon_fn_buff.append(&mut anon_fns);
            local_data_map.insert(parent_fn_id, local_data);

            let fn_type_id = reserved_fn.2;
            // TODO: Insert fn typing context
            universe
                .insert_fn_type_cons(fn_id, fn_type_id, fn_type_cons);

            // TODO: Only insert into fn_map, not universe
            assert!(fn_map.insert(fn_id.clone(), AnalyzableFn::SMPL(SMPLFunction {
                fn_id: fn_id.clone(),
                name: fn_name.clone(),
                type_id: fn_type_id.clone(),
                cfg: cfg.clone(),
                analysis_context: analysis_context.clone(),
                span: fn_span.clone(),
            })).is_none());

            metadata
                .insert_module_fn(
                mod_id.clone(),
                fn_name.clone(),
                fn_id,
            );
            metadata
                .set_fn_annotations(fn_id, &reserved_fn.1.data().annotations);
        }

        for (_, reserved_builtin) in raw_mod.reserved_builtins.iter() {
            let fn_id = reserved_builtin.0;
            let fn_decl = reserved_builtin.1.data();
            let fn_name = fn_decl.name.data();

            let type_cons = type_cons_gen::generate_builtin_fn_type(
                universe,
                metadata,
                features,
                global_data,
                raw_program.scope_map.get(mod_id).unwrap(),
                &TypingContext::empty(),
                fn_id,
                reserved_builtin.1.data(),
            )?;

            let type_id = reserved_builtin.2;
            universe
                .insert_fn_type_cons(fn_id, type_id, type_cons);

            features.add_feature(BUILTIN_FN);

            // TODO: Only insert into fn_map, not universe
            assert!(fn_map.insert(fn_id.clone(), AnalyzableFn::Builtin(BuiltinFunction {
                fn_id: fn_id.clone(),
                name: fn_name.clone(),
                type_id: type_id.clone(),
            })).is_none());

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
        anon_fns: anon_fn_buff,
        anon_fn_parents: anon_fn_parent_buff,
        local_data_map,
    })
}

///
/// Reads a module's top-level using statements.
/// If the using statement refers to an existing module, bring that module into scope
///   while noting that 'used' module as a dependency.
///
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

/// Splits ParsedModule's into a UnscopedRawProgram
/// Per module:
///   1) Checks for top-level name conflicts
///   2) Assigns a TypeID to each type declaration
///   3) Collects top-level use statements
///   4) Assigns the module a ModuleID
fn raw_mod_data(
    global_data: &mut GlobalData,
    modules: Vec<ParsedModule>,
) -> Result<UnscopedRawProgram, AnalysisError> {

    use super::error::TopLevelError;

    let mut mod_map = HashMap::new();
    for module in modules {
        let mut opaque_reserve: HashMap<Ident, ReservedOpaque> = HashMap::new();
        let mut struct_reserve: HashMap<Ident, ReservedStruct> = HashMap::new();
        let mut fn_reserve: HashMap<Ident, ReservedFn>= HashMap::new();
        let mut builtin_fn_reserve: HashMap<Ident, ReservedBuiltinFn> = HashMap::new();
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
                            ReservedFn(global_data.new_fn_id(), d, global_data.new_type_id()),
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
                                global_data.new_type_id(),
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

///
/// Creates a module-level scope per each module based off of the standard scope
///   of the Universe and the module's top-level declarations.
///
fn scope_raw_data_internal(universe: &AnalysisUniverse, unscoped_raw_program: UnscopedRawProgram)
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

///
/// Used by scope_raw_data_internal().
///
/// Reads a module's top-level declarations and maps them into the module's scope.
///
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

/// Insert type constructors into the Universe and a separate type map
fn map_types(
    universe: &mut AnalysisUniverse,
    metadata: &mut Metadata,
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
                    universe,
                    global_data,
                    type_id,
                    raw_program.scope_map.get(mod_id).unwrap(),
                    &TypingContext::empty(),
                    reserved_struct.1.data(),
                )?;

            // TODO: Insert type constructors into program? Or just type map
            assert!(type_map.insert(type_id, struct_type.clone()).is_none());
            universe.manual_insert_type_cons(type_id, struct_type);

            let field_ordering = FieldOrdering::new(type_id, field_ordering);
            metadata
                .insert_field_ordering(type_id, field_ordering);
            metadata
                .set_struct_annotations(
                type_id,
                &reserved_struct.1.data().annotations,
            );
        }

        for (_, reserved_opaque) in raw_mod.reserved_opaque.iter() {
            let type_id = reserved_opaque.0;
            let opaque_type_cons = type_cons_gen::generate_opaque_type_cons(
                universe,
                global_data,
                type_id,
                raw_program.scope_map.get(mod_id).unwrap(),
                &TypingContext::empty(),
                reserved_opaque.1.data(),
            )?;

            assert!(type_map.insert(type_id, opaque_type_cons.clone()).is_none());
            universe
                .manual_insert_type_cons(type_id, opaque_type_cons);
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
