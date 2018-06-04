use std::collections::HashMap;
use std::cell::Cell;
use std::rc::Rc;

use err::Err;
use ast::{ModulePath as AstModulePath, Path, DeclStmt, Struct, Function as AstFunction, Module as AstModule, BuiltinFunction as AstBuiltinFunction, BuiltinFnParams};
use ast::{ AstNode, Ident, UseDecl};

use super::feature_checkers::*;
use super::metadata::*;
use super::smpl_type::*;
use super::semantic_data::*;
use super::semantic_data::Module;
use super::control_flow::CFG;
use super::fn_analyzer::analyze_fn;

struct RawProgram<'a> {
    scopes: HashMap<ModuleId, ScopedData>,
    raw_map: HashMap<&'a Ident, ModuleId>,
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
struct ReservedFn(FnId, TypeId, AstNode<AstFunction>);
struct ReservedBuiltinFn(FnId, TypeId, AstNode<AstBuiltinFunction>);

pub fn check_modules(program: &mut Program, modules: Vec<AstModule>) -> Result<(), Err> {
    let raw_data = raw_mod_data(program, modules);

    let mut mapped_raw = HashMap::new();
    let mut scopes = HashMap::new();

    // Map reserved data
    for (mod_id, raw) in raw_data.iter() {
        let mut scope = program.universe().std_scope();
        map_internal_data(&mut scope, raw);

        mapped_raw.insert(raw.name.data(), mod_id.clone());
        scopes.insert(mod_id.clone(), scope);
    }

    let mut raw_program = RawProgram {
        scopes: scopes,
        raw_map: mapped_raw
    };

    map_usings(&raw_data, &mut raw_program)?;

    for (mod_id, raw_mod) in raw_data.iter() {
        for (_, reserved_type) in raw_mod.reserved_structs.iter() {
            let type_id = reserved_type.0;
            let (struct_type, field_ordering) = generate_struct_type(program, 
                                                               raw_program.scopes.get(mod_id).unwrap(), 
                                                               reserved_type.1.data())?;

            program.universe_mut().insert_type(type_id, SmplType::Struct(struct_type));

            let field_ordering = FieldOrdering::new(type_id, field_ordering);
            program.metadata_mut().insert_field_ordering(type_id, field_ordering);
        }
    }

    Ok(())
}

fn generate_struct_type(program: &mut Program, scope: &ScopedData, struct_def: &Struct) -> Result<(StructType, Vec<FieldId>), Err> {
    let (universe, metadata, features) = program.analysis_context();

    let mut fields = HashMap::new();
    let mut field_map = HashMap::new();
    let mut order = Vec::new();
    if let Some(ref body) = struct_def.body.0 {
        for field in body.iter() {
            let f_id = universe.new_field_id();
            let f_name = field.name.data().clone();
            let f_type_path = &field.field_type;
            let path_data = f_type_path.data();
            let field_type = scope.type_id(universe, path_data.into())?;
            fields.insert(f_id, field_type);
            field_map.insert(f_name, f_id);
            order.push(f_id);

            field_type_scanner(universe, features, field_type);
        }
    } 

    let struct_t = StructType {
        name: struct_def.name.data().clone(),
        fields: fields,
        field_map: field_map,
    };

    Ok((struct_t, order))
}

fn map_usings(raw_modules: &HashMap<ModuleId, RawModData>, raw_prog: &mut RawProgram) -> Result<(), Err> {
    for (id, raw_mod) in raw_modules {
        for use_decl in raw_mod.uses.iter() {
            let import_name = use_decl.data().0.data();
            let import_id = raw_prog.raw_map.get(import_name).ok_or(Err::UnresolvedUses(vec![use_decl.clone()]))?;

            // Get imported module's types and functions
            let (all_types, all_fns) = {
                let imported_scope = raw_prog.scopes.get(import_id).unwrap();
                let all_types = imported_scope.all_types()
                                              .into_iter()
                                              .map(|(path, id)| {
                                                  let mut path = path.clone();
                                                  path.0.insert(0, import_name.clone());
                                                  
                                                  (path, id.clone())
                                              })
                                              .collect::<HashMap<_, _>>();
                let all_fns = imported_scope.all_fns()
                                            .into_iter()
                                            .map(|(path, id)| {
                                                let mut path = path.clone();
                                                path.0.insert(0, import_name.clone());
                                                  
                                                (path, id.clone())
                                            })
                                            .collect::<HashMap<_,_>>();

                (all_types, all_fns)
            };

            let current_module_scope = raw_prog.scopes.get_mut(id).unwrap();

            // Bring imported types into scope
            for (path, imported) in all_types.into_iter() {
                if current_module_scope.insert_type(path.clone().into(), imported).is_some() {
                    panic!("Should not have overrwritten {}. Paths should be unique by prefixing with the originating module.", path);
                }
            }

            // Bring imported functions into scope
            for(path, imported) in all_fns.into_iter() {
                current_module_scope.insert_fn(path, imported);
            }
        }
    }

    Ok(())
}

fn map_internal_data(scope: &mut ScopedData, raw: &RawModData) {
    for (ident, r) in raw.reserved_structs.iter() {
        scope.insert_type(r.1.data().name.data().clone().into(), r.0.clone());
    }

    for (ident, r) in raw.reserved_fns.iter() {
        scope.insert_fn(r.2.data().name.data().clone().into(), r.0.clone());
    }

    for (ident, r) in raw.reserved_builtins.iter() {
        scope.insert_fn(r.2.data().name.data().clone().into(), r.0.clone());
    }
}

fn raw_mod_data(program: &mut Program, modules: Vec<AstModule>) -> HashMap<ModuleId, RawModData> {
    let universe = program.universe_mut();
    let mut mod_map = HashMap::new();
    for module in modules {
        let mut struct_reserve = HashMap::new();
        let mut fn_reserve = HashMap::new();
        let mut builtin_fn_reserve = HashMap::new();
        let mut uses = Vec::new();

        for decl_stmt in module.1.into_iter() {
            match decl_stmt {
                DeclStmt::Struct(d) => {
                    struct_reserve.insert(d.data().name.data().clone().clone(), 
                                          ReservedType(universe.new_type_id(), d));
                }

                DeclStmt::Function(d) => {
                    fn_reserve.insert(d.data().name.data().clone(), 
                               ReservedFn(universe.new_fn_id(),
                                            universe.new_type_id(),
                                            d));
                }

                DeclStmt::BuiltinFunction(d) => {
                    builtin_fn_reserve.insert(d.data().name.data().clone(), 
                               ReservedBuiltinFn(universe.new_fn_id(),
                                                universe.new_type_id(),
                                                d));
                }

                DeclStmt::Use(u) => {
                    uses.push(u);
                }
            }
        }

        let raw = RawModData {
            name: module.0.unwrap(),
            id: universe.new_module_id(),
            reserved_structs: struct_reserve,
            reserved_fns: fn_reserve,
            reserved_builtins: builtin_fn_reserve,
            uses: uses
        };

        mod_map.insert(raw.id.clone(), raw);
    }

    mod_map
}
