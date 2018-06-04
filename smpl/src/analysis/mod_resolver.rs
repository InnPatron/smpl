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

pub fn check_modules(program: &mut Program, modules: Vec<AstModule>) {
    let raw_data = raw_mod_data(program, modules);

    let mut mapped_raw = HashMap::new();
    let mut scopes = HashMap::new();

    // Map reserved data
    for raw in raw_data.iter() {
        let mut scope = program.universe().std_scope();
        map_internal_data(&mut scope, raw);

        mapped_raw.insert(raw.name.data(), raw.id.clone());
        scopes.insert(raw.id.clone(), scope);
    }
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

fn raw_mod_data(program: &mut Program, modules: Vec<AstModule>) -> Vec<RawModData> {
    let universe = program.universe_mut();
    let mut mod_list = Vec::new();
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

        mod_list.push(raw);
    }

    mod_list
}
