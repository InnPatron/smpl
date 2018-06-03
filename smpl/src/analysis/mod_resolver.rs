use std::collections::HashMap;
use std::cell::Cell;
use std::rc::Rc;

use err::Err;
use ast::{Ident, ModulePath as AstModulePath, Path, DeclStmt, Struct, Function as AstFunction, Module as AstModule, BuiltinFunction as AstBuiltinFunction, BuiltinFnParams};

use super::feature_checkers::*;
use super::metadata::*;
use super::smpl_type::*;
use super::semantic_data::*;
use super::semantic_data::Module;
use super::control_flow::CFG;
use super::fn_analyzer::analyze_fn;

#[derive(Debug, Clone)]
pub struct ModData {
    fn_signatures: HashMap<Ident, TypeId>,
    type_map: HashMap<Ident, TypeId>,
    fns: HashMap<FnId, AstNode<AstFunction>>,
    builtin_fns: HashMap<FnId, AstNode<AstBuiltinFunction>>,
}
