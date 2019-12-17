mod feature_checkers;
mod semantic_ck;
mod semantic_data;
mod typed_ast;
#[macro_use]
mod control_flow;
mod analysis_helpers;
mod blocky_linear_cfg_traversal;
mod control_data;
mod expr_flow;
mod linear_cfg_traversal;
mod mod_resolver;
mod resolve_scope;
mod return_trace;
mod type_checker;
mod type_cons_gen;
mod type_equality;
mod type_resolver;
mod unique_linear_cfg_traversal;
mod analysis_context;
mod anon_storage;

pub mod error;
pub mod metadata;
pub(crate) mod type_cons;
pub(crate) mod abstract_type;

pub use self::semantic_data::{FnId, TypeId, ModuleId};

pub(crate) use self::blocky_linear_cfg_traversal::{BlockyPassenger, BlockyTraverser};
pub(crate) use self::control_data::*;
pub(crate) use self::control_flow::CFG;
pub(crate) use self::linear_cfg_traversal::{Passenger, Traverser};
pub(crate) use self::semantic_ck::check_program;
pub(crate) use self::semantic_data::*;
pub(crate) use self::semantic_data::{Function, Module, Program};
pub(crate) use self::type_checker::TypingContext;
pub(crate) use self::typed_ast::*;
