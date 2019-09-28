mod feature_checkers;
mod semantic_ck;
mod semantic_data;
mod typed_ast;
#[macro_use]
mod control_flow;
mod control_data;
pub mod error;
mod expr_flow;
mod linear_cfg_traversal;
mod blocky_linear_cfg_traversal;
pub mod metadata;
mod mod_resolver;
pub mod type_cons;
mod type_cons_gen;
mod type_resolver;
mod resolve_scope;

pub use self::control_data::*;
pub use self::control_flow::CFG;
pub use self::linear_cfg_traversal::{Passenger, Traverser};
pub use self::blocky_linear_cfg_traversal::{BlockyPassenger, BlockyTraverser};
pub use self::semantic_ck::check_program;
pub use self::semantic_data::*;
pub use self::semantic_data::{Function, Module, Program};
pub use self::typed_ast::*;
