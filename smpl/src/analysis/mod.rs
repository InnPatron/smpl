mod feature_checkers;
mod semantic_ck;
mod semantic_data;
mod typed_ast;
#[macro_use]
mod control_flow;
mod control_data;
mod expr_flow;
mod fn_analyzer;
mod linear_cfg_traversal;
mod mod_resolver;
mod type_cons;
mod type_cons_gen;
pub mod error;
pub mod metadata;

pub use self::semantic_ck::check_program;
pub use self::typed_ast::*;
pub use self::semantic_data::*;
pub use self::linear_cfg_traversal::{Passenger, Traverser};
pub use self::semantic_data::{Function, Module, Program};
pub use self::control_flow::CFG;
pub use self::control_data::*;
