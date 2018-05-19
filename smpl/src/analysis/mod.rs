pub mod smpl_type;
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
pub mod metadata;

pub use self::semantic_ck::check_program;
pub use self::typed_ast::*;
pub use self::semantic_data::*;
pub use self::linear_cfg_traversal::{Traverser, Passenger};
pub use self::semantic_data::{Program, Module};
pub use self::control_flow::CFG;
pub use self::control_data::*;
