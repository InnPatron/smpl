/*!

  Core library to parse, analyze, and compile the SMPL language.

  * This crate does **NOT** come with the means to execute SMPL code.
    * Use a crate like [smpli](https://crates.io/crates/smpli) for a runtime/std instead

  # Using the Crate

  1. Find your SMPL module code and create an `UnparsedModule`
  2. Convert your collection of `UnparsedModule` into a `Program` by:
     * Preparse using `parser::parse_module()` and calling `Program::from_parsed()`
     * Call `Program::from_unparsed()` directly
  3. Pass off the `Program` to a code generator (such as in `smpl::byte_gen`)
     * Code generators will have full access to type information, control flow, metadata, etc.
*/

extern crate irmatch;
extern crate failure;
extern crate itertools;
extern crate petgraph;
#[macro_use]
extern crate failure_derive;
extern crate display_derive;

pub mod module;

#[macro_use]
mod ast_macros;
mod ast_node;
mod ast;
mod expr_ast;
mod typable_ast;
// pub mod program;
mod span;
mod analysis;

pub mod parser;
pub mod error;

pub mod prelude {
    pub use crate::module::{ParsedModule, UnparsedModule};
    pub use crate::parser::parse_module;
}
