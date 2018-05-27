use super::builtins::*;

use ast::Module;
use err::Err;
use parser::parse_module;

pub fn include(mut modules: Vec<Module>) -> Vec<Module> {
    let log = parse_module(LOG_DECLARATION).unwrap();
    let convert = parse_module(CONVERT_DECLARATION).unwrap();
    let math = parse_module(MATH_DECLARATION).unwrap();
    let err = parse_module(ERR_DECLARATION).unwrap();

    let mut to_add = vec![log, convert, math, err];

    modules.append(&mut to_add);
    
    modules
}
