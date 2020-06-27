use super::error::*;
use super::new_parser;
use crate::new_ast::*;
use crate::expr_ast::*;
use crate::module::*;
use crate::parser::buffer_input;

macro_rules! include_test {
    ($file_name: expr) => {{
        include_str!(concat!("parser_tests/", $file_name))
    }}
}


macro_rules! test_parse_module {
    ($name: ident) => {
        #[test]
        fn $name() {
            let mod1 = include_test!(concat!(stringify!($name), ".smpl"));

            let mod1 = parse_module(mod1)
                .expect("Module did not parse correctly");
        }
    }
}

fn parse_module(input: &str) -> ParserResult<Module> {
    let source = ModuleSource::Anonymous(None);
    let mut tokens = buffer_input(&source, input);

    new_parser::module(&mut tokens)
}

test_parse_module!(empty_module_declared);
