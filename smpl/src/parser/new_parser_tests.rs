use super::error::*;
use super::new_parser;
use crate::new_ast::*;
use crate::expr_ast::*;
use crate::module::*;
use crate::parser::buffer_input;
use crate::span::*;
use crate::ast_node::*;
use crate::typable_ast::*;

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
    };

    ($name: ident, $expected: expr) => {
        #[test]
        fn $name() {
            let mod1 = include_test!(concat!(stringify!($name), ".smpl"));

            let mod1 = parse_module(mod1)
                .expect("Module did not parse correctly");

            assert_eq!(mod1, $expected);
        }
    }
}

fn parse_module(input: &str) -> ParserResult<Module> {
    let source = ModuleSource::Anonymous(None);
    let mut tokens = buffer_input(&source, input);

    new_parser::module(&mut tokens)
}

test_parse_module!(empty_module_declared,
    module!("mod1" => vec![])

);

test_parse_module!(basic_fn,
    module!("mod1" => vec![
        decl!(FN => Function {
            name: dummy_node!(ident!("foo")),
            params: vec![],
            return_type: Some(dummy_node!(UNTYPED => type_ann!(PATH => Type))),
            body: dummy_node!(UNTYPED => block!(EMPTY)),
            annotations: vec![],
            type_params: None,
            where_clause: None,
        }),

        decl!(FN => Function {
            name: dummy_node!(ident!("bar")),
            params: vec![
                fn_param!(x => type_ann!(PATH => int)),
                fn_param!(y => type_ann!(PATH => int)),
            ],
            return_type: Some(dummy_node!(UNTYPED => type_ann!(PATH => Type))),
            body: dummy_node!(UNTYPED => block!(EMPTY)),
            annotations: vec![],
            type_params: None,
            where_clause: None,
        })
    ])
);

test_parse_module!(basic_structs,
    module!("mod1" => vec![
        decl!(STRUCT => Struct {
            name: dummy_node!(ident!("Foo")),
            body: vec![],
            annotations: Vec::new(),
            type_params: None,
            where_clause: None,
        }),

        decl!(STRUCT => Struct {
            name: dummy_node!(ident!("Bar")),
            body: vec![],
            annotations: Vec::new(),
            type_params: None,
            where_clause: None,
        }),

        decl!(FN => Function {
            name: dummy_node!(ident!("foo")),
            params: vec![],
            return_type: None,
            body: dummy_node!(UNTYPED => block!(EMPTY)),
            annotations: vec![],
            type_params: None,
            where_clause: None,
        })
    ])
);

test_parse_module!(type_annotations_struct_fields);

test_parse_module!(type_annotations_fn_params);
