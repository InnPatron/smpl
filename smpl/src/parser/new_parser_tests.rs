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

            let expected = $expected;

            dbg!(&mod1);
            dbg!(&expected);
            assert_eq!(mod1, expected);
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

test_parse_module!(type_annotations_struct_fields,
    module!("mod1" => vec![
        decl!(STRUCT => Struct {
            name: dummy_node!(ident!("Foo")),
            body: vec![
                struct_field!(a => type_ann!(PATH => int)),
                struct_field!(b => type_ann!(PATH => mod2::foo)),
                struct_field!(c => type_ann!(ARRAY => [type_ann!(PATH => mod3::foo), 5])),
                struct_field!(d => type_ann!(PATH => mod4::bar (TYPE
                    type_ann!(PATH => mod5::foo),
                    type_ann!(PATH => mod6::baz (TYPE
                        type_ann!(PATH => int),
                        type_ann!(PATH => boolean)
                    ))
                ))),
                struct_field!(e => type_ann!(WIDTH => vec![
                    width_constraint!(BASE => BAR)
                ])),
                struct_field!(f => type_ann!(WIDTH => vec![
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ])
                ])),
                struct_field!(g => type_ann!(WIDTH => vec![
                    width_constraint!(BASE => BAR),
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ])
                ])),
                struct_field!(h => type_ann!(WIDTH => vec![
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ]),
                    width_constraint!(BASE => BAR)
                ])),
                struct_field!(i => type_ann!(WIDTH => vec![
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ]),
                    width_constraint!(BASE => BAR),
                    width_constraint!(BASE => BAZ)
                ])),
                struct_field!(j => type_ann!(WIDTH => vec![
                    width_constraint!(BASE => BAR (TYPE type_ann!(PATH => foo))),
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ])
                ]))
            ],
            annotations: Vec::new(),
            type_params: None,
            where_clause: None,
        })
    ])
);

test_parse_module!(type_annotations_fn_params,
    module!("mod1" => vec![
        decl!(FN => Function {
            name: dummy_node!(ident!("Foo")),
            params: vec![
                fn_param!(a => type_ann!(PATH => int)),
                fn_param!(b => type_ann!(PATH => mod2::foo)),
                fn_param!(c => type_ann!(ARRAY => [type_ann!(PATH => mod3::foo), 5])),
                fn_param!(d => type_ann!(PATH => mod4::bar (TYPE
                    type_ann!(PATH => mod5::foo),
                    type_ann!(PATH => mod6::baz (TYPE
                        type_ann!(PATH => int),
                        type_ann!(PATH => boolean)
                    ))
                ))),
                fn_param!(e => type_ann!(WIDTH => vec![
                    width_constraint!(BASE => BAR)
                ])),
                fn_param!(f => type_ann!(WIDTH => vec![
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ])
                ])),
                fn_param!(g => type_ann!(WIDTH => vec![
                    width_constraint!(BASE => BAR),
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ])
                ])),
                fn_param!(h => type_ann!(WIDTH => vec![
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ]),
                    width_constraint!(BASE => BAR)
                ])),
                fn_param!(i => type_ann!(WIDTH => vec![
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ]),
                    width_constraint!(BASE => BAR),
                    width_constraint!(BASE => BAZ)
                ])),
                fn_param!(j => type_ann!(WIDTH => vec![
                    width_constraint!(BASE => BAR (TYPE type_ann!(PATH => foo))),
                    width_constraint!(ANON => vec![
                        struct_field!(x => type_ann!(PATH => int)),
                        struct_field!(y => type_ann!(PATH => int)),
                    ])
                ]))
            ],
            return_type: Some(dummy_node!(UNTYPED => type_ann!(WIDTH => vec![
                width_constraint!(BASE => BAR (TYPE type_ann!(PATH => foo)))
            ]))),
            body: dummy_node!(UNTYPED => block!(EMPTY)),
            annotations: Vec::new(),
            type_params: None,
            where_clause: None,
        })
    ])
);
