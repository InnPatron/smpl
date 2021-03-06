#[cfg(test)]
#[allow(non_snake_case)]
#[rustfmt::skip]
mod parser_tests {
    use crate::ast::*;
    use crate::module::*;
    use crate::parser::expr_parser::*;
    use crate::parser::parser::*;
    use crate::parser::*;
    use crate::span::Span;

    fn parse_expr_quick(input: &str) -> Expr {
        let source = ModuleSource::Anonymous(None);
        let mut tokens = buffer_input(&source, input);
        let expr = piped_expr(&mut tokens, &[]).unwrap();

        expr.to_data().0
    }

    fn parse_stmt_quick(input: &str) -> Stmt {
        let source = ModuleSource::Anonymous(None);
        let mut tokens = buffer_input(&source, input);
        let stmt = teststmt(&mut tokens).unwrap();

        stmt
    }

    macro_rules! wrap_input {
        ($input: expr) => {{
            UnparsedModule::anonymous($input)
        }};
    }

    #[test]
    fn parse_programs() {
        {
            let input = "fn test() -> bla {
    if test {

    }
}";
            parse_module(wrap_input!(input)).unwrap();
        }

        {
            let input = "fn main() {
	let hello: String = \"Hello!\";
}

fn if_basic() -> i32 {
	let name: String = \"if_basic\";

	if true {
		return 0;
	}
}

fn if_default() -> i32 {
	let name: String = \"if_default\";

	if false {
		return 0;
	} elif false {
		return 1;
	}
	
	return 2;
}

fn if_empty() -> i32 {
	let name: String = \"if_empty\";

	if false { } elif false { } elif true { } else { }

	return 100;
}

fn if_complex() -> i32 {
	let name: String = \"if_complex\";

	if true {
		return 0;
	} elif false {
		return 1;
	} else {
		return 2;
	}
}";
            let ast = parse_module(wrap_input!(input)).unwrap().module;
            assert_eq!(ast.1.len(), 5);
        }
    }

    #[test]
    fn test_parse_struct_init() {
        let init_1 = r##" init NAME { }"##;
        let init_2 = r##"
init NAME {
    field1: 1 + 2,
    field2: true
}
"##;

        let init_1 = parse_expr_quick(init_1);
        let init_2 = parse_expr_quick(init_2);

        // Check init_1
        {
            let expected = Expr::StructInit(dummy_node!(StructInit {
                struct_name: type_path!("NAME"),
                field_init: Vec::new(),
            }));

            assert_eq!(init_1, expected);
        }

        // Check init_2
        {
            let field_init = bin_expr!((int!(1 => BoxExpr), 
                                        BinOp::Add, 
                                        int!(2 => BoxExpr)) => BoxExpr);
            let field2_init = boolean!(true => BoxExpr);

            let expected = Expr::StructInit(dummy_node!(StructInit {
                struct_name: type_path!("NAME"),
                field_init: vec![
                    (dummy_node!(ident!("field1")), field_init),
                    (dummy_node!(ident!("field2")), field2_init)
                ],
            }));

            assert_eq!(init_2, expected);
        }
    }

    #[test]
    fn test_parse_anonymous_struct_init() {
        let init_1 = r##" init { }"##;
        let init_2 = r##"
init {
    field1: 1 + 2,
    field2: true
}
"##;

        let init_1 = parse_expr_quick(init_1);
        let init_2 = parse_expr_quick(init_2);

        // Check init_1
        {
            let expected = Expr::AnonStructInit(dummy_node!(AnonStructInit {
                field_init: Vec::new(),
            }));

            assert_eq!(init_1, expected);
        }

        // Check init_2
        {
            let field_init = bin_expr!((int!(1 => BoxExpr), 
                                        BinOp::Add, 
                                        int!(2 => BoxExpr)) => BoxExpr);
            let field2_init = boolean!(true => BoxExpr);

            let expected = Expr::AnonStructInit(dummy_node!(AnonStructInit {
                field_init: vec![
                    (dummy_node!(ident!("field1")), field_init),
                    (dummy_node!(ident!("field2")), field2_init)
                ],
            }));

            assert_eq!(init_2, expected);
        }
    }

    #[test]
    fn test_parse_complex_expr() {
        let input = r##"5 != 3 || "something" == false && true"##;
        let expr = parse_expr_quick(input);

        let lhs;
        let rhs;
        let op;
        match expr {
            Expr::Bin(bin_expr) => {
                let (bin_expr, _) = bin_expr.to_data();
                op = bin_expr.op;
                lhs = bin_expr.lhs;
                rhs = bin_expr.rhs;
            }

            e @ _ => panic!("Expected BinExpr. Found {:?}", e),
        }

        assert_eq!(op, BinOp::LogicalAnd);
        // Check left hand of &&
        {
            let lower_lhs;
            let lower_rhs;
            let lower_op;
            match *lhs {
                Expr::Bin(bin_expr) => {
                    let (bin_expr, _) = bin_expr.to_data();
                    lower_lhs = bin_expr.lhs;
                    lower_rhs = bin_expr.rhs;
                    lower_op = bin_expr.op;
                }

                e @ _ => panic!("Expected BinExpr. Found {:?}", e),
            }

            assert_eq!(lower_op, BinOp::LogicalOr);

            // Check left hand of ||
            {
                match *lower_lhs {
                    Expr::Bin(bin_expr) => {
                        let (bin_expr, _) = bin_expr.to_data();
                        assert_eq!(bin_expr.op, BinOp::InEq);
                        assert_eq!(*bin_expr.lhs, int!(5 => Expr));
                        assert_eq!(*bin_expr.rhs, int!(3 => Expr));
                    }

                    e @ _ => panic!("Expected BinExpr. Found {:?}", e),
                }
            }

            // Check right hand of ||
            {
                match *lower_rhs {
                    Expr::Bin(bin_expr) => {
                        let (bin_expr, _) = bin_expr.to_data();
                        assert_eq!(bin_expr.op, BinOp::Eq);
                        assert_eq!(*bin_expr.lhs, string!("something" => Expr));
                        assert_eq!(*bin_expr.rhs, boolean!(false => Expr));
                    }

                    e @ _ => panic!("Expected BinExpr. Found {:?}", e),
                }
            }
        }

        // Check right hand of &&
        {
            match *rhs {
                Expr::Literal(lit) => {
                    let (lit, _) = lit.to_data();
                    assert_eq!(lit, Literal::Bool(true));
                }

                e @ _ => panic!("Expected Bool. Found {:?}", e),
            }
        }
    }

    #[test]
    fn test_parse_local_var_decl() {
        let input = "let a: int = 10;";

        let stmt = parse_stmt_quick(input);

        let expr_stmt = match stmt {
            Stmt::ExprStmt(s) => s,
            _ => panic!(),
        };

        if let ExprStmt::LocalVarDecl(decl) = expr_stmt.to_data().0 {
            let anno = decl.var_type.unwrap();
            assert_eq!(*anno.data(), TypeAnnotation::Path(type_path!("int")));
            assert_eq!(decl.var_name, dummy_node!(ident!("a")));
        }
    }

    #[test]
    fn test_parse_FnDecl() {
        let input = "fn test_fn(arg: i32, test: float, next: String) { }";

        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        let func = testfn_decl(&mut input).unwrap();

        assert_eq!(func.name, dummy_node!(ident!("test_fn")));
        assert_eq!(func.body, dummy_node!(Block(Vec::new())));

        let expected = vec![
            (dummy_node!(ident!("arg")), type_path!("i32")),
            (dummy_node!(ident!("test")), type_path!("float")),
            (dummy_node!(ident!("next")), type_path!("String")),
        ];
        for (param, expected) in func.params.unwrap().iter().zip(expected.iter()) {
            assert_eq!(param.data().name, expected.0);
            assert_eq!(
                *param.data().param_type.data(),
                TypeAnnotation::Path(expected.1.clone())
            );
        }
    }

    #[test]
    fn test_parse_StructDecl() {
        let input = "
struct TestStruct {
    field1: Type1,
    field2: Type2
}";
        let input2 = "
struct TestStruct {
    field1: Type1,
    field2: Type2,
}";

        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        let _struct = teststruct_decl(&mut input).unwrap();

        let mut input2 = buffer_input(&source, input2);
        let _struct2 = teststruct_decl(&mut input2).unwrap();

        assert_eq!(_struct.name, dummy_node!(ident!("TestStruct")));
        let struct_field_0 = _struct
            .clone()
            .body
            .0
            .map(|v| v.get(0).unwrap().clone())
            .unwrap();
        let struct_field_1 = _struct2
            .clone()
            .body
            .0
            .map(|v| v.get(1).unwrap().clone())
            .unwrap();

        assert_eq!(struct_field_0.name, dummy_node!(ident!("field1")));
        assert_eq!(struct_field_1.name, dummy_node!(ident!("field2")));

        assert_eq!(
            *struct_field_0.field_type.data(),
            TypeAnnotation::Path(type_path!("Type1"))
        );
        assert_eq!(
            *struct_field_1.field_type.data(),
            TypeAnnotation::Path(type_path!("Type2"))
        );

        assert_eq!(_struct.name, _struct2.name);
        assert_eq!(_struct.body, _struct2.body);
    }

    #[test]
    fn test_parse_MathExpr_no_spaces() {
        {
            let input = "1+2";
            let _e = parse_expr_quick(input);
            let _root = {
                let _1 = int!(1 => BoxExpr);
                let _2 = int!(2 => BoxExpr);

                let parent = bin_expr!((_1, BinOp::Add, _2) => Expr);
                parent
            };
        }

        {
            let input = "1.+2";
            let _e = parse_expr_quick(input);
            let _root = {
                let _1 = Box::new(Expr::Literal(AstNode::new(
                    Literal::Float(1.0),
                    Span::dummy(),
                )));
                let _2 = int!(2 => BoxExpr);

                let parent = bin_expr!((_1, BinOp::Add, _2) => Expr);
                parent
            };
        }
    }

    #[test]
    fn test_parse_MathExpr() {
        {
            let input = "1 + 2 * 5";
            let e = parse_expr_quick(input);
            let root = {
                let _1 = int!(1 => BoxExpr);
                let _2 = int!(2 => BoxExpr);
                let _5 = int!(5 => BoxExpr);
                let child = bin_expr!((_2, BinOp::Mul, _5) => BoxExpr);

                let parent = bin_expr!((_1, BinOp::Add, child) => Expr);
                parent
            };
            assert_eq!(e, root);
        }

        {
            let input = "5 * (1 + 2)";
            let e = parse_expr_quick(input);
            let root = {
                let _1 = int!(1 => BoxExpr);
                let _2 = int!(2 => BoxExpr);
                let _5 = int!(5 => BoxExpr);
                let child = bin_expr!((_1, BinOp::Add, _2) => BoxExpr);

                let parent = bin_expr!((_5, BinOp::Mul, child) => Expr);
                parent
            };
            assert_eq!(e, root);
        }

        {
            let input = "5 % 5 * 10 - 321 / 8";
            let e = parse_expr_quick(input);

            let root = {
                let _5 = int!(5 => BoxExpr);
                let _10 = int!(10 => BoxExpr);
                let _321 = int!(321 => BoxExpr);
                let _8 = int!(8 => BoxExpr);

                let lhs_child = bin_expr!((_5.clone(), BinOp::Mod, _5) => BoxExpr);
                let lhs_child = bin_expr!((lhs_child, BinOp::Mul, _10) => BoxExpr);

                let rhs_child = bin_expr!((_321, BinOp::Div, _8) => BoxExpr);

                let parent = bin_expr!((lhs_child, BinOp::Sub, rhs_child) => Expr);
                parent
            };
            assert_eq!(e, root);
        }
    }

    #[test]
    fn test_parse_TruthExpr() {
        {
            let input = "true && true || false";
            let e = parse_expr_quick(input);

            let root = {
                let _true = boolean!(true => BoxExpr);
                let _false = boolean!(false => BoxExpr);

                let lhs_child = bin_expr!((_true.clone(), BinOp::LogicalAnd, _true) => BoxExpr);
                let parent = bin_expr!((lhs_child, BinOp::LogicalOr, _false) => Expr);
                parent
            };

            assert_eq!(e, root);
        }

        {
            let input = "1 + 5 == 2 && 3 != 4";
            let e = parse_expr_quick(input);

            let root = {
                let _1 = int!(1 => BoxExpr);
                let _2 = int!(2 => BoxExpr);
                let _3 = int!(3 => BoxExpr);
                let _4 = int!(4 => BoxExpr);
                let _5 = int!(5 => BoxExpr);

                let lhs_child = bin_expr!((_1, BinOp::Add, _5) => BoxExpr);
                let lhs_child = bin_expr!((lhs_child, BinOp::Eq, _2) => BoxExpr);
                let rhs_child = bin_expr!((_3, BinOp::InEq, _4) => BoxExpr);

                let parent = bin_expr!((lhs_child, BinOp::LogicalAnd, rhs_child) => Expr);
                parent
            };

            assert_eq!(e, root);
        }

        {
            let input = "(1 + 5) * 6 == 2 && 3 != 4";
            let e = parse_expr_quick(input);

            let root = {
                let _1 = int!(1 => BoxExpr);
                let _2 = int!(2 => BoxExpr);
                let _3 = int!(3 => BoxExpr);
                let _4 = int!(4 => BoxExpr);
                let _5 = int!(5 => BoxExpr);
                let _6 = int!(6 => BoxExpr);

                let lhs_child = bin_expr!((_1, BinOp::Add, _5) => BoxExpr);
                let lhs_child = bin_expr!((lhs_child, BinOp::Mul, _6) => BoxExpr);
                let lhs_child = bin_expr!((lhs_child, BinOp::Eq, _2) => BoxExpr);
                let rhs_child = bin_expr!((_3, BinOp::InEq, _4) => BoxExpr);

                let parent = bin_expr!((lhs_child, BinOp::LogicalAnd, rhs_child) => Expr);
                parent
            };

            assert_eq!(e, root);
        }
    }

    #[test]
    fn parse_float_expr() {
        let input = "1.5";
        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        piped_expr(&mut input, &[]).unwrap();
    }

    #[test]
    fn parse_another_complex_expr() {
        let input = "1 + 10 * 4 / 3 == 5 && false";
        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        piped_expr(&mut input, &[]).unwrap();
    }

    #[test]
    fn parse_math_expr() {
        let input = "1 + 10 * 4 / 3";
        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        piped_expr(&mut input, &[]).unwrap();
    }

    #[test]
    fn parse_paren_expr() {
        let input = "(1 + 10) * (4 / 3)";
        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        piped_expr(&mut input, &[]).unwrap();
    }

    #[test]
    fn parse_nested_paren_expr() {
        let input = "((1 + 10) * (4 / (3)))";
        let source = ModuleSource::Anonymous(None);
        let mut input = buffer_input(&source, input);
        piped_expr(&mut input, &[]).unwrap();
    }

    #[test]
    fn parse_annotate_struct() {
        let input = "#[test, foo = \"bar\"]
struct Foo { }";

        let _input = parse_module(wrap_input!(input)).unwrap();
    }

    #[test]
    fn parse_annotate_fn() {
        let input = "#[test, foo = \"bar\"]

fn foo() { }";

        let _input = parse_module(wrap_input!(input)).unwrap();
    }

    #[test]
    fn parse_struct_decl_type_param() {
        let input = "struct Foo(type Bar, Baz) {
    b: Bar,
}";

        let _input = parse_module(wrap_input!(input)).unwrap();
    }

    #[test]
    fn parse_fn_decl_type_param() {
        let input = "fn foo(type Bar, Baz)(a: Bar, b: Baz) -> Baz {
    return a;
}";

        let _input = parse_module(wrap_input!(input)).unwrap();
    }

    #[test]
    fn parse_struct_init_type_arg() {
        let input = "fn foo(a: Bar, b: Baz) {
    init Bar(type Baq, Qux) { fi: fo };
}";

        let _input = parse_module(wrap_input!(input)).unwrap();
    }

    #[test]
    fn parse_fn_call_type_arg() {
        let input = "fn foo(a: Bar, b: Baz) {
    typed(type Bar)();
}";

        let _input = parse_module(wrap_input!(input)).unwrap();
    }

    #[test]
    fn parse_fn_generic_param() {
        let input = "fn foo(bar: fn(type T)(T) -> T) {
    bar();
}";

        let _input = parse_module(wrap_input!(input)).unwrap();
    }

    #[test]
    fn parse_fn_generic() {
        let input = "fn foo(type T)(bar: fn(type T)(T) -> T) {
    bar();
}";

        let _input = parse_module(wrap_input!(input)).unwrap();
    }

    #[test]
    fn parse_type_inst() {
        let input = "fn foo() {
    let a = bar(type int);
}";

        let _input = parse_module(wrap_input!(input)).unwrap();
    }

    #[test]
    fn parse_generic_struct_init() {
        let mod1 = "mod mod1;

struct Foo(type T) {
    f: T,
}

fn foo(type T)(v: T) -> Foo(type T) {
    let f = init Foo(type T) {
        f: v,
    };

    return f;
}";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }

    #[test]
    fn parse_named_width_constraint() {
        let mod1 =
"mod mod1;

fn foo(v: base BAR) {

}";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }

    #[test]
    fn parse_anonymous_width_constraint() {
        let mod1 =
"mod mod1;

fn foo(v: { FOO: BAR}) {

}";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }

    #[test]
    fn parse_width_constraint_list() {
        let mod1 =
"mod mod1;

fn foo(v: { FOO: BAR} + base BAR, f: base BAR + {FOO: BAR, BING: BANG}) {

}";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }

    #[test]
    fn parse_generic_fn_constraint() {
        let mod1 =
"mod mod1;

fn foo(type T)(t: T) -> T
    where T: base Foo {

}

fn bar(type T)(t: T) -> T
    where T: base Foo,
        T: base Bar,
        T: { t: int } + {f : float},
        T: int {

}";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }

    #[test]
    fn parse_generic_struct_constraint() {
        let mod1 =
"mod mod1;

struct Foo(type T)
    where T: base Foo  {
    
    t: T
}

struct Bar(type T)
    where T: base Foo,
        T: base Bar,
        T: { t: int } + {f : float},
        T: int {

    t: T,
}";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }

    #[test]
    fn parse_bind_fn_type_app() {
        let mod1 =
"mod mod1;

fn ident(type T)(t: T) -> T {
    return t;
}

fn test() {
    let my_ident: fn(int) -> int = ident(type int);
    let result: int = my_ident(5);
}";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }

    #[test]
    fn parse_bind_fn_type_app_mod_access() {
        let mod1 =
"mod mod1;

fn ident(type T)(t: T) -> T {
    return t;
}";

        let mod2 =
"mod mod2;

use mod1;

fn test() {
    let my_ident: fn(int) -> int = mod1::ident(type int);
    let result: int = my_ident(5);
}";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
        let _mod2 = parse_module(wrap_input!(mod2)).unwrap();
    }

    #[test]
    fn parse_bind_fn_type_app_mod_access_stmt() {
        let mod1 =
"mod mod1;

fn ident(type T)(t: T) -> T {
    return t;
}";

        let mod2 =
"mod mod2;

use mod1;

fn test() {
    mod1::ident(type int);
}";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
        let _mod2 = parse_module(wrap_input!(mod2)).unwrap();
    }

    #[test]
    fn parse_opaque_type() {
        let mod1 =
"mod mod1;

opaque Vec;";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }

    #[test]
    fn parse_opaque_type_type_params() {
        let mod1 =
"mod mod1;

opaque Vec(type T, B);";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }

    #[test]
    fn parse_opaque_type_type_params_where_clause() {
        let mod1 =
"mod mod1;

opaque Vec(type T, B)
    where T: base Foo;";

        let _mod1 = parse_module(wrap_input!(mod1)).unwrap();
    }
}
