
#[cfg(test)]
#[allow(non_snake_case)]
mod parser_tests {
    use parser::*;
    use ast::*;
    use span::Span;

    #[test]
    fn parse_programs() {
        {
            let input =
"fn test() -> bla {
    if test {

    }
}";
            parse_module(input).unwrap();
        }

        {
            let input =
"fn main() {
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
            let ast = parse_module(input).unwrap();
            assert_eq!(ast.1.len(), 5);
        }
    }

    #[test]
    fn test_parse_struct_init() {
        let init_1 = r##" init NAME { }"##;
        let init_2 =
r##"
init NAME {
    field1: 1 + 2,
    field2: true
}
"##;
        let parser = ExprParser::new();

        let init_1 = wrap_input(init_1);
        let init_2 = wrap_input(init_2);

        let init_1 = parser.parse(init_1).unwrap();
        let init_2 = parser.parse(init_2).unwrap();

        // Check init_1
        {
            let expected = Expr::StructInit(dummy_node!(StructInit {
                struct_name: type_path!("NAME"),
                field_init: None,
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
                field_init: Some(vec![(dummy_node!(ident!("field1")), field_init), 
                                      (dummy_node!(ident!("field2")), field2_init)]),
            }));

            assert_eq!(init_2, expected);
        }
    }

    #[test]
    fn test_parse_complex_expr() {
        let input = r##"5 != 3 || "something" == false && true"##;
        let input = wrap_input(input);
        let parser = ExprParser::new();
        let expr = parser.parse(input).unwrap();

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
                        assert_eq!(*bin_expr.lhs,
                                   string!("something" => Expr));
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
                },

                e @ _ => panic!("Expected Bool. Found {:?}", e),
            }
        }
    }

    #[test]
    fn test_parse_string() {
        let input = r##""test""##;
        let input = wrap_input(input);
        let parser = LiteralParser::new();
        let literal = parser.parse(input).unwrap();
        match literal {
            Literal::String(ref string) => assert_eq!(string, "test"),
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_numbers() {
        let parser = LiteralParser::new();

        let input = "21";
        let input = wrap_input(input);
        let literal = parser.parse(input).unwrap();
        match literal {
            Literal::Int(int) => assert_eq!(int, 21),
            _ => panic!(),
        }

        let input = "-21";
        let input = wrap_input(input);
        let literal = parser.parse(input).unwrap();
        match literal {
            Literal::Int(int) => assert_eq!(int, -21),
            _ => panic!(),
        }

        let input = "21.0";
        let input = wrap_input(input);
        let literal = parser.parse(input).unwrap();
        match literal {
            Literal::Float(float) => assert_eq!(float, 21.0),
            _ => panic!(),
        }

        let input = "21.";
        let input = wrap_input(input);
        let literal = parser.parse(input).unwrap();
        match literal {
            Literal::Float(float) => assert_eq!(float, 21.0),
            _ => panic!(),
        }

        let input = "-21.";
        let input = wrap_input(input);
        let literal = parser.parse(input).unwrap();
        match literal {
            Literal::Float(float) => assert_eq!(float, -21.0),
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_local_var_decl() {
        let parser = ExprStmtParser::new();
        let input = "let a: int = 10;";
        let input = wrap_input(input);
        let stmt = parser.parse(input).unwrap();
        if let ExprStmt::LocalVarDecl(decl) = stmt {
            let anno = decl.var_type.unwrap();
            assert_eq!(*anno.data(), TypeAnnotation::Path(type_path!("int")));
            assert_eq!(decl.var_name, dummy_node!(ident!("a")));
        }
    }

    #[test]
    fn test_parse_FnDecl() {
        let input = "fn test_fn(arg: i32, test: float, next: String) { }";
        let input = wrap_input(input);
        let parser = FnDeclParser::new();
        let func = parser.parse(input).unwrap();
        let func = func.data().clone();
        assert_eq!(func.name, dummy_node!(ident!("test_fn")));
        assert_eq!(func.body, dummy_node!(Block(Vec::new(), Span::new(0, 0), Span::new(0, 0))));

        let expected = vec![(dummy_node!(ident!("arg")), type_path!("i32")),
                            (dummy_node!(ident!("test")), type_path!("float")),
                            (dummy_node!(ident!("next")), type_path!("String"))];
        for (param, expected) in func.params.unwrap().iter().zip(expected.iter()) {
            assert_eq!(param.data().name, expected.0);
            assert_eq!(*param.data().param_type.data(), TypeAnnotation::Path(expected.1.clone()));
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

        let parser = StructDeclParser::new();

        let input = wrap_input(input);
        let _struct = parser.parse(input).unwrap();
        let input2 = wrap_input(input2);
        let _struct2 = parser.parse(input2).unwrap();

        let _struct = _struct.data().clone();
        let _struct2 = _struct2.data().clone();
        assert_eq!(_struct.name, dummy_node!(ident!("TestStruct")));
        let struct_field_0 = _struct.clone().body.0.map(|v| v.get(0).unwrap().clone()).unwrap();
        let struct_field_1 = _struct2.clone().body.0.map(|v| v.get(1).unwrap().clone()).unwrap();

        assert_eq!(struct_field_0.name, dummy_node!(ident!("field1")));
        assert_eq!(struct_field_1.name, dummy_node!(ident!("field2")));

        assert_eq!(*struct_field_0.field_type.data(), TypeAnnotation::Path(type_path!("Type1")));
        assert_eq!(*struct_field_1.field_type.data(), TypeAnnotation::Path(type_path!("Type2")));

        assert_eq!(_struct.name, _struct2.name);
        assert_eq!(_struct.body, _struct2.body);
    }

    #[test]
    fn test_parse_MathExpr_no_spaces() {
        {
            let parser = MathExprParser::new();
            let input = "1+2";
            let input = wrap_input(input);
            let _e = parser.parse(input).unwrap();
            let _root = {
                let _1 = int!(1 => BoxExpr);
                let _2 = int!(2 => BoxExpr);

                let parent = bin_expr!((_1, BinOp::Add, _2) => Expr);
                parent
            };
        }

        {
            let parser = MathExprParser::new();
            let input = "1.+2";
            let input = wrap_input(input);
            let _e = parser.parse(input).unwrap();
            let _root = {
                let _1 = Box::new(Expr::Literal(AstNode::new(Literal::Float(1.0), Span::new(0, 0))));
                let _2 = int!(2 => BoxExpr);

                let parent = bin_expr!((_1, BinOp::Add, _2) => Expr);
                parent
            };
        }
    }

    #[test]
    fn test_parse_MathExpr() {
        {
            let parser = MathExprParser::new();
            let input = "1 + 2 * 5";
            let input = wrap_input(input);
            let e = parser.parse(input).unwrap();
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
            let parser = MathExprParser::new();
            let input = "5 * (1 + 2)";
            let input = wrap_input(input);
            let e = parser.parse(input).unwrap();
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
            let parser = MathExprParser::new();
            let input = "5 % 5 * 10 - 321 / 8";
            let input = wrap_input(input);
            let e = parser.parse(input).unwrap();

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
            let parser = TruthExprParser::new();
            let input = "true && true || false";
            let input = wrap_input(input);
            let e = parser.parse(input).unwrap();
            
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
            let parser = TruthExprParser::new();
            let input = "1 + 5 == 2 && 3 != 4";
            let input = wrap_input(input);
            let e = parser.parse(input).unwrap();

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
            let parser = TruthExprParser::new();
            let input = "(1 + 5) * 6 == 2 && 3 != 4";
            let input = wrap_input(input);
            let e = parser.parse(input).unwrap();

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
        let input = wrap_input(input);
        let parser = ExprParser::new();
        parser.parse(input).unwrap();
    }


    #[test]
    fn parse_annotate_struct() {
        let input =
"#[test, foo = \"bar\"]
struct Foo { }";

        let input = parse_module(input).unwrap();
    }

    #[test]
    fn parse_annotate_fn() {
        let input =
"#[test, foo = \"bar\"]

fn foo() { }";

        let input = parse_module(input).unwrap();
    }
}
