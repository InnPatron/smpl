#[cfg(test)]
mod parser_tests {
    use std::str::FromStr;
    use parser::*;
    use ascii::*;
    use ast::*;

    #[test]
    fn parse_programs() {
        {
            let input =
"fn test() -> bla {
    if test {

    }
}";
            parse_Program(input).unwrap();
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
            let ast = parse_Program(input).unwrap();
            for decl in ast.0.iter() {
                match *decl {
                    DeclStmt::Struct(ref s) => println!("{:?}", s),
                    DeclStmt::Function(ref f) => println!("{:?}", f),
                }
            }
            assert_eq!(ast.0.len(), 5);
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

        let init_1 = parse_Expr(init_1).unwrap();
        let init_2 = parse_Expr(init_2).unwrap();

        // Check init_1
        {
            let expected = Expr::StructInit(StructInit {
                struct_name: path!("NAME"),
                field_init: None,
            });

            assert_eq!(init_1, expected);
        }

        // Check init_2
            {
            let field_init = bin_expr!((int!(1 => BoxExpr), 
                                        BinOp::Add, 
                                        int!(2 => BoxExpr)) => BoxExpr);
            let field2_init = boolean!(true => BoxExpr);

            let expected = Expr::StructInit(StructInit {
                struct_name: path!("NAME"),
                field_init: Some(vec![(ident!("field1"), field_init), 
                                      (ident!("field2"), field2_init)]),
            });

            assert_eq!(init_2, expected);
        }
    }

    #[test]
    fn test_parse_complex_expr() {
        let input = r##"5 != 3 || "something" == false && true"##;
        let expr = parse_Expr(input).unwrap();

        let lhs;
        let rhs;
        let op;
        match expr {
            Expr::Bin(bin_expr) => {
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
                        assert_eq!(bin_expr.op, BinOp::InEq);
                        assert_eq!(*bin_expr.lhs, Expr::Literal(Literal::Int(5)));
                        assert_eq!(*bin_expr.rhs, Expr::Literal(Literal::Int(3)));
                    }

                    e @ _ => panic!("Expected BinExpr. Found {:?}", e),
                }
            }

            // Check right hand of ||
            {
                match *lower_rhs {
                    Expr::Bin(bin_expr) => {
                        assert_eq!(bin_expr.op, BinOp::Eq);
                        assert_eq!(*bin_expr.lhs,
                                   Expr::Literal(Literal::String(AsciiString::from_str("something").unwrap())));
                        assert_eq!(*bin_expr.rhs, Expr::Literal(Literal::Bool(false)));
                    }

                    e @ _ => panic!("Expected BinExpr. Found {:?}", e),
                }
            }
        }

        // Check right hand of &&
        {
            match *rhs {
                Expr::Literal(lit) => {
                    assert_eq!(lit, Literal::Bool(true));
                },

                e @ _ => panic!("Expected Bool. Found {:?}", e),
            }
        }
    }

    #[test]
    fn test_parse_string() {
        let input = r##""test""##;
        let literal = parse_Literal(input).unwrap();
        match literal {
            Literal::String(ref string) => assert_eq!(string, "test"),
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_numbers() {
        let input = "21";
        let literal = parse_Literal(input).unwrap();
        match literal {
            Literal::Int(int) => assert_eq!(int, 21),
            _ => panic!(),
        }

        let input = "-21";
        let literal = parse_Literal(input).unwrap();
        match literal {
            Literal::Int(int) => assert_eq!(int, -21),
            _ => panic!(),
        }

        let input = "21.0";
        let literal = parse_Literal(input).unwrap();
        match literal {
            Literal::Float(float) => assert_eq!(float, 21.0),
            _ => panic!(),
        }

        let input = "21.";
        let literal = parse_Literal(input).unwrap();
        match literal {
            Literal::Float(float) => assert_eq!(float, 21.0),
            _ => panic!(),
        }

        let input = "-21.";
        let literal = parse_Literal(input).unwrap();
        match literal {
            Literal::Float(float) => assert_eq!(float, -21.0),
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_local_var_decl() {
        let input = "let a: int = 10;";
        let stmt = parse_ExprStmt(input).unwrap();
        if let ExprStmt::LocalVarDecl(ref decl) = stmt {
            assert_eq!(decl.var_type, path!("int"));
            assert_eq!(decl.var_name, ident!("a"));
        }
    }

    #[test]
    fn test_parse_FnDecl() {
        let input = "fn test_fn(arg: i32, test: float, next: String) { }";
        let func = parse_FnDecl(input).unwrap();
        assert_eq!(func.name, ident!("test_fn"));
        assert_eq!(func.body, Block(Vec::new()));

        let expected = vec![(ident!("arg"), path!("i32")),
                            (ident!("test"), path!("float")),
                            (ident!("next"), path!("String"))];
        for (param, expected) in func.params.unwrap().iter().zip(expected.iter()) {
            assert_eq!(param.name, expected.0);
            assert_eq!(param.param_type, expected.1);
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

        let _struct = parse_StructDecl(input).unwrap();
        let _struct2 = parse_StructDecl(input2).unwrap();


        assert_eq!(_struct.name, ident!("TestStruct"));
        assert_eq!(_struct.body, StructBody(Some(vec![
            StructField {
                name: ident!("field1"),
                field_type: path!("Type1"),
            },

            StructField {
                name: ident!("field2"),
                field_type: path!("Type2"),
            },
        ])));

        assert_eq!(_struct.name, _struct2.name);
        assert_eq!(_struct.body, _struct2.body);
    }

    #[test]
    fn test_parse_MathExpr_no_spaces() {
        {
            let input = "1+2";
            let e = parse_MathExpr(input).unwrap();
            let root = {
                let _1 = int!(1 => BoxExpr);
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
            let e = parse_MathExpr(input).unwrap();
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
            let e = parse_MathExpr(input).unwrap();
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
            let e = parse_MathExpr(input).unwrap();

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
    fn test_parse_CmpExpr() {
        {
            let input = "true && true || false";
            let e = parse_CmpExpr(input).unwrap();
            
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
            let e = parse_CmpExpr(input).unwrap();

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
            let e = parse_CmpExpr(input).unwrap();

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
}
