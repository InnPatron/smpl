#[cfg(test)]
mod parser_tests {
    use std::str::FromStr;
    use parser::*;
    use ascii::*;
    use ast::*;

    macro_rules! number {
        ($str_num: expr) => {{
            Literal::Number($str_num.to_string())
        }};

        ($str_num: expr => Expr) => {{
            Expr::Literal(AstNode::untyped(number!($str_num)))
        }};

        ($str_num: expr => BoxExpr) => {{
            Box::new(number!($str_num => Expr))
        }}
    }

    #[test]
    fn test_parse_FnDecl() {
        let input = "fn test_fn(i32 arg) { }";
        let func = parse_FnDecl(input).unwrap();
        assert_eq!(func.name, Ident::new("test_fn"));
        assert_eq!(func.args, Some(vec![FnArg { name: Ident::new("arg"), arg_type: Ident::new("i32")}]));
        assert_eq!(func.body, AstNode::untyped(Block(Vec::new())));
    }

    #[test]
    fn test_parse_StructDecl() {
        let input = "
struct TestStruct {
    field1: Type1,
    field2: Type2,
}";
        let _struct = parse_StructDecl(input).unwrap();

        assert_eq!(_struct.name, Ident::new("TestStruct"));
        assert_eq!(_struct.body, StructBody(vec![
            StructField {
                name: Ident::new("field1"),
                field_type: Ident::new("Type1"),
            },

            StructField {
                name: Ident::new("field2"),
                field_type: Ident::new("Type2"),
            },
        ]));
    }

    #[test]
    fn test_parse_MathExpr() {
        {
            let input = "1 + 2 * 5";
            let e = parse_MathExpr(input).unwrap();
            let root = Expr::Bin(AstNode::untyped({
                let _1 = number!("1" => BoxExpr);
                let _2 = number!("2" => BoxExpr);
                let _5 = number!("5" => BoxExpr);
                let child = BinExpr {
                    op: BinOp::Mul,
                    lhs: AstNode::untyped(_2),
                    rhs: AstNode::untyped(_5),
                };

                let child = Expr::Bin(AstNode::untyped(child));

                let parent = BinExpr {
                    op: BinOp::Add,
                    lhs: AstNode::untyped(_1),
                    rhs: AstNode::untyped(Box::new(child)),
                };
                parent
            }));
            assert_eq!(e, root);
        }
    }
}
