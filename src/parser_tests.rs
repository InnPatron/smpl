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

    macro_rules! ident {
        ($ident: expr) => {{
            Ident(AsciiString::from_str($ident).unwrap())
        }};

        ($ident: expr => Expr) => {{ 
            Expr::Ident(AstNode::untyped(ident!($ident)))
        }};

        ($ident: expr => BoxExpr) => {{
            Box::new(ident!($ident => Expr))
        }}
    }

    macro_rules! bin_expr {
        ($lhs: expr, $op: expr, $rhs: expr) => {{
            BinExpr {
                op: $op,
                lhs: AstNode::untyped($lhs),
                rhs: AstNode::untyped($rhs),
            }
        }};

        (($lhs: expr, $op: expr, $rhs: expr) => Expr) => {{
            Expr::Bin(AstNode::untyped(bin_expr!($lhs, $op, $rhs)))
        }};

        (($lhs: expr, $op: expr, $rhs: expr) => BinExpr) => {{
            Box::new(bin_expr!(($lhs, $op, $rhs) => Expr))
        }};
    }

    #[test]
    fn test_parse_FnDecl() {
        let input = "fn test_fn(i32 arg) { }";
        let func = parse_FnDecl(input).unwrap();
        assert_eq!(func.name, ident!("test_fn"));
        assert_eq!(func.args, Some(vec![FnArg { name: ident!("arg"), arg_type: ident!("i32")}]));
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

        assert_eq!(_struct.name, ident!("TestStruct"));
        assert_eq!(_struct.body, StructBody(vec![
            StructField {
                name: ident!("field1"),
                field_type: ident!("Type1"),
            },

            StructField {
                name: ident!("field2"),
                field_type: ident!("Type2"),
            },
        ]));
    }

    #[test]
    fn test_parse_MathExpr() {
        {
            let input = "1 + 2 * 5";
            let e = parse_MathExpr(input).unwrap();
            let root = {
                let _1 = number!("1" => BoxExpr);
                let _2 = number!("2" => BoxExpr);
                let _5 = number!("5" => BoxExpr);
                let child = bin_expr!((_2, BinOp::Mul, _5) => BinExpr);

                let parent = bin_expr!((_1, BinOp::Add, child) => Expr);
                parent
            };
            assert_eq!(e, root);
        }

        {
            let input = "5 * (1 + 2)";
            let e = parse_MathExpr(input).unwrap();
            let root = {
                let _1 = number!("1" => BoxExpr);
                let _2 = number!("2" => BoxExpr);
                let _5 = number!("5" => BoxExpr);
                let child = bin_expr!((_1, BinOp::Add, _2) => BinExpr);

                let parent = bin_expr!((_5, BinOp::Mul, child) => Expr);
                parent
            };
            assert_eq!(e, root);

        }
    }
}
