#[cfg(test)]
mod parser_tests {
    use std::str::FromStr;
    use parser::*;
    use ascii::*;
    use ast::*;

    #[test]
    fn test_parse_FnDecl() {
        let input = "fn test_fn(i32 arg) { }";
        let func = parse_FnDecl(input).unwrap();
        assert_eq!(func.name, Ident::new("test_fn"));
        assert_eq!(func.args, Some(vec![FnArg { name: Ident::new("arg"), arg_type: Ident::new("i32")}]));
        assert_eq!(func.body, AstNode::untyped(Block(Vec::new())));
    }
}
