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
}
