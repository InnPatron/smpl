#[cfg(test)]
mod parser_tests {
    use parser::*;

    #[test]
    fn test_parse_FnDecl() {
        let input = "fn test_fn(i32 arg) { }";
        parse_FnDecl(input).unwrap();
    }
}
