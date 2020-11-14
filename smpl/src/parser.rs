mod error;
mod lexer;
#[macro_use]
mod parser_macros;
mod tokens;
lalrpop_mod!(pub pparser, "/parser/pparser.rs");

pub use self::pparser::ModuleParser;
pub use self::tokens::LiteralData;

#[cfg(test)]
mod tests {
    use super::lexer::Tokenizer;
    use super::pparser::ModuleParser;

    #[test]
    fn dot_fn_calls() {
        let input = "
            mod dot_fn_calls;

            fn foo() {
                a.b.c().d.e.f();
            }
        ";

        let lexer = Tokenizer::new(input);
        let parser = ModuleParser::new();
        // TODO: structural test
        parser.parse(lexer).unwrap();
    }

    #[test]
    fn uni_dot_fn_calls() {
        let input = "
            mod dot_fn_calls;

            fn foo() {
                !!!a.b.c().d.e.f();
            }
        ";

        let lexer = Tokenizer::new(input);
        let parser = ModuleParser::new();
        // TODO: structural test
        parser.parse(lexer).unwrap();
    }

    #[test]
    fn block_expr_return_1() {
        let input = "
            mod dot_fn_calls;

            fn foo() {
                1
            }
        ";

        let lexer = Tokenizer::new(input);
        let parser = ModuleParser::new();
        // TODO: structural test
        parser.parse(lexer).unwrap();
    }

    #[test]
    fn empty_block() {
        let input = "
            mod dot_fn_calls;

            fn foo() { }
        ";

        let lexer = Tokenizer::new(input);
        let parser = ModuleParser::new();
        // TODO: structural test
        parser.parse(lexer).unwrap();
    }
}
