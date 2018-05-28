use std::str::Chars;
use std::iter::{Iterator, Enumerate, Peekable};

use ascii::AsciiString;
use span::Span;

pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

pub enum Token {
    Identifier(AsciiString),
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),

    Fn,
    Struct,
    Mod,
    Use,

    If,
    Else,
    Elif,

    While,

    Assign,

    Eq,
    NEq,

    Gte,
    Gt,
    Lte,
    lt,

    Neg,
    Invert,

    Ref,
    Deref,
    
    Add,
    Sub,
    Mul,
    Div,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Comma,
    Dot,

    Arrow,

    Colon,
    ColonColon,
    Semi,

}

struct CharInput<'input> {
    chars: Peekable<Enumerate<Chars<'input>>>,
}

impl<'input> CharInput<'input> {
    fn new(input: &str) -> CharInput {
        CharInput {
            chars: input.chars().enumerate().peekable()
        }
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.chars.peek().map(|(i, c)| (i.clone(), c.clone()))
    }
}

impl<'input> Iterator for CharInput<'input> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
    }
}


pub struct Tokenizer<'a> {
    input: &'a str,
    chars: CharInput<'a>,
    lookahead: Option<(usize, char)>,
}
