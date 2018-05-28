/// Inspired by Gluon, specifically gluon/parser/src/token.rs
/// https://github.com/gluon-lang/gluon/blob/master/parser/src/token.rs

use std::str::CharIndices;
use std::iter::{Iterator, Peekable, Enumerate};

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

#[derive(Copy, Clone)]
struct Location {
    byte_index: usize,
    char_index: usize,
    line: usize,
    column: usize,
}

impl Location {
    fn new(byte_index: usize, char_index: usize, line: usize, column: usize) -> Location {
        Location {
            byte_index: byte_index,
            char_index: char_index,
            line: line,
            column: column,
        }
    }
}

struct CharInput<'input> {
    chars: Peekable<Enumerate<CharIndices<'input>>>,
    line: usize,
    column: usize,
}

impl<'input> CharInput<'input> {
    fn new(input: &str) -> CharInput {
        CharInput {
            chars: input.char_indices().enumerate().peekable(),
            line: 1,
            column: 1,
        }
    }

    fn peek(&mut self) -> Option<(Location, char)> {
        let line = self.line;
        let column = self.column;
        self.chars.peek().map(|(char_index, (byte_index, c))| {
            (Location::new(byte_index.clone(), char_index.clone(), line, column), c.clone())
        })
    }
}

impl<'input> Iterator for CharInput<'input> {
    type Item = (Location, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next().map(|(char_index, (byte_index, c))| {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            (Location::new(byte_index, char_index, self.line, self.column), c.clone())
        })
    }
}


pub struct Tokenizer<'input> {
    input: &'input str,
    chars: CharInput<'input>,
}

impl<'input> Tokenizer<'input> {
    fn new(input: &str) -> Tokenizer {
        Tokenizer {
            input: input,
            chars: CharInput::new(input),
        }
    }
}
