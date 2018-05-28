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
struct LocationSpan {
    start: Location,
    end: Location,
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

    fn skip_to_end(&mut self) {
        while let Some(_) = self.chars.next() { }
    }

    fn test_lookahead<F>(&mut self, mut test: F) -> bool
        where F: FnMut(char) -> bool {
        self.chars.peek().map_or(false, |c| test(c.1))
    }

    fn slice(&self, start: Location, end: Location) -> &'input str {
        let start = start.byte_index;
        let end = end.byte_index;

        &self.input[start..end]
    }

    fn take_while<F>(&mut self, start: Location, mut acceptor: F) -> (Location, &'input str)
        where F: FnMut(char) -> bool {
    
        self.take_until(start, |c| !acceptor(c))
    }

    fn take_until<F>(&mut self, start: Location, mut terminator: F) -> (Location, &'input str)
        where F: FnMut(char) -> bool {

        let mut current = None;
        while let Some((loc, c)) = self.chars.peek() {
            if terminator(c) {
                // Return the location to the start of the next rule
                return (loc, self.slice(start, loc));
            } else {
                current = self.chars.next();
            }
        }

        match current {
            Some(end_of_input) => (end_of_input.0, self.slice(start, end_of_input.0)),

            // Loop body did not run, assume 'start' location was the end of the input
            None => (start, self.slice(start, start)),

        }
    }
}
