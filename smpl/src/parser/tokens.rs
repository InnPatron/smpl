/// Inspired by Gluon, specifically gluon/parser/src/token.rs
/// https://github.com/gluon-lang/gluon/blob/master/parser/src/token.rs

use std::str::{CharIndices, FromStr};
use std::iter::{Iterator, Peekable, Enumerate};

use ascii::AsciiString;

use span::Span;

pub struct SpannedToken {
    token: Token,
    location: LocationSpan,
}

impl SpannedToken {
    pub fn new(token: Token, location: LocationSpan) -> SpannedToken {
        SpannedToken {
            token: token,
            location: location,
        }
    }

    pub fn to_data(self) -> (LocationSpan, Token) {
        (self.location, self.token)
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn location(&self) -> LocationSpan {
        self.location
    }
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

    Let,

    Assign,

    Eq,
    NEq,

    Gte,
    Gt,
    Lte,
    Lt,

    Invert,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Ref,

    LAnd,
    LOr,

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
pub struct LocationSpan {
    start: Location,
    end: Location,
}

impl LocationSpan {
    pub fn new(start: Location, end: Location) -> LocationSpan {
        LocationSpan {
            start: start,
            end: end,
        }
    }

    pub fn span_1(start: Location, char_size: usize) -> LocationSpan {
        let mut end = start.clone();
        end.byte_index += char_size;
        end.char_index += 1;
        end.column += 1;

        LocationSpan::new(start, end)
    }

    pub fn start(&self) -> Location {
        self.start
    }

    pub fn end(&self) -> Location {
        self.end
    }

    pub fn make_span(&self) -> Span {
        Span::new(self.start.byte_index(), self.end.byte_index())
    }
}


#[derive(Copy, Clone)]
pub struct Location {
    byte_index: usize,
    char_index: usize,
    line: usize,
    column: usize,
}

impl Location {
    pub fn byte_index(&self) -> usize {
        self.byte_index
    }

    pub fn char_index(&self) -> usize {
        self.char_index
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }
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

type SpannedError = (TokenizerError, Location);

pub enum TokenizerError {

}

struct CharInput<'input> {
    chars: Peekable<Enumerate<CharIndices<'input>>>,
    lookahead: Option<(Location, char)>,
    line: usize,
    column: usize,
}

impl<'input> CharInput<'input> {
    fn new(input: &str) -> CharInput {
        CharInput {
            chars: input.char_indices().enumerate().peekable(),
            line: 1,
            column: 1,
            lookahead: None,
        }
    }

    fn peek(&self) -> Option<(Location, char)> {
        self.lookahead
    }
}

impl<'input> Iterator for CharInput<'input> {
    type Item = (Location, char);

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.chars.next().map(|(char_index, (byte_index, c))| {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            (Location::new(byte_index, char_index, self.line, self.column), c.clone())
        });

        let line = self.line;
        let column = self.column; 
        self.lookahead = self.chars.peek().map(|(char_index, (byte_index, c))| {
            (Location::new(byte_index.clone(), char_index.clone(), line, column), c.clone())
        });

        result
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

    fn test_lookahead<F>(&self, mut test: F) -> bool
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

impl<'input> Tokenizer<'input> {
    fn line_comment(&mut self, start: Location) -> (Location, &'input str) {
        self.take_until(start, |c| c == '\n')
    }

    fn op(&mut self, start: Location, c: char) -> Result<SpannedToken, SpannedError> {
        match c {
            '+' => Ok(SpannedToken::new(Token::Plus, LocationSpan::span_1(start, 1))),

            '-' => Ok(SpannedToken::new(Token::Minus, LocationSpan::span_1(start, 1))),

            '*' => Ok(SpannedToken::new(Token::Star, LocationSpan::span_1(start, 1))),

            '/' => Ok(SpannedToken::new(Token::Star, LocationSpan::span_1(start, 1))),

            '%' => Ok(SpannedToken::new(Token::Percent, LocationSpan::span_1(start, 1))),

            '&' => {
                if self.test_lookahead(|c| c == '&') {
                    let (end, _) = self.chars.next().ok_or(unimplemented!())?;
                    Ok(SpannedToken::new(Token::LAnd, LocationSpan::new(start, end)))
                } else {
                    Ok(SpannedToken::new(Token::Ref, LocationSpan::span_1(start, 1)))
                }
            }

            '=' => {
                if self.test_lookahead(|c| c == '=') {
                    let (end, _) = self.chars.next().ok_or(unimplemented!())?;
                    Ok(SpannedToken::new(Token::Eq, LocationSpan::new(start, end)))
                } else {
                    Ok(SpannedToken::new(Token::Assign, LocationSpan::span_1(start, 1)))
                }
            }

            '!' =>  {
                if self.test_lookahead(|c| c == '=') {
                    let (end, _) = self.chars.next().ok_or(unimplemented!())?;
                    Ok(SpannedToken::new(Token::NEq, LocationSpan::new(start, end)))
                } else {
                    Ok(SpannedToken::new(Token::Invert, LocationSpan::span_1(start, 1)))
                }
            }

            '<' => {
                if self.test_lookahead(|c| c == '=') {
                    let (end, _) = self.chars.next().ok_or(unimplemented!())?;
                    Ok(SpannedToken::new(Token::Lte, LocationSpan::new(start, end)))
                } else {
                    Ok(SpannedToken::new(Token::Lt, LocationSpan::span_1(start, 1)))
                }
            }

            '>' => {
                if self.test_lookahead(|c| c == '=') {
                    let (end, _) = self.chars.next().ok_or(unimplemented!())?;
                    Ok(SpannedToken::new(Token::Gte, LocationSpan::new(start, end)))
                } else {
                    Ok(SpannedToken::new(Token::Lt, LocationSpan::span_1(start, 1)))
                }
            }

            _ => unreachable!(),

        }
    }

    fn identifier(&mut self, start: Location) -> SpannedToken {
        let (end, ident) = self.take_while(start, is_ident_continue);

        let token = match ident {
            "fn" => Token::Fn,
            "mod" => Token::Mod,
            "struct" => Token::Struct,
            "use" => Token::Use,
            "if" => Token::If,
            "else" => Token::Else,
            "elif" => Token::Elif,
            "while" => Token::While,
            "let" => Token::Let,
            "true" => Token::BoolLiteral(true),
            "false" => Token::BoolLiteral(false),

            str => Token::Identifier(AsciiString::from_str(str).unwrap()),
        };

        SpannedToken::new(token, LocationSpan::new(start, end))
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<SpannedToken, SpannedError>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((start, c)) = self.chars.next() {
            return match c {
                '/' if self.test_lookahead(is_slash)
                    => {
                    
                    self.line_comment(start);
                    continue;
                },

                '-' if self.test_lookahead(|c| c == '>')
                    => {

                    let (end, _) = self.chars.next().unwrap();
                    Some(Ok(SpannedToken::new(Token::Arrow, LocationSpan::new(start, end))))
                },

                ',' => Some(Ok(SpannedToken::new(Token::Comma,  LocationSpan::span_1(start, 1)))),
                '.' => Some(Ok(SpannedToken::new(Token::Dot,    LocationSpan::span_1(start, 1)))),
                ';' => Some(Ok(SpannedToken::new(Token::Semi,   LocationSpan::span_1(start, 1)))),

                ':' if self.test_lookahead(is_colon) 
                    => {

                    let (end, _) = self.chars.next().unwrap();
                    Some(Ok(SpannedToken::new(Token::ColonColon,  LocationSpan::new(start, end))))
                },

                ':' if self.test_lookahead(is_colon) == false
                    => Some(Ok(SpannedToken::new(Token::Colon,  LocationSpan::span_1(start, 1)))),

                '(' => Some(Ok(SpannedToken::new(Token::LParen,  LocationSpan::span_1(start, 1)))),
                ')' => Some(Ok(SpannedToken::new(Token::RParen,  LocationSpan::span_1(start, 1)))),

                '[' => Some(Ok(SpannedToken::new(Token::LBracket,  LocationSpan::span_1(start, 1)))),
                ']' => Some(Ok(SpannedToken::new(Token::RBracket,  LocationSpan::span_1(start, 1)))),

                '{' => Some(Ok(SpannedToken::new(Token::LBrace,  LocationSpan::span_1(start, 1)))),
                '}' => Some(Ok(SpannedToken::new(Token::RBrace,  LocationSpan::span_1(start, 1)))),

                ch if is_ident_start(ch) => Some(Ok(self.identifier(start))),
                ch if is_op(ch) => Some(self.op(start, ch)),
                ch if ch.is_whitespace() => continue,

                _ => unimplemented!(),
            }
        }

        None
    }
}

fn is_colon(c: char) -> bool {
    c == ':'
}

fn is_slash(c: char) -> bool {
    c == '/'
}

fn is_op(c: char) -> bool {
    c == '+' ||
    c == '-' ||
    c == '*' ||
    c == '/' ||
    c == '%' ||

    c == '&' ||
    c == '|' ||

    c == '!' ||
    c == '=' ||
    c == '>' ||
    c == '<'
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    is_ident_start(c)
}
