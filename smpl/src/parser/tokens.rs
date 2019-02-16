use std::default::Default;
use std::iter::{Enumerate, Iterator, Peekable};
/// Inspired by Gluon, specifically gluon/parser/src/token.rs
/// https://github.com/gluon-lang/gluon/blob/master/parser/src/token.rs
use std::str::CharIndices;

use crate::span::*;

#[derive(Debug)]
pub struct SpannedToken {
    token: Token,
    location: LocationSpan,
}

impl PartialEq for SpannedToken {
    fn eq(&self, other: &SpannedToken) -> bool {
        self.token == other.token
    }
}

#[allow(dead_code)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),

    Fn,
    Struct,
    Mod,
    Use,
    Builtin,
    Unchecked,
    Type,

    Init,

    Continue,
    Break,
    Return,

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

    Pipe,

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

    Pound,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Token::*;
        match *self {
            Identifier(ref s) => write!(f, "{}", s),
            StringLiteral(ref s) => write!(f, "\"{}\"", s),
            IntLiteral(i) => write!(f, "{}", i),
            FloatLiteral(fl) => write!(f, "{}", fl),
            BoolLiteral(b) => write!(f, "{}", b),

            Fn => write!(f, "fn"),
            Struct => write!(f, "struct"),
            Mod => write!(f, "mod"),
            Use => write!(f, "use"),
            Builtin => write!(f, "builtin"),
            Unchecked => write!(f, "UNCHECKED"),

            Type => write!(f, "type"),

            Init => write!(f, "init"),

            Continue => write!(f, "continue"),
            Break => write!(f, "break"),
            Return => write!(f, "return"),

            If => write!(f, "if"),
            Else => write!(f, "else"),
            Elif => write!(f, "elif"),

            While => write!(f, "while"),

            Let => write!(f, "let"),

            Assign => write!(f, "="),

            Eq => write!(f, "=="),
            NEq => write!(f, "!="),

            Gte => write!(f, ">="),
            Gt => write!(f, ">"),
            Lte => write!(f, "<="),
            Lt => write!(f, "<"),

            Invert => write!(f, "!"),

            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Percent => write!(f, "%"),

            Pipe => write!(f, "|>"),

            Ref => write!(f, "&"),

            LAnd => write!(f, "&&"),
            LOr => write!(f, "||"),

            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            LBracket => write!(f, "["),
            RBracket => write!(f, "]"),

            Comma => write!(f, ","),
            Dot => write!(f, "."),

            Arrow => write!(f, "->"),

            Colon => write!(f, ":"),
            ColonColon => write!(f, "::"),
            Semi => write!(f, ";"),

            Pound => write!(f, "#"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Fail)]
#[fail(display = "{} {}", error, location)]
pub struct SpannedError {
    error: TokenizerError,
    location: Location,
}

#[derive(Debug, PartialEq, Clone, Fail)]
pub enum TokenizerError {
    #[fail(display = "Unexpected character: '{}'.", _0)]
    UnexpectedChar(char),

    #[fail(display = "Unexpected end of input.")]
    UnexpectedEndOfInput,

    #[fail(display = "Unterminated string literal.")]
    UnterminatedStringLiteral,

    #[fail(display = "Incomplete token: '{}'.", _0)]
    IncompleteToken(Token),
}

#[derive(Debug)]
struct CharInput<'input> {
    chars: Peekable<Enumerate<CharIndices<'input>>>,
    lookahead: Option<(Location, char)>,
    line: usize,
    column: usize,
    end_of_input: Location,
}

impl<'input> CharInput<'input> {
    fn new(input: &str) -> CharInput {
        let last = if input.len() == 0 {
            0
        } else {
            input.char_indices().rev().next().unwrap().0
        };
        let mut chars = input.char_indices().enumerate().peekable();
        let lookahead = chars.peek().map(|(char_index, (byte_index, c))| {
            (
                Location::new(byte_index.clone(), char_index.clone(), 1, 2),
                c.clone(),
            )
        });
        CharInput {
            chars: chars,
            line: 1,
            column: 0,
            lookahead: lookahead,
            end_of_input: Location::new(last + 1, last, 1, 0),
        }
    }

    fn end_of_input(&self) -> Location {
        self.end_of_input
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

                self.end_of_input.line += 1;
                self.end_of_input.column = 1;
            } else {
                self.column += 1;
                self.end_of_input.column += 1;
            }
            (
                Location::new(byte_index, char_index, self.line, self.column),
                c.clone(),
            )
        });

        let line = self.line;
        let column = self.column;
        self.lookahead = self.chars.peek().map(|(char_index, (byte_index, c))| {
            (
                Location::new(byte_index.clone(), char_index.clone(), line, column),
                c.clone(),
            )
        });

        result
    }
}

#[derive(Debug)]
pub struct Tokenizer<'input> {
    input: &'input str,
    chars: CharInput<'input>,
}

impl<'input> Tokenizer<'input> {
    pub fn new(input: &str) -> Tokenizer {
        Tokenizer {
            input: input,
            chars: CharInput::new(input),
        }
    }

    fn test_lookahead<F>(&self, mut test: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.chars.peek().map_or(false, |c| test(c.1))
    }

    fn slice(&self, start: Location, end: Location) -> &'input str {
        let start = start.byte_index;
        let end = end.byte_index;

        &self.input[start..end]
    }

    fn take_while<F>(&mut self, start: Location, mut acceptor: F) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |c| !acceptor(c))
    }

    fn take_until<F>(&mut self, start: Location, mut terminator: F) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
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
            Some(last_input) => {
                // Upperbounds is EXCLUSIVE
                let mut end = last_input.0.clone();
                end.column += 1;
                end.byte_index += 1;
                end.char_index += 1;

                (start, self.slice(start, end))
            }

            None => {
                // Loop body did not run
                // Slice until the end of input
                (start, self.slice(start, self.chars.end_of_input()))
            }
        }
    }
}

impl<'input> Tokenizer<'input> {
    fn line_comment(&mut self, start: Location) -> (Location, &'input str) {
        self.take_until(start, |c| c == '\n')
    }

    fn op(&mut self, start: Location, c: char) -> Result<SpannedToken, SpannedError> {
        match c {
            '+' => Ok(SpannedToken::new(
                Token::Plus,
                LocationSpan::span_1(start, 1),
            )),

            '-' => Ok(SpannedToken::new(
                Token::Minus,
                LocationSpan::span_1(start, 1),
            )),

            '*' => Ok(SpannedToken::new(
                Token::Star,
                LocationSpan::span_1(start, 1),
            )),

            '/' => Ok(SpannedToken::new(
                Token::Slash,
                LocationSpan::span_1(start, 1),
            )),

            '%' => Ok(SpannedToken::new(
                Token::Percent,
                LocationSpan::span_1(start, 1),
            )),

            '&' => {
                if self.test_lookahead(|c| c == '&') {
                    let (end, _) = self.chars.next().ok_or(SpannedError {
                        error: TokenizerError::UnexpectedEndOfInput,
                        location: start,
                    })?;
                    Ok(SpannedToken::new(
                        Token::LAnd,
                        LocationSpan::new(start, end),
                    ))
                } else {
                    Ok(SpannedToken::new(
                        Token::Ref,
                        LocationSpan::span_1(start, 1),
                    ))
                }
            }

            '|' => {
                if self.test_lookahead(|c| c == '|') {
                    let (end, _) = self.chars.next().ok_or(SpannedError {
                        error: TokenizerError::UnexpectedEndOfInput,
                        location: start,
                    })?;
                    Ok(SpannedToken::new(Token::LOr, LocationSpan::new(start, end)))
                } else if self.test_lookahead(|c| c == '>') {
                    let (end, _) = self.chars.next().ok_or(SpannedError {
                        error: TokenizerError::UnexpectedEndOfInput,
                        location: start,
                    })?;
                    Ok(SpannedToken::new(
                        Token::Pipe,
                        LocationSpan::new(start, end),
                    ))
                } else {
                    Err(SpannedError {
                        error: TokenizerError::IncompleteToken(Token::LOr),
                        location: start,
                    })
                }
            }

            '=' => {
                if self.test_lookahead(|c| c == '=') {
                    let (end, _) = self.chars.next().ok_or(SpannedError {
                        error: TokenizerError::UnexpectedEndOfInput,
                        location: start,
                    })?;
                    Ok(SpannedToken::new(Token::Eq, LocationSpan::new(start, end)))
                } else {
                    Ok(SpannedToken::new(
                        Token::Assign,
                        LocationSpan::span_1(start, 1),
                    ))
                }
            }

            '!' => {
                if self.test_lookahead(|c| c == '=') {
                    let (end, _) = self.chars.next().ok_or(SpannedError {
                        error: TokenizerError::UnexpectedEndOfInput,
                        location: start,
                    })?;
                    Ok(SpannedToken::new(Token::NEq, LocationSpan::new(start, end)))
                } else {
                    Ok(SpannedToken::new(
                        Token::Invert,
                        LocationSpan::span_1(start, 1),
                    ))
                }
            }

            '<' => {
                if self.test_lookahead(|c| c == '=') {
                    let (end, _) = self.chars.next().ok_or(SpannedError {
                        error: TokenizerError::UnexpectedEndOfInput,
                        location: start,
                    })?;
                    Ok(SpannedToken::new(Token::Lte, LocationSpan::new(start, end)))
                } else {
                    Ok(SpannedToken::new(Token::Lt, LocationSpan::span_1(start, 1)))
                }
            }

            '>' => {
                if self.test_lookahead(|c| c == '=') {
                    let (end, _) = self.chars.next().ok_or(SpannedError {
                        error: TokenizerError::UnexpectedEndOfInput,
                        location: start,
                    })?;
                    Ok(SpannedToken::new(Token::Gte, LocationSpan::new(start, end)))
                } else {
                    Ok(SpannedToken::new(Token::Gt, LocationSpan::span_1(start, 1)))
                }
            }

            ch => unreachable!("Missing handler for {}", ch),
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
            "builtin" => Token::Builtin,
            "UNCHECKED" => Token::Unchecked,
            "type" => Token::Type,
            "continue" => Token::Continue,
            "break" => Token::Break,
            "return" => Token::Return,
            "init" => Token::Init,
            "true" => Token::BoolLiteral(true),
            "false" => Token::BoolLiteral(false),

            str => Token::Identifier(str.to_string()),
        };

        SpannedToken::new(token, LocationSpan::new(start, end))
    }

    fn numeric_literal(&mut self, start: Location) -> Result<SpannedToken, SpannedError> {
        let (end, int) = self.take_while(start, is_digit);

        if let Some((_loc, ch)) = self.chars.peek() {
            if ch == '.' {
                self.chars.next(); // Skip '.'
                let (end, float) = self.take_while(start, is_digit);

                if let Some((next, ch)) = self.chars.peek() {
                    if is_ident_continue(ch) {
                        return Err(SpannedError {
                            error: TokenizerError::UnexpectedChar(ch),
                            location: next,
                        });
                    }
                }

                let f = float.parse::<f64>().unwrap();

                return Ok(SpannedToken::new(
                    Token::FloatLiteral(f),
                    LocationSpan::new(start, end),
                ));
            }
        }

        let i = int.parse::<i64>().unwrap();
        Ok(SpannedToken::new(
            Token::IntLiteral(i),
            LocationSpan::new(start, end),
        ))
    }

    fn string_literal(&mut self, start: Location) -> Result<SpannedToken, SpannedError> {
        let mut literal = String::new();
        while let Some((e, ch)) = self.chars.next() {
            match ch {
                '\"' => {
                    return Ok(SpannedToken::new(
                        Token::StringLiteral(literal),
                        LocationSpan::new(start, e),
                    ));
                }

                // TODO: Track escape characters
                ch => literal.push(ch),
            }
        }

        Err(SpannedError {
            error: TokenizerError::UnterminatedStringLiteral,
            location: start,
        })
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<SpannedToken, SpannedError>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((start, c)) = self.chars.next() {
            return match c {
                '/' if self.test_lookahead(is_slash) => {
                    self.line_comment(start);
                    continue;
                }

                '-' if self.test_lookahead(|c| c == '>') => {
                    let (end, _) = self.chars.next().unwrap();
                    Some(Ok(SpannedToken::new(
                        Token::Arrow,
                        LocationSpan::new(start, end),
                    )))
                }

                ',' => Some(Ok(SpannedToken::new(
                    Token::Comma,
                    LocationSpan::span_1(start, 1),
                ))),
                '.' => Some(Ok(SpannedToken::new(
                    Token::Dot,
                    LocationSpan::span_1(start, 1),
                ))),
                ';' => Some(Ok(SpannedToken::new(
                    Token::Semi,
                    LocationSpan::span_1(start, 1),
                ))),

                ':' if self.test_lookahead(is_colon) => {
                    let (end, _) = self.chars.next().unwrap();
                    Some(Ok(SpannedToken::new(
                        Token::ColonColon,
                        LocationSpan::new(start, end),
                    )))
                }

                ':' if self.test_lookahead(is_colon) == false => Some(Ok(SpannedToken::new(
                    Token::Colon,
                    LocationSpan::span_1(start, 1),
                ))),

                '(' => Some(Ok(SpannedToken::new(
                    Token::LParen,
                    LocationSpan::span_1(start, 1),
                ))),
                ')' => Some(Ok(SpannedToken::new(
                    Token::RParen,
                    LocationSpan::span_1(start, 1),
                ))),

                '[' => Some(Ok(SpannedToken::new(
                    Token::LBracket,
                    LocationSpan::span_1(start, 1),
                ))),
                ']' => Some(Ok(SpannedToken::new(
                    Token::RBracket,
                    LocationSpan::span_1(start, 1),
                ))),

                '{' => Some(Ok(SpannedToken::new(
                    Token::LBrace,
                    LocationSpan::span_1(start, 1),
                ))),
                '}' => Some(Ok(SpannedToken::new(
                    Token::RBrace,
                    LocationSpan::span_1(start, 1),
                ))),

                '#' => Some(Ok(SpannedToken::new(
                    Token::Pound,
                    LocationSpan::span_1(start, 1),
                ))),

                '\"' => Some(self.string_literal(start)),

                ch if is_ident_start(ch) => Some(Ok(self.identifier(start))),
                ch if is_digit(ch) || (ch == '-' && self.test_lookahead(is_digit)) => {
                    Some(self.numeric_literal(start))
                }

                ch if is_op(ch) => Some(self.op(start, ch)),
                ch if ch.is_whitespace() => continue,

                _ch => Some(Err(SpannedError {
                    error: TokenizerError::UnexpectedChar(c),
                    location: start,
                })),
            };
        }

        None
    }
}

#[derive(Debug)]
pub struct BufferedTokenizer<'a> {
    tokenizer: Tokenizer<'a>,
    next: Option<Result<SpannedToken, SpannedError>>,
}

impl<'a> BufferedTokenizer<'a> {
    pub fn new(mut tokenizer: Tokenizer) -> BufferedTokenizer {
        let next = tokenizer.next();

        BufferedTokenizer {
            tokenizer: tokenizer,
            next: next,
        }
    }

    pub fn has_next(&self) -> bool {
        self.next.is_some()
    }

    pub fn next(&mut self) -> Option<Result<SpannedToken, SpannedError>> {
        let to_return = self.next.take();

        self.next = self.tokenizer.next();

        to_return
    }

    pub fn peek_is_none(&self) -> bool {
        self.next.is_none()
    }

    pub fn peek<F, R>(&self, closure: F) -> Option<Result<R, SpannedError>>
    where
        F: Fn(&Token) -> R,
    {
        self.next.as_ref().map(|ref r| match r {
            Ok(ref t) => Ok(closure(t.token())),
            Err(ref e) => Err((*e).clone()),
        })
    }
}

fn is_colon(c: char) -> bool {
    c == ':'
}

fn is_slash(c: char) -> bool {
    c == '/'
}

fn is_op(c: char) -> bool {
    c == '+'
        || c == '-'
        || c == '*'
        || c == '/'
        || c == '%'
        || c == '&'
        || c == '|'
        || c == '!'
        || c == '='
        || c == '>'
        || c == '<'
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    is_ident_start(c) || is_digit(c)
}

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unwrap(v: Option<Result<SpannedToken, SpannedError>>) -> Token {
        v.expect("Some").expect("Ok").to_data().1
    }

    #[test]
    fn tokenize_mod_decl() {
        let input = "mod test;";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::Mod, unwrap(tok.next()));
        assert_eq!(Token::Identifier("test".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Semi, unwrap(tok.next()));
        assert_eq!(None, tok.next());
    }

    #[test]
    fn tokenize_struct_decl() {
        let input = "struct Test { field1: i32, field2: f32 }";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::Struct, unwrap(tok.next()));
        assert_eq!(Token::Identifier("Test".to_string()), unwrap(tok.next()));
        assert_eq!(Token::LBrace, unwrap(tok.next()));

        assert_eq!(Token::Identifier("field1".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Colon, unwrap(tok.next()));
        assert_eq!(Token::Identifier("i32".to_string()), unwrap(tok.next()));

        assert_eq!(Token::Comma, unwrap(tok.next()));

        assert_eq!(Token::Identifier("field2".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Colon, unwrap(tok.next()));
        assert_eq!(Token::Identifier("f32".to_string()), unwrap(tok.next()));

        assert_eq!(Token::RBrace, unwrap(tok.next()));
        assert_eq!(None, tok.next());
    }

    #[test]
    fn tokenize_line_comment() {
        let input = "// Test Bla
mod hello;";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::Mod, unwrap(tok.next()));
        assert_eq!(Token::Identifier("hello".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Semi, unwrap(tok.next()));
        assert_eq!(None, tok.next());
    }

    #[test]
    fn tokenize_literals() {
        let input = "true 
false 
1337 
-1337 
1 
-1 
1.0 
1. 
-1. 
-1.0
";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::BoolLiteral(true), unwrap(tok.next()));
        assert_eq!(Token::BoolLiteral(false), unwrap(tok.next()));

        assert_eq!(Token::IntLiteral(1337), unwrap(tok.next()));
        assert_eq!(Token::IntLiteral(-1337), unwrap(tok.next()));
        assert_eq!(Token::IntLiteral(1), unwrap(tok.next()));
        assert_eq!(Token::IntLiteral(-1), unwrap(tok.next()));

        assert_eq!(Token::FloatLiteral(1.0), unwrap(tok.next()));
        assert_eq!(Token::FloatLiteral(1.0), unwrap(tok.next()));
        assert_eq!(Token::FloatLiteral(-1.0), unwrap(tok.next()));
        assert_eq!(Token::FloatLiteral(-1.0), unwrap(tok.next()));

        assert_eq!(None, tok.next());
    }

    #[test]
    fn tokenize_string_literal() {
        let input = "\" this is lit \"
\" -erally\"
";
        let mut tok = Tokenizer::new(input);

        assert_eq!(
            Token::StringLiteral(" this is lit ".to_string()),
            unwrap(tok.next())
        );
        assert_eq!(
            Token::StringLiteral(" -erally".to_string()),
            unwrap(tok.next())
        );

        assert_eq!(None, tok.next());
    }

    #[test]
    fn tokenize_idents() {
        let input = "
_ident
_123
abcd
a1b2
A1b2
";
        let mut tok = Tokenizer::new(input);
        assert_eq!(Token::Identifier("_ident".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Identifier("_123".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Identifier("abcd".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Identifier("a1b2".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Identifier("A1b2".to_string()), unwrap(tok.next()));
    }

    #[test]
    fn tokenize_math_expr() {
        let input = "2.0 * foo - (bar/3)--100 % 1. + 3";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::FloatLiteral(2.0), unwrap(tok.next()));
        assert_eq!(Token::Star, unwrap(tok.next()));
        assert_eq!(Token::Identifier("foo".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Minus, unwrap(tok.next()));

        assert_eq!(Token::LParen, unwrap(tok.next()));
        assert_eq!(Token::Identifier("bar".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Slash, unwrap(tok.next()));
        assert_eq!(Token::IntLiteral(3), unwrap(tok.next()));
        assert_eq!(Token::RParen, unwrap(tok.next()));
        assert_eq!(Token::Minus, unwrap(tok.next()));

        assert_eq!(Token::IntLiteral(-100), unwrap(tok.next()));

        assert_eq!(Token::Percent, unwrap(tok.next()));

        assert_eq!(Token::FloatLiteral(1.0), unwrap(tok.next()));
        assert_eq!(Token::Plus, unwrap(tok.next()));
        assert_eq!(Token::IntLiteral(3), unwrap(tok.next()));
    }

    #[test]
    fn tokenize_keywords() {
        let input = "if struct while fn mod use else elif let";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::If, unwrap(tok.next()));
        assert_eq!(Token::Struct, unwrap(tok.next()));
        assert_eq!(Token::While, unwrap(tok.next()));
        assert_eq!(Token::Fn, unwrap(tok.next()));
        assert_eq!(Token::Mod, unwrap(tok.next()));
        assert_eq!(Token::Use, unwrap(tok.next()));
        assert_eq!(Token::Else, unwrap(tok.next()));
        assert_eq!(Token::Elif, unwrap(tok.next()));
        assert_eq!(Token::Let, unwrap(tok.next()));
    }

    #[test]
    fn tokenize_end_of_input() {
        let input = "let";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::Let, unwrap(tok.next()));
        assert_eq!(None, tok.next());
    }

    #[test]
    fn tokenize_module_path() {
        let input = "foo::bar::baz";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::Identifier("foo".to_string()), unwrap(tok.next()));
        assert_eq!(Token::ColonColon, unwrap(tok.next()));
        assert_eq!(Token::Identifier("bar".to_string()), unwrap(tok.next()));
        assert_eq!(Token::ColonColon, unwrap(tok.next()));
        assert_eq!(Token::Identifier("baz".to_string()), unwrap(tok.next()));
    }

    #[test]
    fn tokenize_field_access() {
        let input = "foo.bar.baz";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::Identifier("foo".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Dot, unwrap(tok.next()));
        assert_eq!(Token::Identifier("bar".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Dot, unwrap(tok.next()));
        assert_eq!(Token::Identifier("baz".to_string()), unwrap(tok.next()));
    }

    #[test]
    fn tokenize_comma_list() {
        let input = "foo, bar,baz";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::Identifier("foo".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Comma, unwrap(tok.next()));
        assert_eq!(Token::Identifier("bar".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Comma, unwrap(tok.next()));
        assert_eq!(Token::Identifier("baz".to_string()), unwrap(tok.next()));
    }

    #[test]
    fn tokenize_assignment() {
        let input = "let foo: bar = baz;";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::Let, unwrap(tok.next()));
        assert_eq!(Token::Identifier("foo".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Colon, unwrap(tok.next()));
        assert_eq!(Token::Identifier("bar".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Assign, unwrap(tok.next()));
        assert_eq!(Token::Identifier("baz".to_string()), unwrap(tok.next()));
        assert_eq!(Token::Semi, unwrap(tok.next()));
        assert_eq!(None, tok.next());
    }

    #[test]
    fn tokenize_single_float() {
        let input = "21.";
        let mut tok = Tokenizer::new(input);

        assert_eq!(Token::FloatLiteral(21.0), unwrap(tok.next()));
    }
}
