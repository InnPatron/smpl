use std::iter::{Enumerate, Iterator, Peekable};
use std::str::CharIndices;

use super::error::*;
use super::tokens::Token;
use crate::ast::Ident;
use crate::expr_ast::LiteralData;
use crate::span::Location;

pub type SpannedToken = (Location, Token, Location);

#[derive(Debug)]
struct CharInput<'input> {
    chars: Peekable<Enumerate<CharIndices<'input>>>,
    lookahead: Option<(Location, char)>,
    line: usize,
    column: usize,
    end_of_input: Location,
}

impl<'input> CharInput<'input> {
    fn new(input: &'input str) -> Self {
        let last = if input.len() == 0 {
            0
        } else {
            input.char_indices().rev().next().unwrap().0
        };
        let mut chars = input.char_indices().enumerate().peekable();
        let lookahead = chars
            .peek()
            .map(|(char_index, (byte_index, c))| (*byte_index, c.clone()));
        CharInput {
            chars: chars,
            line: 1,
            column: 0,
            lookahead: lookahead,
            end_of_input: last + 1,
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
            } else {
                self.column += 1;
            }

            (byte_index.clone(), c.clone())
        });

        let line = self.line;
        let column = self.column;
        self.lookahead =
            self.chars.peek().map(|(char_index, (byte_index, c))| {
                (byte_index.clone(), c.clone())
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
    pub fn new(input: &'input str) -> Self {
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
        let start = start;
        let end = end;

        &self.input[start..end]
    }

    fn take_while<F>(
        &mut self,
        start: Location,
        mut acceptor: F,
    ) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |c| !acceptor(c))
    }

    fn take_until<F>(
        &mut self,
        start: Location,
        mut terminator: F,
    ) -> (Location, &'input str)
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
                end += 1;

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

    fn op(
        &mut self,
        start: Location,
        c: char,
    ) -> Result<SpannedToken, SpannedError> {
        match c {
            '&' => Ok((start, Token::Amp, start + 1)),

            '|' => {
                if self.test_lookahead(|c| c == '>') {
                    let (end, _) = self.chars.next().ok_or(SpannedError {
                        error: TokenizerError::UnexpectedEndOfInput,
                        location: start,
                    })?;
                    Ok((start, Token::Pipe, end))
                } else {
                    Err(SpannedError {
                        error: TokenizerError::IncompleteToken(Token::Pipe),
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
                    Ok((start, Token::EqEq, end))
                } else {
                    Ok((start, Token::Eq, start + 1))
                }
            }

            '!' => {
                if self.test_lookahead(|c| c == '=') {
                    let (end, _) = self.chars.next().ok_or(SpannedError {
                        error: TokenizerError::UnexpectedEndOfInput,
                        location: start,
                    })?;
                    Ok((start, Token::BangEq, end))
                } else {
                    Ok((start, Token::Bang, start + 1))
                }
            }

            '@' => Ok((start, Token::At, start + 1)),

            ch => unreachable!("Missing handler for {}", ch),
        }
    }

    fn identifier(&mut self, start: Location) -> SpannedToken {
        let (end, ident) = self.take_while(start, is_ident_continue);

        let token = match ident {
            "base" => Token::Base,
            "enum" => Token::Enum,
            "fn" => Token::Fn,
            "mod" => Token::Mod,
            "struct" => Token::Struct,
            "opaque" => Token::Opaque,
            "use" => Token::Use,
            "if" => Token::If,
            "else" => Token::Else,
            "elif" => Token::Elif,
            "while" => Token::While,
            "let" => Token::Let,
            "builtin" => Token::Builtin,
            "UNCHECKED" => Token::Unchecked,
            "type" => Token::Type,
            "where" => Token::Where,
            "extract" => Token::Extract,
            "continue" => Token::Continue,
            "break" => Token::Break,
            "return" => Token::Return,
            "init" => Token::Init,
            "true" => Token::BoolLiteral(true),
            "false" => Token::BoolLiteral(false),
            "and" => Token::And,
            "or" => Token::Or,
            "import" => Token::Import,
            "export" => Token::Export,
            "from" => Token::From,
            "as" => Token::As,
            "all" => Token::All,
            "except" => Token::Except,
            "lam" => Token::Lam,
            "_" => Token::Underscore,
            "sig" => Token::Sig,
            "mval" => Token::MVal,
            "impl" => Token::Impl,

            str => Token::Ident(Ident::Unquoted(str.to_string())),
        };

        (start, token, end)
    }

    fn numeric_literal(
        &mut self,
        start: Location,
    ) -> Result<SpannedToken, SpannedError> {
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

                // TODO: Add support for literal suffixes
                let float_literal = LiteralData {
                    data: float.to_string(),
                    suffix: None,
                };
                return Ok((start, Token::FloatLiteral(float_literal), end));
            }
        }

        // TODO: Add support for literal suffixes
        let int_literal = LiteralData {
            data: int.to_string(),
            suffix: None,
        };

        Ok((start, Token::IntLiteral(int_literal), end))
    }

    fn quoted_ident(
        &mut self,
        start: Location,
    ) -> Result<SpannedToken, SpannedError> {
        let mut ident = String::new();
        while let Some((end, ch)) = self.chars.next() {
            match ch {
                '`' => {
                    return Ok((
                        start,
                        Token::Ident(Ident::Quoted(ident)),
                        end,
                    ));
                }

                ch => ident.push(ch),
            }
        }

        Err(SpannedError {
            error: TokenizerError::UnteriminatedQuotedIdent,
            location: start,
        })
    }

    fn string_literal(
        &mut self,
        start: Location,
    ) -> Result<SpannedToken, SpannedError> {
        let mut literal = String::new();
        while let Some((end, ch)) = self.chars.next() {
            match ch {
                '\"' => {
                    // TODO: Add support for literal suffixes
                    let literal = LiteralData {
                        data: literal,
                        suffix: None,
                    };
                    return Ok((start, Token::StringLiteral(literal), end));
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
                    Some(Ok((start, Token::Arrow, end)))
                }

                ',' => Some(Ok((start, Token::Comma, start + 1))),

                '.' => Some(Ok((start, Token::Dot, start + 1))),

                ';' => Some(Ok((start, Token::Semi, start + 1))),

                ':' if self.test_lookahead(is_colon) => {
                    let (end, _) = self.chars.next().unwrap();
                    Some(Ok((start, Token::ColonColon, end)))
                }

                ':' if self.test_lookahead(is_colon) == false => {
                    Some(Ok((start, Token::Colon, start + 1)))
                }

                '(' => Some(Ok((start, Token::LParen, start + 1))),

                ')' => Some(Ok((start, Token::RParen, start + 1))),

                '[' => Some(Ok((start, Token::LBracket, start + 1))),

                ']' => Some(Ok((start, Token::RBracket, start + 1))),

                '{' => Some(Ok((start, Token::LBrace, start + 1))),

                '}' => Some(Ok((start, Token::RBrace, start + 1))),

                '#' => Some(Ok((start, Token::Pound, start + 1))),

                '+' => Some(Ok((start, Token::Plus, start + 1))),

                '\"' => Some(self.string_literal(start)),
                '`' => Some(self.quoted_ident(start)),

                ch if is_ident_start(ch) => Some(Ok(self.identifier(start))),
                ch if is_digit(ch)
                    || (ch == '-' && self.test_lookahead(is_digit)) =>
                {
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

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    is_ident_start(c) || is_digit(c)
}

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

fn is_colon(c: char) -> bool {
    c == ':'
}

fn is_slash(c: char) -> bool {
    c == '/'
}

fn is_op(c: char) -> bool {
    c == '&' || c == '|' || c == '!' || c == '='
}
