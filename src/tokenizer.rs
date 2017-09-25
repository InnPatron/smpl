use ascii::*;
use super::Span;

pub fn tokenize(input: AsciiString) -> Result<Vec<Token>, ()> {
    let mut result = Vec::new();
    for (index, char) in input.into_iter().enumerate() {
        match char.as_char() {
            ';' => result.push(Token::new_at_location(TokenKind::Semicolon, index)),
            ',' => result.push(Token::new_at_location(TokenKind::Comma, index)),
            '.' => result.push(Token::new_at_location(TokenKind::Dot, index)),
            '=' => result.push(Token::new_at_location(TokenKind::Eq, index)),
            '<' => result.push(Token::new_at_location(TokenKind::LArrow, index)),
            '>' => result.push(Token::new_at_location(TokenKind::RArrow, index)),
            '{' => result.push(Token::new_at_location(TokenKind::LBrace, index)),
            '}' => result.push(Token::new_at_location(TokenKind::RBrace, index)),
            '(' => result.push(Token::new_at_location(TokenKind::LParen, index)),
            ')' => result.push(Token::new_at_location(TokenKind::RParen, index)),

            '+' => result.push(Token::new_at_location(TokenKind::Plus, index)),
            '-' => result.push(Token::new_at_location(TokenKind::Minus, index)),
            '*' => result.push(Token::new_at_location(TokenKind::Asterisk, index)),
            '/' => result.push(Token::new_at_location(TokenKind::Slash, index)),
            '%' => result.push(Token::new_at_location(TokenKind::Percent, index)),
            '&' => result.push(Token::new_at_location(TokenKind::Ampersand, index)),
            '!' => result.push(Token::new_at_location(TokenKind::Bang, index)),
            c @ _ => { 
                if c.is_whitespace() {
                    result.push(Token::new_at_location(TokenKind::Whitespace, index));
                } else if c.is_numeric() {
                    result.push(Token::new_at_location(TokenKind::Digit(c.to_string()
                                                                        .parse::<u8>()
                                                                        .unwrap()), index));
                } else if c.is_alphabetic() {
                    result.push(Token::new_at_location(TokenKind::Char(AsciiChar::from(c).unwrap()), index));
                } else {
                    unimplemented!("Unrecognized character. Should be err");
                }

            },
        }
    }

    Ok(result)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    fn new(kind: TokenKind, start: usize, end: usize) -> Token {
        Token {
            kind: kind,
            span: Span {
                start: start,
                end: end
            }
        }
    }

    fn new_at_location(kind: TokenKind, location: usize) -> Token {
        Token {
            kind: kind,
            span: Span {
                start: location,
                end: location + 1,
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Char(AsciiChar),
    Digit(u8),
    Whitespace,
    
    Comma,
    Dot,
    Eq,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LArrow,
    RArrow,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Ampersand,
    Bang
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use super::*;
    use ascii::*;

    #[test]
    fn braces() {
        let input = "{}";
        let input = tokenize(AsciiString::from_str(input).unwrap()).unwrap();
        assert_eq!(input.len(), 2);
        assert_eq!(input[0].kind, TokenKind::LBrace);
        assert_eq!(input[1].kind, TokenKind::RBrace);
    }
}
