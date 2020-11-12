use crate::ast::Ident;

#[derive(Clone, Debug, PartialEq)]
pub struct LiteralData {
    pub data: String,
    pub suffix: Option<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(Ident),
    StringLiteral(LiteralData),
    IntLiteral(LiteralData),
    FloatLiteral(LiteralData),
    BoolLiteral(bool),

    // Decl tokens
    Fn,
    Struct,
    Opaque,
    Mod,
    Use,
    Builtin,
    Unchecked,
    Type,
    Where,

    Comma,
    Arrow,

    Colon,
    ColonColon,
    Semi,
    Pound,

    Base,

    Import,
    Export,
    Except,
    From,
    All,
    As,

    // Stmt or expressions
    Underscore,
    Init,

    Extract,
    Continue,
    Break,
    Return,
    If,
    Else,
    Elif,
    While,
    Let,

    EqEq,
    BangEq,
    Eq,
    Dot,

    At,
    Amp,

    Pipe,

    Bang,
    And,
    Or,

    // Pairs
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
}
