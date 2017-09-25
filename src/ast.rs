pub struct Program(pub Vec<Statement>);

use super::Span;

pub struct Statement {
    span: Span
}

pub enum Keyword {
    If,
    While,
    Break,
    Return,
    Struct,
    Function,
}
