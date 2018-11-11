use std::iter::{Iterator, Peekable};

use span::*;
use ast::*;
use super::tokens::*;
use super::parser::ParseErr;

pub enum Delimiter {
    RParen,
    RBracket,
    Comma,
}

pub fn expr(tokens: &mut BufferedTokenizer, delim_token: &[Delimiter], min_precedence: u64) 
    -> ParseErr<Expr> {

    unimplemented!()
}

pub fn parse_primary(tokens: &mut BufferedTokenizer) -> ParseErr<Expr> {
    unimplemented!()
}

fn get_op(token: &Token) -> Option<BinOp> {
    use self::Token::*;
    match token {
        Plus => Some(BinOp::Add),
        Minus => Some(BinOp::Sub),
        Star => Some(BinOp::Mul),
        Slash => Some(BinOp::Div),
        Percent => Some(BinOp::Mod),

        Gte => Some(BinOp::GreaterEq),
        Gt => Some(BinOp::Greater),
        Lte => Some(BinOp::LesserEq),
        Lt => Some(BinOp::Lesser),

        LAnd => Some(BinOp::LogicalAnd),
        LOr => Some(BinOp::LogicalOr),

        Eq => Some(BinOp::Eq),
        NEq => Some(BinOp::InEq),

        _ => None
    }
}

fn bin_op_precedence(op: &BinOp) -> u64 {
    use self::BinOp::*;
    match op {
        Add => 2,
        Sub => 2,
        Mul => 3,
        Div => 3,
        Mod => 3,

        LogicalAnd => 6,
        LogicalOr => 6,
        GreaterEq => 4,
        LesserEq => 4,
        Greater => 4,
        Lesser => 4,
        Eq => 5,
        InEq => 5,
    }
}

fn is_left_associative(op: &BinOp) -> bool {
    use self::BinOp::*;
    match op {
        Add => true,
        Sub => true,
        Mul => true,
        Div => true,
        Mod => true,

        LogicalAnd => true,
        LogicalOr => true,
        GreaterEq => true,
        LesserEq => true,
        Greater => true,
        Lesser => true,
        Eq => true,
        InEq => true,
    }
}
