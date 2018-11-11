use std::iter::{Iterator, Peekable};

use span::*;
use ast::*;
use super::tokens::*;
use super::parser::ParseErr;
use crate::consume_token;

pub enum Delimiter {
    RParen,
    RBracket,
    Comma,
}

pub fn expr(tokens: &mut BufferedTokenizer, 
            mut lhs: AstNode<Expr>, 
            delim_token: &[Delimiter], 
            min_precedence: u64) 
    -> ParseErr<AstNode<Expr>> {

    enum PeekResult {
        Execute(BinOp),
        Break,
    }

    if tokens.has_next() == false {
        return Err("Unexpected end of input".to_string());
    }

    loop {

        let peek_result = tokens.peek(|tok| {
            let op = match get_op(tok) {
                Some(op) => op,
                None => return PeekResult::Break,
            };

            if bin_op_precedence(&op) >= min_precedence {
                PeekResult::Execute(op)
            } else {
                PeekResult::Break
            }
        }).map_err(|e| format!("{:?}", e))?;

        if let PeekResult::Break = peek_result {
            break;
        }

        let (next_span, next) = consume_token!(tokens);
        let main_op = get_op(&next).unwrap();
        let main_prec = bin_op_precedence(&main_op);

        let mut rhs = parse_primary(tokens)?;

        loop {
            let peek_result = tokens.peek(|tok| {
                let op = match get_op(tok) {
                    Some(op) => op,
                    None => return PeekResult::Break,
                };

                if bin_op_precedence(&op) >= main_prec ||
                (is_left_associative(&op) == false && bin_op_precedence(&op) == main_prec) {
                    PeekResult::Execute(op)
                } else {
                    PeekResult::Break
                }
            }).map_err(|e| format!("{:?}", e))?;

            let rhs_op_peek = match peek_result {
                PeekResult::Execute(op) => op,
                PeekResult::Break => break,
            };

            let rhs_op_prec = bin_op_precedence(&rhs_op_peek);
            
            rhs = expr(tokens, rhs, delim_token, rhs_op_prec)?;
        }

        let span = Span::combine(lhs.span(), rhs.span());

        let bin_expr = {
            let (lhs, _) = lhs.to_data();
            let (rhs, _) = rhs.to_data();

            BinExpr {
                op: main_op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        };

        lhs = AstNode::new(Expr::Bin(AstNode::new(bin_expr, span)), span);
    }

    Ok(lhs)
}

pub fn parse_primary(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    enum PrimaryDec {
        Ident,
        Literal,
        UniExpr,
        LParen,
        Err,
    }

    match tokens.peek(|tok| {
        match tok {
            Token::Plus => PrimaryDec::UniExpr,
            Token::Minus => PrimaryDec::UniExpr,

            Token::IntLiteral(_) => PrimaryDec::Literal,
            Token::FloatLiteral(_) => PrimaryDec::Literal,
            Token::BoolLiteral(_) => PrimaryDec::Literal,
            Token::StringLiteral(_) => PrimaryDec::Literal,

            Token::LParen => PrimaryDec::LParen,

            Token::Identifier(_) => PrimaryDec::Ident,

            _ => PrimaryDec::Err,
        }
    }).map_err(|e| format!("{:?}", e))? {

        PrimaryDec::Ident => unimplemented!(),

        PrimaryDec::UniExpr => unimplemented!(),

        PrimaryDec::Literal => {
            let (next_span, next) = tokens
                .next()
                .unwrap()
                .map_err(|e| format!("{:?}", e))?
                .to_data();

            let literal = match next {
                Token::IntLiteral(i) => Literal::Int(i),
                Token::FloatLiteral(f) => Literal::Float(f),
                Token::BoolLiteral(b) => Literal::Bool(b),
                Token::StringLiteral(s) => Literal::String(s),

                _ => unreachable!(),
            };

            let span = next_span.make_span();

            Ok(AstNode::new(Expr::Literal(AstNode::new(literal, span)), span))
        }

        PrimaryDec::LParen => unimplemented!(),

        PrimaryDec::Err => unimplemented!(),
    }
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
