use std::iter::{Iterator, Peekable};

use span::*;
use ast::*;
use super::tokens::*;
use super::parser::ParseErr;
use crate::consume_token;

#[derive(PartialEq)]
pub enum Delimiter {
    RParen,
    RBracket,
    Comma,
}

pub fn expr(tokens: &mut BufferedTokenizer, 
            mut lhs: AstNode<Expr>, 
            delim_tokens: &[Delimiter], 
            min_precedence: u64) 
    -> ParseErr<AstNode<Expr>> {

    enum PeekResult {
        Execute(BinOp),
        Break,
    }

    loop {

        if tokens.has_next() == false {
            return Ok(lhs);
        }

        let peek_result = tokens.peek(|tok| {

            if is_delim(tok, delim_tokens) {
                return PeekResult::Break;
            }

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
            if tokens.has_next() == false {
                break;
            }

            let peek_result = tokens.peek(|tok| {

                // TODO: Is this delimiter check correct?
                if is_delim(tok, delim_tokens) {
                    return PeekResult::Break;
                }

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
            
            rhs = expr(tokens, rhs, delim_tokens, rhs_op_prec)?;
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

        PrimaryDec::Ident => parse_ident_leaf(tokens),

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

        PrimaryDec::LParen => {
            let (lspan, _) = consume_token!(tokens, Token::LParen);

            let inner_lhs = parse_primary(tokens)?;
            let inner = expr(tokens, inner_lhs, &[Delimiter::RParen], 0)?;

            let (rspan, _) = consume_token!(tokens, Token::RParen);

            let span = LocationSpan::new(lspan.start(), rspan.end());
            let span = span.make_span();

            Ok(inner)
        }

        PrimaryDec::Err => unimplemented!(),
    }
}

fn parse_ident_leaf(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    enum IdentLeafDec {
        AccessPath,
        ModulePath,
        Singleton,
        FnCall,
        Indexing,
    }

    let (base_span, base_ident) = consume_token!(tokens,
                                                 Token::Identifier(ident) => Ident(ident));

    match tokens.peek(|tok| {
        match tok {
            Token::Dot => IdentLeafDec::AccessPath,
            Token::ColonColon => IdentLeafDec::ModulePath,
            Token::LParen => IdentLeafDec::FnCall,
            Token::LBracket => IdentLeafDec::Indexing,
            _ => IdentLeafDec::Singleton,
        }
    }).map_err(|e| format!("{:?}", e))? {

        IdentLeafDec::AccessPath => unimplemented!(),
        IdentLeafDec::ModulePath => module_path(tokens, base_ident, base_span),
        IdentLeafDec::FnCall => unimplemented!(),
        IdentLeafDec::Indexing => unimplemented!(),
        IdentLeafDec::Singleton => {
            let span = base_span.make_span();
            Ok(AstNode::new(Expr::Binding(AstNode::new(base_ident, span)), span))
        }
    }
}

fn fn_args(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Option<Vec<AstNode<Expr>>>>> {
    let (lspan, _) = consume_token!(tokens, Token::LParen);

    let mut args: Option<Vec<AstNode<Expr>>> = None;

    while tokens.has_next() &&
        tokens.peek(|tok| {
            match tok {
                Token::RParen => false,

                _ => true,
            }
        }).map_err(|e| format!("{:?}", e))? {

        let arg = parse_primary(tokens)?;
        let arg = expr(tokens, arg, &[Delimiter::RParen, Delimiter::Comma], 0)?;

        match args {
            Some(mut a) => {
                a.push(arg);
                args = Some(a);
            }
            None => args = Some(vec![arg]),
        }

        if tokens.peek(|tok| {
            match tok {
                Token::Comma => true,
                _ => false,
            }
        }).map_err(|e| format!("{:?}", e))? {
            let _comma = consume_token!(tokens, Token::Comma);
        }
    }

    let (rspan, _) = consume_token!(tokens, Token::RParen);

    let span = LocationSpan::new(lspan.start(), rspan.end());
    let span = span.make_span();

    Ok(AstNode::new(args, span))
}

fn module_path(tokens: &mut BufferedTokenizer, base: Ident, base_span: LocationSpan) 
    -> ParseErr<AstNode<Expr>> {

    // Assume there at least 1 '::'
    let root = AstNode::new(base, base_span.make_span());
    let mut path = vec![root];
    let mut end = base_span;

    while tokens.has_next() && 
        tokens.peek(|tok| {
            match tok {
                Token::ColonColon => true,
                _ => false,
            }
        }).map_err(|e| format!("{:?}", e))? {
    
        let (cspan, _) = consume_token!(tokens, Token::ColonColon);
        let (ispan, ident) = consume_token!(tokens, 
                                            Token::Identifier(i) => Ident(i));

        let span = ispan;
        end = span;     // Widen path span to end of current ident

        path.push(AstNode::new(ident, span.make_span()));
    }

    // End of module path
    // Check if FN call
    if tokens.has_next() &&
        tokens.peek(|tok| {
            match tok {
                Token::LParen => true,
                _ => false,
            }
        }).map_err(|e| format!("{:?}", e))? {

        // FN call
        let (args, args_span) = fn_args(tokens)?.to_data();

        let start = base_span.make_span();

        let span = Span::combine(start, args_span);

        let fn_call = FnCall {
            path: ModulePath(path), 
            args: args.map(|v| v.into_iter().map(|e| e.to_data().0).collect::<Vec<_>>()),
        };

        // TODO: FnCall chain check

        Ok(AstNode::new(Expr::FnCall(AstNode::new(fn_call, span)), span))

    } else {
        let span = LocationSpan::new(base_span.start(), end.end());
        let span = span.make_span();

        let mod_access = AstNode::new(ModulePath(path), span);
        Ok(AstNode::new(Expr::ModAccess(mod_access), span))
    }
}

fn is_delim(token: &Token, delim: &[Delimiter]) -> bool {
    let token = match token {
        Token::RParen => Delimiter::RParen,
        Token::RBracket => Delimiter::RBracket,
        Token::Comma => Delimiter::Comma,

        _ => return false,
    };

    delim.contains(&token)
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
