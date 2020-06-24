use std::ops::FnOnce;

use super::error::*;
use super::tokens::*;
use super::type_parser::*;
use crate::ast_node::{Spanned, AstNode};
use crate::new_ast::*;
use crate::span::*;
use crate::typable_ast::Typable;
use crate::expr_ast::*;

type BindingPower = u64;
type LedAction = Box<FnOnce(&mut BufferedTokenizer, Expr, BindingPower, &[ExprDelim]) -> ParserResult<Expr>>;
type ExprAction = Box<FnOnce(&mut BufferedTokenizer) -> ParserResult<Expr>>;
type StmtAction = Box<FnOnce(&mut BufferedTokenizer) -> ParserResult<Stmt>>;
type LbpData = (BindingPower, BindingPower, LedAction);

enum ExprDelim {
    Semi,
    Comma,
    NewBlock,
}

pub fn block(tokens: &mut BufferedTokenizer) -> ParserResult<TypedNode<Block>> {
    let (lspan, _) = consume_token!(
        tokens,
        Token::LBrace,
        parser_state!("block", "lbrace")
    );

    let mut stmts = Vec::new();

    while peek_token!(tokens,
        |tok| match tok {
            Token::RBrace => false,
            _ => true,
        },
        parser_state!("block", "rbrace")
    ) {

        let stmt = production!(
            stmt(tokens),
            parser_state!("block", "stmt")
        );

        stmts.push(stmt);
    }

    let (rspan, _) = consume_token!(
        tokens,
        Token::RBrace,
        parser_state!("block", "rbrace")
    );

    let block_span = LocationSpan::combine(lspan, rspan);

    let block = AstNode::new(Block(stmts), block_span);

    Ok(Typable::untyped(block))
}

fn stmt(tokens: &mut BufferedTokenizer) -> ParserResult<Stmt> {

    let action: StmtAction = peek_token!(tokens,
        |tok| match tok {
            Token::Break
                | Token::Return
                | Token::Continue => keyword_expr(tok.clone()),
            Token::If => Box::new(parse_if),
            Token::While => todo!(),

            _ => todo!(),
        },
        parser_state!("stmt", "stmt-kind")
    );

    action(tokens)
}

fn keyword_expr(kind: Token) -> StmtAction {
    Box::new(|tokens| {
        match kind {
            Token::Break => {
                let (break_span, _) = consume_token!(
                    tokens,
                    Token::Break,
                    parser_state!("break-stmt", "break")
                );

                let expr = if peek_token!(tokens,
                    |tok| match tok {
                        Token::Semi => false,
                        _ => true,
                    },
                    parser_state!("break-stmt", "expr?")
                ) {
                    // No semicolon found
                    // Parse for expression
                    let expr = production!(
                        top_level_expr(tokens, &[ExprDelim::Semi]),
                        parser_state!("break-stmt", "expr")
                    );

                    Some(expr)
                } else {
                    None
                };

                // Semicolon found
                let (semi_span, _) = consume_token!(
                    tokens,
                    Token::Semi,
                    parser_state!("break-stmt", "semi")
                );

                let break_span = LocationSpan::combine(break_span, semi_span);

                let node_break = AstNode::new(expr, break_span.clone());

                Ok(Stmt::ExprStmt(ExprStmt::Break(node_break)))
            }

            Token::Return => {
                let (return_span, _) = consume_token!(
                    tokens,
                    Token::Return,
                    parser_state!("return-stmt", "return")
                );

                let expr = if peek_token!(tokens,
                    |tok| match tok {
                        Token::Semi => false,
                        _ => true,
                    },
                    parser_state!("return-stmt", "expr?")
                ) {
                    // No semicolon found
                    // Parse for expression
                    let expr = production!(
                        top_level_expr(tokens, &[ExprDelim::Semi]),
                        parser_state!("return-stmt", "expr")
                    );

                    Some(expr)
                } else {
                    None
                };

                // Semicolon found
                let (semi_span, _) = consume_token!(
                    tokens,
                    Token::Semi,
                    parser_state!("return-stmt", "semi")
                );

                let return_span = LocationSpan::combine(return_span, semi_span);

                let node_return = AstNode::new(expr, return_span.clone());

                Ok(Stmt::ExprStmt(ExprStmt::Return(node_return)))
            }

            Token::Continue => {
                let (continue_span, _) = consume_token!(
                    tokens,
                    Token::Return,
                    parser_state!("continue-stmt", "continue")
                );

                // Semicolon found
                let (semi_span, _) = consume_token!(
                    tokens,
                    Token::Semi,
                    parser_state!("continue-stmt", "semi")
                );

                let continue_span = LocationSpan::combine(continue_span, semi_span);
                Ok(Stmt::ExprStmt(ExprStmt::Continue(AstNode::new((), continue_span))))
            }

            tok => panic!("keyword_expr cannot handle {:?}", tok),
        }
    })
}

fn parse_if(tokens: &mut BufferedTokenizer) -> ParserResult<Stmt> {

    enum IfDec {
        Elif,
        Else,
        End,
    }

    let (ifloc, _) =
        consume_token!(tokens, Token::If, parser_state!("if-expr", "if"));

    let mut end = ifloc.clone();

    // Parse the first
    let first_branch = production!(
        if_branch(tokens),
        parser_state!("if-expr", "first branch")
    );
    end = first_branch.block.data().span();

    let mut branches = vec![first_branch];
    let mut default_branch = None;

    //
    loop {
        match peek_token!(
            tokens,
            |tok| match tok {
                Token::Elif => IfDec::Elif,
                Token::Else => IfDec::Else,

                _ => IfDec::End,
            },
            parser_state!("if-stmt", "branches")
        ) {
            IfDec::Elif => {
                let _elif = consume_token!(
                    tokens,
                    Token::Elif,
                    parser_state!("if-stmt", "elif")
                );

                let branch = production!(
                    if_branch(tokens),
                    parser_state!("if-stmt", "elif branch")
                );
                end = branch.block.data().span();

                branches.push(branch);
            }

            IfDec::Else => {
                let _else = consume_token!(
                    tokens,
                    Token::Else,
                    parser_state!("if-stmt", "else")
                );
                let block = production!(
                    block(tokens),
                    parser_state!("if-stmt", "else-block")
                );

                end = block.data().span();
                default_branch = Some(block);

                break;
            }

            IfDec::End => break,
        }
    }

    let if_span = LocationSpan::combine(ifloc, end);

    let if_node = AstNode::new(If {
        branches,
        default_branch,
    }, if_span);

    if nud_action(tokens).is_ok() {
        // if-expr is in a statement position
        // Example:
        //      if foo { bar; } x = 1;
        Ok(Stmt::ExprStmt(ExprStmt::If(if_node)))
    } else {
        // if-expr is in an expression position
        // Example:
        //      if foo { bar; } + 4;
        //
        // NOTE: Well-formedness of if expressions handled by a separate pass

        let expr = production!(
            expr_with_left(
                tokens,
                Expr::If(Box::new(Typable::untyped(if_node))),
                &[ExprDelim::Semi],
                0),
            parser_state!("expr", "right"));

        Ok(Stmt::Expr(expr))
    }
}

fn if_branch(tokens: &mut BufferedTokenizer) -> ParserResult<Branch> {
    let conditional = top_level_expr(tokens, &[])?;

    let block = production!(block(tokens), parser_state!("if-branch", "block"));

    let branch = Branch {
        conditional: conditional,
        block: block,
    };

    Ok(branch)
}

fn top_level_expr(tokens: &mut BufferedTokenizer, delimiters: &[ExprDelim])
    -> ParserResult<Expr> {

    parse_expr(tokens, delimiters, 0)
}

fn parse_expr(
    tokens: &mut BufferedTokenizer,
    delimiters: &[ExprDelim],
    min_bp: BindingPower,
    ) -> ParserResult<Expr> {

    if tokens.eof() {
        todo!("Unexpected EOF 1");
    }

    // TODO: eat whitespace
    let expr_action = nud_action(tokens)?;
    let left: Expr = expr_action(tokens)?;
    expr_with_left(tokens, left, delimiters, min_bp)
}

fn expr_with_left(
    tokens: &mut BufferedTokenizer,
    mut left: Expr,
    delimiters: &[ExprDelim],
    min_bp: BindingPower,
    ) -> ParserResult<Expr> {

    while !tokens.eof() {
        // TODO: Delimiter check

        // TODO: postfix

        // TODO: eat whitespace

        // TODO: Delimiter check
        if tokens.eof() {
            break;
        }

        let (lbp, rbp, expr_action) = led_action(tokens)?;

        if lbp <= min_bp {
            break;
        }

        left = expr_action(tokens, left, rbp, delimiters)?;
    }

    Ok(left)
}


fn led_action(tokens: &BufferedTokenizer) -> ParserResult<LbpData> {

    fn binexpr(op: &Token) -> LedAction {
        let op = op.clone();
        Box::new(|tokens, left, rbp, delim| parse_binexpr(tokens, left, op, rbp, delim))
    }

    macro_rules! unreachable_led {
        ($msg: expr) => {{
            Box::new(|_: &mut BufferedTokenizer, _: Expr, _: BindingPower, _: &[ExprDelim]| -> ParserResult<Expr> {
                unreachable!($msg)
            }) as LedAction
        }}
    }

    let (lbp, rbp, action) = peek_token!(tokens,
        |tok| match tok {

                Token::RParen => (0, 0, unreachable_led!("led rparen")),

                Token::Pipe         => (10, 10, binexpr(tok)),

                Token::Plus         => (20, 20, binexpr(tok)),
                Token::Minus        => (20, 20, binexpr(tok)),
                Token::Star         => (30, 30, binexpr(tok)),
                Token::Slash        => (30, 30, binexpr(tok)),

                Token::Dot          => (100, 99, binexpr(tok)),

                Token::ColonColon   => (120, 120, binexpr(tok)),
                Token::Assign       => (140, 140, binexpr(tok)),

                _ => todo!(),

         },
        parser_state!("expr", "led")
    );

    Ok((lbp, rbp, action))
}



fn parse_binexpr(tokens: &mut BufferedTokenizer, left: Expr,
    op: Token, rbp: BindingPower, delims: &[ExprDelim])
    -> ParserResult<Expr> {

    macro_rules! basic_binop {
        ($left: expr, $op: expr, $rbp: expr, $delims: expr) => {{

            let lhs = Box::new($left);
            let rhs = Box::new(parse_expr(tokens, $delims, $rbp)?);

            let binexpr_span = Span::combine(lhs.span(), rhs.span());

            Expr::Bin(Typable::untyped(AstNode::new(BinExpr {
                op: $op,
                lhs,
                rhs,
            }, binexpr_span)))
        }}
    }

    match op {
        Token::Plus     => Ok(basic_binop!(left, BinOp::Add, rbp, delims)),
        Token::Minus    => Ok(basic_binop!(left, BinOp::Sub, rbp, delims)),
        Token::Star     => Ok(basic_binop!(left, BinOp::Mul, rbp, delims)),
        Token::Slash    => Ok(basic_binop!(left, BinOp::Div, rbp, delims)),
        Token::Pipe     => Ok(basic_binop!(left, BinOp::Pipe, rbp, delims)),

        _ => todo!(),
    }
}

fn nud_action(tokens: &BufferedTokenizer) -> ParserResult<ExprAction> {

    let action: ExprAction = peek_token!(tokens,
        |tok| match tok {
            Token::IntLiteral(..)
                | Token::FloatLiteral(..)
                | Token::BoolLiteral(..)
                | Token::StringLiteral(..) => Box::new(parse_literal) as ExprAction,

            Token::Plus
                | Token::Minus
                | Token::Bang => Box::new(uni_expr) as ExprAction,

            Token::LParen => Box::new(paren_expr) as ExprAction,

            Token::LBrace => Box::new(block_expr) as ExprAction,

            Token::Identifier(..) => Box::new(ident_expr) as ExprAction,
            _ => todo!()
        },
        parser_state!("expr", "nud")
    );

    Ok(action)
}

fn block_expr(tokens: &mut BufferedTokenizer) -> ParserResult<Expr> {
    let block = block(tokens)?;

    Ok(Expr::Block(block))
}

fn ident_expr(tokens: &mut BufferedTokenizer) -> ParserResult<Expr> {
    let (ident_span, ident) = consume_token!(tokens,
        Token::Identifier(ident) => Ident(ident),
        parser_state!("identifier-leaf", "root")
    );

    let ident_node = Typable::untyped(AstNode::new(ident, ident_span));

    Ok(Expr::Binding(ident_node))
}

fn paren_expr(tokens: &mut BufferedTokenizer) -> ParserResult<Expr> {
    let _ = consume_token!(
        tokens,
        Token::LParen,
        parser_state!("paren-expr", "lparen")
    );

    let inner = production!(
        parse_expr(tokens, &[ExprDelim::Semi], 0),
        parser_state!("paren-expr", "inner-expr")
    );

    let _ = consume_token!(
        tokens,
        Token::RParen,
        parser_state!("paren-expr", "rparen")
    );

    Ok(inner)
}

fn uni_expr(tokens: &mut BufferedTokenizer) -> ParserResult<Expr> {
    let (op_span, uniop) = tokens
        .next()
        .unwrap()
        .map_err(|e| parser_error!(e.into(), parser_state!("uni-expr", "uni-op")))?
        .to_data();

    let uniop = match uniop {
        Token::Plus => None,

        Token::Minus => Some(UniOp::Negate),

        Token::Bang => Some(UniOp::LogicalInvert),

        _ => unreachable!(),
    };

    let base = production!(
        parse_expr(tokens, &[ExprDelim::Semi], 0),
        parser_state!("uni-expr", "base")
    );

    let base_span = base.span();

    let uni_expr_span = Span::combine(op_span, base_span);

    match uniop {
        Some(op) => {
            let uni_expr = UniExpr {
                op,
                expr: Box::new(base),
            };

            let uni_node = Typable::untyped(AstNode::new(uni_expr, uni_expr_span));

            Ok(Expr::Uni(uni_node))
        }


        None => Ok(base),
    }
}

fn parse_literal(tokens: &mut BufferedTokenizer) -> ParserResult<Expr> {
    let (next_span, next) = tokens
        .next()
        .unwrap()
        .map_err(|e| parser_error!(e.into(), parser_state!("literal")))?
        .to_data();

    let literal = match next {
        Token::IntLiteral(i) => Literal::Int(i),
        Token::FloatLiteral(f) => Literal::Float(f),
        Token::BoolLiteral(b) => Literal::Bool(b),
        Token::StringLiteral(s) => Literal::String(s),

        _ => unreachable!(),
    };

    let literal_node = Expr::Literal(Typable::untyped(AstNode::new(literal, next_span)));

    Ok(literal_node)
}
