use std::ops::FnOnce;

use super::error::*;
use super::tokens::*;
use super::type_parser::*;
use crate::ast_node::AstNode;
use crate::new_ast::*;
use crate::span::*;
use crate::typable_ast::Typable;
use crate::expr_ast::*;

type BindingPower = u64;
type ExprAction = Box<FnOnce(&mut BufferedTokenizer) -> ParserResult<TypedNode<Expr>>>;
type StmtAction = Box<FnOnce(&mut BufferedTokenizer) -> ParserResult<Stmt>>;
type LbpData = (BindingPower, BindingPower, ExprAction);

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
            Token::If => todo!(),
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
                        top_level_expr(tokens, &[Token::Semi]),
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
                        top_level_expr(tokens, &[Token::Semi]),
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

fn top_level_expr(tokens: &mut BufferedTokenizer, delimiters: &[Token])
    -> ParserResult<TypedNode<Expr>> {

    todo!();
}

fn nud(tokens: &BufferedTokenizer) -> ParserResult<TypedNode<Expr>> {
    todo!();
}

fn expr_with_left(
    tokens: &mut BufferedTokenizer,
    left: TypedNode<Expr>,
    delimiters: &[Token],
    min_bp: BindingPower,
    ) -> ParserResult<TypedNode<Expr>> {
    todo!();
}
