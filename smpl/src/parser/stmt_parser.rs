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
type ExprAction = Box<FnOnce(&mut BufferedTokenizer) -> ParserResult<Expr>>;
type StmtAction = Box<FnOnce(&mut BufferedTokenizer) -> ParserResult<Stmt>>;
type LbpData = (BindingPower, BindingPower, ExprAction);

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

    let nud = nud_action(tokens)?;
    let left = nud(tokens)?;

    todo!();
}

fn nud_action(tokens: &BufferedTokenizer) -> ParserResult<ExprAction> {

    let action = peek_token!(tokens,
        |tok| match tok {
            Token::IntLiteral(..)
                | Token::FloatLiteral(..)
                | Token::BoolLiteral(..)
                | Token::StringLiteral(..) => Box::new(parse_literal),

            _ => todo!()
        },
        parser_state!("expr", "nud")
    );

    Ok(action)
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

fn expr_with_left(
    tokens: &mut BufferedTokenizer,
    left: Expr,
    delimiters: &[ExprDelim],
    min_bp: BindingPower,
    ) -> ParserResult<Expr> {
    todo!();
}
