use std::collections::HashMap;
use std::iter::Iterator;

use super::error::*;
use super::tokens::*;
use crate::new_ast::*;
use crate::ast_node::AstNode;
use crate::typable_ast::{Typable, Typed};
use crate::span::*;

pub fn type_annotation(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<TypeAnnotation>> {
    todo!();
}

fn width_constraint_list(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<Vec<AstNode<WidthConstraint>>>> {
    todo!();
}

fn width_constraint(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<WidthConstraint>> {
    todo!();
}

fn array_type(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<TypeAnnotation>> {
    todo!();
}

fn fn_type(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<TypeAnnotation>> {
    todo!();
}

fn fn_type_params(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<Vec<AstNode<TypeAnnotation>>> {
    todo!();
}

pub fn type_param_list(tokens: &mut BufferedTokenizer) -> ParserResult<TypeParams> {
    let _lparen = consume_token!(
        tokens,
        Token::LParen,
        parser_state!("type_param_list", "lparen")
    );

    type_param_list_post_lparen(tokens)
}

pub fn type_param_list_post_lparen(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<TypeParams> {
    let _type = consume_token!(
        tokens,
        Token::Type,
        parser_state!("type-param-list", "type")
    );

    let mut type_params = vec![consume_token!(tokens,
                                              Token::Identifier(ident) => Ident(ident),
                                              parser_state!("type-param-list", "type-param"))];

    loop {
        if peek_token!(
            tokens,
            |tok| match tok {
                Token::Comma => true,
                _ => false,
            },
            parser_state!("type-param-list", "comma-separator")
        ) {
            let _comma = consume_token!(
                tokens,
                Token::Comma,
                parser_state!("type-param-list", "comma-separator")
            );
            if peek_token!(
                tokens,
                |tok| match tok {
                    Token::RParen => false,
                    _ => true,
                },
                parser_state!("type-param-list", "rparen?")
            ) {
                type_params.push(consume_token!(tokens,
                                   Token::Identifier(ident) => Ident(ident),
                                   parser_state!("type-param-list", "type-param")));
                continue;
            }
        }

        break;
    }

    let _rparen = consume_token!(
        tokens,
        Token::RParen,
        parser_state!("type-param-list", "rparen")
    );

    let type_params = type_params
        .into_iter()
        .map(|(span, ident)| AstNode::new(ident, span))
        .collect::<Vec<_>>();

    Ok(TypeParams {
        params: type_params,
    })
}

pub fn type_arg_list(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<Vec<AstNode<TypeAnnotation>>> {
    let _lparen = consume_token!(
        tokens,
        Token::LParen,
        parser_state!("type-arg-list", "lparen")
    );

    type_arg_list_post_lparen(tokens)
}

pub fn type_arg_list_post_lparen(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<Vec<AstNode<TypeAnnotation>>> {
    let _type = consume_token!(
        tokens,
        Token::Type,
        parser_state!("type-arg-list", "type")
    );

    let mut type_args = vec![production!(
        type_annotation(tokens),
        parser_state!("type-arg-list", "type-arg")
    )];

    loop {
        if peek_token!(
            tokens,
            |tok| match tok {
                Token::Comma => true,
                _ => false,
            },
            parser_state!("type-arg-list", "comma-separator")
        ) {
            let _comma = consume_token!(
                tokens,
                Token::Comma,
                parser_state!("type-arg-list", "comma-separator")
            );
            if peek_token!(
                tokens,
                |tok| match tok {
                    Token::RParen => false,
                    _ => true,
                },
                parser_state!("type-arg-list", "rparen?")
            ) {
                type_args.push(production!(
                    type_annotation(tokens),
                    parser_state!("type-arg-list", "type-arg")
                ));
                continue;
            }
        }

        break;
    }

    let _rparen = consume_token!(
        tokens,
        Token::RParen,
        parser_state!("type-arg-list", "rparen")
    );

    Ok(type_args)
}

pub fn where_clause(tokens: &mut BufferedTokenizer) -> ParserResult<WhereClause> {
    let _where = consume_token!(
        tokens,
        Token::Where,
        parser_state!("where-clause", "where")
    );

    let mut parameter_constraints = HashMap::new();

    loop {
        let (param_span, parameter) = consume_token!(
            tokens,
            Token::Identifier(ident) => Ident(ident),
            parser_state!("where-clause-constraints", "param"));
        let _colon = consume_token!(
            tokens,
            Token::Colon,
            parser_state!("where-clause-constraints", "colon")
        );
        let annotation = production!(
            type_annotation(tokens),
            parser_state!("where-clause-constraints", "annotation")
        );

        parameter_constraints
            .entry(AstNode::new(parameter, param_span))
            .or_insert(Vec::new())
            .push(annotation);

        if peek_token!(
            tokens,
            |tok| match tok {
                Token::Comma => true,
                _ => false,
            },
            parser_state!("where-clause-constraints", "comma?")
        ) {
            let _comma = consume_token!(
                tokens,
                Token::Comma,
                parser_state!("where-clause-constraints", "comma")
            );
        } else {
            // No more commas
            // Assume no more where clause constraints
            break;
        }
    }

    let where_clause = WhereClause(parameter_constraints);
    Ok(where_clause)
}
