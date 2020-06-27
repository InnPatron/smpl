use std::collections::HashMap;
use std::iter::Iterator;

use super::error::*;
use super::new_parser;
use super::tokens::*;
use crate::new_ast::*;
use crate::ast_node::{Spanned, AstNode};
use crate::typable_ast::{Typable, Typed};
use crate::span::*;

type BindingPower = u64;
type TypeAnnAction = Box<FnOnce(&mut BufferedTokenizer) -> ParserResult<AstNode<TypeAnn>>>;
type PostAction = Box<FnOnce(&mut BufferedTokenizer, AstNode<TypeAnn>) -> ParserResult<AstNode<TypeAnn>>>;
type PostData = (BindingPower, PostAction);
type LbpData = (BindingPower, BindingPower, LedAction);
type LedAction = Box<FnOnce(&mut BufferedTokenizer, AstNode<TypeAnn>, BindingPower, &[AnnDelim]) -> ParserResult<AstNode<TypeAnn>>>;

pub use self::top_level_type_ann as type_annotation;

#[derive(PartialEq, Eq)]
pub enum AnnDelim {
    NewBlock,
    Assign,
    Comma,
    Semi,
}

pub fn top_level_type_ann(
    tokens: &mut BufferedTokenizer,
    ann_delims: &[AnnDelim],
) -> ParserResult<AstNode<TypeAnn>> {
    type_ann(tokens, 0, ann_delims)
}

fn type_ann(
    tokens: &mut BufferedTokenizer,
    min_bp: BindingPower,
    ann_delims: &[AnnDelim],
) -> ParserResult<AstNode<TypeAnn>> {

    let ann_action = nud_action(tokens)?;
    let mut left: AstNode<TypeAnn> = ann_action(tokens)?;

    while !tokens.eof() {
        if delim_break(tokens, ann_delims)? {
            break;
        }

        // Postfix operator?
        if let Some((lbp, post_action)) = postfix_action(tokens)? {
            if lbp <= min_bp {
                break;
            }

            left = post_action(tokens, left)?;
            continue;
        }

        // TODO: eat whitespace

        // Infix operator
        let (lbp, rbp, expr_action) = led_action(tokens)?;

        if lbp <= min_bp {
            break;
        }

        left = expr_action(tokens, left, rbp, ann_delims)?;
    }

    Ok(left)
}

fn led_action(tokens: &BufferedTokenizer) -> ParserResult<LbpData> {

    macro_rules! unreachable_led {
        ($msg: expr) => {{
            Box::new(|_: &mut BufferedTokenizer, _: AstNode<TypeAnn>, _: BindingPower, _: &[AnnDelim]| -> ParserResult<AstNode<TypeAnn>> {
                unreachable!($msg)
            }) as LedAction
        }}
    }

    let (lbp, rbp, action) = peek_token!(tokens, SPAN
        |tok, span| match tok {

                Token::RParen       => Ok((0, 0, unreachable_led!("led rparen"))),
                Token::RBrace       => Ok((0, 0, unreachable_led!("led rbrace"))),

                Token::Plus         => Ok((20, 20, Box::new(chain_constraints) as LedAction)),

                _                   => Err(parser_error!(ParserErrorKind::UnexpectedToken(tok.clone()), parser_state!("type-led"), span)),

         },
        parser_state!("expr", "led")
    )?;

    Ok((lbp, rbp, action))
}

fn chain_constraints(tokens: &mut BufferedTokenizer, left: AstNode<TypeAnn>, rbp: BindingPower, upper_delims: &[AnnDelim]) -> ParserResult<AstNode<TypeAnn>> {

    let _plus = consume_token!(tokens,
        Token::Plus,
        parser_state!("chain-constraint", "plus")
    );

    let right = top_level_type_ann(tokens, upper_delims)?;

    let (left, left_span) = left.split();
    let (right, right_span) = right.split();

    let ann = match (left, right) {
        (TypeAnn::WidthConstraints(mut left), TypeAnn::WidthConstraints(mut right)) => {
            left.append(&mut right);

            TypeAnn::WidthConstraints(left)
        }

        _ => todo!()
    };

    let span = Span::combine(left_span, right_span);

    Ok(AstNode::new(ann, span))
}

fn postfix_action(tokens: &BufferedTokenizer) -> ParserResult<Option<PostData>> {
    Ok(peek_token!(tokens,
        |tok| match tok {
            // TODO: Review LBP of postfix operators
            Token::LParen => Some((50, Box::new(typed_path) as PostAction)),

            _ => None,
        },
        parser_state!("expr", "postfix_action?")
    ))
}

fn typed_path(tokens: &mut BufferedTokenizer, left: AstNode<TypeAnn>) -> ParserResult<AstNode<TypeAnn>> {
    let (arg_list, end_span) = type_arg_list(tokens)?;

    let (left, left_span) = left.split();

    let new_ann: TypeAnn = match left {
        TypeAnn::ModulePath(path) => {

            let path_span = Span::combine(path.span(), end_span);

            let node = AstNode::new(TypedPath {
                base: path,
                params: arg_list,
            }, path_span);

            TypeAnn::Path(Typable::untyped(node))
        }

        _ => todo!(),
    };

    todo!();
}

fn delim_break(tokens: &BufferedTokenizer, ann_delims: &[AnnDelim]) -> ParserResult<bool> {
    Ok(peek_token!(tokens,
        |tok| match tok {

            Token::Assign => ann_delims.contains(&AnnDelim::Assign),
            Token::Comma => ann_delims.contains(&AnnDelim::Comma),
            Token::LBrace => ann_delims.contains(&AnnDelim::NewBlock),
            Token::Semi => ann_delims.contains(&AnnDelim::Semi),

            _ => false,
        },
        parser_state!("expr", "delim-check")
    ))
}

fn nud_action(tokens: &BufferedTokenizer) -> ParserResult<TypeAnnAction> {
    let action: TypeAnnAction = peek_token!(tokens,
        |tok| match tok {

            Token::Fn => Box::new(fn_type) as TypeAnnAction,

            Token::LBracket => Box::new(array_type) as TypeAnnAction,

            Token::LBrace => Box::new(width_constraint_ann) as TypeAnnAction,

            Token::Base => Box::new(width_constraint_ann) as TypeAnnAction,

            Token::Identifier(..) => Box::new(module_binding_ann) as TypeAnnAction,

            _ => todo!()
        },
        parser_state!("expr", "nud")
    );

    Ok(action)
}

fn module_binding_ann(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<TypeAnn>> {

    let module_path = new_parser::module_binding(tokens)?;

    let span = module_path.span();

    let node = AstNode::new(TypeAnn::ModulePath(Typable::untyped(module_path)), span);

    Ok(node)
}

fn width_constraint_ann(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<TypeAnn>> {
    let (constraint, span) = width_constraint(tokens)?.split();

    let node = AstNode::new(TypeAnn::WidthConstraints(
        vec![constraint]
    ), span);

    Ok(node)
}

fn width_constraint(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<WidthConstraint>> {
    enum WCDec {
        Base,
        Anon,
        Error(Token),
    }

    match peek_token!(
        tokens,
        |tok| match tok {
            Token::LBrace => WCDec::Anon,
            Token::Base => WCDec::Base,
            _ => WCDec::Error(tok.clone()),
        },
        parser_state!("width-constraint", "constraint-peek")
    ) {
        WCDec::Base => {
            let (base_loc, _) = consume_token!(
                tokens,
                Token::Base,
                parser_state!("width-constraint", "base")
            );

            let (name, name_loc) = production!(
                type_annotation(tokens, &[]),
                parser_state!("width-constraint", "constraint-base")
            )
            .split();

            let span = LocationSpan::combine(base_loc, name_loc.clone());

            let constraint = AstNode::new(
                WidthConstraint::BaseStruct(TypedNode::untyped(AstNode::new(name, name_loc))),
                span,
            );

            Ok(constraint)
        }

        WCDec::Anon => {
            let (l_loc, _) = consume_token!(
                tokens,
                Token::LBrace,
                parser_state!("width-constraint", "anonymous-open")
            );
            let fields = production!(
                new_parser::struct_field_list(tokens),
                parser_state!("width-constraint", "anonymous-struct")
            )
                .into_iter()
                .map(|struct_field| (struct_field.name, struct_field.field_type))
                .collect();

            let (r_loc, _) = consume_token!(
                tokens,
                Token::RBrace,
                parser_state!("width-constraint", "anonymous-close")
            );
            let span = LocationSpan::combine(l_loc, r_loc);
            let constraint =
                AstNode::new(WidthConstraint::Anonymous(fields), span);

            Ok(constraint)
        }

        WCDec::Error(tok) => panic!("{}", tok),
    }
}

// TODO: Un-lengthed arrays
fn array_type(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<TypeAnn>> {
    let (lloc, _) = consume_token!(
        tokens,
        Token::LBracket,
        parser_state!("array-type", "lbracket")
    );
    let base_type = production!(
        type_annotation(tokens, &[AnnDelim::Semi]),
        parser_state!("array-type", "base-type")
    );
    let _semi = consume_token!(
        tokens,
        Token::Semi,
        parser_state!("array-type", "semicolon")
    );
    let (_, number) = consume_token!(tokens,
                                     Token::IntLiteral(i) => i,
                                     parser_state!("array-type", "array size"));
    let (rloc, _) = consume_token!(
        tokens,
        Token::RBracket,
        parser_state!("array-type", "rbracket")
    );

    let array_type_span = LocationSpan::combine(lloc, rloc);

    if number <= 0 {
        unimplemented!(
            "Parser error: number of elements must be greater than 0. Found {}",
            number
        );
    }

    Ok(AstNode::new(
        TypeAnn::Array(Box::new(Typable::untyped(base_type)), number as u64),
        array_type_span,
    ))
}

fn fn_type(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<TypeAnn>> {
    let (fnloc, _) =
        consume_token!(tokens, Token::Fn, parser_state!("fn-type", "fn"));
    let _lparen = consume_token!(
        tokens,
        Token::LParen,
        parser_state!("fn-type", "(type?)param lparen")
    );

    let type_params = if peek_token!(
        tokens,
        |tok| match tok {
            Token::Type => true,
            _ => false,
        },
        parser_state!("fn-type", "type?")
    ) {
        let type_params = production!(
            type_param_list_post_lparen(tokens),
            parser_state!("fn-type", "type-params")
        );

        // Consume parameter lparen
        let _lparen = consume_token!(
            tokens,
            Token::LParen,
            parser_state!("fn-type", "param lparen")
        );

        Some(type_params)
    } else {
        None
    };

    let mut params = Vec::new();
    if peek_token!(
        tokens,
        |tok| match tok {
            Token::RParen => false,
            _ => true,
        },
        parser_state!("fn-type", "param rparen")
    ) {
        params = production!(
            fn_type_params(tokens),
            parser_state!("fn-type", "fn-type-params")
        );
    }

    let (rparenloc, _) = consume_token!(
        tokens,
        Token::RParen,
        parser_state!("fn-type", "param rparen")
    );

    let mut fn_type_span = LocationSpan::combine(fnloc, rparenloc);

    let mut return_type = None;
    if peek_token!(
        tokens,
        |tok| match tok {
            Token::Arrow => true,
            _ => false,
        },
        parser_state!("fn-type", "return type arrow")
    ) {
        let _arrow = consume_token!(
            tokens,
            Token::Arrow,
            parser_state!("fn-type", "return type arrow")
        );
        let ret = production!(
            type_annotation(tokens, &[]),
            parser_state!("fn-type", "return type")
        );
        let return_span = ret.span();

        return_type = Some(Box::new(Typable::untyped(ret)));
        fn_type_span = Span::combine(fn_type_span, return_span);
    }

    Ok(AstNode::new(
        TypeAnn::FnType(type_params, params, return_type),
        fn_type_span,
    ))
}

fn fn_type_params(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<Vec<TypedNode<TypeAnn>>> {
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
) -> ParserResult<(Vec<TypedNode<TypeAnn>>, Span)> {
    let _lparen = consume_token!(
        tokens,
        Token::LParen,
        parser_state!("type-arg-list", "lparen")
    );

    type_arg_list_post_lparen(tokens)
}

pub fn type_arg_list_post_lparen(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<(Vec<TypedNode<TypeAnn>>, Span)> {
    let _type = consume_token!(
        tokens,
        Token::Type,
        parser_state!("type-arg-list", "type")
    );

    let mut type_args = vec![Typable::untyped(production!(
        type_annotation(tokens, &[AnnDelim::Comma]),
        parser_state!("type-arg-list", "type-arg")
    ))];

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
                type_args.push(Typable::untyped(production!(
                    type_annotation(tokens, &[AnnDelim::Comma]),
                    parser_state!("type-arg-list", "type-arg")
                )));
                continue;
            }
        }

        break;
    }

    let (rparen_span, _) = consume_token!(
        tokens,
        Token::RParen,
        parser_state!("type-arg-list", "rparen")
    );

    Ok((type_args, rparen_span))
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
            type_annotation(tokens, &[AnnDelim::Comma, AnnDelim::NewBlock]),
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
