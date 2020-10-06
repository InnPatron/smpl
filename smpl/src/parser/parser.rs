use super::error::*;
use super::tokens::*;
use super::type_parser::*;
use super::stmt_parser;
use crate::ast::*;
use crate::ast_node::{Spanned, AstNode};
use crate::typable_ast::Typable;
use crate::span::*;
use crate::expr_ast::Block;

pub fn module(tokens: &mut BufferedTokenizer) -> ParserResult<Module<(), ()>> {
    enum ModDec {
        Struct,
        Opaque,
        Annotation,
        Function(bool),
        Use,
        Err,
    }

    let mut name = None;
    if peek_token!(
        tokens,
        |tok| match tok {
            Token::Mod => true,
            _ => false,
        },
        parser_state!("module", "mod-decl")
    ) {
        // Found mod declaration
        name = Some(production!(
            module_decl(tokens),
            parser_state!("module", "mod-decl")
        ));
    }

    let mut decls = Vec::new();
    let mut anno = Vec::new();

    while tokens.has_next() {
        match peek_token!(
            tokens,
            |tok| match tok {
                Token::Struct => ModDec::Struct,
                Token::Pound => ModDec::Annotation,
                Token::Fn => ModDec::Function(false),
                Token::Builtin => ModDec::Function(true),
                Token::Opaque => ModDec::Opaque,
                Token::Use => ModDec::Use,
                _ => ModDec::Err,
            },
            parser_state!("module", "decl-kind")
        ) {
            ModDec::Opaque => {
                decls.push(DeclStmt::Opaque(production!(
                    opaque_decl(tokens, anno),
                    parser_state!("module", "opaque-decl")
                )));
                anno = Vec::new();
            }

            ModDec::Struct => {
                decls.push(DeclStmt::Struct(production!(
                    struct_decl(tokens, anno),
                    parser_state!("module", "struct-decl")
                )));
                anno = Vec::new();
            }

            ModDec::Annotation => {
                anno = production!(
                    annotations(tokens),
                    parser_state!("module", "annotation")
                );
            }

            ModDec::Function(is_builtin) => {
                decls.push(production!(
                    fn_decl(tokens, anno, is_builtin),
                    parser_state!("module", "fn-decl")
                ));
                anno = Vec::new();
            }

            ModDec::Use => {
                decls.push(production!(
                    use_decl(tokens),
                    parser_state!("module", "use-decl")
                ));
                anno = Vec::new();
            }

            ModDec::Err => {
                unimplemented!("Unexpected token: {:?}", tokens.next().unwrap())
            }
        }
    }

    Ok(Module {
        ident: name,
        top_levels: decls,
    })
}

fn annotations(tokens: &mut BufferedTokenizer) -> ParserResult<Vec<Annotation>> {
    let mut annotations = Vec::new();

    while peek_token!(
        tokens,
        |tok| match tok {
            Token::Pound => true,
            _ => false,
        },
        parser_state!("annotations", "more-annotation-indication")
    ) {
        let _pound = consume_token!(
            tokens,
            Token::Pound,
            parser_state!("annotations", "pound")
        );
        let _lbracket = consume_token!(
            tokens,
            Token::LBracket,
            parser_state!("annotations", "lbracket")
        );
        annotations.push(Annotation {
            keys: production!(
                kv_list(tokens),
                parser_state!("annotations", "kv-list")
            ),
        });
        let _rbracket = consume_token!(
            tokens,
            Token::RBracket,
            parser_state!("annotations", "rbracket")
        );
    }

    Ok(annotations)
}

fn kv_list(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<Vec<(Ident, Option<String>)>> {
    let mut list = vec![kv_pair(tokens)?];

    loop {
        if peek_token!(
            tokens,
            |tok| match tok {
                Token::Comma => true,
                _ => false,
            },
            parser_state!("kv-list", "comma-separator")
        ) {
            let _comma = consume_token!(
                tokens,
                Token::Comma,
                parser_state!("kv-list", "comma-separator")
            );
            if peek_token!(
                tokens,
                |tok| match tok {
                    Token::RBracket => false,
                    _ => true,
                },
                parser_state!("kv-list", "rbracket")
            ) {
                list.push(production!(
                    kv_pair(tokens),
                    parser_state!("kv-list", "kv-pair")
                ));
                continue;
            }
        }

        break;
    }

    Ok(list)
}

fn kv_pair(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<(Ident, Option<String>)> {
    let (_, ident) = consume_token!(tokens,
                                    Token::Identifier(i) => i,
                                    parser_state!("kvpair", "key"));

    if peek_token!(
        tokens,
        |tok| match tok {
            Token::Assign => true,
            _ => false,
        },
        parser_state!("kv-pair", "=")
    ) {
        let _assign = consume_token!(
            tokens,
            Token::Assign,
            parser_state!("kvpair", "assign")
        );
        let (_, v) = consume_token!(tokens,
                                    Token::StringLiteral(s) => s,
                                    parser_state!("kvpair", "value"));
        Ok((Ident::Name(ident), Some(v)))
    } else {
        Ok((Ident::Name(ident), None))
    }
}

fn module_decl(tokens: &mut BufferedTokenizer) -> ParserResult<AstNode<Ident>> {
    // Consume MOD
    let (modloc, _) =
        consume_token!(tokens, Token::Mod, parser_state!("mod-decl", "mod"));
    let (_idloc, ident) = consume_token!(tokens,
                                         Token::Identifier(i) => Ident::Name(i),
                                         parser_state!("mod-decl", "name"));
    let (semiloc, _) = consume_token!(
        tokens,
        Token::Semi,
        parser_state!("mod-decl", "semicolon")
    );

    let span = LocationSpan::combine(modloc, semiloc);
    Ok(AstNode::new(ident, span))
}

fn use_decl(tokens: &mut BufferedTokenizer) -> ParserResult<DeclStmt<(), ()>> {
    let (uspan, _) =
        consume_token!(tokens, Token::Use, parser_state!("use-decl", "use"));
    let (mspan, module) = consume_token!(tokens,
                                         Token::Identifier(i) => Ident::Name(i),
                                         parser_state!("use-decl", "name"));
    let _semi = consume_token!(
        tokens,
        Token::Semi,
        parser_state!("use-decl", "semicolon")
    );

    let span = LocationSpan::combine(uspan, mspan.clone());

    let use_decl = UseDecl(AstNode::new(module, mspan));

    let use_decl = DeclStmt::Use(AstNode::new(use_decl, span));

    Ok(use_decl)
}

fn fn_decl(
    tokens: &mut BufferedTokenizer,
    annotations: Vec<Annotation>,
    is_builtin: bool,
) -> ParserResult<DeclStmt<(), ()>> {
    let mut span = Span::dummy();
    if is_builtin {
        let (bloc, _builtin) = consume_token!(
            tokens,
            Token::Builtin,
            parser_state!("fn-decl", "builtin")
        );
        span = bloc;
    }

    let (fnloc, _) =
        consume_token!(tokens, Token::Fn, parser_state!("fn-decl", "fn"));
    if !is_builtin {
        span = fnloc;
    }

    let (idloc, ident) = consume_token!(tokens,
                                        Token::Identifier(i) => Ident::Name(i),
                                        parser_state!("fn-decl", "name"));
    let _lparen = consume_token!(
        tokens,
        Token::LParen,
        parser_state!("fn-decl", "(type?) parameter lparen")
    );

    let type_params = if peek_token!(
        tokens,
        |tok| match tok {
            Token::Type => true,
            _ => false,
        },
        parser_state!("fn-decl", "type parameters?")
    ) {
        let params = Some(type_param_list_post_lparen(tokens)?);

        // Consume next lparen for actual parameters
        let _lparen = consume_token!(
            tokens,
            Token::LParen,
            parser_state!("fn-decl", "parameter lparen")
        );

        params
    } else {
        None
    };

    let params = if peek_token!(
        tokens,
        |tok| match tok {
            Token::Unchecked => true,
            _ => false,
        },
        parser_state!("fn-decl", "UNCHECKED paramter")
    ) {
        let _unchecked = consume_token!(
            tokens,
            Token::Unchecked,
            parser_state!("fn-decl", "UNCHECKED parameter")
        );
        BuiltinFnParams::Unchecked
    } else {
        if peek_token!(
            tokens,
            |tok| match tok {
                Token::RParen => false,
                _ => true,
            },
            parser_state!("fn-decl", "rparen")
        ) {
            BuiltinFnParams::Checked(production!(
                fn_param_list(tokens),
                parser_state!("fn-decl", "fn-param-list")
            ))
        } else {
            BuiltinFnParams::Checked(Vec::with_capacity(0))
        }
    };

    let (rloc, _) = consume_token!(
        tokens,
        Token::RParen,
        parser_state!("fn-decl", "parameter rparen")
    );
    span = Span::combine(span, rloc);

    let mut return_type = None;
    if peek_token!(
        tokens,
        |tok| match tok {
            Token::Arrow => true,
            _ => false,
        },
        parser_state!("fn-decl", "return type arrow?")
    ) {
        let _arrow = consume_token!(
            tokens,
            Token::Arrow,
            parser_state!("fn-decl", "return type arrow")
        );
        return_type = Some(Typable::untyped(production!(
            type_annotation(tokens, &[AnnDelim::NewBlock]),
            parser_state!("fn-decl", "return type")
        )));
    }

    let where_clause = if peek_token!(
        tokens,
        |tok| match tok {
            Token::Where => true,
            _ => false,
        },
        parser_state!("fn-decl", "where-clause?")
    ) {
        Some(production!(
            where_clause(tokens),
            parser_state!("fn-decl", "where-clause")
        ))
    } else {
        None
    };

    let mut body: Option<Typable<AstNode<Block<(), ()>>>> = None;
    if !is_builtin {
        body = Some(stmt_parser::block(tokens)?);
    }

    if is_builtin {
        let (semiloc, _) = consume_token!(
            tokens,
            Token::Semi,
            parser_state!("fn-decl", "builtin-semicolon")
        );
        span = Span::combine(span, semiloc);
    }

    if is_builtin {
        Ok(DeclStmt::BuiltinFunction(AstNode::new(
            BuiltinFunction {
                name: AstNode::new(ident, idloc),
                params,
                return_type,
                annotations: annotations,
                type_params: type_params,
                where_clause: where_clause,
            },
            span,
        )))
    } else {
        let params = match params {
            BuiltinFnParams::Unchecked => {
                return Err(parser_error!(
                    ParserErrorKind::NonbuiltinUncheckedParameters,
                    parser_state!("fn-decl", "param-validation")
                ));
            }
            BuiltinFnParams::Checked(p) => p,
        };

        let body = body.ok_or(parser_error!(
            ParserErrorKind::NoFnBody,
            parser_state!("fn-decl", "body")
        ))?;

        Ok(DeclStmt::Function(AstNode::new(
            Function {
                name: AstNode::new(ident, idloc),
                params,
                return_type,
                body,
                annotations: annotations,
                type_params: type_params,
                where_clause: where_clause,
            },
            span,
        )))
    }
}

pub fn fn_param_list(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<Vec<Typable<AstNode<FnParameter>>>> {
    let mut list = vec![production!(
        fn_param(tokens),
        parser_state!("fn-param-list", "fn-param")
    )];

    loop {
        if peek_token!(
            tokens,
            |tok| match tok {
                Token::Comma => true,
                _ => false,
            },
            parser_state!("fn-param-list", "comma separator")
        ) {
            let _comma = consume_token!(
                tokens,
                Token::Comma,
                parser_state!("fn-param-list", "comma separator")
            );
            if peek_token!(
                tokens,
                |tok| match tok {
                    Token::RParen => false,
                    _ => true,
                },
                parser_state!("fn-param-list", "rparen")
            ) {
                list.push(production!(
                    fn_param(tokens),
                    parser_state!("fn-param-list", "fn-param")
                ));
                continue;
            }
        }

        break;
    }

    Ok(list)
}

fn fn_param(tokens: &mut BufferedTokenizer) -> ParserResult<Typable<AstNode<FnParameter>>> {
    let (idloc, ident) = consume_token!(tokens,
                                        Token::Identifier(i) => Ident::Name(i),
                                        parser_state!("fn-param", "parameter name"));
    let _colon = consume_token!(
        tokens,
        Token::Colon,
        parser_state!("fn-param", "param type colon")
    );
    let ann = production!(
        type_annotation(tokens, &[AnnDelim::Comma]),
        parser_state!("fn-param", "param-type")
    );

    let span = Span::combine(idloc.clone(), ann.span());
    let param = FnParameter {
        name: AstNode::new(ident, idloc),
        param_type: ann,
    };

    Ok(Typable::untyped(AstNode::new(param, span)))
}

fn opaque_decl(
    tokens: &mut BufferedTokenizer,
    anns: Vec<Annotation>,
) -> ParserResult<AstNode<Opaque>> {
    let (opaque_loc, _) = consume_token!(
        tokens,
        Token::Opaque,
        parser_state!("opaque-decl", "opaque")
    );
    let (name_loc, struct_name) = consume_token!(tokens,
                                               Token::Identifier(i) => Ident::Name(i),
                                               parser_state!("opaque-decl", "name"));

    let type_params = if peek_token!(
        tokens,
        |tok| match tok {
            Token::LParen => true,

            _ => false,
        },
        parser_state!("opaque-decl", "type-parameters?")
    ) {
        Some(type_param_list(tokens)?)
    } else {
        None
    };

    let where_clause = if peek_token!(
        tokens,
        |tok| match tok {
            Token::Where => true,
            _ => false,
        },
        parser_state!("struct-decl", "where-clause?")
    ) {
        Some(production!(
            where_clause(tokens),
            parser_state!("struct-decl", "where-clause")
        ))
    } else {
        None
    };

    let (semi_loc, _) = consume_token!(
        tokens,
        Token::Semi,
        parser_state!("opaque-decl", "end-semi")
    );

    let overall_span = LocationSpan::combine(opaque_loc, semi_loc);

    Ok(AstNode::new(
        Opaque {
            name: AstNode::new(struct_name, name_loc),
            annotations: anns,
            type_params: type_params,
            where_clause: where_clause,
        },
        overall_span,
    ))
}

fn struct_decl(
    tokens: &mut BufferedTokenizer,
    anns: Vec<Annotation>,
) -> ParserResult<AstNode<Struct>> {
    let (struct_loc, _) = consume_token!(
        tokens,
        Token::Struct,
        parser_state!("struct-decl", "struct")
    );
    let (name_loc, struct_name) = consume_token!(tokens,
                                               Token::Identifier(i) => Ident::Name(i),
                                               parser_state!("struct-decl", "name"));

    let type_params = if peek_token!(
        tokens,
        |tok| match tok {
            Token::LParen => true,

            _ => false,
        },
        parser_state!("struct-decl", "type-parameters?")
    ) {
        Some(type_param_list(tokens)?)
    } else {
        None
    };

    let where_clause = if peek_token!(
        tokens,
        |tok| match tok {
            Token::Where => true,
            _ => false,
        },
        parser_state!("struct-decl", "where-clause?")
    ) {
        Some(production!(
            where_clause(tokens),
            parser_state!("struct-decl", "where-clause")
        ))
    } else {
        None
    };

    let _lbrace = consume_token!(
        tokens,
        Token::LBrace,
        parser_state!("struct-decl", "fields lbrace")
    );

    // Check if no struct fields
    let body = if peek_token!(
        tokens,
        |tok| match tok {
            Token::RBrace => false,
            _ => true,
        },
        parser_state!("struct-decl", "fields rbrace")
    ) {
        let body = production!(
            struct_field_list(tokens),
            parser_state!("struct-decl", "field-list")
        );
        body
    } else {
        vec![]
    };

    // Get Keys
    let (rloc, _) = consume_token!(
        tokens,
        Token::RBrace,
        parser_state!("struct-decl", "fields rbrace")
    );

    let overall_span = LocationSpan::combine(struct_loc, rloc);

    Ok(AstNode::new(
        Struct {
            name: AstNode::new(struct_name, name_loc),
            body: body,
            annotations: anns,
            type_params: type_params,
            where_clause: where_clause,
        },
        overall_span,
    ))
}

pub fn struct_field_list(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<Vec<StructField>> {
    let mut list = vec![production!(
        struct_field(tokens),
        parser_state!("struct-field-list", "struct-field")
    )];

    loop {
        if peek_token!(
            tokens,
            |tok| match tok {
                Token::Comma => true,
                _ => false,
            },
            parser_state!("struct-field-list", "comma separator")
        ) {
            let _comma = consume_token!(
                tokens,
                Token::Comma,
                parser_state!("struct-field-list", "comma separator")
            );
            if peek_token!(
                tokens,
                |tok| match tok {
                    Token::RBrace => false,
                    _ => true,
                },
                parser_state!("struct-field-list", "rbrace")
            ) {
                list.push(production!(
                    struct_field(tokens),
                    parser_state!("struct-field-list", "struct-field")
                ));
                continue;
            }
        }

        break;
    }

    Ok(list)
}

fn struct_field(tokens: &mut BufferedTokenizer) -> ParserResult<StructField> {
    let (idloc, ident) = consume_token!(tokens,
                                        Token::Identifier(i) => Ident::Name(i),
                                        parser_state!("struct-field", "name"));
    let _colon = consume_token!(
        tokens,
        Token::Colon,
        parser_state!("struct-field", "field type colon")
    );
    let ann = production!(
        type_annotation(tokens, &[AnnDelim::Comma]),
        parser_state!("struct-field", "type annotation")
    );

    Ok(StructField {
        name: AstNode::new(ident, idloc),
        field_type: Typable::untyped(ann),
    })
}

pub fn module_binding(
    tokens: &mut BufferedTokenizer,
) -> ParserResult<AstNode<ModulePath>> {
    let mut path = Vec::new();
    let (floc, first) = consume_token!(tokens,
                                        Token::Identifier(i) => Ident::Name(i),
                                        parser_state!("module-binding", "root"));

    let mut binding_span = floc.clone();
    path.push(AstNode::new(first, floc));

    if peek_token!(
        tokens,
        |tok| match tok {
            Token::ColonColon => true,
            _ => false,
        },
        parser_state!("module-binding", "segment coloncolon")
    ) {
        let _coloncolon = consume_token!(
            tokens,
            Token::ColonColon,
            parser_state!("module-binding", "segment coloncolon")
        );
        let (nloc, next) = consume_token!(tokens,
                                          Token::Identifier(i) => Ident::Name(i),
                                          parser_state!("module-binding", "segment name"));
        path.push(AstNode::new(next, nloc.clone()));
        binding_span = LocationSpan::combine(binding_span, nloc);
    }

    Ok(AstNode::new(ModulePath(path), binding_span))
}
