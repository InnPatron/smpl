use std::collections::HashMap;
use std::iter::Iterator;

use super::expr_parser::*;
use super::parser_err::*;
use super::tokens::*;
use crate::ast::*;
use crate::span::*;

pub type ParseErr<T> = Result<T, ParserError>;

macro_rules! consume_token  {

    ($input: expr, $state: expr) => {{
        let next = $input.next()
            .ok_or(parser_error!(ParserErrorKind::UnexpectedEOI, $state))?
            .map_err(|e| parser_error!(ParserErrorKind::TokenizerError(e), $state))?;
        next.to_data()
    }};

    ($input: expr, $token: pat, $state: expr) => {{
        let next = $input.next()
            .ok_or(parser_error!(ParserErrorKind::UnexpectedEOI, $state))?
            .map_err(|e| parser_error!(ParserErrorKind::TokenizerError(e), $state))?;
        let data = next.to_data();
        match data.1 {
            $token => data,
            _ => Err(parser_error!(ParserErrorKind::UnexpectedToken(data.1), $state, Some(data.0)))?,
        }
    }};

    ($input: expr, $token: pat => $e: expr, $state: expr) => {{
        let next = $input.next()
            .ok_or(parser_error!(ParserErrorKind::UnexpectedEOI, $state))?
            .map_err(|e| parser_error!(ParserErrorKind::TokenizerError(e), $state))?;
        let data = next.to_data();
        match data.1 {
            $token => (data.0, $e),
            _ => Err(parser_error!(ParserErrorKind::UnexpectedToken(data.1), $state, Some(data.0)))?,
        }
    }};
}

macro_rules! peek_token {
    ($tokenizer: expr, $lam: expr, $state: expr) => {
        ($tokenizer)
            .peek($lam)
            .ok_or(parser_error!(ParserErrorKind::UnexpectedEOI, $state))?
            .map_err(|e| parser_error!(e.into(), $state))?
    };
}

pub fn module(tokens: &mut BufferedTokenizer) -> ParseErr<Module> {
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

    let module = Module(name, decls);

    Ok(module)
}

fn annotations(tokens: &mut BufferedTokenizer) -> ParseErr<Vec<Annotation>> {
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
) -> ParseErr<Vec<(Ident, Option<String>)>> {
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
) -> ParseErr<(Ident, Option<String>)> {
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
        Ok((Ident(ident), Some(v)))
    } else {
        Ok((Ident(ident), None))
    }
}

fn use_decl(tokens: &mut BufferedTokenizer) -> ParseErr<DeclStmt> {
    let (uspan, _) =
        consume_token!(tokens, Token::Use, parser_state!("use-decl", "use"));
    let (mspan, module) = consume_token!(tokens, 
                                         Token::Identifier(i) => Ident(i),
                                         parser_state!("use-decl", "name"));
    let _semi = consume_token!(
        tokens,
        Token::Semi,
        parser_state!("use-decl", "semicolon")
    );

    let span = LocationSpan::new(uspan.start(), mspan.end());

    let use_decl = UseDecl(AstNode::new(module, mspan));

    let use_decl = DeclStmt::Use(AstNode::new(use_decl, span));

    Ok(use_decl)
}

fn where_clause(tokens: &mut BufferedTokenizer) -> ParseErr<WhereClause> {
    let _where = consume_token!(
        tokens,
        Token::Where,
        parser_state!("where-clause", "where")
    );

    let mut parameter_constraints = HashMap::new();

    loop {
        let (_, parameter) = consume_token!(
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
            .entry(parameter)
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

#[cfg(test)]
pub fn testfn_decl(tokens: &mut BufferedTokenizer) -> ParseErr<Function> {
    let decl = fn_decl(tokens, vec![], false)?;
    match decl {
        DeclStmt::Function(f) => Ok(f.to_data().0),
        _ => unreachable!(),
    }
}

fn fn_decl(
    tokens: &mut BufferedTokenizer,
    annotations: Vec<Annotation>,
    is_builtin: bool,
) -> ParseErr<DeclStmt> {
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
                                        Token::Identifier(i) => Ident(i),
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
            BuiltinFnParams::Checked(Some(production!(
                fn_param_list(tokens),
                parser_state!("fn-decl", "fn-param-list")
            )))
        } else {
            BuiltinFnParams::Checked(None)
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
        return_type = Some(production!(
            type_annotation(tokens),
            parser_state!("fn-decl", "return type")
        ));
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

    let mut body: Option<AstNode<Block>> = None;
    if !is_builtin {
        body = Some(block(tokens)?);
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
                params: params,
                return_type: return_type,
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

        let body = match body {
            Some(b) => b,
            None => {
                return Err(parser_error!(
                    ParserErrorKind::NoFnBody,
                    parser_state!("fn-decl", "body")
                ));
            }
        };

        Ok(DeclStmt::Function(AstNode::new(
            Function {
                name: AstNode::new(ident, idloc),
                params: params,
                return_type: return_type,
                body: body,
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
) -> ParseErr<Vec<AstNode<FnParameter>>> {
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

fn fn_param(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<FnParameter>> {
    let (idloc, ident) = consume_token!(tokens, 
                                        Token::Identifier(i) => Ident(i),
                                        parser_state!("fn-param", "parameter name"));
    let _colon = consume_token!(
        tokens,
        Token::Colon,
        parser_state!("fn-param", "param type colon")
    );
    let ann = production!(
        type_annotation(tokens),
        parser_state!("fn-param", "param-type")
    );

    let span = Span::combine(idloc, ann.span());
    let param = FnParameter {
        name: AstNode::new(ident, idloc),
        param_type: ann,
    };

    Ok(AstNode::new(param, span))
}

#[cfg(test)]
pub fn teststruct_decl(tokens: &mut BufferedTokenizer) -> ParseErr<Struct> {
    let decl = struct_decl(tokens, vec![])?.to_data().0;
    Ok(decl)
}

fn opaque_decl(
    tokens: &mut BufferedTokenizer,
    anns: Vec<Annotation>,
) -> ParseErr<AstNode<Opaque>> {
    let (opaque_loc, _) = consume_token!(
        tokens,
        Token::Opaque,
        parser_state!("opaque-decl", "opaque")
    );
    let (name_loc, struct_name) = consume_token!(tokens, 
                                               Token::Identifier(i) => Ident(i),
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

    let overall_span = LocationSpan::new(opaque_loc.start(), semi_loc.start());

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
) -> ParseErr<AstNode<Struct>> {
    let (struct_loc, _) = consume_token!(
        tokens,
        Token::Struct,
        parser_state!("struct-decl", "struct")
    );
    let (name_loc, struct_name) = consume_token!(tokens, 
                                               Token::Identifier(i) => Ident(i),
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
        StructBody(Some(body))
    } else {
        // Empty struct body
        StructBody(None)
    };

    // Get Keys
    let (rloc, _) = consume_token!(
        tokens,
        Token::RBrace,
        parser_state!("struct-decl", "fields rbrace")
    );

    let overall_span = LocationSpan::new(struct_loc.start(), rloc.start());

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

fn struct_field_list(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<Vec<StructField>> {
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

fn struct_field(tokens: &mut BufferedTokenizer) -> ParseErr<StructField> {
    let (idloc, ident) = consume_token!(tokens, 
                                        Token::Identifier(i) => Ident(i),
                                        parser_state!("struct-field", "name"));
    let _colon = consume_token!(
        tokens,
        Token::Colon,
        parser_state!("struct-field", "field type colon")
    );
    let ann = production!(
        type_annotation(tokens),
        parser_state!("struct-field", "type annotation")
    );

    Ok(StructField {
        name: AstNode::new(ident, idloc),
        field_type: ann,
    })
}

fn module_decl(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Ident>> {
    // Consume MOD
    let (modloc, _) =
        consume_token!(tokens, Token::Mod, parser_state!("mod-decl", "mod"));
    let (_idloc, ident) = consume_token!(tokens, 
                                         Token::Identifier(i) => Ident(i),
                                         parser_state!("mod-decl", "name"));
    let (semiloc, _) = consume_token!(
        tokens,
        Token::Semi,
        parser_state!("mod-decl", "semicolon")
    );

    let span = LocationSpan::new(modloc.start(), semiloc.end());
    Ok(AstNode::new(ident, span))
}

pub fn type_annotation(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<AstNode<TypeAnnotation>> {
    enum TypeAnnDec {
        Module,
        FnType,
        ArrayType,
        WidthConstraint,
        Err,
    }

    match peek_token!(
        tokens,
        |tok| match tok {
            Token::Fn => TypeAnnDec::FnType,
            Token::Identifier(_) => TypeAnnDec::Module,
            Token::LBracket => TypeAnnDec::ArrayType,
            Token::Base => TypeAnnDec::WidthConstraint,
            Token::LBrace => TypeAnnDec::WidthConstraint,

            _ => TypeAnnDec::Err,
        },
        parser_state!("type-annotation", "annotation-kind")
    ) {
        TypeAnnDec::Module => {
            let (bind, span) = production!(
                module_binding(tokens),
                parser_state!("type-annotation", "module-binding")
            )
            .to_data();

            let bind = if peek_token!(
                tokens,
                |tok| match tok {
                    Token::LParen => true,
                    _ => false,
                },
                parser_state!("type-annotation", "type-arguments")
            ) {
                let type_arg_list = production!(
                    type_arg_list(tokens),
                    parser_state!("type-annotation", "type-app")
                );

                TypedPath::Parameterized(bind, type_arg_list)
            } else {
                TypedPath::NillArity(bind)
            };

            Ok(AstNode::new(TypeAnnotation::Path(bind), span))
        }

        TypeAnnDec::FnType => Ok(production!(
            fn_type(tokens),
            parser_state!("type-annotation", "fn-type")
        )),

        TypeAnnDec::ArrayType => Ok(production!(
            array_type(tokens),
            parser_state!("type-annotation", "array-type")
        )),

        TypeAnnDec::WidthConstraint => {
            let constraints = production!(
                width_constraint_list(tokens),
                parser_state!("type-annotation", "width-constraint-list")
            );

            let (constraints, constraints_span) = constraints.to_data();

            Ok(AstNode::new(
                TypeAnnotation::WidthConstraint(constraints),
                constraints_span,
            ))
        }

        TypeAnnDec::Err => unimplemented!(),
    }
}

fn width_constraint_list(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<AstNode<Vec<AstNode<WidthConstraint>>>> {
    let first = production!(
        width_constraint(tokens),
        parser_state!("width-constraint-list", "constraint[0]")
    );

    let mut span = first.span();
    let mut constraints = vec![first];
    let mut counter = 1;
    while peek_token!(
        tokens,
        |tok| match tok {
            Token::Plus => true,
            _ => false,
        },
        parser_state!(
            "width-constraint-list",
            &format!("peek-constraint[{}]", counter)
        )
    ) {
        let _plus = consume_token!(
            tokens,
            Token::Plus,
            parser_state!("width-constraint-list", "constraint-concat")
        );
        let next_constraint = production!(
            width_constraint(tokens),
            parser_state!(
                "width-constraint-list",
                &format!("constraint[{}]", counter)
            )
        );

        span = LocationSpan::new(span.start(), next_constraint.span().end());
        constraints.push(next_constraint);

        counter += 1;
    }

    let constraints = AstNode::new(constraints, span);
    Ok(constraints)
}

fn width_constraint(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<AstNode<WidthConstraint>> {
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
                type_annotation(tokens),
                parser_state!("width-constraint", "constraint-base")
            )
            .to_data();

            let span = LocationSpan::new(base_loc.start(), name_loc.end());

            let constraint = AstNode::new(
                WidthConstraint::BaseStruct(AstNode::new(name, name_loc)),
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
                struct_field_list(tokens),
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
            let span = LocationSpan::new(l_loc.start(), r_loc.end());
            let constraint =
                AstNode::new(WidthConstraint::Anonymous(fields), span);

            Ok(constraint)
        }

        WCDec::Error(tok) => panic!("{}", tok),
    }
}

pub fn module_binding(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<AstNode<ModulePath>> {
    let mut path = Vec::new();
    let (floc, first) = consume_token!(tokens, 
                                        Token::Identifier(i) => Ident(i),
                                        parser_state!("module-binding", "root"));

    let mut binding_span = LocationSpan::new(floc.start(), floc.end());
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
                                          Token::Identifier(i) => Ident(i),
                                          parser_state!("module-binding", "segment name"));
        path.push(AstNode::new(next, nloc));
        binding_span = LocationSpan::new(floc.start(), nloc.end());
    }

    Ok(AstNode::new(ModulePath(path), binding_span))
}

fn array_type(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<AstNode<TypeAnnotation>> {
    let (lloc, _) = consume_token!(
        tokens,
        Token::LBracket,
        parser_state!("array-type", "lbracket")
    );
    let base_type = Box::new(production!(
        type_annotation(tokens),
        parser_state!("array-type", "base-type")
    ));
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

    let array_type_span = LocationSpan::new(lloc.start(), rloc.end());

    if number <= 0 {
        unimplemented!(
            "Parser error: number of elements must be greater than 0. Found {}",
            number
        );
    }

    Ok(AstNode::new(
        TypeAnnotation::Array(base_type, number as u64),
        array_type_span,
    ))
}

fn fn_type(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<AstNode<TypeAnnotation>> {
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

    let mut params = None;
    if peek_token!(
        tokens,
        |tok| match tok {
            Token::RParen => false,
            _ => true,
        },
        parser_state!("fn-type", "param rparen")
    ) {
        params = Some(production!(
            fn_type_params(tokens),
            parser_state!("fn-type", "fn-type-params")
        ));
    }

    let (rparenloc, _) = consume_token!(
        tokens,
        Token::RParen,
        parser_state!("fn-type", "param rparen")
    );

    let mut fn_type_span = LocationSpan::new(fnloc.start(), rparenloc.end());

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
            type_annotation(tokens),
            parser_state!("fn-type", "return type")
        );
        let return_span = ret.span();

        return_type = Some(Box::new(ret));
        fn_type_span = Span::combine(fn_type_span, return_span);
    }

    Ok(AstNode::new(
        TypeAnnotation::FnType(type_params, params, return_type),
        fn_type_span,
    ))
}

fn fn_type_params(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<Vec<AstNode<TypeAnnotation>>> {
    let mut list = vec![production!(
        type_annotation(tokens),
        parser_state!("fn-type-params", "param-type")
    )];

    loop {
        if peek_token!(
            tokens,
            |tok| match tok {
                Token::Comma => true,
                _ => false,
            },
            parser_state!("fn-type-params", "comma separator")
        ) {
            let _comma = consume_token!(
                tokens,
                Token::Comma,
                parser_state!("fn-type-params", "comma separator")
            );
            if peek_token!(
                tokens,
                |tok| match tok {
                    Token::RParen => false,
                    _ => true,
                },
                parser_state!("fn-type-params", "rparen")
            ) {
                list.push(production!(
                    type_annotation(tokens),
                    parser_state!("fn-type-params", "param-type")
                ));
                continue;
            }
        }

        break;
    }

    Ok(list)
}

pub fn block(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Block>> {
    let (lloc, _) =
        consume_token!(tokens, Token::LBrace, parser_state!("block", "lbrace"));

    let mut stmts = Vec::new();
    // Parse for all statements untile '}'
    while peek_token!(
        tokens,
        |tok| match tok {
            Token::RBrace => false,
            _ => true,
        },
        parser_state!("block", "rbrace")
    ) {
        stmts.push(production!(stmt(tokens), parser_state!("block", "stmt")));
    }

    let (rloc, _) =
        consume_token!(tokens, Token::RBrace, parser_state!("block", "rbrace"));

    let span = LocationSpan::new(lloc.start(), rloc.end());

    let block = Block(stmts);

    Ok(AstNode::new(block, span))
}

#[cfg(test)]
pub fn teststmt(tokens: &mut BufferedTokenizer) -> ParseErr<Stmt> {
    stmt(tokens)
}

fn stmt(tokens: &mut BufferedTokenizer) -> ParseErr<Stmt> {
    enum StmtDec {
        Continue,
        Break,
        Return,

        While,
        If,

        LocalVar,
        PotentialAssign,

        Expr,
    }

    let stmt = match peek_token!(
        tokens,
        |tok| match tok {
            Token::Continue => StmtDec::Continue,
            Token::Break => StmtDec::Break,
            Token::Return => StmtDec::Return,

            Token::While => StmtDec::While,
            Token::If => StmtDec::If,

            Token::Let => StmtDec::LocalVar,

            Token::Identifier(_) => StmtDec::PotentialAssign,

            _ => StmtDec::Expr,
        },
        parser_state!("stmt", "stmt kind")
    ) {
        StmtDec::Continue => Stmt::ExprStmt(production!(
            continue_stmt(tokens),
            parser_state!("stmt", "continue")
        )),

        StmtDec::Break => Stmt::ExprStmt(production!(
            break_stmt(tokens),
            parser_state!("stmt", "break")
        )),

        StmtDec::Return => Stmt::ExprStmt(production!(
            return_stmt(tokens),
            parser_state!("stmt", "return")
        )),

        StmtDec::While => Stmt::ExprStmt(production!(
            while_stmt(tokens),
            parser_state!("stmt", "while")
        )),

        StmtDec::If => Stmt::ExprStmt(production!(
            if_stmt(tokens),
            parser_state!("stmt", "if")
        )),

        StmtDec::LocalVar => Stmt::ExprStmt(production!(
            local_var_decl(tokens),
            parser_state!("stmt", "local-var-decl")
        )),

        StmtDec::PotentialAssign => production!(
            potential_assign(tokens),
            parser_state!("stmt", "potential-assign")
        ),

        StmtDec::Expr => {
            let expr = production!(
                piped_expr(tokens, &[Delimiter::Semi]),
                parser_state!("stmt", "stmt-expr")
            );

            let _semi = consume_token!(
                tokens,
                Token::Semi,
                parser_state!("stmt-expr", "semicolon")
            );

            Stmt::Expr(expr)
        }
    };

    Ok(stmt)
}

fn potential_assign(tokens: &mut BufferedTokenizer) -> ParseErr<Stmt> {
    enum Dec {
        AccessPath,
        ModulePath,
        FnCallOrTypeArgFnCall,
        Indexing,
        DefSingletonAssignment,
        DefSingletonExpr,
    }

    enum PathDec {
        Assign,
        Expr,
    }

    let (base_span, base_ident) = consume_token!(tokens,
                                                 Token::Identifier(ident) => Ident(ident),
                                                 parser_state!("potential-assign", "root"));

    // Check if there is a full Access Path
    // If there is module path or function call, parse for expr ';'
    // If there is a 'ident = ...', parse for assignment
    // If there is a 'ident op ...', parse for expr ';'
    let path = match peek_token!(
        tokens,
        |tok| match tok {
            Token::Dot => Dec::AccessPath,
            Token::ColonColon => Dec::ModulePath,
            Token::LParen => Dec::FnCallOrTypeArgFnCall,
            Token::LBracket => Dec::Indexing,
            Token::Assign => Dec::DefSingletonAssignment,
            _ => Dec::DefSingletonExpr,
        },
        parser_state!("potential-assign", "lhs kind")
    ) {
        Dec::AccessPath => {
            let span = base_span;
            let root = PathSegment::Ident(AstNode::new(base_ident, span));

            let (path, _span) = production!(
                access_path(tokens, root),
                parser_state!("potential-assign", "access-path")
            )
            .to_data();

            match path {
                Expr::FieldAccess(path) => path,
                _ => unreachable!(),
            }
        }

        Dec::ModulePath => {
            // Expecting: expr ';'
            // Module paths are not lvalues

            let path = production!(
                expr_module_path(tokens, base_ident, base_span),
                parser_state!("stmt-expr-module-path", "module-path")
            );

            let expr = production!(
                prebase_piped_expr(tokens, path, &[Delimiter::Semi]),
                parser_state!("stmt-expr-module-path", "expr")
            );

            let _semi = consume_token!(
                tokens,
                Token::Semi,
                parser_state!("stmt-expr-module-path", "semicolon")
            );

            return Ok(Stmt::Expr(expr));
        }

        Dec::FnCallOrTypeArgFnCall => {
            // Expecting: expr ';'
            // Function calls are not lvalues

            let path =
                ModulePath(vec![AstNode::new(base_ident, base_span.clone())]);
            let path_span = base_span.clone();

            let (lspan, _lparen) = consume_token!(
                tokens,
                Token::LParen,
                parser_state!("stmt-expr-potential-fn-call", "lparen")
            );

            let (type_args, args, args_span) = if peek_token!(
                tokens,
                |tok| match tok {
                    Token::Type => true,
                    _ => false,
                },
                parser_state!("stmt-expr-potential-fn-call", "type-args?")
            ) {
                let type_args = production!(
                    type_arg_list_post_lparen(tokens),
                    parser_state!("stmt-expr-fn-call", "type-args")
                );

                enum LocalDec {
                    TypeApp,
                    FnCall,
                    Err,
                }

                // Checks if a function call or a typed path
                match peek_token!(
                    tokens,
                    |tok| match tok {
                        Token::LParen => LocalDec::FnCall,
                        Token::Semi => LocalDec::TypeApp,

                        _ => LocalDec::Err,
                    },
                    parser_state!(
                        "stmt-expr-fn-call-or-type-app?",
                        "fn-call-lparen"
                    )
                ) {
                    LocalDec::TypeApp => {
                        // TODO: type arg spans
                        // A type-app, NOT a function call
                        let _semi = consume_token!(
                            tokens,
                            Token::Semi,
                            parser_state!("stmt-expr-type-app", "semicolon")
                        );
                        let typed_path =
                            TypedPath::Parameterized(path, type_args);
                        let typed_path =
                            Expr::Path(AstNode::new(typed_path, path_span));
                        return Ok(Stmt::Expr(AstNode::new(
                            typed_path, path_span,
                        )));
                    }

                    LocalDec::FnCall => (),

                    LocalDec::Err => unimplemented!(
                        "Unexpected token: {:?}",
                        tokens.next().unwrap()
                    ),
                }

                let (args, args_span) = production!(
                    fn_args(tokens),
                    parser_state!("expr-module-path", "fn-call")
                )
                .to_data();

                (Some(type_args), args, args_span)
            } else {
                let (args, args_span) = production!(
                    fn_args_post_lparen(tokens, lspan),
                    parser_state!("stmt-expr-fn-call", "fn-args")
                )
                .to_data();

                (None, args, args_span)
            };

            let args =
                args.map(|v| v.into_iter().map(|a| a.to_data().0).collect());

            let fn_path = match type_args {
                Some(args) => TypedPath::Parameterized(path, args),
                None => TypedPath::NillArity(path),
            };

            let fn_call = FnCall {
                path: AstNode::new(fn_path, base_span),
                args: args,
            };

            let span = Span::combine(base_span, args_span);
            let expr_base = AstNode::new(fn_call, span);
            let expr_base = AstNode::new(Expr::FnCall(expr_base), span);

            let expr = production!(
                prebase_piped_expr(tokens, expr_base, &[Delimiter::Semi]),
                parser_state!("stmt-expr-fn-call", "expr")
            );

            let _semi = consume_token!(
                tokens,
                Token::Semi,
                parser_state!("stmt-expr-fn-call", "semicolon")
            );

            return Ok(Stmt::Expr(expr));
        }

        Dec::Indexing => {
            let _lbracket = consume_token!(
                tokens,
                Token::LBracket,
                parser_state!("potential_assign-indexing", "lbracket")
            );
            let indexer = production!(
                piped_expr(tokens, &[Delimiter::RBracket]),
                parser_state!("potential_assign-indexing", "indexer-expr")
            );
            let (indexer, _) = indexer.to_data();
            let (_rspan, _rbracket) = consume_token!(
                tokens,
                Token::RBracket,
                parser_state!("potential_assign-indexing", "rbracket")
            );

            if peek_token!(
                tokens,
                |tok| match tok {
                    Token::Dot => true,
                    _ => false,
                },
                parser_state!("potential-assign", "is-access-path")
            ) {
                // Access path with indexing as root
                let span = base_span;
                let root = PathSegment::Indexing(
                    AstNode::new(base_ident, span),
                    Box::new(indexer),
                );

                let (path, _span) = production!(
                    access_path(tokens, root),
                    parser_state!("potential-assign", "access-path")
                )
                .to_data();
                match path {
                    Expr::FieldAccess(path) => path,
                    _ => unreachable!(),
                }
            } else {
                //Single indexing
                let span = base_span;
                let root = PathSegment::Indexing(
                    AstNode::new(base_ident, span),
                    Box::new(indexer),
                );

                let path = vec![root];
                let path = Path(path);

                AstNode::new(path, span)
            }
        }

        Dec::DefSingletonAssignment => {
            let _assignop = consume_token!(
                tokens,
                Token::Assign,
                parser_state!("assignment", "=")
            );

            let value = production!(
                piped_expr(tokens, &[Delimiter::Semi]),
                parser_state!("assignment", "value")
            );
            let (value, value_span) = value.to_data();

            let _semi = consume_token!(
                tokens,
                Token::Semi,
                parser_state!("assignment", "semicolon")
            );

            let segment =
                PathSegment::Ident(AstNode::new(base_ident, base_span));
            let path = vec![segment];
            let path = AstNode::new(Path(path), base_span);

            let assignment_span = Span::combine(base_span, value_span);
            let assignment = Assignment {
                name: path,
                value: value,
            };

            let assignment = ExprStmt::Assignment(assignment);

            return Ok(Stmt::ExprStmt(AstNode::new(
                assignment,
                assignment_span,
            )));
        }

        Dec::DefSingletonExpr => {
            // Expecting: expr ';'
            // 'ident op' is not a lvalue
            let _span = base_span;
            let expr = production!(
                piped_expr(tokens, &[Delimiter::Semi]),
                parser_state!("expr-stmt-singleton", "expr")
            );

            let _semi = consume_token!(
                tokens,
                Token::Semi,
                parser_state!("expr-stmt-singleton", "semicolon")
            );

            return Ok(Stmt::Expr(expr));
        }
    };

    // Found a full path
    // Check if it's an assignment or expression

    match peek_token!(
        tokens,
        |tok| match tok {
            Token::Assign => PathDec::Assign,
            _ => PathDec::Expr,
        },
        parser_state!("full-path-potential-assign", "=;")
    ) {
        PathDec::Assign => {
            let path = path;

            let _assign = consume_token!(
                tokens,
                Token::Assign,
                parser_state!("assignment", "=")
            );

            let (value, value_span) = production!(
                piped_expr(tokens, &[Delimiter::Semi]),
                parser_state!("assignment", "expr")
            )
            .to_data();

            let _semi = consume_token!(
                tokens,
                Token::Semi,
                parser_state!("assignment", "semicolon")
            );

            let span = Span::combine(path.span(), value_span);
            let assignment = Assignment {
                name: path,
                value: value,
            };

            Ok(Stmt::ExprStmt(AstNode::new(
                ExprStmt::Assignment(assignment),
                span,
            )))
        }

        PathDec::Expr => {
            let _span = path.span();
            let _path = Expr::FieldAccess(path);

            let expr = production!(
                piped_expr(tokens, &[Delimiter::Semi]),
                parser_state!("stmt-expr-path", "expr")
            );

            let _semi = consume_token!(
                tokens,
                Token::Semi,
                parser_state!("stmt-expr-path", "semicolon")
            );

            Ok(Stmt::Expr(expr))
        }
    }
}

fn local_var_decl(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<AstNode<ExprStmt>> {
    let (letloc, _) = consume_token!(
        tokens,
        Token::Let,
        parser_state!("local-var-decl", "let")
    );

    let (iloc, ident) = consume_token!(tokens, 
                                       Token::Identifier(i) => Ident(i),
                                       parser_state!("local-var-decl", "name"));
    let ident = AstNode::new(ident, iloc);

    let mut type_anno = None;

    if peek_token!(
        tokens,
        |tok| match tok {
            Token::Colon => true,
            _ => false,
        },
        parser_state!("local-var-decl", "type colon")
    ) {
        let _colon = consume_token!(
            tokens,
            Token::Colon,
            parser_state!("local-var-decl", "type colon")
        );
        type_anno = Some(production!(
            type_annotation(tokens),
            parser_state!("local-var-decl", "type annotation")
        ));
    }

    let _assign = consume_token!(
        tokens,
        Token::Assign,
        parser_state!("local-var-decl", "=")
    );

    let init_value = production!(
        piped_expr(tokens, &[Delimiter::Semi]),
        parser_state!("local-var-decl", "value")
    )
    .to_data();

    let (semiloc, _) = consume_token!(
        tokens,
        Token::Semi,
        parser_state!("local-var-decl", "semicolon")
    );

    let span = LocationSpan::new(letloc.start(), semiloc.end());

    let local_var_decl = LocalVarDecl {
        var_type: type_anno,
        var_name: ident,
        var_init: init_value.0,
    };

    Ok(AstNode::new(ExprStmt::LocalVarDecl(local_var_decl), span))
}

fn if_stmt(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ExprStmt>> {
    enum IfDec {
        Elif,
        Else,
        End,
    }

    let (ifloc, _) =
        consume_token!(tokens, Token::If, parser_state!("if-stmt", "if"));

    let mut end = ifloc;

    let first_branch = production!(
        if_branch(tokens),
        parser_state!("if-stmt", "first branch")
    );
    end = first_branch.block.span();

    let mut branches = vec![first_branch];
    let mut default_branch = None;

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
                end = branch.block.span();

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

                end = block.span();
                default_branch = Some(block);

                break;
            }

            IfDec::End => break,
        }
    }

    let span = Span::combine(ifloc, end);

    let if_stmt = If {
        branches: branches,
        default_block: default_branch,
    };

    Ok(AstNode::new(ExprStmt::If(if_stmt), span))
}

fn if_branch(tokens: &mut BufferedTokenizer) -> ParseErr<Branch> {
    let conditional = piped_expr(tokens, &[Delimiter::LBrace])?;

    let block = production!(block(tokens), parser_state!("if-branch", "block"));

    let branch = Branch {
        conditional: conditional,
        block: block,
    };

    Ok(branch)
}

fn while_stmt(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ExprStmt>> {
    let (whileloc, _) = consume_token!(
        tokens,
        Token::While,
        parser_state!("while-stmt", "while")
    );

    let conditional = production!(
        piped_expr(tokens, &[Delimiter::LBrace]),
        parser_state!("while-stmt", "condition")
    );

    let block =
        production!(block(tokens), parser_state!("while-stmt", "block"));

    let span = Span::combine(whileloc, block.span());

    let while_stmt = While {
        conditional: conditional,
        block: block,
    };

    Ok(AstNode::new(ExprStmt::While(while_stmt), span))
}

fn return_stmt(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ExprStmt>> {
    let (returnloc, _) = consume_token!(
        tokens,
        Token::Return,
        parser_state!("return-stmt", "return")
    );

    let mut end = returnloc;

    let expr = if peek_token!(
        tokens,
        |tok| match tok {
            Token::Semi => true,
            _ => false,
        },
        parser_state!("return-stmt", "semi-colon")
    ) {
        // No expression
        let (semiloc, _) = consume_token!(
            tokens,
            Token::Semi,
            parser_state!("return-stmt", "semicolon")
        );
        end = semiloc;

        None
    } else {
        // Expression
        let expr = production!(
            piped_expr(tokens, &[Delimiter::Semi]),
            parser_state!("return-stmt", "expr")
        );

        let _semi = consume_token!(
            tokens,
            Token::Semi,
            parser_state!("return-stmt", "semicolon")
        );

        end = expr.span();

        Some(expr)
    };

    let span = Span::combine(returnloc, end);
    Ok(AstNode::new(
        ExprStmt::Return(span, expr.map(|e| e.to_data().0)),
        span,
    ))
}

fn continue_stmt(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<AstNode<ExprStmt>> {
    let (contloc, _) = consume_token!(
        tokens,
        Token::Continue,
        parser_state!("continue-stmt", "continue")
    );
    let (semiloc, _) = consume_token!(
        tokens,
        Token::Semi,
        parser_state!("continue-stmt", "semicolon")
    );

    let span = LocationSpan::new(contloc.start(), semiloc.end());
    Ok(AstNode::new(ExprStmt::Continue(span), span))
}

fn break_stmt(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ExprStmt>> {
    let (contloc, _) = consume_token!(
        tokens,
        Token::Break,
        parser_state!("break-stmt", "break")
    );
    let (semiloc, _) = consume_token!(
        tokens,
        Token::Semi,
        parser_state!("break-stmt", "semicolon")
    );

    let span = LocationSpan::new(contloc.start(), semiloc.end());
    Ok(AstNode::new(ExprStmt::Break(span), span))
}

fn type_param_list(tokens: &mut BufferedTokenizer) -> ParseErr<TypeParams> {
    let _lparen = consume_token!(
        tokens,
        Token::LParen,
        parser_state!("type_param_list", "lparen")
    );

    type_param_list_post_lparen(tokens)
}

fn type_param_list_post_lparen(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<TypeParams> {
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
) -> ParseErr<Vec<TypeAnnotation>> {
    let _lparen = consume_token!(
        tokens,
        Token::LParen,
        parser_state!("type-arg-list", "lparen")
    );

    type_arg_list_post_lparen(tokens)
}

pub fn type_arg_list_post_lparen(
    tokens: &mut BufferedTokenizer,
) -> ParseErr<Vec<TypeAnnotation>> {
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

    let type_args = type_args
        .into_iter()
        .map(|node| node.to_data().0)
        .collect::<Vec<_>>();

    Ok(type_args)
}
