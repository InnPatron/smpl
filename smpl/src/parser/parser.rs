use std::iter::{Iterator, Peekable};

use span::*;
use ast::*;
use super::tokens::*;
use super::expr_parser::*;

pub type ParseErr<T> = Result<T, String>;

#[macro_export]
macro_rules! consume_token  {

    ($input: expr) => {{
        let next = $input.next()
            .ok_or("Unexpected end of input")?
            .map_err(|e| format!("{:?}", e))?;
        next.to_data()
    }};

    ($input: expr, $token: pat) => {{
        let next = $input.next()
            .ok_or("Unexpected end of input")?
            .map_err(|e| format!("{:?}", e))?;
        let data = next.to_data();
        match data.1 {
            $token => data,
            _ => Err(format!("Unexpected token {:?}", data.1))?,
        }
    }};

    ($input: expr, $token: pat => $e: expr) => {{
        let next = $input.next()
            .ok_or("Unexpected end of input")?
            .map_err(|e| format!("{:?}", e))?;
        let data = next.to_data();
        match data.1 {
            $token => (data.0, $e),
            _ => Err(format!("Unexpected token {:?}", data.1))?,
        }
    }};
}

pub fn module(tokens: &mut BufferedTokenizer) -> ParseErr<Module> {

    enum ModDec {
        Struct,
        Annotation,
        Function(bool),
        Err,
    }

    let mut name = None;
    if tokens.peek(|tok| {
        match tok {
            Token::Mod => true,
            _ => false,
        }
    }).map_err(|e| format!("{:?}", e))? {
        // Found mod declaration
        name = Some(module_decl(tokens)?);
    }

    let mut decls = Vec::new();
    let mut anno = Vec::new();

    while tokens.has_next() {
        match tokens.peek(|tok| {
            match tok {
                Token::Struct => ModDec::Struct,
                Token::Pound => ModDec::Annotation,
                Token::Fn => ModDec::Function(false),
                Token::Builtin => ModDec::Function(true),
                _ => ModDec::Err,
            }
        }).map_err(|e| format!("{:?}", e))? {
            ModDec::Struct => {
                decls.push(DeclStmt::Struct(struct_decl(tokens, anno)?));
                anno = Vec::new();
            }

            ModDec::Annotation => {
                anno = annotations(tokens)?;
            },

            ModDec::Function(is_builtin) => {
                decls.push(fn_decl(tokens, anno, is_builtin)?);
                anno = Vec::new();
            }

            ModDec::Err => unimplemented!("Unexpected token: {:?}", tokens.next().unwrap()),
        }
    }

    unimplemented!()
}

fn annotations(tokens: &mut BufferedTokenizer) -> ParseErr<Vec<Annotation>> {
    let mut annotations = Vec::new();

    while tokens.has_next() && tokens.peek(|tok| {
        match tok {
            Token::Pound => true,
            _ => false,
        }
    }).map_err(|e| format!("{:?}", e))? {
        let _pound = consume_token!(tokens, Token::Pound);
        let _lbracket = consume_token!(tokens, Token::LBracket);
        annotations.push(Annotation{ keys: kv_list(tokens)? });
        let _rbracket = consume_token!(tokens, Token::RBracket);
    }

    Ok(annotations)
}

fn kv_list(tokens: &mut BufferedTokenizer) -> ParseErr<Vec<(Ident, Option<String>)>> {
    let mut list = vec![kv_pair(tokens)?];

    while tokens.has_next() {
        if tokens.peek(|tok| {
            match tok {
                Token::Comma => true,
                _ => false
            }
        }).map_err(|e| format!("{:?}", e))? {
            let _comma = consume_token!(tokens, Token::Comma);
            if tokens.has_next() && tokens.peek(|tok| {
                match tok {
                    Token::RBracket => false,
                    _ => true,
                }
            }).map_err(|e| format!("{:?}", e))? {
                list.push(kv_pair(tokens)?);
                continue;
            }
        }

        break;
    }

    Ok(list)
}

fn kv_pair(tokens: &mut BufferedTokenizer) -> ParseErr<(Ident, Option<String>)> {
    let (_, ident) = consume_token!(tokens, Token::Identifier(i) => i);

    if tokens.peek(|tok| {
        match tok {
            Token::Eq => true,
            _ => false,
        }
    }).map_err(|e| format!("{:?}", e))? {
        let _eq = consume_token!(tokens, Token::Eq);
        let (_, v) = consume_token!(tokens, Token::StringLiteral(s) => s);
        Ok((Ident(ident), Some(v)))
    } else {
        Ok((Ident(ident), None))
    }
}

fn fn_decl(tokens: &mut BufferedTokenizer, annotations: Vec<Annotation>, is_builtin: bool) -> ParseErr<DeclStmt> {
    let mut span = Span::new(0, 0);
    if is_builtin {
        let (bloc, builtin) = consume_token!(tokens, Token::Builtin);
        span = bloc.make_span();
    }

    let (fnloc, _) = consume_token!(tokens, Token::Fn);
    if !is_builtin {
        span = fnloc.make_span();
    }

    let (idloc, ident) = consume_token!(tokens, Token::Identifier(i) => Ident(i));
    let _lparen = consume_token!(tokens, Token::LParen);

    let params = if tokens.peek(|tok| {
        match tok {
            Token::Unchecked => true,
            _ => false
        }
    }).map_err(|e| format!("{:?}", e))? {
        BuiltinFnParams::Unchecked
    } else {
        if tokens.peek(|tok| {
            match tok {
                Token::RParen => false,
                _ => true,
            }
        }).map_err(|e| format!("{:?}", e))? {
            BuiltinFnParams::Checked(Some(fn_param_list(tokens)?))
        } else {
            BuiltinFnParams::Checked(None)
        }
    };
        

    let (rloc, _) = consume_token!(tokens, Token::RParen);
    span = Span::combine(span, rloc.make_span());

    let mut body: Option<AstNode<Block>> = None;
    if !is_builtin {
        body = Some(block(tokens)?);
    }

    let mut return_type = None;
    if tokens.peek(|tok| {
        match tok {
            Token::Arrow => true,
            _ => false,
        }
    }).map_err(|e| format!("{:?}", e))? {
        let _arrow = consume_token!(tokens, Token::Arrow);
        return_type = Some(type_annotation(tokens)?);
    }

    if is_builtin {
        let (semiloc, _) = consume_token!(tokens, Token::Semi);
        span = Span::combine(span, semiloc.make_span());
    }

    if is_builtin {
        Ok(DeclStmt::BuiltinFunction(
            AstNode::new(
                BuiltinFunction {
                    name: AstNode::new(ident, idloc.make_span()),
                    params: params,
                    return_type: return_type,
                    annotations: annotations,
                },
                span)
            )
        )
    } else {
        let params = match params {
            BuiltinFnParams::Unchecked => return Err("Unchecked parameters in non-builtin function".to_string()),
            BuiltinFnParams::Checked(p) => p,
        };

        let body = match body {
            Some(b) => b,
            None => return Err("Function has no body".to_string()),
        };

        Ok(DeclStmt::Function(
            AstNode::new(
                Function {
                    name: AstNode::new(ident, idloc.make_span()),
                    params: params,
                    return_type: return_type,
                    body: body,
                    annotations: annotations,
                },
                span)
            )
        )
    }
}

pub fn fn_param_list(tokens: &mut BufferedTokenizer) -> ParseErr<Vec<AstNode<FnParameter>>> {
    let mut list = vec![fn_param(tokens)?];

    while tokens.has_next() {
        if tokens.peek(|tok| {
            match tok {
                Token::Comma => true,
                _ => false
            }
        }).map_err(|e| format!("{:?}", e))? {
            let _comma = consume_token!(tokens, Token::Comma);
            if tokens.has_next() && tokens.peek(|tok| {
                match tok {
                    Token::RParen => false,
                    _ => true,
                }
            }).map_err(|e| format!("{:?}", e))? {
                list.push(fn_param(tokens)?);
                continue;
            }
        }

        break;
    }

    Ok(list)
}

fn fn_param(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<FnParameter>> {
    let (idloc, ident) = consume_token!(tokens, Token::Identifier(i) => Ident(i));
    let _colon = consume_token!(tokens, Token::Colon);
    let ann = type_annotation(tokens)?;

    let span = Span::combine(idloc.make_span(), ann.span());
    let param = FnParameter {
        name: AstNode::new(ident, idloc.make_span()),
        param_type: ann,
    };

    Ok(AstNode::new(param, span))
}

fn struct_decl(tokens: &mut BufferedTokenizer, anns: Vec<Annotation>) -> ParseErr<AstNode<Struct>> {

    let (structLoc, _) = consume_token!(tokens, Token::Struct);
    let (nameLoc, structName) = consume_token!(tokens, Token::Identifier(i) => Ident(i));
    let _lbrace = consume_token!(tokens, Token::LBrace);

    // Check if no struct fields
    let body = if tokens.peek(|tok| {
        match tok {
            Token::RBrace => false,
            _ => true,
        }
    }).map_err(|e| format!("{:?}", e))? {
        let body = struct_field_list(tokens)?;
        StructBody(Some(body))
    } else {
        // Empty struct body
        StructBody(None)
    };

    // Get Keys
    let (rloc, _) = consume_token!(tokens, Token::RBrace);

    let overallSpan = LocationSpan::new(structLoc.start(), rloc.start());

    Ok(AstNode::new(Struct {
            name: AstNode::new(structName, nameLoc.make_span()),
            body: body,
            annotations: anns,
        }, overallSpan.make_span())
    )
}

fn struct_field_list(tokens: &mut BufferedTokenizer) -> ParseErr<Vec<StructField>> {
    let mut list = vec![struct_field(tokens)?];

    while tokens.has_next() {
        if tokens.peek(|tok| {
            match tok {
                Token::Comma => true,
                _ => false
            }
        }).map_err(|e| format!("{:?}", e))? {
            let _comma = consume_token!(tokens, Token::Comma);
            if tokens.has_next() && tokens.peek(|tok| {
                match tok {
                    Token::RBrace => false,
                    _ => true,
                }
            }).map_err(|e| format!("{:?}", e))? {
                list.push(struct_field(tokens)?);
                continue;
            }
        }

        break;
    }

    Ok(list)
}


fn struct_field(tokens: &mut BufferedTokenizer) -> ParseErr<StructField> {
    let (idloc, ident) = consume_token!(tokens, Token::Identifier(i) => Ident(i));
    let _colon = consume_token!(tokens, Token::Colon);
    let ann = type_annotation(tokens)?;

    Ok(StructField {
        name: AstNode::new(ident, idloc.make_span()),
        field_type: ann,
    })
}

fn module_decl(tokens: &mut BufferedTokenizer) -> ParseErr<Ident> {
    // Consume MOD
    let _mod_kw = consume_token!(tokens, Token::Mod);
    let (_idloc, ident) = consume_token!(tokens, Token::Identifier(i) => Ident(i));
    let _semi = consume_token!(tokens, Token::Semi);

    Ok(ident)
}

pub fn type_annotation(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<TypeAnnotation>> {
    
    enum TypeAnnDec {
        Module,
        FnType,
        ArrayType,
        Err,
    }

    if tokens.has_next() == false {
        unimplemented!("Unexpected End of Input: Expected Type Annotation");
    }
    match tokens.peek(|tok| {
        match tok {
            Token::Fn => TypeAnnDec::FnType,
            Token::Identifier(_) => TypeAnnDec::Module,
            Token::LBracket => TypeAnnDec::ArrayType,

            _ => TypeAnnDec::Err,
        }

    }).map_err(|e| format!("{:?}", e))? {
        TypeAnnDec::Module => {
            let (bind, span) = module_binding(tokens)?.to_data();

            Ok(AstNode::new(TypeAnnotation::Path(bind), span))
        },
        TypeAnnDec::FnType => fn_type(tokens),
        TypeAnnDec::ArrayType => array_type(tokens),

        TypeAnnDec::Err => unimplemented!(),
    }
}

pub fn module_binding(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ModulePath>> {
    let mut path = Vec::new();
    let (floc, first) =  consume_token!(tokens, Token::Identifier(i) => Ident(i));

    let mut binding_span = LocationSpan::new(floc.start(), floc.end());
    path.push(AstNode::new(first, floc.make_span()));

    if tokens.peek(|tok| {
        match tok {
            Token::ColonColon => true,
            _ => false,
        }
    }).map_err(|e| format!("{:?}", e))? {
        let _coloncolon = consume_token!(tokens, Token::ColonColon);
        let (nloc, next) = consume_token!(tokens, Token::Identifier(i) => Ident(i));
        path.push(AstNode::new(next, nloc.make_span()));
        binding_span = LocationSpan::new(floc.start(), nloc.end());
    }

    Ok(AstNode::new(ModulePath(path), binding_span.make_span()))
}

fn array_type(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<TypeAnnotation>> {
    let (lloc, _) = consume_token!(tokens, Token::LBracket);
    let base_type = Box::new(type_annotation(tokens)?);
    let _semi = consume_token!(tokens, Token::Semi);
    let (_, number) = consume_token!(tokens, Token::IntLiteral(i) => i);
    let (rloc, _) = consume_token!(tokens, Token::RBracket);

    let array_type_span = LocationSpan::new(lloc.start(), rloc.end());

    if (number <= 0) {
        unimplemented!("Parser error: number of elements must be greater than 0. Found {}", number);
    }

    Ok(AstNode::new(TypeAnnotation::Array(base_type, number as u64), 
                    array_type_span.make_span()))
}

fn fn_type(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<TypeAnnotation>> {
    let (fnloc, _) = consume_token!(tokens, Token::Fn);
    let _lparen = consume_token!(tokens, Token::LParen);
        
    let mut params = None;
    if tokens.has_next() && tokens.peek(|tok| {
        match tok {
            Token::RParen => false,
            _ => true,
        }
    }).map_err(|e| format!("{:?}", e))? {
        params = Some(fn_type_params(tokens)?);
    }

    let (rparenloc, _) = consume_token!(tokens, Token::RParen);

    let mut fn_type_span = LocationSpan::new(fnloc.start(), rparenloc.end()).make_span();

    let mut return_type = None;
    if tokens.has_next() && tokens.peek(|tok| {
        match tok {
            Token::Arrow => true,
            _ => false,
        }
    }).map_err(|e| format!("{:?}", e))? {
        let _arrow = consume_token!(tokens, Token::Arrow);
        let ret = type_annotation(tokens)?;
        let return_span = ret.span();

        return_type = Some(Box::new(ret));
        fn_type_span = Span::combine(fn_type_span, return_span);
    }

    Ok(AstNode::new(TypeAnnotation::FnType(params, return_type), fn_type_span))
}

fn fn_type_params(tokens: &mut BufferedTokenizer) -> ParseErr<Vec<AstNode<TypeAnnotation>>> {
    let mut list = vec![type_annotation(tokens)?];

    while tokens.has_next() {
        if tokens.peek(|tok| {
            match tok {
                Token::Comma => true,
                _ => false
            }
        }).map_err(|e| format!("{:?}", e))? {
            let _comma = consume_token!(tokens, Token::Comma);
            if tokens.has_next() && tokens.peek(|tok| {
                match tok {
                    Token::RParen => false,
                    _ => true,
                }
            }).map_err(|e| format!("{:?}", e))? {
                list.push(type_annotation(tokens)?);
                continue;
            }
        }

        break;
    }

    Ok(list)
}

pub fn block(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Block>> {

    enum BlockDec {
        Continue,
        Break,
        Return,

        While,

        Finished,
        Expr,
    }

    let (lloc, _) = consume_token!(tokens, Token::LBrace);

    let mut stmts = Vec::new();
    // Parse for all statements untile '}'
    while tokens.has_next() {
        match tokens.peek(|tok| {
            match tok {
                Token::Continue => BlockDec::Continue,
                Token::Break => BlockDec::Break,
                Token::Return => BlockDec::Return,

                Token::While => BlockDec::While,
                
                Token::RBrace => BlockDec::Finished,

                _ => BlockDec::Expr,
            }
        }).map_err(|e| format!("{:?}", e))? {

            BlockDec::Continue => {
                stmts.push(Stmt::ExprStmt(continue_stmt(tokens)?));
            }

            BlockDec::Break => {
                stmts.push(Stmt::ExprStmt(break_stmt(tokens)?));
            }

            BlockDec::Return => {
                stmts.push(Stmt::ExprStmt(return_stmt(tokens)?));
            }

            BlockDec::While => {
                stmts.push(Stmt::ExprStmt(while_stmt(tokens)?));
            }

            BlockDec::Expr => {
                let primary = parse_primary(tokens)?;
                let expr = expr(tokens, primary, &[Delimiter::Semi], 0)?;

                stmts.push(Stmt::Expr(expr));
            }

            BlockDec::Finished => break,
        }
    }
    
    let (rloc, _) = consume_token!(tokens, Token::RBrace);

    let span = LocationSpan::new(lloc.start(), rloc.end());

    let block = Block(stmts);

    Ok(AstNode::new(
        block,
        span.make_span())
    )
}

fn while_stmt(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ExprStmt>> {
    let (whileloc, _) = consume_token!(tokens, Token::While);

    let primary = parse_primary(tokens)?;
    let conditional = expr(tokens, primary, &[Delimiter::LBrace], 0)?;

    let block = block(tokens)?;

    let span = Span::combine(whileloc.make_span(), block.span());

    let while_stmt = While {
        conditional: conditional,
        block: block,
    };

    Ok(AstNode::new(ExprStmt::While(while_stmt), span))
}

fn return_stmt(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ExprStmt>> {
    let (returnloc, _) = consume_token!(tokens, Token::Return);

    let mut end = returnloc.make_span();

    let expr = if tokens.peek(|tok| {
        match tok {
            Token::Semi => true,
            _ => false,
        }
    }).map_err(|e| format!("{:?}", e))? {
        // No expression
        let (semiloc, _) = consume_token!(tokens, Token::Semi);
        end = semiloc.make_span();

        None
    } else {
        // Expression
        let primary = parse_primary(tokens)?;
        let expr = expr(tokens, primary, &[Delimiter::Semi], 0)?;

        let _semi = consume_token!(tokens, Token::Semi);

        end = expr.span();

        Some(expr)
    };

    let span = Span::combine(returnloc.make_span(), end);
    Ok(AstNode::new(ExprStmt::Return(span, expr.map(|e| e.to_data().0)), span))
}

fn continue_stmt(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ExprStmt>> {
    let (contloc, _) = consume_token!(tokens, Token::Continue);
    let (semiloc, _) = consume_token!(tokens, Token::Semi);

    let span = LocationSpan::new(contloc.start(), semiloc.end());
    Ok(AstNode::new(
        ExprStmt::Continue(span.make_span()),
        span.make_span())
    )
}

fn break_stmt(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ExprStmt>> {
    let (contloc, _) = consume_token!(tokens, Token::Break);
    let (semiloc, _) = consume_token!(tokens, Token::Semi);

    let span = LocationSpan::new(contloc.start(), semiloc.end());
    Ok(AstNode::new(
        ExprStmt::Break(span.make_span()),
        span.make_span())
    )
}
