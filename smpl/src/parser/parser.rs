use std::iter::{Iterator, Peekable};

use span::*;
use ast::*;
use super::tokens::*;

pub type ParseErr<T> = Result<T, String>;

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

    let (semiloc, _) = consume_token!(tokens, Token::Semi);
    span = Span::combine(span, semiloc.make_span());

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

fn fn_param_list(tokens: &mut BufferedTokenizer) -> ParseErr<Vec<AstNode<FnParameter>>> {
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

fn type_annotation(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<TypeAnnotation>> {
    
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

fn module_binding(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<ModulePath>> {
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

fn block(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Block>> {
    let (lloc, _) = consume_token!(tokens, Token::LBrace);
    let (rloc, _) = consume_token!(tokens, Token::RBrace);

    let span = LocationSpan::new(lloc.start(), rloc.end());
    Ok(AstNode::new(
        unimplemented!(),
        span.make_span())
    )
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

fn expr(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    logic_expr(tokens)
}

fn bin_op(token: Token) -> BinOp {
    match token {
        Token::Eq => BinOp::Eq,
        Token::NEq => BinOp::InEq,

        Token::Gte => BinOp::GreaterEq,
        Token::Gt => BinOp::Greater,
        Token::Lte => BinOp::LesserEq,
        Token::Lt => BinOp::Lesser,

        Token::Plus => BinOp::Add,
        Token::Minus => BinOp::Sub,
        Token::Star => BinOp::Mul,
        Token::Slash => BinOp::Div,
        Token::Percent => BinOp::Mod,

        Token::LAnd => BinOp::LogicalAnd,
        Token::LOr => BinOp::LogicalOr,

        _ => panic!("Unrecognized bin op: {:?}", token),
    }
}

fn logic_expr(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    let lhs = equal_expr(tokens)?;

    if tokens.peek(|tok| {
        match tok {
            Token::LAnd | Token::LOr => true,
            _ => false
        }
    }).map_err(|e| format!("{:?}", e))? {
        let (_, op) = consume_token!(tokens);
        let rhs = logic_expr(tokens)?;

        let (lhs, seq) = lhs.to_data();
        let (rhs, srhs) = rhs.to_data();
        let span = Span::combine(seq, srhs);

        let bin = BinExpr {
            op: bin_op(op),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };

        let bin = AstNode::new(bin, span);
        return Ok(AstNode::new(Expr::Bin(bin), span));
    }

    Ok(lhs)
}

fn equal_expr(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    let lhs = relative_expr(tokens)?;

    if tokens.peek(|tok| {
        match tok {
            Token::Eq | Token::NEq => true,
            _ => false
        }
    }).map_err(|e| format!("{:?}", e))? {
        let (_, op) = consume_token!(tokens);
        let rhs = equal_expr(tokens)?;

        let (lhs, seq) = lhs.to_data();
        let (rhs, srhs) = rhs.to_data();
        let span = Span::combine(seq, srhs);

        let bin = BinExpr {
            op: bin_op(op),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };

        let bin = AstNode::new(bin, span);
        return Ok(AstNode::new(Expr::Bin(bin), span));
    }

    Ok(lhs)
}

fn relative_expr(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    let lhs = math_expr(tokens)?;

    if tokens.peek(|tok| {
        match tok {
            Token::Gte | Token::Gt | Token::Lte | Token::Lt => true,
            _ => false
        }
    }).map_err(|e| format!("{:?}", e))? {
        let (_, op) = consume_token!(tokens);
        let rhs = math_expr(tokens)?;

        let (lhs, seq) = lhs.to_data();
        let (rhs, srhs) = rhs.to_data();
        let span = Span::combine(seq, srhs);

        let bin = BinExpr {
            op: bin_op(op),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };

        let bin = AstNode::new(bin, span);
        return Ok(AstNode::new(Expr::Bin(bin), span));
    }

    Ok(lhs)
}

fn math_expr(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    let lhs = factor(tokens)?;

    if tokens.peek(|tok| {
        match tok {
            Token::Plus | Token::Minus => true,
            _ => false
        }
    }).map_err(|e| format!("{:?}", e))? {
        let (_, op) = consume_token!(tokens);
        let rhs = math_expr(tokens)?;

        let (lhs, seq) = lhs.to_data();
        let (rhs, srhs) = rhs.to_data();
        let span = Span::combine(seq, srhs);

        let bin = BinExpr {
            op: bin_op(op),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };

        let bin = AstNode::new(bin, span);
        return Ok(AstNode::new(Expr::Bin(bin), span));
    }

    Ok(lhs)
}

fn factor(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    let lhs = uni_expr(tokens)?;

    if tokens.peek(|tok| {
        match tok {
            Token::Star | Token::Slash | Token::Percent => true,
            _ => false
        }
    }).map_err(|e| format!("{:?}", e))? {
        let (_, op) = consume_token!(tokens);
        let rhs = factor(tokens)?;

        let (lhs, seq) = lhs.to_data();
        let (rhs, srhs) = rhs.to_data();
        let span = Span::combine(seq, srhs);

        let bin = BinExpr {
            op: bin_op(op),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };

        let bin = AstNode::new(bin, span);
        return Ok(AstNode::new(Expr::Bin(bin), span));
    }

    Ok(lhs)
}

fn uni_op(token: Token) -> UniOp {
    match token {
        Token::Minus => UniOp::Negate,
        Token::Invert => UniOp::LogicalInvert,
        _ => panic!("Unrecognized uniop: {:?}", token)
    }
}

fn uni_expr(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    if tokens.has_next() && tokens.peek(|tok| {
        match tok {
            Token::Minus | Token::Invert | Token::Ref | Token::Star => true,
            _ => false
        }
    }).map_err(|e| format!("{:?}", e))? {

        let (lop, op) = consume_token!(tokens);
        let base = leaf(tokens)?;

        let (base, sbase) = base.to_data();

        let span = Span::combine(lop.make_span(), sbase);

        let uni_expr = UniExpr {
            op: uni_op(op),
            expr: Box::new(base),
        };

        let uni_expr = AstNode::new(uni_expr, span);

        Ok(AstNode::new(Expr::Uni(uni_expr), span))

    } else {
        leaf(tokens)
    }
}

fn leaf(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    enum LeafDec {
        Paren,
        Ident,
        StructInit,
        ArrayInit,
        AnonymousFn,
        Err,
    }
    if tokens.has_next() {
        match tokens.peek(|tok| {
            match tok {
                Token::LParen => LeafDec::Paren,
                Token::Identifier(i) => LeafDec::Ident,
                Token::Init => LeafDec::StructInit,
                Token::LBracket => LeafDec::ArrayInit,
                Token::Fn => LeafDec::AnonymousFn,
                _ => LeafDec::Err,
            }

        }).map_err(|e| format!("{:?}", e))? {

            LeafDec::Paren => {
                let (lloc, _) = consume_token!(tokens, Token::LParen);
                let expr = expr(tokens)?;
                let (rloc, _) = consume_token!(tokens, Token::RParen);

                Ok(expr)
            }

            LeafDec::Ident => leaf_ident(tokens),

            LeafDec::StructInit => struct_init(tokens),
            
            LeafDec::ArrayInit => array_init(tokens),

            LeafDec::AnonymousFn => anonymous_fn(tokens),

            LeafDec::Err => unimplemented!("Unexpected token"),
        }
    } else {
        
        unimplemented!("Unexpected end of input");
    }
}

fn leaf_ident(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    unimplemented!()
}

fn struct_init(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    let (linit, _) = consume_token!(tokens, Token::Init);
    let (path, _) = module_binding(tokens)?.to_data();

    let _lbrace = consume_token!(tokens, Token::LBrace);

    let mut init = None;
    if tokens.peek(|tok| {
        match tok {
            Token::RBrace => false,
            _ => true,
        }

    }).map_err(|e| format!("{:?}", e))? {
        init = Some(struct_field_init_list(tokens)?);
    }

    let (lroc, _rbrace) = consume_token!(tokens, Token::RBrace);

    let span = LocationSpan::new(linit.start(), lroc.end());

    let struct_init = StructInit {
        struct_name: path,
        field_init: init,
    };

    let struct_init = AstNode::new(struct_init, span.make_span());

    Ok(AstNode::new(Expr::StructInit(struct_init), span.make_span()))
}

fn struct_field_init_list(tokens: &mut BufferedTokenizer) -> ParseErr<Vec<(AstNode<Ident>, Box<Expr>)>> {
    let mut list = vec![struct_field_init(tokens)?];

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
                list.push(struct_field_init(tokens)?);
                continue;
            }
        }

        break;
    }

    Ok(list)
}

fn struct_field_init(tokens: &mut BufferedTokenizer) -> ParseErr<(AstNode<Ident>, Box<Expr>)> {
    let (iloc, ident) = consume_token!(tokens, Token::Identifier(i) => Ident(i));

    let _colon = consume_token!(tokens, Token::Colon);

    let (expr, _) = expr(tokens)?.to_data();

    Ok((AstNode::new(ident, iloc.make_span()), Box::new(expr)))
}

fn array_init(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    unimplemented!()
}

fn anonymous_fn(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {

    let (fnloc, _) = consume_token!(tokens, Token::Fn);

    let _lparen = consume_token!(tokens, Token::LParen);

    let params = if tokens.peek(|tok| {
        match tok {
            Token::RParen => false,
            _ => true,
        }
    }).map_err(|e| format!("{:?}", e))? {
        Some(fn_param_list(tokens)?)
    } else {
        None
    };
        

    let (rloc, _) = consume_token!(tokens, Token::RParen);

    let body = block(tokens)?;

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

    let span = Span::combine(fnloc.make_span(), body.span());

    let anon = AnonymousFn {
        params: params,
        return_type: return_type,
        body: body,
    };

    let anon = AstNode::new(anon, span);

    Ok(AstNode::new(Expr::AnonymousFn(anon), span))
}
