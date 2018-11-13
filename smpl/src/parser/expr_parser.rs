use std::iter::{Iterator, Peekable};

use span::*;
use ast::*;
use super::tokens::*;
use super::parser::{module_binding as full_module_binding, ParseErr, fn_param_list, block, type_annotation};
use crate::consume_token;

#[derive(PartialEq, Clone)]
pub enum Delimiter {
    RParen,
    RBracket,
    Comma,
    Semi,
    LBrace,
    Pipe,
}

pub fn piped_expr(tokens: &mut BufferedTokenizer, delim_tokens: &[Delimiter]) 
    -> ParseErr<AstNode<Expr>> {
    let mut delimiters = delim_tokens.to_vec();
    delimiters.push(Delimiter::Pipe);

    let primary_base = parse_primary(tokens)?;
    let expr_base = expr(tokens, primary_base, &delim_tokens, 0)?;

    let mut piped_exprs = Vec::new();

    while tokens.has_next() &&
        tokens.peek(|tok| {
            match tok {
                Token::Pipe => true,
                _ => false,
            }
        }).map_err(|e| format!("{:?}", e))? {

        let _pipe = consume_token!(tokens, Token::Pipe);

        let primary = parse_primary(tokens)?;
        let expr = expr(tokens, primary, &delim_tokens, 0)?;

        piped_exprs.push(expr);
    }

    if piped_exprs.len() > 0 {
        let (expr_base, eloc) = expr_base.to_data();
        let expr_base = match expr_base {
            Expr::FnCall(f) => f,

            e @ _ => return Err(format!("Can only pipe function calls. Found:\n{:?}", e)),

        };

        let piped_exprs = piped_exprs
            .into_iter()
            .map(|e| {
                let (e, espan) = e.to_data();
                match e {
                    Expr::FnCall(f) => Ok(f),
                    e @ _ => Err(format!("Can only pipe function calls. Found:\n{:?}", e)),
                }
            })
            .collect::<Result<Vec<AstNode<FnCall>>, String>>()?;

        let end = piped_exprs.last().unwrap().span();
        let span = Span::combine(eloc, end);

        let fn_chain = FnCallChain {
            base: expr_base,
            chain: piped_exprs,
        };

        let fn_chain = AstNode::new(fn_chain, span);

        Ok(AstNode::new(Expr::FnCallChain(fn_chain), span))

    } else {
        Ok(expr_base)
    }
}

fn expr(tokens: &mut BufferedTokenizer, 
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

                if bin_op_precedence(&op) > main_prec ||
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

fn parse_primary(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    enum PrimaryDec {
        Ident,
        Literal,
        UniExpr,
        LParen,
        Err,
        StructInit,
        ArrayInit,
        AnonFn,
    }

    match tokens.peek(|tok| {
        match tok {
            Token::Plus => PrimaryDec::UniExpr,
            Token::Minus => PrimaryDec::UniExpr,
            Token::Invert => PrimaryDec::UniExpr,

            Token::IntLiteral(_) => PrimaryDec::Literal,
            Token::FloatLiteral(_) => PrimaryDec::Literal,
            Token::BoolLiteral(_) => PrimaryDec::Literal,
            Token::StringLiteral(_) => PrimaryDec::Literal,

            Token::LParen => PrimaryDec::LParen,

            Token::Identifier(_) => PrimaryDec::Ident,

            Token::Init => PrimaryDec::StructInit,
            Token::LBracket => PrimaryDec::ArrayInit,

            Token::Fn => PrimaryDec::AnonFn,

            _ => PrimaryDec::Err,
        }
    }).map_err(|e| format!("{:?}", e))? {

        PrimaryDec::Ident => parse_ident_leaf(tokens),

        PrimaryDec::UniExpr => {
            let (uspan, uop) = consume_token!(tokens);
            
            let uop = match uop {
                Token::Plus => return parse_primary(tokens),

                Token::Minus => UniOp::Negate,

                Token::Invert => UniOp::LogicalInvert,

                _ => unreachable!(),
            };

            let base = parse_primary(tokens)?;
            let (base, sbase) = base.to_data();

            let span = Span::combine(uspan.make_span(), sbase);

            let uexpr = UniExpr {
                op: uop,
                expr: Box::new(base),
            };

            Ok(AstNode::new(Expr::Uni(AstNode::new(uexpr, span)), span))
        }

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

        PrimaryDec::StructInit => struct_init(tokens),

        PrimaryDec::ArrayInit => array_init(tokens),

        PrimaryDec::AnonFn => anonymous_fn(tokens),

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

        IdentLeafDec::AccessPath => {
            let span = base_span.make_span();
            let root = PathSegment::Ident(AstNode::new(base_ident, span));
            access_path(tokens, root)
        }
        IdentLeafDec::ModulePath => expr_module_path(tokens, base_ident, base_span),

        IdentLeafDec::FnCall => {
            let args = fn_args(tokens)?;
            let (args, arg_span) = args.to_data();
            let args = args.map(|v| v.into_iter().map(|a| a.to_data().0).collect());

            let called = ModulePath(vec![AstNode::new(base_ident, base_span.make_span())]);

            let fn_call = FnCall {
                path: called,
                args: args,
            };

            let span = Span::combine(base_span.make_span(), arg_span);
            Ok(AstNode::new(Expr::FnCall(AstNode::new(fn_call, span)), span))
        }

        IdentLeafDec::Indexing => {
            let _lbracket = consume_token!(tokens, Token::LBracket);
            let indexer = parse_primary(tokens)?;
            let indexer = expr(tokens, indexer, &[Delimiter::RBracket], 0)?;
            let (indexer, _) = indexer.to_data();
            let (rspan, _rbracket) = consume_token!(tokens, Token::RBracket);

            if tokens.peek(|tok| {
                match tok {
                    Token::Dot => true,
                    _ => false,
                }
            }).map_err(|e| format!("{:?}", e))? {

                // Access path with indexing as root
                let span = base_span.make_span();
                let root = PathSegment::Indexing(AstNode::new(base_ident, span), Box::new(indexer));
                access_path(tokens, root)

            } else {

                // Single indexing
                let binding = Expr::Binding(AstNode::new(base_ident, base_span.make_span()));
                let indexing = Indexing {
                    array: Box::new(binding),
                    indexer: Box::new(indexer),
                };

                let span = Span::combine(base_span.make_span(), rspan.make_span());
                Ok(AstNode::new(Expr::Indexing(AstNode::new(indexing, span)), span))
            }
        },
        IdentLeafDec::Singleton => {
            let span = base_span.make_span();
            Ok(AstNode::new(Expr::Binding(AstNode::new(base_ident, span)), span))
        }
    }
}

pub fn access_path(tokens: &mut BufferedTokenizer, root: PathSegment) 
    -> ParseErr<AstNode<Expr>> {

    let start = match root {
        PathSegment::Ident(ref i) => i.span(),
        PathSegment::Indexing(ref i, _) => i.span(),
    };
    
    let mut end = start;
    let mut path = vec![root];
    while tokens.has_next() && tokens.peek(|tok| {
        match tok {
            Token::Dot => true,
            _ => false,
        }
    }).map_err(|e| format!("{:?}", e))? {

        let _dot = consume_token!(tokens, Token::Dot);
        let path_segment = path_segment(tokens)?;

        end = match path_segment {
            PathSegment::Ident(ref i) => i.span(),
            PathSegment::Indexing(ref i, _) => i.span(),
        };
        path.push(path_segment);
    }

    let span = Span::combine(start, end);

    Ok(AstNode::new(Expr::FieldAccess(AstNode::new(Path(path), span)), span))
    
}

// At end of path_segment, next token should be DOT or end of path
fn path_segment(tokens: &mut BufferedTokenizer) -> ParseErr<PathSegment> {

    enum SegmentDec {
        Dot,
        Indexing,
        End,
    }

    let (ispan, ident) = consume_token!(tokens, Token::Identifier(i) => Ident(i));

    match tokens.peek(|tok| {
        match tok {
            Token::Dot => SegmentDec::Dot,
            Token::LBracket => SegmentDec::Indexing,
            _ => SegmentDec::End,
        }
    }).map_err(|e| format!("{:?}", e))? {

        SegmentDec::Dot => (),
        SegmentDec::End => (),

        SegmentDec::Indexing => {
            // TODO: Convert path indexing segment to use Expr, Expr form instead of Ident form
            // TODO: Allow multiple indexing
            
            let _lbracket = consume_token!(tokens, Token::LBracket);

            let indexer = parse_primary(tokens)?;
            let indexer = expr(tokens, indexer, &[Delimiter::RBracket], 0)?;
            let (indexer, _) = indexer.to_data();

            let _rbracket = consume_token!(tokens, Token::RBracket);

            return Ok(PathSegment::Indexing(AstNode::new(ident, ispan.make_span()), 
                                            Box::new(indexer)));
        }
    }

    let span = ispan.make_span();
    Ok(PathSegment::Ident(AstNode::new(ident, span)))
}

pub fn fn_args(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Option<Vec<AstNode<Expr>>>>> {
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

pub fn expr_module_path(tokens: &mut BufferedTokenizer, base: Ident, base_span: LocationSpan) 
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

fn struct_init(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {
    let (linit, _) = consume_token!(tokens, Token::Init);
    let (path, _) = full_module_binding(tokens)?.to_data();

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

    let field_init = parse_primary(tokens)?;
    let (expr, _) = expr(tokens, 
                         field_init, 
                         &[Delimiter::Comma, Delimiter::RParen], 
                         0)?
        .to_data();

    Ok((AstNode::new(ident, iloc.make_span()), Box::new(expr)))
}

fn array_init(tokens: &mut BufferedTokenizer) -> ParseErr<AstNode<Expr>> {

    enum InitDec {
        SingleList,
        List,
        Value,
        Err,
    }

    let (lloc, _) = consume_token!(tokens, Token::LBracket);

    let base_expr = parse_primary(tokens)?;
    let (base_expr, _) = expr(tokens, 
                              base_expr,
                              &[Delimiter::Comma, Delimiter::RBracket],
                              0)?
        .to_data();

    let init = if tokens.has_next() {
        match tokens.peek(|tok| {
            match tok {
                Token::Comma => InitDec::List,
                Token::Semi => InitDec::Value,
                Token::RBracket => InitDec::SingleList,
                _ => InitDec::Err
            }
        }).map_err(|e| format!("{:?}", e))? {

            InitDec::SingleList => ArrayInit::InitList(vec![base_expr]),

            InitDec::List => {
                let mut list = array_init_list(tokens)?;

                list.insert(0, base_expr);

                ArrayInit::InitList(list)
            }

            InitDec::Value => {
                let _semi = consume_token!(tokens, Token::Semi);
                let (_, number) = consume_token!(tokens, Token::IntLiteral(i) => i);

                if number <= 0 {
                    unimplemented!("Invalid array size: {}", number);
                }

                ArrayInit::Value(Box::new(base_expr), number as u64)
            }

            InitDec::Err => unimplemented!("Unexpected token"),
        }
    } else {
        unimplemented!("Unexpected end of input");
    };

    let (rloc, _) = consume_token!(tokens, Token::RBracket);

    let span = LocationSpan::new(lloc.start(), rloc.end());

    let array_init = AstNode::new(init, span.make_span());

    Ok(AstNode::new(Expr::ArrayInit(array_init), span.make_span()))
}

fn array_init_list(tokens: &mut BufferedTokenizer) -> ParseErr<Vec<Expr>> {
    // First element already consumed, check for rest of list
    let mut list = Vec::new(); 

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
                let data = parse_primary(tokens)?;
                list.push(expr(tokens,
                               data,
                               &[Delimiter::Comma, Delimiter::RBracket],
                               0
                               )?
                          .to_data()
                          .0);
                continue;
            }
        }

        break;
    }

    Ok(list)
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

    let body = block(tokens)?;

    let span = Span::combine(fnloc.make_span(), body.span());

    let anon = AnonymousFn {
        params: params,
        return_type: return_type,
        body: body,
    };

    let anon = AstNode::new(anon, span);

    Ok(AstNode::new(Expr::AnonymousFn(anon), span))
}

fn is_delim(token: &Token, delim: &[Delimiter]) -> bool {
    let token = match token {
        Token::RParen => Delimiter::RParen,
        Token::RBracket => Delimiter::RBracket,
        Token::Comma => Delimiter::Comma,
        Token::Semi => Delimiter::Semi,
        Token::LBrace => Delimiter::LBrace,
        Token::Pipe => Delimiter::Pipe,

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
    // Precedence based off of Clang precedence table 
    // OperatorPrecedence.h
    use self::BinOp::*;
    match op {
        Add => 13,
        Sub => 13,
        Mul => 14,
        Div => 14,
        Mod => 14,

        LogicalAnd => 4,
        LogicalOr => 4,
        GreaterEq => 10,
        LesserEq => 10,
        Greater => 10,
        Lesser => 10,
        Eq => 9,
        InEq => 9,
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
