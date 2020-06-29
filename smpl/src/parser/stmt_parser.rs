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
type PostAction = Box<FnOnce(&mut BufferedTokenizer, Expr, &[ExprDelim]) -> ParserResult<Expr>>;
type LedAction = Box<FnOnce(&mut BufferedTokenizer, Expr, BindingPower, &[ExprDelim]) -> ParserResult<Expr>>;
type ExprAction = Box<FnOnce(&mut BufferedTokenizer) -> ParserResult<Expr>>;
type StmtAction = Box<FnOnce(&mut BufferedTokenizer) -> ParserResult<Stmt>>;
type LbpData = (BindingPower, BindingPower, LedAction);
type PostData = (BindingPower, PostAction);

#[derive(PartialEq, Eq)]
pub enum ExprDelim {
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
            Token::Let => Box::new(parse_let),
            Token::While => todo!(),

            _ => Box::new(semi_expr),
        },
        parser_state!("stmt", "stmt-kind")
    );

    action(tokens)
}

fn semi_expr(tokens: &mut BufferedTokenizer) -> ParserResult<Stmt> {
    let expr = top_level_expr(tokens, &[ExprDelim::Semi])?;

    let _semi = consume_token!(tokens,
        Token::Semi,
        parser_state!("semi-expr", ";")
    );


    Ok(Stmt::Expr(expr))
}

fn parse_let(tokens: &mut BufferedTokenizer) -> ParserResult<Stmt> {
    let (let_span, _) = consume_token!(tokens,
        Token::Let,
        parser_state!("let-stmt", "let")
    );

    let (name_span, name) = consume_token!(tokens,
        Token::Identifier(id) => Ident(id),
        parser_state!("let-stmt", "name")
    );

    let type_ann: Option<TypedNode<TypeAnn>> = if peek_token!(tokens,
        |tok| match tok {
            Token::Colon => true,
            _ => false
        },
        parser_state!("let-stmt", "type-ann?")
    ) {
        Some(Typable::untyped(top_level_type_ann(tokens, &[AnnDelim::Assign])?))
    } else {
        None
    };

    let _assign = consume_token!(tokens,
        Token::Assign,
        parser_state!("let-stmt", "=")
    );

    let init = top_level_expr(tokens, &[ExprDelim::Semi])?;

    let (semi_span, _) = consume_token!(tokens,
        Token::Semi,
        parser_state!("let-stmt", "semi")
    );

    let var_name = AstNode::new(name, name_span);

    let node_span = Span::combine(let_span, semi_span);
    let node = AstNode::new(LetStmt {
        var_name,
        type_ann,
        init,
    }, node_span);

    Ok(Stmt::ExprStmt(ExprStmt::Let(
        node
    )))
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

fn parse_while(tokens: &mut BufferedTokenizer) -> ParserResult<Stmt> {

    enum WhileDec {
        Elif,
        Else,
        End,
    }

    let (while_span, _) =
        consume_token!(tokens, Token::While, parser_state!("while-expr", "while"));

    let conditional = top_level_expr(tokens, &[ExprDelim::NewBlock])?;

    let while_body = production!(block(tokens), parser_state!("while-branch", "block"));

    let mut branches = Vec::new();
    let mut default_branch = None;
    let mut end = while_body.span();

    loop {
        match peek_token!(
            tokens,
            |tok| match tok {
                Token::Elif => WhileDec::Elif,
                Token::Else => WhileDec::Else,

                _ => WhileDec::End,
            },
            parser_state!("while-expr", "branches")
        ) {
            WhileDec::Elif => {
                let _elif = consume_token!(
                    tokens,
                    Token::Elif,
                    parser_state!("while-expr", "elif")
                );

                let branch = production!(
                    if_branch(tokens),
                    parser_state!("while-expr", "elif branch")
                );
                end = branch.block.data().span();

                branches.push(branch);
            }

            WhileDec::Else => {
                let _else = consume_token!(
                    tokens,
                    Token::Else,
                    parser_state!("while-expr", "else")
                );
                let block = production!(
                    block(tokens),
                    parser_state!("while-expr", "else-block")
                );

                end = block.data().span();
                default_branch = Some(block);

                break;
            }

            WhileDec::End => break,
        }
    }

    let while_span = LocationSpan::combine(while_span, end);

    let while_node = AstNode::new(While {
        conditional,
        body: while_body,
        branches,
        default_branch,
    }, while_span);

    if nud_action(tokens).is_ok() {
        Ok(Stmt::ExprStmt(ExprStmt::While(while_node)))
    } else {

        let expr = production!(
            expr_with_left(
                tokens,
                Expr::While(Box::new(Typable::untyped(while_node))),
                &[ExprDelim::Semi],
                0),
            parser_state!("expr", "right"));

        Ok(Stmt::Expr(expr))
    }
}

fn parse_if(tokens: &mut BufferedTokenizer) -> ParserResult<Stmt> {

    let if_node = if_core(tokens)?;

    if nud_action(tokens).is_err() {
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
            parser_state!("if-semi-expr", "right"));

        let _semi = consume_token!(tokens,
            Token::Semi,
            parser_state!("if-semi-expr", ";")
        );

        Ok(Stmt::Expr(expr))
    }
}

fn if_core(tokens: &mut BufferedTokenizer) -> ParserResult<AstNode<If>> {
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

    Ok(if_node)
}

fn if_branch(tokens: &mut BufferedTokenizer) -> ParserResult<Branch> {
    let conditional = top_level_expr(tokens, &[ExprDelim::NewBlock])?;

    let block = production!(block(tokens), parser_state!("if-branch", "block"));

    let branch = Branch {
        conditional: conditional,
        block: block,
    };

    Ok(branch)
}

pub fn top_level_expr(tokens: &mut BufferedTokenizer, delimiters: &[ExprDelim])
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
        if delim_break(tokens, delimiters)? {
            break;
        }

        // Postfix operator?
        if let Some((lbp, expr_action)) = postfix_action(tokens)? {
            if lbp <= min_bp {
                break;
            }

            left = expr_action(tokens, left, delimiters)?;
            continue;
        }

        // TODO: eat whitespace

        // Infix operator
        let (lbp, rbp, expr_action) = led_action(tokens)?;

        if lbp <= min_bp {
            break;
        }

        left = expr_action(tokens, left, rbp, delimiters)?;
    }

    Ok(left)
}

fn delim_break(tokens: &BufferedTokenizer, delims: &[ExprDelim]) -> ParserResult<bool> {
    Ok(peek_token!(tokens,
        |tok| match tok {

            Token::Semi => delims.contains(&ExprDelim::Semi),
            Token::Comma => delims.contains(&ExprDelim::Comma),
            Token::LBrace => delims.contains(&ExprDelim::NewBlock),

            _ => false,
        },
        parser_state!("expr", "delim-check")
    ))
}

fn postfix_action(tokens: &BufferedTokenizer) -> ParserResult<Option<PostData>> {

    Ok(peek_token!(tokens,
        |tok| match tok {
            // TODO: Review LBP of postfix operators
            Token::LParen => Some((50, Box::new(parse_fn_call) as PostAction)),
            Token::LBracket => Some((50, Box::new(parse_index_access) as PostAction)),

            _ => None,
        },
        parser_state!("expr", "postfix_action?")
    ))
}

fn parse_fn_call(tokens: &mut BufferedTokenizer, left: Expr, upper_delims: &[ExprDelim]) -> ParserResult<Expr> {

    let _lparen = consume_token!(
        tokens,
        Token::LParen,
        parser_state!("fn-call", "lparen")
    );

    let mut args: Vec<Expr> = Vec::new();

    while peek_token!(tokens,
        |tok| match tok {
            Token::RParen => false,

            _ => true,
        },
        parser_state!("fn-call", "arg?")
    ) {
        let arg = top_level_expr(tokens, &[ExprDelim::Comma])?;

        args.push(arg);

        if peek_token!(tokens,
            |tok| match tok {
                Token::Comma => true,
                _ => false,
            },
            parser_state!("fn-call", "comma?")
        ) {
            // Found comma
            let _comma = consume_token!(tokens,
                Token::Comma,
                parser_state!("fn-call", "comma")
            );
            continue;
        } else {
            // No comma
            break;
        }
    }

    let (rspan, _) = consume_token!(
        tokens,
        Token::RParen,
        parser_state!("fn-call", "rparen")
    );

    let fn_call_span = Span::combine(left.span(), rspan);

    let fn_call_node = AstNode::new(FnCall {
        fn_value: Box::new(left),
        args,
    }, fn_call_span);

    Ok(Expr::FnCall(Typable::untyped(fn_call_node)))
}

fn parse_index_access(tokens: &mut BufferedTokenizer, left: Expr, upper_delims: &[ExprDelim]) -> ParserResult<Expr> {
    let _lbracket = consume_token!(
        tokens,
        Token::LBracket,
        parser_state!("index-access", "lbracket")
    );

    let indexing_expr = top_level_expr(tokens, &[])?;

    let (rbracket_span, _) = consume_token!(
        tokens,
        Token::RBracket,
        parser_state!("index-access", "lbracket")
    );

    let indexing_span = Span::combine(left.span(), rbracket_span);

    let indexing_node = AstNode::new(IndexAccess {
        base: Box::new(left),
        indexer: Box::new(indexing_expr),
    }, indexing_span);

    Ok(Expr::IndexAccess(Typable::untyped(indexing_node)))
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

                Token::RParen       => (0, 0, unreachable_led!("led rparen")),
                Token::RBrace       => (0, 0, unreachable_led!("led rbrace")),
                Token::RBracket     => (0, 0, unreachable_led!("led rbracket")),

                Token::Pipe         => (10, 10, binexpr(tok)),

                // TODO: Check precedence
                Token::Eq           => (15, 15, binexpr(tok)),
                Token::NEq          => (17, 17, binexpr(tok)),
                Token::Gt           => (17, 17, binexpr(tok)),
                Token::Gte          => (17, 17, binexpr(tok)),
                Token::Lt           => (17, 17, binexpr(tok)),
                Token::Lte          => (17, 17, binexpr(tok)),

                Token::LAnd         => (18, 18, binexpr(tok)),
                Token::LOr          => (18, 18, binexpr(tok)),

                Token::Plus         => (20, 20, binexpr(tok)),
                Token::Minus        => (20, 20, binexpr(tok)),
                Token::Star         => (30, 30, binexpr(tok)),
                Token::Slash        => (30, 30, binexpr(tok)),

                Token::Dot          => (100, 100, binexpr(tok)),

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

        Token::Eq       => Ok(basic_binop!(left, BinOp::Eq, rbp, delims)),
        Token::NEq      => Ok(basic_binop!(left, BinOp::Neq, rbp, delims)),
        Token::Gte      => Ok(basic_binop!(left, BinOp::Gte, rbp, delims)),
        Token::Gt       => Ok(basic_binop!(left, BinOp::Gt, rbp, delims)),
        Token::Lte      => Ok(basic_binop!(left, BinOp::Lte, rbp, delims)),
        Token::Lt       => Ok(basic_binop!(left, BinOp::Lt, rbp, delims)),

        Token::LAnd     => Ok(basic_binop!(left, BinOp::LAnd, rbp, delims)),
        Token::LOr      => Ok(basic_binop!(left, BinOp::LOr, rbp, delims)),

        Token::Assign   => Ok(basic_binop!(left, BinOp::Assign, rbp, delims)),

        Token::Dot      => Ok(basic_binop!(left, BinOp::Dot, rbp, delims)),

        Token::ColonColon => {
            let right = parse_expr(tokens, delims, rbp)?;
            match (left, right) {
                (Expr::Binding(left), Expr::Binding(right)) => {
                    let left = left.into_data();
                    let right = right.into_data();
                    let path_span = Span::combine(left.span(), right.span());
                    let module_path_node = AstNode::new(ModulePath(vec![left, right]), path_span);

                    Ok(Expr::ModulePath(Typable::untyped(module_path_node)))
                }

                (Expr::ModulePath(mut path), Expr::Binding(right)) => {
                    let right = right.into_data();
                    path.data_mut().node_mut().0.push(right);

                    Ok(Expr::ModulePath(path))
                }

                (left, right) => todo!("Unexpected left for operator \"::\""),
            }
        }

        _ => todo!(),
    }
}

fn nud_action(tokens: &BufferedTokenizer) -> ParserResult<ExprAction> {

    let action: ExprAction = peek_token!(tokens, SPAN
        |tok, span| match tok {
            Token::IntLiteral(..)
                | Token::FloatLiteral(..)
                | Token::BoolLiteral(..)
                | Token::StringLiteral(..) => Ok(Box::new(parse_literal) as ExprAction),

            Token::Plus
                | Token::Minus
                | Token::Bang => Ok(Box::new(uni_expr) as ExprAction),

            Token::LParen => Ok(Box::new(paren_expr) as ExprAction),

            Token::LBrace => Ok(Box::new(block_expr) as ExprAction),

            Token::Init => Ok(Box::new(init_expr) as ExprAction),

            Token::If => Ok(Box::new(if_expr) as ExprAction),

            Token::Identifier(..) => Ok(Box::new(ident_expr) as ExprAction),

            _ => Err(parser_error!(ParserErrorKind::UnexpectedToken(tok.clone()), parser_state!("expr", "nud"), span)),
        },
        parser_state!("expr", "nud")
    )?;

    Ok(action)
}

fn if_expr(tokens: &mut BufferedTokenizer) -> ParserResult<Expr> {
    let if_node = if_core(tokens)?;

    Ok(Expr::If(Box::new(Typable::untyped(if_node))))
}

fn init_expr(tokens: &mut BufferedTokenizer) -> ParserResult<Expr> {
    let (init_span, _) = consume_token!(tokens,
        Token::Init,
        parser_state!("init-expr", "init")
    );

    if peek_token!(tokens,
        |tok| match tok {
            Token::LBracket => true,
            _ => false,
        },
        parser_state!("init-expr", "init-kind?")
    ) {
        struct_init(tokens, init_span)
    } else {
        array_init(tokens, init_span)
    }
}

// 'init [v1, v2, ... vn; X]'
// 'init [v1, v2, ... vn]'
fn array_init(tokens: &mut BufferedTokenizer, mut init_span: Span) -> ParserResult<Expr> {

    enum InitListDecision {
        Value,
        RepetitionCount,
        Break,
        Err,
    }

    let _lbracket = consume_token!(tokens,
        Token::LBracket,
        parser_state!("array-init", "lbracket")
    );

    let mut pattern = Vec::new();
    let mut repetition_count = None;
    let mut parse_repetition_count = false;
    while peek_token!(tokens,
        |tok| match tok {
            Token::RBracket => false,
            _ => true,
        },
        parser_state!("array-init", "rbracket?")
    ) {
        let expr = top_level_expr(tokens, &[])?;

        if parse_repetition_count {
            repetition_count = Some(Box::new(expr));
            break;
        } else {
            pattern.push(expr);
        }

        match peek_token!(tokens,
            |tok| match tok {
                Token::Semi => InitListDecision::RepetitionCount,
                Token::Comma => InitListDecision::Value,
                Token::RBracket => InitListDecision::Break,
                _ => InitListDecision::Err,
            },
            parser_state!("array-init", "comma-semi?")
        ) {
            InitListDecision::RepetitionCount => parse_repetition_count = true,

            InitListDecision::Break => break,

            InitListDecision::Err => todo!(),

            _ => continue,
        }
    }

    let (rbracket_span, _) = consume_token!(tokens,
        Token::RBracket,
        parser_state!("array-init", "rbracket")
    );

    init_span = Span::combine(init_span, rbracket_span);
    let array_init_node = AstNode::new(ArrayInit {
        pattern,
        repetition_count
    }, init_span);

    Ok(Expr::ArrayInit(Typable::untyped(array_init_node)))
}

fn struct_init(tokens: &mut BufferedTokenizer, init_span: Span) -> ParserResult<Expr> {

    let typed_path: Option<AstNode<TypedPath>> = if peek_token!(tokens,
        |tok| match tok {
            Token::LBrace => false,
            _ => true,

        },
        parser_state!("struct-init", "anonymous?")) {

        // Expecting a (potentially typed) module path
        let module_path_expr = top_level_expr(tokens, &[ExprDelim::NewBlock])?;

        Some(try_expr_to_path(module_path_expr)?)

    } else {
        None
    };

    let _lbrace = consume_token!(tokens,
        Token::LBrace,
        parser_state!("struct-init", "lbrace")
    );

    let mut field_init: Vec<(AstNode<Ident>, Box<Expr>)> = Vec::new();
    while peek_token!(tokens,
        |tok| match tok {
            Token::RBrace => false,

            _ => true,
        },
        parser_state!("struct-init", "field-init?")
    ) {

        // TODO: Pattern parsing here
        let (field_span, field) = consume_token!(tokens,
            Token::Identifier(ident) => Ident(ident),
            parser_state!("struct-init", "field-ident")
        );

        let _colon = consume_token!(tokens,
            Token::Colon,
            parser_state!("struct-init", "init-colon")
        );

        let init_value = top_level_expr(tokens, &[ExprDelim::Comma])?;

        let field = AstNode::new(field, field_span);
        field_init.push((field, Box::new(init_value)));

        if peek_token!(tokens,
            |tok| match tok {
                Token::Comma => true,
                _ => false,
            },
            parser_state!("stuct-init", "comma?")
        ) {
            // Found comma
            let _comma = consume_token!(tokens,
                Token::Comma,
                parser_state!("struct-init", "comma")
            );
            continue;
        } else {
            // No comma
            break;
        }
    }


    let (rbrace_span, _) = consume_token!(tokens,
        Token::RBrace,
        parser_state!("struct-init", "rbrace")
    );

    let init_span = Span::combine(init_span, rbrace_span);
    let init_node = AstNode::new(StructInit {
        struct_name: Typable::untyped(typed_path),
        field_init,
    }, init_span);

    Ok(Expr::StructInit(Typable::untyped(init_node)))
}

fn try_expr_to_path(expr: Expr) -> ParserResult<AstNode<TypedPath>> {
    match expr {
        Expr::ModulePath(path) => {
            let path: AstNode<ModulePath> = path.into_data();
            let path_span = path.span();
            Ok(AstNode::new(TypedPath {
                base: Typable::untyped(path),
                params: vec![],
            }, path_span))
        },

        Expr::Binding(binding) => {
            let binding: AstNode<Ident> = binding.into_data();
            let binding_span = binding.span().clone();

            let module_path = AstNode::new(ModulePath(vec![binding]), binding_span.clone());

            Ok(AstNode::new(TypedPath {
                base: Typable::untyped(module_path),
                params: vec![],
            }, binding_span))
        },

        Expr::Path(p) => Ok(p.into_data()),

        expr => todo!("Unexpected expression for type name: {:?}", expr),
    }
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
