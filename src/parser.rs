// program -> stmt_seq+
// stmt_seq -> stmt+
// stmt -> expr_stmt ";" | decl_stmt
// expr -> assignment | bin_expr | uni_expr | if_expr | while_expr | return | break | continue | literal | ident
// | "(" expr ")"
// 
// assignment -> ident? ident "=" expr_stmt
//
// bin_expr -> expr bin_op expr
// bin_op -> "+" | "-" | "*" | "/" | "%"
//
// uni_expr -> uni_op expr
// uni_op -> "-" | "*" | "&" | "!"
//
// if_expr -> if expr { stmt_seq }
// while_expr while expr { stmt_seq }
//
// literal -> bool | number | string
// bool -> "true | "false"
//
// return -> "return" expr ";"
// break -> "break" expr ";"
// continue -> "continue" ";"
//
// decl_stmt -> fn_decl | struct_decl
// fn_decl -> "fn" ident (arg_list) { stmt_seq }
// struct_decl -> "struct" ident { field_list }

use std::iter::Peekable;
use ast::*;
use tokenizer::*;
use super::Span;
use ascii::*;

pub fn parse_program(input: &[Token]) -> Result<Program, Err> {
    unimplemented!();   
}

fn parse_toplevel<I>(input: Peekable<I>) -> Result<(), Err> where I: Iterator<Item = Token> {
    unimplemented!();
}


fn parse_decl_stmt<I>(input: Peekable<I>) -> Result<(), Err> where I: Iterator<Item = Token> {
    unimplemented!();  
}

fn parse_keyword<I>(mut input: Peekable<I>) -> Result<Keyword, Err> where I: Iterator<Item = Token> {
    let mut buffer = AsciiString::new();
    loop {
        {
            match input.peek() {
                None => break,
                Some(ref t) => {
                    match t.kind {
                        TokenKind::Char(_) => (),
                        _ => break,
                    }
                }
            }
        }

        let next_ch = {
            let next_t = input.next().unwrap();
            if let TokenKind::Char(ch) = next_t.kind {
                ch
            } else {
                panic!("Next token has to be a TokenKind::Char according to the previous check");
            }
        };

        buffer.push(next_ch);
    }

    match buffer.as_str() {
        "if" => Ok(Keyword::If),
        "while" => Ok(Keyword::While),
        "break" => Ok(Keyword::Break),
        "return" => Ok(Keyword::Struct),
        "fn" => Ok(Keyword::Function),
        _ => unimplemented!("buffer does not match any keyword"),
    }
}

fn eat_whitespace<I>(mut input: Peekable<I>) where I: Iterator<Item = Token> {
    loop {
        if let Some(ref t) = input.peek() {
            match t.kind {
                TokenKind::Whitespace => (),
                _ => break,
            }
        } else {
            break;
        }

        input.next().unwrap();
    }
}

pub struct Err {
    kind: ErrKind,
    span: Span
}

pub enum ErrKind {
    
}
