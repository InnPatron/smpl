// program -> decl_stmt
// decl_stmt -> fn_decl | struct_decl
// fn_decl -> "fn" ident (arg_list) { stmt_seq }
// struct_decl -> "struct" ident { field_list }
// stmt_seq -> stmt+
// stmt -> expr ";" | expr_stmt
// expr -> bin_expr | uni_expr | return | break | continue | literal | ident
// | "(" expr ")"
// expr_stmt -> if_expr | while_expr | local_var_decl | assignment
//
// local_var_decl -> path assignment
//
// assignment -> ident "=" expr ";"
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
// return -> "return" expr
// break -> "break" expr
// continue -> "continue"
// 
// path -> ident path_tail
// path_tail -> "." ident | path_tail | E

use std::iter::Peekable;
use ast::*;
use tokenizer::*;
use super::Span;
use ascii::*;

/// If rule matches, return result but DO NOT ADVANCE INPUT ITERATOR.
/// This macro is for lookahead ONLY.
///
/// To try matching a rule and advancing an iterator, use 'try_rule!' macro.
macro_rules! peek_rule {
    ($input: expr, $rule: ident) => {{
        let mut try_iter = $input.clone();
        match $rule(&mut try_iter) {
            Ok(t) => Some(t),

            Err(_) => None,
        }
    }}
}

/// If rule matches, return result and ADVANCE INPUT ITERATOR.
///
/// To try matching a rule without advancing an iterator, use 'peek_rule!' macro.
macro_rules! try_rule {
    ($input: expr, $rule: ident) => {{
        let mut try_iter = $input.clone();
        match $rule(&mut try_iter) {
            Ok(t) => {
                *$input = try_iter;
                Some(t)
            },

            Err(_) => None,
        }
    }}
}

macro_rules! t_match {
    ($input: expr, $expect: pat, $err: expr) => {{
        if let Some(ref t) = $input.next() {
            match t.kind {
                $expect => (),
                _ => return $err,
            }
        } else {
            return $err;
        }
    }}
}

macro_rules! t_peek {
    ($input: expr, $expect: pat) => {{
        if let Some(ref t) = $input.peek() {
            match t.kind {
                $expect => true,
                _ => false,
            }
        } else {
            false
        }
    }}
}

macro_rules! t_consume {
    ($input: expr) => {{ $input.next() }}
}

pub fn parse_program(input: &[Token]) -> Result<Program, Err> {
    unimplemented!();   
}

fn parse_toplevel<I>(input: &mut Peekable<I>) -> Result<(), Err> where I: Iterator<Item = Token> + Clone {
    eat_whitespace(input);
    let mut top_level_decls: Vec<DeclStmt> = Vec::new();
    match parse_keyword(input)? {
        Keyword::Struct => top_level_decls.push(parse_struct_def(input)?.into()),

        Keyword::Function => unimplemented!(),
        _ => unimplemented!("Expected struct or function declaration"),
    }

    unimplemented!();
}

fn parse_fn_def<I>(input: &mut Peekable<I>) -> Result<Function, Err> where I: Iterator<Item = Token> + Clone {
    //assumes found fn keyword
    eat_whitespace(input);
    let name = parse_ident(input)?;
    eat_whitespace(input);
    let args = parse_fn_args(input)?;
    unimplemented!();
}

fn parse_fn_args<I>(input: &mut Peekable<I>) -> Result<Vec<FnArg>, Err> where I: Iterator<Item = Token> + Clone {
    t_match!(input, TokenKind::LParen, unimplemented!("( missing to denote arg list."));
    let mut args = Vec::new();
    loop {
        eat_whitespace(input);
        args.push(parse_arg_pair(input)?);
        eat_whitespace(input);
        if t_peek!(input, TokenKind::Comma) {
            t_match!(input, TokenKind::Comma, panic!("t_peek failed for Comma while fn arg parsing"));
        } else {
            break;
        }
    }

    eat_whitespace(input);
    t_match!(input, TokenKind::RParen, unimplemented!(") missing to end arg list"));
    Ok(args)
}

fn parse_arg_pair<I>(input: &mut Peekable<I>) -> Result<FnArg, Err> where I: Iterator<Item = Token> + Clone {
    let arg_type = parse_ident(input)?;
    eat_whitespace(input);
    let name = parse_ident(input)?;

    Ok(FnArg {
        name: name,
        arg_type: arg_type,
    })
}

fn parse_struct_def<I>(input: &mut Peekable<I>) -> Result<Struct, Err> where I: Iterator<Item = Token> + Clone {
    //assumes found struct keyword
    eat_whitespace(input);
    let name = parse_ident(input)?;
    eat_whitespace(input);
    let body = parse_struct_body(input)?;
    Ok(Struct {
        name: name,
        body: body
    })
}

fn parse_struct_body<I>(input: &mut Peekable<I>) -> Result<StructBody, Err> where I: Iterator<Item = Token> + Clone {
    t_match!(input, TokenKind::LBrace, unimplemented!("{{ not found to delimit struct body"));

    let mut fields = Vec::new();
    loop {
        eat_whitespace(input);
        if t_peek!(input, TokenKind::RBrace) {
            break;
        } else {
            fields.push(parse_struct_field(input)?);
            eat_whitespace(input);
            t_match!(input, TokenKind::Semicolon, unimplemented!("; not found after field declaration"));
        }
    }

    t_match!(input, TokenKind::RBrace, unimplemented!("}} not found to delimit struct body"));
    
    Ok(StructBody(fields))
}

fn parse_struct_field<I>(input: &mut Peekable<I>) -> Result<StructField, Err> where I: Iterator<Item = Token> + Clone {
    let field_type = parse_ident(input)?;
    eat_whitespace(input);
    let field_name = parse_ident(input)?;

    Ok(StructField {
        name: field_name,
        field_type: field_type
    })
}

/// stmt -> expr ";" | expr_stmt
fn parse_stmt<I>(input: &mut Peekable<I>) -> Result<Statement, Err> where I: Iterator<Item = Token> + Clone {
    if let Some(keyword) = peek_rule!(input, parse_keyword) {
        match keyword {
            Keyword::If => unimplemented!("Parse if statement"),
            Keyword::While => unimplemented!("Parse while statement"),
            _ => unimplemented!("Error. Unexpected keyword {:?}", keyword),
        }
    } else if let Some(p) = peek_rule!(input, parse_path) {
        // Path with only 1 segment is just an ident (and could also be the name of a variable to
        // assign to). Need to immediately find '=' next to determine if assignment of local var
        // declaration.
        if p.0.len() == 1 {
            let mut peek_input = input.clone();
            parse_path(&mut peek_input).unwrap();
            eat_whitespace(&mut peek_input);
            if t_peek!(peek_input, TokenKind::Eq) {
                // Found an assignment
            }
        }
        // parse local variable declaration
        let type_path = parse_path(input).unwrap();
        eat_whitespace(input);
        let var_name = parse_ident(input)?;
        eat_whitespace(input);
        t_match!(input, TokenKind::Eq, unimplemented!("Expected '='  to initialize local variable."));
        eat_whitespace(input);
        unimplemented!("Parse expression for initialization.");
    }
    unimplemented!();

}

fn parse_paren_expr<I>(input: &mut Peekable<I>) -> Result<Expr, Err> where I: Iterator<Item = Token> + Clone {
    t_match!(input, TokenKind::LParen, unimplemented!("( missing to denote start of paren expr)"));
    eat_whitespace(input);
    //TODO: parse expr
    eat_whitespace(input);
    t_match!(input, TokenKind::RParen, unimplemented!(") missing to denote end of paren expr"));
    unimplemented!("Parse and return expression");
}

fn parse_block<I>(input: &mut Peekable<I>) -> Result<Block, Err> where I: Iterator<Item = Token> + Clone {
    t_match!(input, TokenKind::LBrace, unimplemented!("{{ missing to denote start of block"));
    let mut stmts = Vec::new();
    loop {
        eat_whitespace(input);
        if t_peek!(input, TokenKind::RBrace) {
            break;
        } else {
            unimplemented!("Parse statement and add it to stmts");
        }
    }
    eat_whitespace(input);
    t_match!(input, TokenKind::RBrace, panic!("Should not occur due to above check"));

    Ok(Block(stmts))
}

fn parse_path<I>(input: &mut Peekable<I>) -> Result<Path, Err> where I: Iterator<Item = Token> + Clone {
    let mut segments = Vec::new();
    segments.push(parse_ident(input)?);

    if t_peek!(input, TokenKind::Dot) {
        t_consume!(input);
        segments.append(&mut parse_path_tail(input)?);
    }

    Ok(Path(segments))
    
}

fn parse_path_tail<I>(input: &mut Peekable<I>) -> Result<Vec<Ident>, Err> where I: Iterator<Item = Token> + Clone {

    let mut segments = Vec::new();
    loop {
        segments.push(parse_ident(input)?);
        if t_peek!(input, TokenKind::Dot) {
            t_consume!(input);
        } else {
            break;
        }
    }

    Ok(segments)
}

fn parse_ident<I>(input: &mut Peekable<I>) -> Result<Ident, Err> where I: Iterator<Item = Token> + Clone {
    use std::str::FromStr;
    let mut buffer = AsciiString::new();
    match input.next() {
        None => unimplemented!("Error, expected an alphabetic; found nothing"),
        Some(ref t) => {
            match t.kind {
                TokenKind::Char(ch) => buffer.push(ch),
                tk @ _ => unimplemented!("Error, expected alphabetic; found {:?}", tk),
            }
        }
    }
    loop {
        {
            match input.peek() {
                None => break,
                Some(ref t) => {
                    match t.kind {
                        TokenKind::Char(_) => (),
                        TokenKind::Digit(_) => (),
                        _ => break,
                    }
                }
            }
        }

        let next_t = input.next().unwrap();
        match next_t.kind {
            TokenKind::Char(c) => buffer.push(c),
            TokenKind::Digit(n) => buffer.push_str(&AsciiString::from_str(&format!("{}", n)).unwrap()),
            _ => panic!("Should not have passed above check"),
        }
    }

    if buffer.len() == 0 {
        unimplemented!("Error, produced identity with 0 length");
    }

    Ok(Ident(buffer))
}

fn parse_keyword<I>(mut input: &mut Peekable<I>) -> Result<Keyword, Err> where I: Iterator<Item = Token> + Clone {
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

fn eat_whitespace<I>(input: &mut Peekable<I>) where I: Iterator<Item = Token> + Clone {
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

#[derive(Debug)]
pub struct Err {
    kind: ErrKind,
    span: Span
}

#[derive(Debug)]
pub enum ErrKind {
    
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use tokenizer::*;
    use ascii::*;
    use tokenizer::TokenKind::*;
    use super::*;

    macro_rules! input {
        ($input: expr) => {{
            let input = tokenize(AsciiString::from_str($input).unwrap()).unwrap();
            input.into_iter().peekable()
        }}
    }

    #[test]
    fn whitespace() {
        let input = tokenize(AsciiString::from_str("\n\t\t abcde").unwrap()).unwrap();
        let mut input = input.into_iter().peekable();
        eat_whitespace(&mut input);
        {
            assert_eq!(input.map(|t| t.kind).collect::<Vec<_>>(),
                        vec![Char(AsciiChar::from('a').unwrap()),
                        Char(AsciiChar::from('b').unwrap()),
                        Char(AsciiChar::from('c').unwrap()),
                        Char(AsciiChar::from('d').unwrap()),
                        Char(AsciiChar::from('e').unwrap())
                        ]
                        );
        }
    }

    #[test]
    fn matching() {
        {
            let mut input = input!("{ }");
            eat_whitespace(&mut input);
            t_match!(input, TokenKind::LBrace, panic!("Failed to find LBrace"));
            eat_whitespace(&mut input);
            t_match!(input, TokenKind::RBrace, panic!("Failed to find RBrace"));
        }
    }

    #[test]
    fn ident() {
        let input = tokenize(AsciiString::from_str("\n\t\t a12bcde \n fasdfa").unwrap()).unwrap();
        let mut input = input.into_iter().peekable();
        eat_whitespace(&mut input);
        let ident = parse_ident(&mut input).unwrap();
        assert_eq!(ident.0, "a12bcde");
        eat_whitespace(&mut input);
        let ident = parse_ident(&mut input).unwrap();
        assert_eq!(ident.0, "fasdfa");
    }

    #[test]
    fn struct_def_parse() {
        let input = "TestStruct { Type1 name1; Type2 name2; Type3 name3; }";
        let input = tokenize(AsciiString::from_str(input).unwrap()).unwrap();
        {
            use self::TokenKind::*;
            assert_eq!(input.clone().into_iter().map(|t| t.kind).collect::<Vec<_>>(),
                       vec![
                            Char(AsciiChar::T),
                            Char(AsciiChar::e),
                            Char(AsciiChar::s),
                            Char(AsciiChar::t),
                            Char(AsciiChar::S),
                            Char(AsciiChar::t),
                            Char(AsciiChar::r),
                            Char(AsciiChar::u),
                            Char(AsciiChar::c),
                            Char(AsciiChar::t),
                            Whitespace,

                            LBrace,

                            Whitespace,
                            Char(AsciiChar::T),
                            Char(AsciiChar::y),
                            Char(AsciiChar::p),
                            Char(AsciiChar::e),
                            Digit(1),
                            Whitespace,
                            Char(AsciiChar::n),
                            Char(AsciiChar::a),
                            Char(AsciiChar::m),
                            Char(AsciiChar::e),
                            Digit(1),
                            Semicolon,

                            Whitespace,
                            Char(AsciiChar::T),
                            Char(AsciiChar::y),
                            Char(AsciiChar::p),
                            Char(AsciiChar::e),
                            Digit(2),
                            Whitespace,
                            Char(AsciiChar::n),
                            Char(AsciiChar::a),
                            Char(AsciiChar::m),
                            Char(AsciiChar::e),
                            Digit(2),
                            Semicolon,

                            Whitespace,
                            Char(AsciiChar::T),
                            Char(AsciiChar::y),
                            Char(AsciiChar::p),
                            Char(AsciiChar::e),
                            Digit(3),
                            Whitespace,
                            Char(AsciiChar::n),
                            Char(AsciiChar::a),
                            Char(AsciiChar::m),
                            Char(AsciiChar::e),
                            Digit(3),
                            Semicolon,

                            Whitespace,
                            RBrace
                                ]);
        }
        let mut input = input.into_iter().peekable();
        eat_whitespace(&mut input);
        let struct_def = parse_struct_def(&mut input).unwrap();
        assert_eq!(struct_def.name,
                   Ident(AsciiString::from_str("TestStruct").unwrap()));
        assert_eq!(struct_def.body.0,
                   vec![
                        StructField { name: Ident(AsciiString::from_str("name1").unwrap()), 
                            field_type: Ident(AsciiString::from_str("Type1").unwrap()) },
                       StructField { name: Ident(AsciiString::from_str("name2").unwrap()), 
                            field_type: Ident(AsciiString::from_str("Type2").unwrap()) },
                        StructField { name: Ident(AsciiString::from_str("name3").unwrap()), 
                            field_type: Ident(AsciiString::from_str("Type3").unwrap()) }
                   ]);
    }

    #[test]
    fn fn_args() {
        let mut input = input!("(Type1 arg1, Type2 arg2, Type3 arg3   )");
        let args = parse_fn_args(&mut input).unwrap();
        assert_eq!(args,
                   vec![
                        FnArg {
                            name: Ident(AsciiString::from_str("arg1").unwrap()),
                            arg_type: Ident(AsciiString::from_str("Type1").unwrap()),
                        },

                        FnArg {
                            name: Ident(AsciiString::from_str("arg2").unwrap()),
                            arg_type: Ident(AsciiString::from_str("Type2").unwrap()),
                        },

                        FnArg {
                            name: Ident(AsciiString::from_str("arg3").unwrap()),
                            arg_type: Ident(AsciiString::from_str("Type3").unwrap()),
                        },
                   ]);
    }

    #[test]
    fn path() {
        let mut input = input!("p1.p2.p3.p4;");
        let path = parse_path(&mut input).unwrap();
        assert_eq!(path.0,
                   vec![
                        Ident(AsciiString::from_str("p1").unwrap()),
                        Ident(AsciiString::from_str("p2").unwrap()),
                        Ident(AsciiString::from_str("p3").unwrap()),
                        Ident(AsciiString::from_str("p4").unwrap()),
                   ]);

        let mut input = input!("p1");
        let path = parse_path(&mut input).unwrap();
        assert_eq!(path.0,
                   vec![
                        Ident(AsciiString::from_str("p1").unwrap()),
                   ]);
    }
}
