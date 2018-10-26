use std::iter::{Iterator, Peekable};

use span::*;
use ast::*;
use super::tokens::*;

pub type ParseErr<T> = Result<T, String>;

macro_rules! consume_token  {
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
                _ => unimplemented!(),
            }
        }).map_err(|e| format!("{:?}", e))? {
            ModDec::Struct => {
                decls.push(DeclStmt::Struct(struct_decl(tokens, anno)?));
                anno = Vec::new();
            }

            ModDec::Annotation => {
                anno = annotations(tokens)?;
            },
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
        unimplemented!("Parse for struct fields");
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

fn module_decl(tokens: &mut BufferedTokenizer) -> ParseErr<Ident> {
    // Consume MOD
    let _mod_kw = consume_token!(tokens, Token::Mod);
    let (_idloc, ident) = consume_token!(tokens, Token::Identifier(i) => Ident(i));
    let _semi = consume_token!(tokens, Token::Semi);

    Ok(ident)
}
