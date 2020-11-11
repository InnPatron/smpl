macro_rules! ast_node {
    ($t: expr, $l: expr, $r: expr) => {{
        AstNode::new($t, Span::new($l, $r))
    }};
}

macro_rules! ident {
    ($t: expr) => {{
        match $t {
            Token::Ident(i) => i,
            _ => panic!("Not Token::Ident: {:?}", $t),
        }
    }};
}
