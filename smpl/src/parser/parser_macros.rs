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

macro_rules! literal_data {
    ($t: expr) => {{
        match $t {
            Token::StringLiteral(d) => d,
            Token::IntLiteral(d) => d,
            Token::FloatLiteral(d) => d,
            _ => panic!("Not a literal token: {:?}", $t),
        }
    }};
}
