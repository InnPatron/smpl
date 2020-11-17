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
    ($t: expr => string) => {{
        match $t {
            Token::StringLiteral(d) => d,
            _ => panic!("Not a string literal token: {:?}", $t),
        }
    }};

    ($t: expr => float) => {{
        match $t {
            Token::FloatLiteral(d) => d,
            _ => panic!("Not a float literal token: {:?}", $t),
        }
    }};

    ($t: expr => int) => {{
        match $t {
            Token::IntLiteral(d) => d,
            _ => panic!("Not an int literal token: {:?}", $t),
        }
    }};
}
