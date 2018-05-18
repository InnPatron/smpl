#[allow(unused_macros)]
macro_rules! boolean {
    ($val: expr) => {{
        use ast::Literal;
        Literal::Bool($val)
    }};

    ($val: expr => Expr) => {{
        use ast::Expr;
        use span::Span;
        Expr::Literal(AstNode::new(boolean!($val), Span::new(0, 0)))
    }};

    ($val: expr => BoxExpr) => {{
        Box::new(boolean!($val => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! int {
    ($int: expr) => {{
        use ast::Literal;
        Literal::Int($int)
    }};

    ($int: expr => Expr) => {{
        use ast::Expr;
        use span::Span;
        Expr::Literal(AstNode::new(int!($int), Span::new(0, 0)))
    }};

    ($int: expr => BoxExpr) => {{
        Box::new(int!($int => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! ident {
    ($ident: expr) => {{
        use ast::Ident;
        use ascii::*;
        use std::str::FromStr;
        Ident(AsciiString::from_str($ident).unwrap())
    }};

    ($ident: expr => Expr) => {{
        use ast::Expr;
        use span::Span;
        Expr::Ident(AstNode::new(ident!($ident), Span::new(0, 0)))
    }};

    ($ident: expr => BoxExpr) => {{
        Box::new(ident!($ident => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! path {
    ($($segment: expr),*) => {{
        use span::Span;
        use ast::PathSegment as ASTPathSegment;
        let mut v = Vec::new();
        $(v.push(ASTPathSegment::Ident(AstNode::new(ident!($segment), Span::new(0, 0))));)*;
        Path(v)
    }};
}

#[allow(unused_macros)]
macro_rules! type_path {
    ($($segment: expr),*) => {{
        use span::Span;
        let mut v = Vec::new();
        $(v.push(AstNode::new(ident!($segment), Span::new(0, 0)));)*;
        ModulePath(v)
    }};
}

#[allow(unused_macros)]
macro_rules! bin_expr {
    ($lhs: expr, $op: expr, $rhs: expr) => {{
        use ast::BinExpr;
        BinExpr {
            op: $op,
            lhs: $lhs,
            rhs: $rhs,
        }
    }};

    (($lhs: expr, $op: expr, $rhs: expr) => Expr) => {{
        use ast::Expr;
        use span::Span;
        Expr::Bin(AstNode::new(bin_expr!($lhs, $op, $rhs), Span::new(0, 0)))
    }};

    (($lhs: expr, $op: expr, $rhs: expr) => BoxExpr) => {{
        Box::new(bin_expr!(($lhs, $op, $rhs) => Expr))
    }};
}
