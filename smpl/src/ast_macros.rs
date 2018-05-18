#[allow(unused_macros)]
macro_rules! boolean {
    ($val: expr) => {{
        use ast::Literal;
        Literal::Bool($val)
    }};

    ($val: expr => Expr) => {{
        use ast::Expr;
        Expr::Literal(boolean!($val))
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
        Expr::Literal(int!($int))
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
        Expr::Ident(ident!($ident))
    }};

    ($ident: expr => BoxExpr) => {{
        Box::new(ident!($ident => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! path {
    ($($segment: expr),*) => {{
        use ast::PathSegment as ASTPathSegment;
        let mut v = Vec::new();
        $(v.push(ASTPathSegment::Ident(ident!($segment)));)*;
        Path(v)
    }};
}

#[allow(unused_macros)]
macro_rules! type_path {
    ($($segment: expr),*) => {{
        let mut v = Vec::new();
        $(v.push(ident!($segment));)*;
        ModulePath(v)
    }};
}

#[allow(unused_macros)]
macro_rules! internal_module_path {
    ($($segment: expr),*) => {{
        use span::Span;
        let mut v = Vec::new();
        $(v.push(ident!($segment));)*;
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
        Expr::Bin(bin_expr!($lhs, $op, $rhs))
    }};

    (($lhs: expr, $op: expr, $rhs: expr) => BoxExpr) => {{
        Box::new(bin_expr!(($lhs, $op, $rhs) => Expr))
    }};
}
