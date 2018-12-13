#[allow(unused_macros)]
macro_rules! dummy_node {
    ($val: expr) => {{
        use crate::span::Span;
        AstNode::new($val, Span::dummy())
    }};
}

#[allow(unused_macros)]
macro_rules! boolean {
    ($val: expr) => {{
        use crate::ast::Literal;
        Literal::Bool($val)
    }};

    ($val: expr => Expr) => {{
        use crate::ast::Expr;
        Expr::Literal(dummy_node!(boolean!($val)))
    }};

    ($val: expr => BoxExpr) => {{
        Box::new(boolean!($val => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! string {
    ($val: expr) => {{
        use crate::ast::Literal;
        Literal::String($val.to_string())
    }};

    ($val: expr => Expr) => {{
        use crate::ast::Expr;
        Expr::Literal(dummy_node!(string!($val)))
    }};

    ($val: expr => BoxExpr) => {{
        Box::new(string!($val => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! int {
    ($int: expr) => {{
        use crate::ast::Literal;
        Literal::Int($int)
    }};

    ($int: expr => Expr) => {{
        use crate::ast::Expr;
        Expr::Literal(dummy_node!(int!($int)))
    }};

    ($int: expr => BoxExpr) => {{
        Box::new(int!($int => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! ident {
    ($ident: expr) => {{
        use crate::ast::Ident;
        Ident($ident.to_string())
    }};

    ($ident: expr => Expr) => {{
        use ast::Expr;
        Expr::Ident(dummy_node!(ident!($ident)))
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
        $(v.push(ASTPathSegment::Ident(dummy_node!(ident!($segment))));)*;
        Path(v)
    }};
}

#[allow(unused_macros)]
macro_rules! type_path {
    ($($segment: expr),*) => {{
        let mut v = Vec::new();
        $(v.push(dummy_node!(ident!($segment)));)*;
        ModulePath(v)
    }};
}

#[allow(unused_macros)]
macro_rules! internal_module_path {
    ($($segment: expr),*) => {{
        let mut v = Vec::new();
        $(v.push(ident!($segment));)*;
        ModulePath(v)
    }};
}

#[allow(unused_macros)]
macro_rules! bin_expr {
    ($lhs: expr, $op: expr, $rhs: expr) => {{
        use crate::ast::BinExpr;
        BinExpr {
            op: $op,
            lhs: $lhs,
            rhs: $rhs,
        }
    }};

    (($lhs: expr, $op: expr, $rhs: expr) => Expr) => {{
        use crate::ast::Expr;
        Expr::Bin(dummy_node!(bin_expr!($lhs, $op, $rhs)))
    }};

    (($lhs: expr, $op: expr, $rhs: expr) => BoxExpr) => {{
        Box::new(bin_expr!(($lhs, $op, $rhs) => Expr))
    }};
}
