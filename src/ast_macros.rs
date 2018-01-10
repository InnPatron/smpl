#[allow(unused_macros)]
macro_rules! boolean {
    ($val: expr) => {{
        Literal::Bool($val)
    }};

    ($val: expr => Expr) => {{
        Expr::Literal(boolean!($val))
    }};

    ($val: expr => BoxExpr) => {{
        Box::new(boolean!($val => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! number {
    ($str_num: expr) => {{
        Literal::Number($str_num.to_string())
    }};

    ($str_num: expr => Expr) => {{
        Expr::Literal(number!($str_num))
    }};

    ($str_num: expr => BoxExpr) => {{
        Box::new(number!($str_num => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! ident {
    ($ident: expr) => {{
        Ident(AsciiString::from_str($ident).unwrap())
    }};

    ($ident: expr => Expr) => {{ 
        Expr::Ident(ident!($ident))
    }};

    ($ident: expr => BoxExpr) => {{
        Box::new(ident!($ident => Expr))
    }}
}

#[allow(unused_macros)]
macro_rules! path {
    ($($segment: expr),*) => {{
        let mut v = Vec::new();
        $(v.push(ident!($segment));)*;
        Path(v)
    }};
}

#[allow(unused_macros)]
macro_rules! bin_expr {
    ($lhs: expr, $op: expr, $rhs: expr) => {{
        BinExpr {
            op: $op,
            lhs: $lhs,
            rhs: $rhs,
        }
    }};

    (($lhs: expr, $op: expr, $rhs: expr) => Expr) => {{
        Expr::Bin(bin_expr!($lhs, $op, $rhs))
    }};

    (($lhs: expr, $op: expr, $rhs: expr) => BoxExpr) => {{
        Box::new(bin_expr!(($lhs, $op, $rhs) => Expr))
    }};
}
