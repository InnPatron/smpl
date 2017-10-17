extern crate itertools;
extern crate ascii;

macro_rules! boolean {
    ($val: expr) => {{
        Literal::Bool($val)
    }};

    ($val: expr => Expr) => {{
        Expr::Literal(AstNode::typed(boolean!($val), SmplType::Bool))
    }};

    ($val: expr => BoxExpr) => {{
        Box::new(boolean!($val => Expr))
    }}
}

macro_rules! number {
    ($str_num: expr) => {{
        Literal::Number($str_num.to_string())
    }};

    ($str_num: expr => Expr) => {{
        Expr::Literal(AstNode::untyped(number!($str_num)))
    }};

    ($str_num: expr => BoxExpr) => {{
        Box::new(number!($str_num => Expr))
    }}
}

macro_rules! ident {
    ($ident: expr) => {{
        Ident(AsciiString::from_str($ident).unwrap())
    }};

    ($ident: expr => Expr) => {{ 
        Expr::Ident(AstNode::untyped(ident!($ident)))
    }};

    ($ident: expr => BoxExpr) => {{
        Box::new(ident!($ident => Expr))
    }}
}

macro_rules! path {
    ($($segment: expr),*) => {{
        let mut v = Vec::new();
        $(v.push(ident!($segment));)*;
        Path(v)
    }};
}

macro_rules! bin_expr {
    ($lhs: expr, $op: expr, $rhs: expr) => {{
        BinExpr {
            op: $op,
            lhs: AstNode::untyped($lhs),
            rhs: AstNode::untyped($rhs),
        }
    }};

    (($lhs: expr, $op: expr, $rhs: expr) => Expr) => {{
        Expr::Bin(AstNode::untyped(bin_expr!($lhs, $op, $rhs)))
    }};

    (($lhs: expr, $op: expr, $rhs: expr) => BoxExpr) => {{
        Box::new(bin_expr!(($lhs, $op, $rhs) => Expr))
    }};
}

mod parser;
mod ast;
mod smpl_type;
mod type_ck;
#[cfg(test)]
mod parser_tests;

use std::ops::Range;

pub type Span = Range<usize>;
