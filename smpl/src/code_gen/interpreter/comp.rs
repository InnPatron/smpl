use std::ops::{Add, BitAnd, BitOr, Div, Mul, Neg, Not, Sub};

use crate::ast::BinOp;

pub fn not<T: Not<Output = T>>(t: T) -> T {
    !t
}

pub fn negate<T: Neg<Output = T>>(t: T) -> T {
    -t
}

pub fn is_logical(op: BinOp) -> bool {
    match op {
        BinOp::LogicalAnd | BinOp::LogicalOr => true,
        _ => false,
    }
}

pub fn is_math(op: BinOp) -> bool {
    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => true,
        _ => false,
    }
}

pub fn math_op<T: Add<Output = T> + Sub<Output = T> + Div<Output = T> + Mul<Output = T>>(
    op: BinOp,
    lhs: T,
    rhs: T,
) -> T {
    irmatch!(op;
        BinOp::Add => lhs + rhs,
        BinOp::Sub => lhs - rhs,
        BinOp::Mul => lhs * rhs,
        BinOp::Div => lhs / rhs
    )
}

pub fn partial_cmp<T: PartialEq>(op: BinOp, lhs: T, rhs: T) -> bool {
    irmatch!(op;
             BinOp::Eq => lhs == rhs,
             BinOp::InEq => lhs != rhs
    )
}

pub fn cmp<T: PartialOrd>(op: BinOp, lhs: T, rhs: T) -> bool {
    irmatch!(op;
        BinOp::Eq => lhs == rhs,
        BinOp::InEq => lhs != rhs,
        BinOp::GreaterEq => lhs >= rhs,
        BinOp::LesserEq => lhs <= rhs,
        BinOp::Lesser => lhs < rhs,
        BinOp::Greater => lhs > rhs
    )
}

pub fn logical<T: BitAnd<Output = T> + BitOr<Output = T>>(op: BinOp, lhs: T, rhs: T) -> T {
    irmatch!(op;
             BinOp::LogicalAnd => lhs & rhs,
             BinOp::LogicalOr => lhs | rhs
    )
}
