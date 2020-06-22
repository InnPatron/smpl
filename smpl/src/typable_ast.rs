use std::fmt;

use crate::analysis::abstract_type::AbstractType;
use crate::expr_ast::Expr;

pub struct Typable<T> {
    data: T,
    typ: Option<AbstractType>,
}

impl<T> Typable<T> {
    pub fn untyped(data: T) -> Self {
        Typable {
            data,
            typ: None
        }
    }

    pub fn typed(data: T, typ: AbstractType) -> Self {
        Typable {
            data,
            typ: Some(typ),
        }
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut T {
        &mut self.data
    }

    pub fn into_data(self) -> T {
        self.data
    }
}

impl<T> PartialEq for Typable<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Typable<T>) -> bool {
        self.data == other.data
    }
}

impl<T> Eq for Typable<T> where T: Eq {}

impl<T> ::std::hash::Hash for Typable<T>
where
    T: ::std::hash::Hash,
{
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

impl<T> Clone for Typable<T>
where
    T: Clone,
{
    fn clone(&self) -> Typable<T> {
        Typable {
            data: self.data.clone(),
            typ: self.typ.clone(),
        }
    }
}

impl<T> ::std::fmt::Debug for Typable<T>
where
    T: ::std::fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        ::std::fmt::Debug::fmt(&self.data, f)
    }
}

pub trait Typed {
    fn typ(&self) -> &AbstractType;

    fn set_type(&mut self, t: AbstractType);
}

impl<T> Typed for Typable<T> {
    fn typ(&self) -> &AbstractType {
        self.typ.as_ref().expect("Missing type")
    }

    fn set_type(&mut self, t: AbstractType) {
        if self.typ.is_some() {
            panic!("Attempting to overrwite a type");
        }

        self.typ = Some(t);
    }
}

impl Typed for Expr {
    fn typ(&self) -> &AbstractType {
        match *self {
            Expr::Assignment(ref typed) => typed.typ(),
            Expr::If(ref typed) => typed.typ(),
            Expr::While(ref typed) => typed.typ(),
            Expr::Bin(ref typed) => typed.typ(),
            Expr::Uni(ref typed) => typed.typ(),
            Expr::Literal(ref typed) => typed.typ(),
            Expr::Binding(ref typed) => typed.typ(),
            Expr::Access(ref typed) => typed.typ(),
            Expr::FnCall(ref typed) => typed.typ(),
            Expr::StructInit(ref typed) => typed.typ(),
            Expr::ArrayInit(ref typed) => typed.typ(),
            Expr::AnonymousFn(ref typed) => typed.typ(),
            Expr::Path(ref typed) => typed.typ(),
        }
    }

    fn set_type(&mut self, t: AbstractType) {
        match *self {
            Expr::Assignment(ref mut typed) => typed.set_type(t),
            Expr::If(ref mut typed) => typed.set_type(t),
            Expr::While(ref mut typed) => typed.set_type(t),
            Expr::Bin(ref mut typed) => typed.set_type(t),
            Expr::Uni(ref mut typed) => typed.set_type(t),
            Expr::Literal(ref mut typed) => typed.set_type(t),
            Expr::Binding(ref mut typed) => typed.set_type(t),
            Expr::Access(ref mut typed) => typed.set_type(t),
            Expr::FnCall(ref mut typed) => typed.set_type(t),
            Expr::StructInit(ref mut typed) => typed.set_type(t),
            Expr::ArrayInit(ref mut typed) => typed.set_type(t),
            Expr::AnonymousFn(ref mut typed) => typed.set_type(t),
            Expr::Path(ref mut typed) => typed.set_type(t),
        }
    }
}
