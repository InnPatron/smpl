use std::fmt;

use crate::analysis::abstract_type::AbstractType;

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

    pub fn typ(&self) -> &AbstractType {
        self.typ.as_ref().expect("Missing type")
    }

    pub fn set_type(&mut self, t: AbstractType) {
        if self.typ.is_some() {
            panic!("Attempting to overrwite a type");
        }

        self.typ = Some(t);
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
}

impl<T> Typed for Typable<T> {
    fn typ(&self) -> &AbstractType {
        self.typ()
    }
}
