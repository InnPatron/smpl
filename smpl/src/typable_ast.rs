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

pub trait Typed {
    fn typ(&self) -> &AbstractType;
}

impl<T> Typed for Typable<T> {
    fn typ(&self) -> &AbstractType {
        self.typ()
    }
}
