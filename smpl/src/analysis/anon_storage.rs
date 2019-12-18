use std::collections::HashMap;

use super::semantic_data::FnId;

pub struct AnonStorage<T>(HashMap<FnId, T>);

impl<T> AnonStorage<T> {
    pub(super) fn new() -> Self {
        AnonStorage(HashMap::new())
    }

    pub(super) fn from_iter<I>(i: I) -> Self
        where I: Iterator<Item=(FnId, T)> {

        AnonStorage(i.collect())
    }

    pub(super) fn insert(&mut self, fn_id: FnId, data: T) {
        if self.0.insert(fn_id, data).is_some() {
            panic!("Overriding anonymous storage for {}", fn_id);
        }
    }

    pub(super) fn append(&mut self, other: &mut AnonStorage<T>) {
        other.0
            .drain()
            .for_each(|(fn_id, t)| self.insert(fn_id, t));
    }

    pub(super) fn data(self) -> impl Iterator<Item=(FnId, T)> {
        self.0.into_iter()
    }

    pub(super) fn ref_data(&self) -> impl Iterator<Item=(FnId, &T)> {
        self.0.iter().map(|(fn_id, t)| (fn_id.clone(), t))
    }

    pub(super) fn remove(&mut self, fn_id: FnId) -> T {
        self.0
            .remove(&fn_id)
            .expect(&format!("No data for anonymous fn: {}", fn_id))
    }

}
