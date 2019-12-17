use std::collections::HashMap;

use super::semantic_data::FnId;

pub struct AnonStorage<T>(HashMap<FnId, T>);

impl<T> AnonStorage<T> {
    pub(super) fn new() -> Self {
        AnonStorage(HashMap::new())
    }

    pub(super) fn insert(&mut self, fn_id: FnId, data: T) {
        if self.0.insert(fn_id, data).is_some() {
            panic!("Overriding anonymous storage for {}", fn_id);
        }
    }

    // pub fn data(&self) -> impl Iterator<Item=(&FnId,
}
