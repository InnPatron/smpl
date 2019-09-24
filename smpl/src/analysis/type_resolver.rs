
use super::type_cons::*;
use super::error::TypeError;
use super::semantic_data::ScopedData;

pub fn resolve_types(scoped_data: &ScopedData, lhs: &AbstractType, 
    rhs: &AbstractType) -> Result<(), TypeError> {

    unimplemented!();
}
