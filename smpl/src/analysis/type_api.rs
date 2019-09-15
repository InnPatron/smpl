use super::*;
use super::type_cons::{ AbstractType, TypeCons };

#[derive(Debug, Clone)]
pub enum TypeAPIError {
    TyParamArity {
        expected: usize,
        found: usize,
    },

    AppOnPrimitive(PrimitiveKind),
}

#[derive(Debug, Clone)]
pub enum PrimitiveKind {
    String,
    Int,
    Float,
    Bool,
    Unit,
}

pub fn apply(universe: &Universe, type_id: TypeId, parameters: Vec<Type>) 
    -> Result<Type, TypeAPIError> {

    let type_cons = universe
        .get_type_cons(type_id)
        .expect(&format!("Unknown type constructor for {}", type_id));

    match type_cons {
        TypeCons::UncheckedFunction {
            type_params,
            return_type,
        } => {

        }

        TypeCons::Function {
            type_params,
            parameters,
            return_type,
        } => {

        }

        TypeCons::Array {
            element_type,
            size,
        } => {

        }

        TypeCons::Record {
            type_id,
            type_params,
            fields,
            field_map,
        } => {

        }

        TypeCons::Int => return Err(TypeAPIError::AppOnPrimitive(PrimitiveKind::Int)),
        TypeCons::Float => return Err(TypeAPIError::AppOnPrimitive(PrimitiveKind::Float)),
        TypeCons::String => return Err(TypeAPIError::AppOnPrimitive(PrimitiveKind::String)),
        TypeCons::Bool => return Err(TypeAPIError::AppOnPrimitive(PrimitiveKind::Bool)),
        TypeCons::Unit => return Err(TypeAPIError::AppOnPrimitive(PrimitiveKind::Unit)),
    }

    unimplemented!()
}
