use std::collections::HashMap;

use super::semantic_data::{FieldId, TypeId};
use super::smpl_type::*;
use super::error::ApplicationError;

pub enum TypeCons {

    Function { 
        type_params: Vec<TypeId>,
        parameters: Vec<TypeId>,
    },

    Array,

    Record {
        type_params: Vec<TypeId>,
        fields: Vec<TypeId>,
    },

    Int,
    Float,
    String,
    Bool,
    Unit,
}

pub enum TypeArg {
    Type(TypeId),
    Number(i64),
}

impl TypeCons {
    fn apply(&self, mut args: Option<Vec<TypeArg>>) -> Result<SmplType, ApplicationError> {

        match *self {
            TypeCons::Array => {
                let mut args = args.ok_or(ApplicationError::Arity {
                    expected: 2,
                    found: 0
                })?;

                if args.len() != 2 {
                    return Err(ApplicationError::Arity {
                        expected: 2,
                        found: args.len()
                    });
                }

                let array_size = args.pop().unwrap();
                let element_type = args.pop().unwrap();

                let element_type = match element_type {
                    TypeArg::Type(id) => id,
                    TypeArg::Number(_) => return Err(ApplicationError::ExpectedType { 
                        param_position: 0
                    }),
                };

                let array_size = match array_size {
                    TypeArg::Type(_) => return Err(ApplicationError::ExpectedNumber { 
                        param_position: 1
                    }),

                    TypeArg::Number(num) =>  num
                };

                if array_size <= 0 {
                    return Err(ApplicationError::InvalidNumber {
                        param_position: 1,
                        found: array_size,
                    });
                }


                let array_type = ArrayType { 
                    base_type: element_type,
                    size: array_size as u64
                };

                Ok(SmplType::Array(array_type))
            }

            _ => unimplemented!(),
        }
    }
}
