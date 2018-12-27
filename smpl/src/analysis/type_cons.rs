use std::collections::HashMap;

use crate::ast::Ident;

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
        name: Ident,
        type_params: Vec<TypeId>,
        fields: HashMap<FieldId, TypeId>,
        field_map: HashMap<Ident, FieldId>,
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

            TypeCons::Record {
                name: ref name,
                type_params: ref type_params,
                fields: ref fields,
                field_map: ref field_map,
            } => {
                if type_params.len() > 0 {
                    let mut args = args.ok_or(ApplicationError::Arity {
                        expected: type_params.len(),
                        found: 0
                    })?;

                    if args.len() != type_params.len() {
                        return Err(ApplicationError::Arity {
                            expected: type_params.len(),
                            found: args.len()
                        });
                    }

                    // Map parameters to arguments
                    let mut param_to_arg_map = HashMap::new();
                    for (pos, (param, arg)) in type_params.iter().zip(args).enumerate() {
                        match arg {
                            TypeArg::Type(id) => param_to_arg_map.insert(param, id),

                            TypeArg::Number(_) => {
                                return Err(ApplicationError::ExpectedType {
                                    param_position: pos
                                });
                            }
                        };
                    }

                    // Apply type arguments to the record type
                    let applied_fields = fields
                        .iter()
                        .map(|(field_id, field_type_id)| {

                            let type_id = param_to_arg_map.get(field_type_id)
                                .unwrap_or(field_type_id);

                            (field_id.clone(), type_id.clone())
                        })
                    .collect::<HashMap<_,_>>();

                    let struct_type = StructType {
                        name: name.clone(),
                        fields: applied_fields.clone(),
                        field_map: field_map.clone(),
                    };

                    Ok(SmplType::Struct(struct_type))

                } else {
                    if let Some(args) = args {
                        Err(ApplicationError::Arity {
                            expected: 0,
                            found: args.len(),
                        })
                    } else {
                        let struct_type = StructType {
                            name: name.clone(),
                            fields: fields.clone(),
                            field_map: field_map.clone(),
                        };

                        Ok(SmplType::Struct(struct_type))
                    }
                }
            }

            _ => unimplemented!(),
        }
    }
}
