use super::semantic_data::TypeId;
use super::smpl_type::*;

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

impl TypeCons {
    fn apply(&self, mut args: Vec<TypeId>, numeric: Option<Vec<i64>>) -> Result<SmplType, ()> {

        match *self {
            TypeCons::Array => {
                if args.len() != 1 {
                    unimplemented!("Invalid Array application");
                }

                let size = if let Some(mut numeric) = numeric {
                    if numeric.len() != 1 {
                        unimplemented!("Invalid Array application");
                    } else {
                        let size = numeric.pop().unwrap();

                        if size <= 0 {
                            unimplemented!("Invalid Array application");
                        }
                        size
                    }

                } else {
                    unimplemented!("Invalid Array application");
                };

                let element_type = args.pop().unwrap();

                let array_type = ArrayType { 
                    base_type: element_type,
                    size: size as u64
                };

                Ok(SmplType::Array(array_type))
            }

            _ => unimplemented!(),
        }
    }
}
