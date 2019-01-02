use std::collections::{HashMap, HashSet};

use crate::ast::{Struct, Ident, ModulePath, TypeAnnotation, TypeAnnotationRef};

use super::semantic_data::{FieldId, TypeId, Program, ScopedData};
use super::smpl_type::*;
use super::error::{AnalysisError, TypeError, ApplicationError};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCons {

    Function { 
        type_params: Vec<TypeId>,
        parameters: Vec<TypeId>,
        return_type: TypeId,
    },

    Array {
        element_type: TypeId,
        size: u64,
    },

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

pub fn type_cons_from_annotation<'a, 'b, 'c, 'd, T: Into<TypeAnnotationRef<'c>>>(
    program: &'a mut Program,
    scope: &'b ScopedData,
    anno: T,
    type_params: &'d HashMap<&'d Ident, TypeId>
    ) -> Result<TypeId, AnalysisError> {

    match anno.into() {
        TypeAnnotationRef::Path(typed_path) => {
            if typed_path.module_path().0.len() == 1 {
                // Check if path refers to type parameter
            } else {
                // Get type id from Scope
            }

            unimplemented!()
        },

        TypeAnnotationRef::Array(element_type, size) => {
            let element_type_cons = type_cons_from_annotation(program,
                                                              scope,
                                                              element_type.data(),
                                                              type_params)?;
            let cons = TypeCons::Array {
                element_type: element_type_cons,
                size: size.clone(),
            };

            // TODO: Insert type constructor in the universe
            unimplemented!()
        },

        TypeAnnotationRef::FnType(args, ret_type) => {

            let arg_type_cons = match args.map(|slice|{
                slice.iter().map(|arg| type_cons_from_annotation(program,
                                                                 scope,
                                                                 arg.data(),
                                                                 type_params)
                                 )
                    .collect::<Result<Vec<_>, _>>()
            }) {
                Some(args) => Some(args?),
                None => None,
            };

            let return_type_cons = match ret_type.map(|ret_type| {
                type_cons_from_annotation(program,
                                          scope,
                                          ret_type.data(),
                                          type_params)
            }) {
                Some(ret) => Some(ret?),
                None => None,
            };

            let cons = TypeCons::Function {
                type_params: type_params
                    .values()
                    .map(|type_id| type_id.clone())
                    .collect(),
                parameters: arg_type_cons.unwrap_or(Vec::new()),
                return_type: return_type_cons.unwrap_or(program.universe().unit()),
            };

            // TODO: Insert type constructor in the universe
            unimplemented!()
        },
    }

}

// TODO: Store type constructors in Program
pub fn generate_struct_type_cons(
    program: &mut Program,
    scope: &ScopedData,
    struct_def: &Struct,
) -> Result<Vec<FieldId>, AnalysisError> {
    let (universe, _metadata, _features) = program.analysis_context();

    // Check no parameter naming conflicts
    let mut type_parameter_map = HashMap::new();
    match struct_def.type_params {
        Some(ref params) => {
            for p in params.params.iter() {
                if type_parameter_map.contains_key(p.data()) {
                    return Err(TypeError::ParameterNamingConflict {
                        ident: p.data().clone(),
                    }.into());
                } else {
                    type_parameter_map.insert(p.data(), universe.new_type_id());
                }
            }
        }

        None => (),
    }

    // Generate the constructor
    let mut fields = HashMap::new();
    let mut field_map = HashMap::new();
    let mut order = Vec::new();
    if let Some(ref body) = struct_def.body.0 {
        for field in body.iter() {
            let f_id = universe.new_field_id();
            let f_name = field.name.data().clone();
            let f_type_path = &field.field_type;
            let path_data = f_type_path.data();

            let field_type = match struct_def.type_params {

                // TODO: Recursively generate type constructor generators for fields
                Some(ref params) => {
                    unimplemented!()
                }

                // No type parameters, behave as if concrete type constructor
                None => scope.type_id(universe, path_data.into())?,
            };

            // Map field to type constructor
            fields.insert(f_id, field_type);

            if field_map.contains_key(&f_name) {
                return Err(TypeError::FieldNamingConflict {
                    ident: f_name.clone(),
                }.into());
            } else {
                field_map.insert(f_name, f_id);
            }

            order.push(f_id);
        }
    }

    // TODO: Insert type constructor
    let type_cons = TypeCons::Record {
        name: struct_def.name.data().clone(),
        fields: fields,
        field_map: field_map,
        type_params: type_parameter_map
            .values()
            .map(|id| id.clone())
            .collect(),
    };
    

    Ok(order)
}
