use std::collections::{HashMap, HashSet};

use crate::ast::{Struct, Ident, ModulePath, TypeAnnotation, TypeAnnotationRef, TypeParams};

use super::semantic_data::{FieldId, TypeId, TypeParamId, Program, ScopedData, Universe, FnId};
use super::error::{AnalysisError, TypeError, ApplicationError};

#[derive(Debug, Clone)]
pub enum TypeCons {

    UncheckedFunction {
        type_params: Option<Vec<TypeParamId>>,
        return_type: TypeApp,
    },

    Function { 
        type_params: Option<Vec<TypeParamId>>,
        parameters: Vec<TypeApp>,
        return_type: TypeApp,
    },

    Array { 
        element_type: TypeApp,
        size: u64 
    },

    Record {
        type_id: TypeId,
        type_params: Option<Vec<TypeParamId>>,
        fields: HashMap<FieldId, TypeApp>,
        field_map: HashMap<Ident, FieldId>,
    },

    Int,
    Float,
    String,
    Bool,
    Unit,
}

impl TypeCons {

    pub fn is_unchecked_fn(&self) -> bool {
        if let TypeCons::UncheckedFunction { .. } = *self {
            true
        } else {
            false
        }
    }

    fn type_params(&self) -> Option<&[TypeParamId]> {
        match *self {

            TypeCons::Function {
                type_params: ref type_params,
                ..
            } => type_params.as_ref().map(|v| v.as_slice()),

            TypeCons::Record {
                type_params: ref type_params,
                ..
            } => type_params.as_ref().map(|v| v.as_slice()),

            TypeCons::UncheckedFunction {
                type_params: ref type_params,
                ..
            } => type_params.as_ref().map(|v| v.as_slice()),

            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeApp {
    Applied {
        type_cons: TypeId,
        args: Option<Vec<TypeApp>>,
    },

    Param(TypeParamId),
}

impl TypeApp {

    pub fn type_cons(&self) -> Option<TypeId> {
        match *self {
            TypeApp::Applied {
                type_cons: ref tc,
                ..
            } => Some(tc.clone()),

            TypeApp::Param(_) => None,
        }
    }
}

pub fn type_app_from_annotation<'a, 'b, 'c, 'd, T: Into<TypeAnnotationRef<'c>>>(
    universe: &'a mut Universe,
    scope: &'b ScopedData,
    anno: T,
    ) -> Result<TypeApp, AnalysisError> {

    match anno.into() {
        TypeAnnotationRef::Path(typed_path) => {
            // Check if path refers to type parameter
            // Assume naming conflicts detected at type parameter declaration
            if typed_path.module_path().0.len() == 1 {

                let ident = typed_path.module_path().0.get(0).unwrap().data();
                let type_param = scope.type_param(ident);
                
                // Found a type parameter
                if let Some(tp_id) = type_param {

                    // Do not allow type arguments on a type parameter
                    if typed_path.annotations().is_some() {
                        return Err(TypeError::ParameterizedParameter {
                            ident: typed_path
                                .module_path()
                                .0
                                .get(0)
                                .unwrap()
                                .data()
                                .clone()
                        }.into());
                    }


                    return Ok(TypeApp::Param(tp_id));
                }
            }

            // Not a type parameter
            let type_cons_path = super::semantic_data::ModulePath::new(
                typed_path
                .module_path()
                .0
                .clone()
                .into_iter()
                .map(|node| node.data().clone())
                .collect()
            );
            let type_cons = scope
                .type_cons(universe, &type_cons_path)
                .ok_or(AnalysisError::UnknownType(typed_path.module_path().clone()))?;

            let type_args = typed_path.annotations().map(|ref vec| {
                vec
                    .iter()
                    .map(|anno| type_app_from_annotation(universe, scope, anno))
                    .collect::<Result<Vec<_>,_>>()
            });

            let type_args = match type_args {
                Some(dat) => Some(dat?),

                None => None,
            };

            Ok(TypeApp::Applied {
                type_cons: type_cons,
                args: type_args
            })
        },

        TypeAnnotationRef::Array(element_type, size) => {
            let element_type_app = type_app_from_annotation(universe,
                                                              scope,
                                                              element_type.data())?;
            let cons = TypeCons::Array {
                element_type: element_type_app,
                size: *size,
            };

            let type_id = universe.new_type_id();
            universe.insert_type_cons(type_id, cons);
            
            Ok(TypeApp::Applied {
                type_cons: type_id,
                args: None,
            })
        },

        TypeAnnotationRef::FnType(tp, args, ret_type) => {

            let (local_type_params, new_scope) = match tp.map(|local_type_params| {
                let mut new_scope = scope.clone();
                let mut local_param_ids = Vec::new();
                let local_type_param_id = universe.new_type_param_id();

                // Insert local type parameters into the current scope
                for p in local_type_params.params.iter() {
                    if new_scope.insert_type_param(p.data().clone(), local_type_param_id) {
                        return Err(TypeError::TypeParameterNamingConflict { 
                            ident: p.data().clone()
                        });
                    }

                    local_param_ids.push(local_type_param_id);
                }

                Ok((local_param_ids, new_scope))
            }) {
                Some(data) => {
                    let data = data?;

                    (Some(data.0), Some(data.1))
                },

                None => (None, None),
            };

            let scope = new_scope
                .as_ref()
                .unwrap_or(scope);

            let arg_type_cons = match args.map(|slice|{
                slice.iter().map(|arg| type_app_from_annotation(universe,
                                                                 scope,
                                                                 arg.data())
                                 )
                    .collect::<Result<Vec<_>, _>>()
            }) {
                Some(args) => Some(args?),
                None => None,
            };

            let return_type_cons = match ret_type.map(|ret_type| {
                type_app_from_annotation(universe,
                                          scope,
                                          ret_type.data())
            }) {
                Some(ret) => Some(ret?),
                None => None,
            };

            let cons = TypeCons::Function {
                type_params: local_type_params,
                parameters: arg_type_cons.unwrap_or(Vec::new()),
                return_type: return_type_cons.unwrap_or(TypeApp::Applied {
                    type_cons: universe.unit(),
                    args: None,
                }),
            };

            let type_id = universe.new_type_id();
            universe.insert_type_cons(type_id, cons);

            Ok(TypeApp::Applied {
                type_cons: type_id,
                args: None,
            })
        },
    }
}
