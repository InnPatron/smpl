use std::collections::{HashMap, HashSet};

use crate::ast::{Struct, Ident, ModulePath, TypeAnnotation, TypeAnnotationRef, TypeParams};

use super::semantic_data::{FieldId, TypeId, TypeParamId, Program, ScopedData, Universe, FnId};
use super::error::{AnalysisError, TypeError, ApplicationError};

macro_rules! nill_check {
    ($type_args: expr) => {{
        if $type_args.is_some() {
            // TODO: error on type args to type cons int, bool, etc
            unimplemented!()
        }
    }}
}

#[derive(PartialEq, Clone)]
pub enum Type {
    UncheckedFunction {
        return_type: Box<Type>,
    },

    Function { 
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },

    Array { 
        element_type: Box<Type>,
        size: u64 
    },

    Record {
        type_id: TypeId,
        fields: HashMap<FieldId, Type>,
        field_map: HashMap<Ident, FieldId>,
    },

    Param(TypeParamId),

    Int,
    Float,
    String,
    Bool,
    Unit,
}

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

    fn apply(&self, universe: &Universe) -> Result<Type, TypeError> {
        let mut param_map = HashMap::new();

        self.apply_internal(universe, &param_map)
    }

    fn apply_internal(&self, universe: &Universe, param_map: &HashMap<TypeParamId, TypeApp>) -> Result<Type, TypeError> {
        match *self {
            TypeApp::Applied {
                type_cons: ref type_cons,
                args: ref type_args,
            } => {
                let type_cons = universe.get_type_cons(*type_cons).unwrap();
                let new_param_map = match (type_cons.type_params(), type_args) {

                    (Some(ref type_params), Some(ref type_args)) => {
                        if type_params.len() != type_args.len() {
                            return Err(ApplicationError::Arity { 
                                expected: type_params.len(), 
                                found: type_args.len(),
                            }.into());
                        }

                        let mut param_map = param_map.clone();
                        
                        for (param_id, type_arg) in type_params.iter().zip(type_args.iter()) {
                            param_map.insert(param_id.clone(), type_arg.clone());
                        }

                        Some(param_map)
                    },

                    (Some(ref type_params), None) => {
                        return Err(ApplicationError::Arity { 
                            expected: type_params.len(), 
                            found: 0,
                        }.into());
                    },

                    (None, Some(ref type_args)) => {
                        return Err(ApplicationError::Arity { 
                            expected: 0, 
                            found: type_args.len(),
                        }.into());
                    },

                    (None, None) => None,

                };

                let param_map = new_param_map
                    .as_ref()
                    .unwrap_or(param_map);

                match type_cons {
                    TypeCons::Function { 
                        type_params: ref type_params,
                        parameters: ref parameters,
                        return_type: ref return_type,
                    } => {
                        let parameters = parameters
                            .iter()
                            .map(|app| app.apply_internal(universe, param_map))
                            .collect::<Result<Vec<_>, _>>()?;

                        let return_type = return_type.apply_internal(universe, param_map)?;

                        Ok(Type::Function {
                            parameters: parameters,
                            return_type: Box::new(return_type),
                        })
                    },

                    TypeCons::UncheckedFunction { 
                        type_params: ref type_params,
                        return_type: ref return_type,
                    } => {

                        let return_type = return_type.apply_internal(universe, param_map)?;

                        Ok(Type::UncheckedFunction {
                            return_type: Box::new(return_type),
                        })
                    },

                    TypeCons::Array { 
                        element_type: ref element_type,
                        size: size,
                    } => {

                        let element_type = element_type.apply_internal(universe, param_map)?;
                        Ok(Type::Array {
                            element_type: Box::new(element_type),
                            size: size
                        })
                    },

                    TypeCons::Record {
                        type_id: type_id,
                        type_params: ref type_params,
                        fields: ref fields,
                        field_map: ref field_map,
                    } => {
                        Ok(Type::Record {
                            type_id: type_id.clone(),
                            fields: fields
                                .iter()
                                .map(|(k, v)| {
                                    match v.apply_internal(universe, param_map) {
                                        Ok(v) => Ok((k.clone(), v)),
                                        Err(e) => Err(e),
                                    }
                                 })
                                .collect::<Result<HashMap<_,_>, _>>()?,

                            field_map: field_map.clone(),
                        })
                    },

                    TypeCons::Int => {
                        nill_check!(type_args);
                        Ok(Type::Int)
                    },

                    TypeCons::Float => {
                        nill_check!(type_args);
                        Ok(Type::Float)
                    },

                    TypeCons::Bool => {
                        nill_check!(type_args);
                        Ok(Type::Bool)
                    },

                    TypeCons::String => {
                        nill_check!(type_args);
                        Ok(Type::String)
                    },

                    TypeCons::Unit => {
                        nill_check!(type_args);
                        Ok(Type::Unit)
                    }
                }
            }

            TypeApp::Param(ref param_id) => {
                if let Some(type_app) = param_map.get(param_id) {
                    // Equivalence relation between type parameters
                    type_app.apply_internal(universe, param_map)
                } else {
                    // Final equivalence to another type parameter
                    Ok(Type::Param(param_id.clone()))
                }
            }
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
            universe.insert_generated_type_cons(type_id, cons);
            
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
            universe.insert_generated_type_cons(type_id, cons);

            Ok(TypeApp::Applied {
                type_cons: type_id,
                args: None,
            })
        },
    }
}

pub fn type_app_eq(universe: &Universe, lhs: &TypeApp, rhs: &TypeApp) -> Result<bool, TypeError> {
    let lhs = lhs.apply(universe)?;
    let rhs = rhs.apply(universe)?;

    Ok(lhs == rhs)
}

fn type_app_eq_rec(universe: &Universe, lhs: &TypeApp, rhs: &TypeApp) -> bool {
    // Assume types have been applied
    match (lhs, rhs) {
        (&TypeApp::Applied {
            type_cons: ref lhs_type_cons,
            args: ref lhs_args,
        }, &TypeApp::Applied {
            type_cons: ref rhs_type_cons,
            args: ref rhs_args,
        }) => {
            let lhs = universe.get_type_cons(*lhs_type_cons).unwrap();
            let rhs = universe.get_type_cons(*rhs_type_cons).unwrap();

            match (lhs, rhs) {
                (TypeCons::Int, TypeCons::Int) => true,
                (TypeCons::Float, TypeCons::Float) => true,
                (TypeCons::Bool, TypeCons::Bool) => true,
                (TypeCons::String, TypeCons::String) => true,
                (TypeCons::Unit, TypeCons::Unit) => true,

                (TypeCons::Array { 
                    element_type: ref lhs_e_type,
                    size: lhs_size,
                }, TypeCons::Array {
                    element_type: ref rhs_e_type,
                    size: rhs_size,
                }) => {
                    (lhs_size == rhs_size) && 
                        type_app_eq_rec(universe, lhs_e_type, rhs_e_type)
                },

                (TypeCons::Function { 
                    type_params: _,
                    parameters: ref lhs_params,
                    return_type: ref lhs_ret,
                }, TypeCons::Function { 
                    type_params: _,
                    parameters: ref rhs_params,
                    return_type: ref rhs_ret,
                }) => {
                    if lhs_params.len() != rhs_params.len() {
                        return false;
                    }

                    if type_app_eq_rec(universe, lhs_ret, rhs_ret) == false {
                        return false;
                    }

                    for (lhs, rhs) in lhs_params.iter().zip(rhs_params.iter()) {
                        if type_app_eq_rec(universe, lhs, rhs) == false {
                            return false;
                        }
                    }

                    true
                },

                (TypeCons::UncheckedFunction { 
                    type_params: _,
                    return_type: ref lhs_ret,
                }, TypeCons::UncheckedFunction { 
                    type_params: _,
                    return_type: ref rhs_ret,
                }) => {
                    type_app_eq_rec(universe, lhs_ret, rhs_ret)
                }

                (TypeCons::Record {
                    type_id: lhs_type_id,
                    type_params: _,
                    fields: ref lhs_field_id_type_map,
                    field_map: _,
                }, TypeCons::Record {
                    type_id: rhs_type_id,
                    type_params: _,
                    fields: ref rhs_field_id_type_map,
                    field_map: _,
                }) => {
                    
                    // For nominal typing
                    if lhs_type_id != rhs_type_id {
                        return false;
                    }

                    // Since they both originate from same type cons, field names should be equal
                    for (field_id, lhs_field_app) in lhs_field_id_type_map {
                        let rhs_field_app = rhs_field_id_type_map.get(field_id).unwrap();

                        if type_app_eq_rec(universe, lhs_field_app, rhs_field_app) == false {
                            return false;
                        }
                    }
                    
                    true
                },

                _ => false,
            }
        },

        (&TypeApp::Param(lhs_param), &TypeApp::Param(rhs_param)) => {
            lhs_param == rhs_param
        },

        _ => false,
    }
}
