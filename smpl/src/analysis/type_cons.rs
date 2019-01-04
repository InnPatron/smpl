use std::collections::{HashMap, HashSet};

use crate::ast::{Struct, Ident, ModulePath, TypeAnnotation, TypeAnnotationRef};

use super::semantic_data::{FieldId, TypeId, TypeParamId, Program, ScopedData, Universe};
use super::smpl_type::*;
use super::error::{AnalysisError, TypeError, ApplicationError};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCons {

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
        name: Ident,
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

            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeApp {
    Applied {
        type_cons: Box<TypeCons>,
        args: Option<Vec<TypeApp>>
    },

    Param(TypeParamId),
}

impl TypeApp {

    pub fn apply(&self) -> Result<TypeApp, TypeError> {
        let mut param_map = HashMap::new();

        self.apply_internal(&param_map)
    }

    fn apply_internal(&self, param_map: &HashMap<TypeParamId, TypeApp>) -> Result<TypeApp, TypeError> {
        match *self {
            TypeApp::Applied {
                type_cons: ref type_cons,
                args: ref type_args,
            } => {
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

                match **type_cons {
                    TypeCons::Function { 
                        type_params: ref type_params,
                        parameters: ref parameters,
                        return_type: ref return_type,
                    } => {
                        let parameters = parameters
                            .iter()
                            .map(|app| app.apply_internal(param_map))
                            .collect::<Result<Vec<_>, _>>()?;

                        let return_type = return_type.apply_internal(param_map)?;

                        let type_cons = TypeCons::Function {
                            type_params: type_params.clone(),
                            parameters: parameters,
                            return_type: return_type,
                        };
                        Ok(TypeApp::Applied {
                            type_cons: Box::new(type_cons),
                            args: None,
                        })
                    },

                    TypeCons::Array { 
                        element_type: ref element_type,
                        size: size,
                    } => {

                        let type_cons = TypeCons::Array {
                            element_type: element_type.apply_internal(param_map)?,
                            size: size
                        };

                        Ok(TypeApp::Applied {
                            type_cons: Box::new(type_cons),
                            args: None
                        })
                    },

                    TypeCons::Record {
                        name: ref name,
                        type_params: ref type_params,
                        fields: ref fields,
                        field_map: ref field_map,
                    } => {
                        let type_cons = TypeCons::Record {
                            name: name.clone(),
                            type_params: type_params.clone(),
                            fields: fields
                                .iter()
                                .map(|(k, v)| {
                                    match v.apply_internal(param_map) {
                                        Ok(v) => Ok((k.clone(), v)),
                                        Err(e) => Err(e),
                                    }
                                 })
                                .collect::<Result<HashMap<_,_>, _>>()?,

                            field_map: field_map.clone(),
                        };

                        Ok(TypeApp::Applied {
                            type_cons: Box::new(type_cons),
                            args: None,
                        })
                    },

                    _ => Ok(self.clone()),
                }
            }

            TypeApp::Param(ref param_id) => {
                if let Some(type_app) = param_map.get(param_id) {
                    // Equivalence relation between type parameters
                    type_app.apply_internal(param_map)
                } else {
                    // Final equivalence to another type parameter
                    Ok(TypeApp::Param(param_id.clone()))
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
            let type_cons = scope.type_cons(universe, &type_cons_path)?;

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
                type_cons: Box::new(type_cons),
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
            
            Ok(TypeApp::Applied {
                type_cons: Box::new(cons),
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
                    type_cons: Box::new(TypeCons::Unit),
                    args: None,
                }),
            };

            Ok(TypeApp::Applied {
                type_cons: Box::new(cons),
                args: None,
            })
        },
    }
}

// TODO: Store type constructors in Program
pub fn generate_struct_type_cons(
    program: &mut Program,
    scope: &ScopedData,
    struct_def: &Struct,
) -> Result<(TypeCons, Vec<FieldId>), AnalysisError> {
    let (universe, _metadata, _features) = program.analysis_context();

    // Check no parameter naming conflicts
    let mut new_scope = scope.clone();
    let mut type_parameter_map = HashMap::new();
    match struct_def.type_params {
        Some(ref params) => {
            for p in params.params.iter() {

                // Check for type parameter naming conflict
                if type_parameter_map.contains_key(p.data()) {

                    // Naming conflict
                    return Err(TypeError::ParameterNamingConflict {
                        ident: p.data().clone(),
                    }.into());
                } else {

                    let type_param_id = universe.new_type_param_id();
                    // Insert type parameter into scope
                    new_scope.insert_type_param(p.data().clone(), type_param_id);
                    // Insert type parameter into set
                    type_parameter_map.insert(p.data(), type_param_id);
                }
            }
        }

        None => (),
    }

    // Generate the constructor
    let scope = new_scope;
    let mut fields = HashMap::new();
    let mut field_map = HashMap::new();
    let mut order = Vec::new();
    if let Some(ref body) = struct_def.body.0 {
        for field in body.iter() {
            let f_id = universe.new_field_id();
            let f_name = field.name.data().clone();

            let field_type_annotation = field.field_type.data();

            // TODO: Insert type parameters into scope
            let field_type_app = type_app_from_annotation(universe,
                                                          &scope,
                                                          field_type_annotation)?;

            // Map field to type constructor
            fields.insert(f_id, field_type_app);

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

    let type_params = type_parameter_map
        .values()
        .map(|id| id.clone())
        .collect::<Vec<_>>();

    let type_params = if type_params.len() > 0 {
        Some(type_params)
    } else {
        None
    };

    let type_cons = TypeCons::Record {
        name: struct_def.name.data().clone(),
        fields: fields,
        field_map: field_map,
        type_params: type_params,
    };

    Ok((type_cons, order))
}
