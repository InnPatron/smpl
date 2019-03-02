use std::collections::HashMap;

use crate::ast::{
    AnonymousFn, BuiltinFnParams, BuiltinFunction, Function, Ident, Struct, TypeParams,
};
use crate::feature::*;

use super::error::{AnalysisError, TypeError};
use super::metadata::*;
use super::semantic_data::{FieldId, FnId, Program, ScopedData, TypeId, TypeParamId, Universe};
use super::type_cons::*;

fn type_param_map(
    universe: &Universe,
    type_params: Option<&TypeParams>,
    mut new_scope: ScopedData,
) -> Result<(ScopedData, HashMap<Ident, TypeParamId>), AnalysisError> {
    let mut type_parameter_map = HashMap::new();
    match type_params {
        Some(ref params) => {
            for p in params.params.iter() {
                // Check for type parameter naming conflict
                if type_parameter_map.contains_key(p.data()) {
                    // Naming conflict
                    return Err(TypeError::ParameterNamingConflict {
                        ident: p.data().clone(),
                    }
                    .into());
                } else {
                    let type_param_id = universe.new_type_param_id();
                    // Insert type parameter into scope
                    new_scope.insert_type_param(p.data().clone(), type_param_id);
                    // Insert type parameter into set
                    type_parameter_map.insert(p.data().clone(), type_param_id);
                }
            }
        }

        None => (),
    }

    Ok((new_scope, type_parameter_map))
}

// TODO: Store type constructors in Program
pub fn generate_struct_type_cons(
    program: &mut Program,
    type_id: TypeId,
    scope: &ScopedData,
    struct_def: &Struct,
) -> Result<(TypeCons, Vec<FieldId>), AnalysisError> {
    let (universe, _metadata, _features) = program.analysis_context();

    // Check no parameter naming conflicts
    let (scope, type_parameter_map) =
        type_param_map(universe, struct_def.type_params.as_ref(), scope.clone())?;

    // Generate the constructor
    let mut fields = HashMap::new();
    let mut field_map = HashMap::new();
    let mut order = Vec::new();
    if let Some(ref body) = struct_def.body.0 {
        for field in body.iter() {
            let f_id = universe.new_field_id();
            let f_name = field.name.data().clone();

            let field_type_annotation = field.field_type.data();

            // TODO: Insert type parameters into scope
            let field_type_app = type_app_from_annotation(universe, &scope, field_type_annotation)?;

            // Map field to type constructor
            fields.insert(f_id, field_type_app);

            if field_map.contains_key(&f_name) {
                return Err(TypeError::FieldNamingConflict {
                    ident: f_name.clone(),
                }
                .into());
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
        type_id: type_id,
        fields: fields,
        field_map: field_map,
        type_params: type_params,
    };

    Ok((type_cons, order))
}

pub fn generate_fn_type(
    program: &mut Program,
    scope: &ScopedData,
    fn_id: FnId,
    fn_def: &Function,
) -> Result<(ScopedData, TypeCons), AnalysisError> {

    let (universe, metadata, _features) = program.analysis_context();

    // Check no parameter naming conflicts
    let (scope, type_parameter_map) =
        type_param_map(universe, fn_def.type_params.as_ref(), scope.clone())?;

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let anno = anno.data();
            let type_app = type_app_from_annotation(universe, &scope, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::App {
            type_cons: universe.unit(),
            args: None,
        },
    };

    let params = match fn_def.params {
        Some(ref params) => {
            let mut typed_params = Vec::new();
            let mut param_metadata = Vec::new();
            for param in params.iter() {
                let param = param.data();
                let param_anno = param.param_type.data();

                let param_type = type_app_from_annotation(universe, &scope, param_anno)?;

                typed_params.push(param_type);

                param_metadata.push(FunctionParameter::new(
                    param.name.data().clone(),
                    universe.new_var_id(),
                ));

                // TODO: Function signature scanner?
            }

            metadata.insert_function_param_ids(fn_id, param_metadata);

            typed_params
        }
        None => {
            metadata.insert_function_param_ids(fn_id, Vec::with_capacity(0));
            Vec::with_capacity(0)
        }
    };

    let type_params = type_parameter_map
        .values()
        .map(|id| id.clone())
        .collect::<Vec<_>>();

    let type_params = if type_params.len() > 0 {
        Some(type_params)
    } else {
        None
    };

    let type_cons = TypeCons::Function {
        type_params: type_params,
        parameters: params,
        return_type: ret_type,
    };

    Ok((scope, type_cons))
}

pub fn generate_builtin_fn_type(
    program: &mut Program,
    scope: &ScopedData,
    fn_id: FnId,
    fn_def: &BuiltinFunction,
) -> Result<TypeCons, AnalysisError> {
    let (universe, metadata, features) = program.analysis_context();

    // Check no parameter naming conflicts
    let (scope, type_parameter_map) =
        type_param_map(universe, fn_def.type_params.as_ref(), scope.clone())?;

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let anno = anno.data();
            let type_app = type_app_from_annotation(universe, &scope, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::App {
            type_cons: universe.unit(),
            args: None,
        },
    };

    let params = match fn_def.params {
        BuiltinFnParams::Checked(ref params) => match *params {
            Some(ref params) => {
                let mut typed_params = Vec::new();
                let mut param_metadata = Vec::new();
                for param in params.iter() {
                    let param = param.data();
                    let param_anno = param.param_type.data();

                    let param_type = type_app_from_annotation(universe, &scope, param_anno)?;

                    typed_params.push(param_type);

                    param_metadata.push(FunctionParameter::new(
                        param.name.data().clone(),
                        universe.new_var_id(),
                    ));

                    // TODO: Function signature scanner?
                }

                metadata.insert_function_param_ids(fn_id, param_metadata);

                typed_params
            }
            None => {
                metadata.insert_function_param_ids(fn_id, Vec::with_capacity(0));
                Vec::with_capacity(0)
            }
        },

        BuiltinFnParams::Unchecked => {
            metadata.insert_unchecked_builtin_params(fn_id);
            features.add_feature(UNCHECKED_BUILTIN_FN_PARAMS);

            let type_params = type_parameter_map
                .values()
                .map(|id| id.clone())
                .collect::<Vec<_>>();

            let type_params = if type_params.len() > 0 {
                Some(type_params)
            } else {
                None
            };

            let type_cons = TypeCons::UncheckedFunction {
                type_params: type_params,
                return_type: ret_type,
            };

            return Ok(type_cons);
        }
    };

    let type_params = type_parameter_map
        .values()
        .map(|id| id.clone())
        .collect::<Vec<_>>();

    let type_params = if type_params.len() > 0 {
        Some(type_params)
    } else {
        None
    };

    let type_cons = TypeCons::Function {
        type_params: type_params,
        parameters: params,
        return_type: ret_type,
    };

    Ok(type_cons)
}

pub fn generate_anonymous_fn_type(
    program: &mut Program,
    scope: &ScopedData,
    fn_id: FnId,
    fn_def: &AnonymousFn,
) -> Result<(ScopedData, TypeCons), AnalysisError> {
    let (universe, metadata, _features) = program.analysis_context();

    // Check no parameter naming conflicts
    // TODO: Allow type parameters on anonymous functions?
    let scope = scope.clone();

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let anno = anno.data();
            let type_app = type_app_from_annotation(universe, &scope, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::App {
            type_cons: universe.unit(),
            args: None,
        },
    };

    let params = match fn_def.params {
        Some(ref params) => {
            let mut typed_params = Vec::new();
            let mut param_metadata = Vec::new();
            for param in params.iter() {
                let param = param.data();
                let param_anno = param.param_type.data();

                let param_type = type_app_from_annotation(universe, &scope, param_anno)?;

                typed_params.push(param_type);

                param_metadata.push(FunctionParameter::new(
                    param.name.data().clone(),
                    universe.new_var_id(),
                ));

                // TODO: Function signature scanner?
            }

            metadata.insert_function_param_ids(fn_id, param_metadata);

            typed_params
        }
        None => {
            metadata.insert_function_param_ids(fn_id, Vec::with_capacity(0));
            Vec::with_capacity(0)
        }
    };

    let type_params = None;

    let type_cons = TypeCons::Function {
        type_params: type_params,
        parameters: params,
        return_type: ret_type,
    };

    Ok((scope, type_cons))
}
