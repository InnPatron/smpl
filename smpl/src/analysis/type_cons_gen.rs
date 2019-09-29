use std::collections::HashMap;

use crate::ast::{
    AnonymousFn, BuiltinFnParams, BuiltinFunction, Function, Ident, Struct, TypeParams as AstTypeParams, WhereClause
};
use crate::feature::*;

use super::error::{AnalysisError, TypeError};
use super::metadata::*;
use super::semantic_data::{FieldId, FnId, Program, TypeId, TypeParamId, Universe};
use super::resolve_scope::ScopedData;
use super::type_cons::*;

type TypeParamMap = HashMap<Ident, (TypeParamId, Option<AbstractWidthConstraint>)>;

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
        type_param_map(universe, 
                       struct_def.type_params.as_ref(), 
                       struct_def.where_clause.as_ref(),
                       scope,
                       scope.clone())?;

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
            let field_type_app = type_from_ann(universe, &scope, field_type_annotation)?;

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

    let type_params = type_params_from_param_map(type_parameter_map);

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
        type_param_map(universe, 
                       fn_def.type_params.as_ref(), 
                       fn_def.where_clause.as_ref(),
                       scope,
                       scope.clone())?;

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let anno = anno.data();
            let type_app = type_from_ann(universe, &scope, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::App {
            type_cons: universe.unit(),
            args: Vec::new(),
        },
    };

    let params = match fn_def.params {
        Some(ref params) => {
            let mut typed_params = Vec::new();
            let mut param_metadata = Vec::new();
            for param in params.iter() {
                let param = param.data();
                let param_anno = param.param_type.data();

                let param_type = type_from_ann(universe, &scope, param_anno)?;

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

    let type_params = type_params_from_param_map(type_parameter_map);

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
        type_param_map(universe, 
                       fn_def.type_params.as_ref(), 
                       fn_def.where_clause.as_ref(), 
                       scope,
                       scope.clone())?;

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let anno = anno.data();
            let type_app = type_from_ann(universe, &scope, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::App {
            type_cons: universe.unit(),
            args: Vec::new(),
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

                    let param_type = type_from_ann(universe, &scope, param_anno)?;

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

            let type_params = type_params_from_param_map(type_parameter_map);

            let type_cons = TypeCons::UncheckedFunction {
                type_params: type_params,
                return_type: ret_type,
            };

            return Ok(type_cons);
        }
    };

    let type_params = type_params_from_param_map(type_parameter_map);

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
            let type_app = type_from_ann(universe, &scope, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::App {
            type_cons: universe.unit(),
            args: Vec::new(),
        },
    };

    let params = match fn_def.params {
        Some(ref params) => {
            let mut typed_params = Vec::new();
            let mut param_metadata = Vec::new();
            for param in params.iter() {
                let param = param.data();
                let param_anno = param.param_type.data();

                let param_type = type_from_ann(universe, &scope, param_anno)?;

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

    // TODO: Type parameters on anonymous functions?
    let type_params = TypeParams::empty();

    let type_cons = TypeCons::Function {
        type_params: type_params,
        parameters: params,
        return_type: ret_type,
    };

    Ok((scope, type_cons))
}

fn type_param_map(
    universe: &mut Universe,
    type_params: Option<&AstTypeParams>,
    where_clause: Option<&WhereClause>,
    current_scope: &ScopedData,
    mut new_scope: ScopedData,
) -> Result<(ScopedData, TypeParamMap), AnalysisError> {
    let mut type_parameter_map = HashMap::new();

    if let Some(params) = type_params {
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
                // Insert type parameter into set
                type_parameter_map.insert(p.data().clone(), type_param_id);
            }
        }
    }

    let mut constraint_map = HashMap::new();
    if let Some(where_clause) = where_clause {
        for (ident, vec_ast_type_ann) in where_clause.0.iter() {

            // Remove from type_parameter_map
            match type_parameter_map.remove(ident) {

                Some(tp) => {
                    if vec_ast_type_ann.len() > 1 {
                        // TODO: Allow multiple constraint declarations on one type param? 
                        // where A: { ... }
                        //       A: { ... }
                        // For now, disallow to simplify...

                        unimplemented!("Multiple declarations on one type param:{}", ident);
                    }

                    let ast_constraint = vec_ast_type_ann.get(0).unwrap();
                    let abstract_type = type_from_ann(universe, 
                                                              &new_scope, 
                                                              ast_constraint.data())?;

                    // TODO: Also insert into the typing env
                    new_scope.insert_type_param(ident.clone(), tp.clone());

                    if let AbstractType::WidthConstraint(constraint) = abstract_type {
                        // Insert type parameter into scope
                        constraint_map.insert(ident.clone(), (tp.clone(), Some(constraint)));
                    } else {
                        // TODO: found non-constraint in constraint position
                        unimplemented!("found non-constraint in constraint position");
                    }

                },

                None => {
                    // TODO: where clause with unknown TP
                    return Err(TypeError::UnknownTypeParameter {
                        ident: ident.clone()
                    }.into());
                }
            }
        }
    }

    // Any type param still left in type_parameter_map has no constraint
    for (ident, id) in type_parameter_map.into_iter() {
        // TODO: Also insert into the typing env
        new_scope.insert_type_param(ident.clone(), id);
        constraint_map.insert(ident, (id.clone(), None));
    }

    Ok((new_scope, constraint_map))
}

fn type_params_from_param_map(map: TypeParamMap) -> TypeParams {
    let mut type_params = TypeParams::new();
    for (_ident, (id, constraint)) in map.into_iter() {
        type_params.add_param(id, constraint);
    }

    type_params
}
