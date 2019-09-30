use std::collections::HashMap;

use crate::ast::{
    AnonymousFn, BuiltinFnParams, BuiltinFunction, Function, Ident, Struct, TypeParams as AstTypeParams, WhereClause
};
use crate::feature::*;

use super::error::{AnalysisError, TypeError};
use super::metadata::*;
use super::semantic_data::{FieldId, FnId, Program, TypeId, TypeParamId, Universe};
use super::resolve_scope::ScopedData;
use super::type_checker::TypingContext;
use super::type_cons::*;

type TypeParamMap = HashMap<Ident, (TypeParamId, Option<AbstractWidthConstraint>)>;

// TODO: Store type constructors in Program
pub fn generate_struct_type_cons(
    program: &mut Program,
    type_id: TypeId,
    scope: &ScopedData,
    typing_context: &TypingContext,
    struct_def: &Struct,
) -> Result<(TypeCons, Vec<FieldId>), AnalysisError> {
    let (universe, _metadata, _features) = program.analysis_context();

    let mut scope = scope.clone();
    let mut typing_context = typing_context.clone();

    // Check no parameter naming conflicts
    type_param_map(universe, 
                   struct_def.type_params.as_ref(), 
                   struct_def.where_clause.as_ref(),
                   &mut scope,
                   &mut typing_context)?;

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
            let field_type_app = type_from_ann(
                universe, &scope, &typing_context, field_type_annotation)?;

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

    let type_params = make_type_params(&scope, &typing_context);

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
    typing_context: &TypingContext,
    fn_id: FnId,
    fn_def: &Function,
) -> Result<(ScopedData, TypingContext, TypeCons), AnalysisError> {

    let (universe, metadata, _features) = program.analysis_context();

    let mut scope = scope.clone();
    let mut typing_context = typing_context.clone();


    // Check no parameter naming conflicts
    type_param_map(universe, 
                   fn_def.type_params.as_ref(), 
                   fn_def.where_clause.as_ref(),
                   &mut scope,
                   &mut typing_context)?;

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let anno = anno.data();
            let type_app = type_from_ann(universe, &scope, &typing_context, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::Unit,
    };

    let params = match fn_def.params {
        Some(ref params) => {
            let mut typed_params = Vec::new();
            let mut param_metadata = Vec::new();
            for param in params.iter() {
                let param = param.data();
                let param_anno = param.param_type.data();

                let param_type = type_from_ann(universe, &scope, &typing_context, param_anno)?;

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

    let type_params = make_type_params(&scope, &typing_context);

    let type_cons = TypeCons::Function {
        type_params: type_params,
        parameters: params,
        return_type: ret_type,
    };

    Ok((scope, typing_context, type_cons))
}

pub fn generate_builtin_fn_type(
    program: &mut Program,
    scope: &ScopedData,
    typing_context: &TypingContext,
    fn_id: FnId,
    fn_def: &BuiltinFunction,
) -> Result<TypeCons, AnalysisError> {
    let (universe, metadata, features) = program.analysis_context();

    let mut scope = scope.clone();
    let mut typing_context = typing_context.clone();

    // Check no parameter naming conflicts
    type_param_map(universe, 
                   fn_def.type_params.as_ref(), 
                   fn_def.where_clause.as_ref(), 
                   &mut scope,
                   &mut typing_context)?;

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let anno = anno.data();
            let type_app = type_from_ann(universe, &scope, &typing_context, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::Unit,
    };

    let params = match fn_def.params {
        BuiltinFnParams::Checked(ref params) => match *params {
            Some(ref params) => {
                let mut typed_params = Vec::new();
                let mut param_metadata = Vec::new();
                for param in params.iter() {
                    let param = param.data();
                    let param_anno = param.param_type.data();

                    let param_type = type_from_ann(universe, &scope, &typing_context, param_anno)?;

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

            let type_params = make_type_params(&scope, &typing_context);

            let type_cons = TypeCons::UncheckedFunction {
                type_params: type_params,
                return_type: ret_type,
            };

            return Ok(type_cons);
        }
    };

    let type_params = make_type_params(&scope, &typing_context);

    let type_cons = TypeCons::Function {
        type_params: type_params,
        parameters: params,
        return_type: ret_type,
    };

    Ok(type_cons)
}

pub fn generate_anonymous_fn_type(
    universe: &Universe,
    scope: &ScopedData,
    typing_context: &TypingContext,
    fn_id: FnId,
    fn_def: &AnonymousFn,
) -> Result<(ScopedData, TypeCons), AnalysisError> {

    // Check no parameter naming conflicts
    // TODO: Allow type parameters on anonymous functions?
    let scope = scope.clone();

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let anno = anno.data();
            let type_app = type_from_ann(universe, &scope, typing_context, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::Unit,
    };

    let params = match fn_def.params {
        Some(ref params) => {
            let mut typed_params = Vec::new();
            let mut param_metadata = Vec::new();
            for param in params.iter() {
                let param = param.data();
                let param_anno = param.param_type.data();

                let param_type = type_from_ann(universe, &scope, typing_context, param_anno)?;

                typed_params.push(param_type);

                param_metadata.push(FunctionParameter::new(
                    param.name.data().clone(),
                    universe.new_var_id(),
                ));

                // TODO: Function signature scanner?
            }
            typed_params
        }
        None => {
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
    current_scope: &mut ScopedData,
    typing_context: &mut TypingContext,
) -> Result<(), AnalysisError> {

    let mut type_parameter_map = HashMap::new();

    // TODO: Add type parameters as reflexive self?

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
                
                // Add type parameters to typing context to allow recursive constraints
                typing_context.type_params
                    .insert(type_param_id, AbstractType::Param(type_param_id.clone()));
            }
        }
    }

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
                        current_scope, 
                        typing_context,
                        ast_constraint.data())?;

                    // TODO: Also insert into the typing env
                    current_scope.insert_type_param(ident.clone(), tp.clone());

                    if let AbstractType::WidthConstraint(constraint) = abstract_type {
                        // Insert type parameter into scope
                        typing_context.type_params.insert(tp.clone(), 
                            AbstractType::ConstrainedParam(tp.clone(), Box::new(abstract_type)));
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
        current_scope.insert_type_param(ident.clone(), id);
        typing_context.type_params.insert(id.clone(), AbstractType::Param(id.clone()));
    }

    Ok(())
}

fn make_type_params(scope: &ScopedData, typing_context: &TypingContext) -> TypeParams {

    let mut tp = TypeParams::new();
    for param_id in scope.type_params() {
        match typing_context.type_params.get(&param_id).unwrap() {
            AbstractType::Param(id) => {
                tp.add_param(id.clone(), None);
            }

            AbstractType::ConstrainedParam(ref id, ref constraint) => {
                match **constraint {
                    AbstractType::WidthConstraint(ref awc) => {
                        tp.add_param(id.clone(), Some(awc.clone()));
                    },

                    _ => unreachable!("At this stage, type parameters should only be Param(id) or WidthConstraint"),
                }
                
            }

            _ => unreachable!("Not a valid type for a type parameter"),

        }
    }
    unimplemented!();
}
