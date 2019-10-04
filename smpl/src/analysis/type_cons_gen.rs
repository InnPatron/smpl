use std::collections::HashMap;

use crate::ast::{
    AnonymousFn, BuiltinFnParams, BuiltinFunction, Function, Ident, Struct, TypeParams as AstTypeParams, WhereClause
};
use crate::feature::*;

use super::error::{AnalysisError, TypeError};
use super::metadata::*;
use super::semantic_data::{FieldId, FnId, Program, TypeId, TypeParamId, TypeVarId, Universe};
use super::resolve_scope::ScopedData;
use super::type_checker::TypingContext;
use super::type_cons::*;

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
    let (type_params, _type_param_scope, _type_param_typing_context) = 
        type_param_map(universe, 
                   struct_def.type_params.as_ref(), 
                   struct_def.where_clause.as_ref(),
                   &scope,
                   &typing_context)?;

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

    let type_cons = TypeCons::Record {
        type_id: type_id,
        fields: fields,
        field_map: field_map,
        type_params: type_params,
    };

    Ok((type_cons, order))
}

pub fn generate_fn_type_cons(universe: &Universe,
    outer_scope: &ScopedData,
    outer_context: &TypingContext,
    fn_id: FnId,
    fn_def: &Function)
    -> Result<TypeCons, AnalysisError> {

    let (type_params, type_param_scope, type_param_typing_context) = 
        type_param_map(universe, 
           fn_def.type_params.as_ref(), 
           fn_def.where_clause.as_ref(),
           &outer_scope,
           &outer_context)?;

    let return_type = match fn_def.return_type {
        Some(ref ann) => {
            let ann = ann.data();

            type_from_ann(universe, &type_param_scope, &type_param_typing_context, ann)?
        }

        None => AbstractType::Unit,
    };

    let typed_formal_params = match fn_def.params {
        Some(ref params) => {
            let mut typed_formal_params = Vec::new();

            for param in params.iter() {
                let param = param.data();
                let param_ann = param.param_type.data();

                let param_type =
                    type_from_ann(universe, &type_param_scope, &type_param_typing_context, param_ann)?;

                typed_formal_params.push(param_type);
            }

            typed_formal_params
        },

        None => Vec::new(),
    };

    Ok(TypeCons::Function {
        type_params: type_params,
        parameters: typed_formal_params,
        return_type: return_type,
    })
}

pub fn generate_builtin_fn_type(
    program: &mut Program,
    outer_scope: &ScopedData,
    outer_typing_context: &TypingContext,
    fn_id: FnId,
    fn_def: &BuiltinFunction,
) -> Result<TypeCons, AnalysisError> {
    let (universe, metadata, features) = program.analysis_context();

    let mut fn_scope = outer_scope.clone();
    let mut fn_typing_context = outer_typing_context.clone();

    // Check no parameter naming conflicts
    let (type_params, type_param_scope, type_param_typing_context) = 
        type_param_map(universe, 
           fn_def.type_params.as_ref(), 
           fn_def.where_clause.as_ref(), 
           outer_scope,
           outer_typing_context)?;

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let anno = anno.data();
            let type_app = 
                type_from_ann(universe, &type_param_scope, &type_param_typing_context, anno)?;
            // TODO: Function signature scanner?
            type_app
        }
        None => AbstractType::Unit,
    };

    // TODO: Insert existential type variables representing the type parameters in any context 
    // Go through all type paremters in scope (type_param_scope.type_variables)
    //   and replace with new ones

    let params = match fn_def.params {
        BuiltinFnParams::Checked(ref params) => match *params {
            Some(ref params) => {
                let mut typed_params = Vec::new();
                let mut param_metadata = Vec::new();
                for param in params.iter() {
                    let param = param.data();
                    let param_anno = param.param_type.data();

                    let param_type = 
                        type_from_ann(universe, &type_param_scope, &type_param_typing_context, param_anno)?;

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

            let type_cons = TypeCons::UncheckedFunction {
                type_params: type_params,
                return_type: ret_type,
            };

            return Ok(type_cons);
        }
    };

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
    universe: &Universe,
    ast_type_params: Option<&AstTypeParams>,
    where_clause: Option<&WhereClause>,
    outer_scope: &ScopedData,
    outer_typing_context: &TypingContext,
) -> Result<(TypeParams, ScopedData, TypingContext), AnalysisError> {

    // Used only to map placeholder type variables
    let mut current_scope = outer_scope.clone();
    let mut typing_context = outer_typing_context.clone();

    let mut type_params = TypeParams::new();
    let mut internal_type_map: HashMap<_, (TypeParamId, TypeVarId)> = HashMap::new();

    // TODO: Add type parameters as reflexive self?

    if let Some(params) = ast_type_params {
        for p in params.params.iter() {
            // Check for type parameter naming conflict
            if internal_type_map.contains_key(p.data()) {
                // Naming conflict
                return Err(TypeError::ParameterNamingConflict {
                    ident: p.data().clone(),
                }
                .into());
            } else {
                let type_param_id = universe.new_type_param_id(); 
                let type_var_id = universe.new_type_var_id();
                // Insert type parameter into set
                internal_type_map.insert(p.data().clone(), (type_param_id, type_var_id));
                
                // TODO: What kind of recursion to support? Probably equirecursive
                // Add type parameters to typing context to allow recursive constraints
                typing_context.type_vars
                    .insert(type_var_id.clone(), AbstractType::Any);
            }
        }
    }

    if let Some(where_clause) = where_clause {
        for (ident, vec_ast_type_ann) in where_clause.0.iter() {

            // Remove from type_parameter_map
            match internal_type_map.remove(ident) {

                Some((type_param_id, type_var_id)) => {
                    if vec_ast_type_ann.len() > 1 {
                        // TODO: Allow multiple constraint declarations on one type param? 
                        // where A: { ... }
                        //       A: { ... }
                        // For now, disallow to simplify...

                        unimplemented!("Multiple declarations on one type param:{}", ident);
                    }

                    let ast_constraint = vec_ast_type_ann.get(0).unwrap();
                    let abstract_type = type_from_ann(universe, 
                        &current_scope, 
                        &typing_context,
                        ast_constraint.data())?;

                    // TypeVar already in TypingContext as TypeVar(self_id)
                    current_scope.insert_type_var(ident.clone(), type_var_id.clone());

                    if let AbstractType::WidthConstraint(constraint) = abstract_type {
                        let abstract_type = AbstractType::WidthConstraint(constraint);

                        // Insert type var into scope
                        typing_context.type_vars.insert(type_var_id.clone(), 
                            abstract_type);

                        // Add type parameter to type constructor
                        type_params.add_param(type_param_id.clone(), 
                            None, type_var_id.clone());
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
    for (ident, (type_param_id, type_var_id)) in internal_type_map.into_iter() {
        // TODO: Also insert into the typing env
        current_scope.insert_type_var(ident.clone(), type_var_id);
        typing_context.type_vars.insert(type_var_id.clone(), 
            AbstractType::Any);
        type_params.add_param(type_param_id.clone(), 
            None, type_var_id.clone());
    }

    Ok((type_params, current_scope, typing_context))
}
