use std::collections::HashMap;

use crate::ast::{
    AnonymousFn, BuiltinFnParams, BuiltinFunction, Function, Opaque, Struct,
    TypeParams as AstTypeParams, WhereClause,
};
use crate::feature::*;
use crate::span::Span;

use super::error::{AnalysisError, TypeError};
use super::metadata::*;
use super::resolve_scope::ScopedData;
use super::semantic_data::{
    FieldId, FnId, Program, TypeId, TypeParamId, TypeVarId,
};
use super::type_checker::TypingContext;
use super::type_cons::*;
use super::abstract_type::*;
use super::analysis_context::{ GlobalData, AnalysisUniverse };

// TODO: Store type constructors in Program
pub fn generate_struct_type_cons(
    universe: &AnalysisUniverse,
    global_data: &mut GlobalData,
    type_id: TypeId,
    scope: &ScopedData,
    typing_context: &TypingContext,
    struct_def: &Struct,
) -> Result<(TypeCons, Vec<FieldId>), AnalysisError> {

    // Check no parameter naming conflicts
    let (type_params, type_param_scope, type_param_typing_context) =
        type_param_map(
            universe,
            global_data,
            struct_def.type_params.as_ref(),
            struct_def.where_clause.as_ref(),
            &scope,
            &typing_context,
        )?;

    // Generate the constructor
    let mut fields = HashMap::new();
    let mut field_map = HashMap::new();
    let mut order = Vec::new();
    if let Some(ref body) = struct_def.body.0 {
        for field in body.iter() {
            let f_id = global_data.new_field_id();
            let f_name = field.name.data().clone();

            let field_type_annotation = &field.field_type;

            // TODO: Insert type parameters into scope
            let field_type_app = type_from_ann(
                &type_param_scope,
                &type_param_typing_context,
                field_type_annotation,
            )?;

            // Map field to type constructor
            fields.insert(f_id, field_type_app);

            if field_map.contains_key(&f_name) {
                return Err(TypeError::FieldNamingConflict {
                    ident: f_name.clone(),
                    span: field.name.span(),
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

pub fn generate_fn_type_cons(
    universe: &AnalysisUniverse,
    metadata: &mut Metadata,
    global_data: &mut GlobalData,
    outer_scope: &ScopedData,
    outer_context: &TypingContext,
    fn_id: FnId,
    fn_def: &Function,
) -> Result<TypeCons, AnalysisError> {
    let (type_params, type_param_scope, type_param_typing_context) =
        type_param_map(
            universe,
            global_data,
            fn_def.type_params.as_ref(),
            fn_def.where_clause.as_ref(),
            &outer_scope,
            &outer_context,
        )?;

    let return_type = match fn_def.return_type {
        Some(ref ann) => {

            type_from_ann(
                &type_param_scope,
                &type_param_typing_context,
                ann,
            )?
        }

        None => AbstractType::Unit(fn_def.name.span()),
    };

    let typed_formal_params = match fn_def.params {
        Some(ref params) => {
            let mut typed_formal_params = Vec::new();

            for param in params.iter() {
                let param = param.data();
                let param_ann = &param.param_type;

                let param_type = type_from_ann(
                    &type_param_scope,
                    &type_param_typing_context,
                    param_ann,
                )?;

                typed_formal_params.push(param_type);

                // TODO: Insert param type metadata somewhere?
                //   Probably in analyze_fn() when mapping paramaters into scope
                //   ~770
            }

            typed_formal_params
        }

        None => Vec::new(),
    };

    Ok(TypeCons::Function {
        type_params: type_params,
        parameters: typed_formal_params,
        return_type: return_type,
    })
}

pub fn generate_builtin_fn_type(
    universe: &AnalysisUniverse,
    metadata: &mut Metadata,
    features: &mut PresentFeatures,
    global_data: &mut GlobalData,
    outer_scope: &ScopedData,
    outer_typing_context: &TypingContext,
    fn_id: FnId,
    fn_def: &BuiltinFunction,
) -> Result<TypeCons, AnalysisError> {

    let _fn_scope = outer_scope.clone();
    let _fn_typing_context = outer_typing_context.clone();

    // Check no parameter naming conflicts
    let (type_params, type_param_scope, type_param_typing_context) =
        type_param_map(
            universe,
            global_data,
            fn_def.type_params.as_ref(),
            fn_def.where_clause.as_ref(),
            outer_scope,
            outer_typing_context,
        )?;

    let ret_type = match fn_def.return_type {
        Some(ref anno) => {
            let type_app = type_from_ann(
                &type_param_scope,
                &type_param_typing_context,
                anno,
            )?;
            // TODO: Function signature scanner?
            type_app
        }

        None => AbstractType::Unit(fn_def.name.span()),
    };

    // TODO: Insert existential type variables representing the type parameters in any context
    // Go through all type paremters in scope (type_param_scope.type_variables)
    //   and replace with new ones

    let params = match fn_def.params {
        BuiltinFnParams::Checked(ref params) => match *params {
            Some(ref params) => {
                let mut typed_params = Vec::new();
                for param in params.iter() {
                    let param = param.data();
                    let param_anno = &param.param_type;

                    let param_type = type_from_ann(
                        &type_param_scope,
                        &type_param_typing_context,
                        param_anno,
                    )?;

                    typed_params.push(param_type);

                    // TODO: Insert param type metadata somewhere?


                    // TODO: Function signature scanner?
                }

                typed_params
            }
            None => {
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
    universe: &AnalysisUniverse,
    metadata: &mut Metadata,
    global_data: &mut GlobalData,
    outer_scope: &ScopedData,
    outer_context: &TypingContext,
    fn_id: FnId,
    fn_def: &AnonymousFn,
) -> Result<TypeCons, AnalysisError> {
    let (type_params, type_param_scope, type_param_typing_context) =
        type_param_map(universe, global_data, None, None, &outer_scope, &outer_context)?;

    let return_type = match fn_def.return_type {
        Some(ref ann) => {

            type_from_ann(
                &type_param_scope,
                &type_param_typing_context,
                ann,
            )?
        }

        None => AbstractType::Unit(fn_def.body.span()),
    };

    let typed_formal_params = match fn_def.params {
        Some(ref params) => {
            let mut typed_formal_params = Vec::new();

            for param in params.iter() {
                let param = param.data();
                let param_ann = &param.param_type;

                let param_type = type_from_ann(
                    &type_param_scope,
                    &type_param_typing_context,
                    param_ann,
                )?;

                typed_formal_params.push(param_type);

                // TODO: Insert param type metadata somewhere?
            }

            typed_formal_params
        }

        None => Vec::new(),
    };

    Ok(TypeCons::Function {
        type_params: type_params,
        parameters: typed_formal_params,
        return_type: return_type,
    })
}

fn type_param_map(
    universe: &AnalysisUniverse,
    global_data: &mut GlobalData,
    ast_type_params: Option<&AstTypeParams>,
    where_clause: Option<&WhereClause>,
    outer_scope: &ScopedData,
    outer_typing_context: &TypingContext,
) -> Result<(TypeParams, ScopedData, TypingContext), AnalysisError> {
    // Used only to map placeholder type variables
    let mut current_scope = outer_scope.clone();
    let mut typing_context = outer_typing_context.clone();

    let mut internal_type_map: HashMap<_, (Span, TypeParamId, TypeVarId)> =
        HashMap::new();
    let mut type_param_order = Vec::new();

    // TODO: Add type parameters as reflexive self?

    if let Some(params) = ast_type_params {
        for p in params.params.iter() {
            // Check for type parameter naming conflict
            if internal_type_map.contains_key(p.data()) {
                // Naming conflict
                return Err(TypeError::ParameterNamingConflict {
                    ident: p.data().clone(),
                    span: p.span(),
                }
                .into());
            } else {
                let type_param_id = global_data.new_type_param_id();
                let type_var_id = global_data.new_type_var_id();
                // Insert type parameter into set
                internal_type_map
                    .insert(p.data().clone(), (p.span(), type_param_id, type_var_id));

                // TODO: What kind of recursion to support? Probably equirecursive
                // Add type parameters to typing context to allow recursive constraints
                typing_context
                    .type_vars
                    .insert(type_var_id.clone(), AbstractType::Any(p.span()));
                type_param_order.push(type_param_id);
            }
        }
    }

    let mut finished: HashMap<TypeParamId, (Option<_>, _, Span)> = HashMap::new();
    if let Some(where_clause) = where_clause {
        for (ident, vec_ast_type_ann) in where_clause.0.iter() {
            // Remove from type_parameter_map
            match internal_type_map.remove(ident.data()) {
                Some((param_span, type_param_id, type_var_id)) => {
                    if vec_ast_type_ann.len() > 1 {
                        // TODO: Allow multiple constraint declarations on one type param?
                        // where A: { ... }
                        //       A: { ... }
                        // For now, disallow to simplify...

                        unimplemented!(
                            "Multiple declarations on one type param:{}",
                            ident.data()
                        );
                    }

                    let ast_constraint = vec_ast_type_ann.get(0).unwrap();
                    let abstract_type = type_from_ann(
                        &current_scope,
                        &typing_context,
                        ast_constraint,
                    )?;

                    // TypeVar already in TypingContext as TypeVar(self_id)
                    current_scope
                        .insert_type_var(ident.data().clone(), type_var_id.clone());

                    if let AbstractType::WidthConstraint {
                        data: span,
                        width: constraint
                    } = abstract_type {
                        let abstract_type =
                            AbstractType::WidthConstraint {
                                data: span,
                                width: constraint.clone(),
                            };

                        // Insert type var into scope
                        typing_context
                            .type_vars
                            .insert(type_var_id.clone(), abstract_type);

                        finished.insert(
                            type_param_id.clone(),
                            (Some(constraint), type_var_id.clone(), param_span),
                        );
                    } else {
                        // TODO: found non-constraint in constraint position
                        unimplemented!(
                            "found non-constraint in constraint position"
                        );
                    }
                }

                None => {
                    // TODO: where clause with unknown TP
                    return Err(TypeError::UnknownTypeParameter {
                        ident: ident.data().clone(),
                        span: ident.span(),
                    }
                    .into());
                }
            }
        }
    }

    // Any type param still left in type_parameter_map has no constraint
    for (ident, (param_span, type_param_id, type_var_id)) in internal_type_map.into_iter() {
        // TODO: Also insert into the typing env
        current_scope.insert_type_var(ident.clone(), type_var_id);
        typing_context
            .type_vars
            .insert(type_var_id.clone(), AbstractType::Any(param_span.clone()));

        finished.insert(type_param_id.clone(), (None, type_var_id.clone(), param_span));
    }

    let mut type_params = TypeParams::new();
    // NEED TO PRESERVE ORDER
    for param_id in type_param_order {
        let (opt, ty, param_span) = finished.remove(&param_id).unwrap();
        type_params.add_param(param_id.clone(), opt, ty, param_span);
    }

    Ok((type_params, current_scope, typing_context))
}

pub fn generate_opaque_type_cons(
    universe: &AnalysisUniverse,
    global_data: &mut GlobalData,
    type_id: TypeId,
    scope: &ScopedData,
    typing_context: &TypingContext,
    opaque_def: &Opaque,
) -> Result<TypeCons, AnalysisError> {

    // Check no parameter naming conflicts
    let (type_params, _type_param_scope, _type_param_typing_context) =
        type_param_map(
            universe,
            global_data,
            opaque_def.type_params.as_ref(),
            opaque_def.where_clause.as_ref(),
            &scope,
            &typing_context,
        )?;

    let type_cons = TypeCons::Opaque {
        type_id: type_id,
        type_params: type_params,
    };

    Ok(type_cons)
}
