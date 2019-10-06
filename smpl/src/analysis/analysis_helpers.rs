use std::collections::HashMap;

use crate::feature::*;
use crate::ast::Function;
use crate::ast;

use super::error::{AnalysisError, TypeError};
use super::metadata::*;
use super::semantic_data::{FieldId, FnId, Program, TypeId, TypeParamId, TypeVarId, Universe};
use super::resolve_scope::ScopedData;
use super::type_checker::TypingContext;
use super::type_cons::*;

pub struct ContextData<'a> {
    type_params: Option<&'a ast::TypeParams>,
    params: Option<&'a [ast::AstNode<ast::FnParameter>]>,
    clear_variables: bool,
}

impl<'a> From<&'a ast::Function> for ContextData<'a> {

    fn from(ast_fn: &ast::Function) -> ContextData {
        ContextData {
            type_params: ast_fn.type_params.as_ref(),
            params: ast_fn.params.as_ref().map(|v| v.as_slice()),
            clear_variables: true,
        }
    }
}

impl<'a> From<&'a ast::AnonymousFn> for ContextData<'a> {

    fn from(ast_fn: &ast::AnonymousFn) -> ContextData {
        ContextData {
            type_params: None,
            params: ast_fn.params.as_ref().map(|v| v.as_slice()),
            clear_variables: true,
        }
    }
}

pub fn generate_fn_analysis_data<'a, 'b, 'c, 'd, 'e, T>(universe: &'a Universe,
    outer_scope: &'b ScopedData,
    outer_context: &'c TypingContext,
    fn_type_cons: &'d TypeCons,
    fn_def: &'e T)
    -> Result<(ScopedData, TypingContext), AnalysisError> 
    where &'e T: Into<ContextData<'e>> {

    let fn_def: ContextData = fn_def.into();

    let mut fn_scope = outer_scope.clone();
    let mut fn_context = outer_context.clone();

    if fn_def.clear_variables {
        fn_scope.clear_scoped_vars();
    }

    match fn_type_cons {

        TypeCons::Function {
            ref type_params,
            ref parameters,
            ..
        } => {
            
            // Map placeholder type variables to an existential type variable
            if let Some(ref tps) =  fn_def.type_params {
                for (param_name, (type_param_id, constraint)) in tps.params
                    .iter()
                    .zip(type_params.iter()) {

                    let existential_type_var = universe.new_type_var_id();
                    let placeholder_variable = type_params.placeholder_type_var(type_param_id);

                    fn_scope.insert_type_var(param_name.data().clone(), 
                        existential_type_var);

                    fn_context.type_vars
                        .insert(placeholder_variable, AbstractType::TypeVar(existential_type_var));
                    match constraint {
                        Some(width_constraint) => {
                            fn_context.type_vars
                                .insert(existential_type_var, 
                                    AbstractType::WidthConstraint(width_constraint.clone()));
                        }

                        None => {
                            fn_context.type_vars
                                .insert(existential_type_var, 
                                    AbstractType::TypeVar(existential_type_var));
                        }
                    }
                }
            }

            // Map formal parameters into scope and typing context
            if let Some(ref formal_params) = fn_def.params {
                for (formal_param, formal_param_type) in formal_params
                    .iter().zip(parameters.iter()) {

                    let formal_param_var_id = universe.new_var_id();
                    let formal_param_type = formal_param_type.apply(
                        universe,
                        &fn_scope,
                        &fn_context)?;

                    fn_scope.insert_var(formal_param.data().name.data().clone(),
                        formal_param_var_id);

                    fn_context.var_type_map
                        .insert(formal_param_var_id, formal_param_type);
                }

            }
            
        }

        _ => unreachable!("Only pass in a function type constructor"),
    }

    Ok((fn_scope, fn_context))
}
