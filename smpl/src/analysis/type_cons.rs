use std::collections::HashMap;

use crate::ast::{AstNode, Ident, WidthConstraint};
use crate::span::Span;

use super::abstract_type::*;
use super::error::{AnalysisError, ApplicationError, TypeError as ATypeError};
use super::semantic_data::{FieldId, TypeId, TypeParamId, TypeVarId, Universe};

macro_rules! nill_check {
    ($type_args: expr) => {{
        if $type_args.is_some() {
            // TODO: error on type args to type cons int, bool, etc
            unimplemented!()
        }
    }};
}

/// Use TypeCons and AbstractType for type constructor mapping and graphing
#[derive(Debug, Clone)]
pub enum TypeCons {
    UncheckedFunction {
        type_params: TypeParams,
        return_type: AbstractType,
    },

    Function {
        type_params: TypeParams,
        parameters: Vec<AbstractType>,
        return_type: AbstractType,
    },

    Record {
        type_id: TypeId,
        type_params: TypeParams,
        fields: HashMap<FieldId, AbstractType>,
        field_map: HashMap<Ident, FieldId>,
    },

    Opaque {
        type_id: TypeId,
        type_params: TypeParams,
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

    pub fn type_params(&self) -> Option<&TypeParams> {
        match *self {
            TypeCons::Function {
                ref type_params, ..
            } => Some(type_params),

            TypeCons::Record {
                ref type_params, ..
            } => Some(type_params),

            TypeCons::UncheckedFunction {
                ref type_params, ..
            } => Some(type_params),

            TypeCons::Opaque {
                ref type_params, ..
            } => Some(type_params),

            TypeCons::Int
            | TypeCons::Float
            | TypeCons::String
            | TypeCons::Bool
            | TypeCons::Unit => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeParams {
    params: Vec<(TypeParamId, AbstractType)>,
    placeholder_variables: HashMap<TypeParamId, TypeVarId>,
}

impl TypeParams {
    pub fn new() -> TypeParams {
        TypeParams {
            params: Vec::new(),
            placeholder_variables: HashMap::new(),
        }
    }

    pub fn empty() -> TypeParams {
        TypeParams::new()
    }

    pub fn add_param(
        &mut self,
        param: TypeParamId,
        constraint: Option<AbstractWidthConstraint>,
        placeholder_var: TypeVarId,
        var_span: Span,
    ) {
        let constraint = constraint
            .map(|awc| AbstractType::WidthConstraint {
                data: var_span.clone(),
                width: awc,
            })
            .unwrap_or(AbstractType::Any(var_span));

        self.params.push((param, constraint));
        self.placeholder_variables.insert(param, placeholder_var);
    }

    pub fn len(&self) -> usize {
        self.params.len()
    }

    // Guarenteed to be AbstractType::WidthConstraint or AbstractType::Any
    pub fn iter(&self) -> impl Iterator<Item = (TypeParamId, &AbstractType)> {
        self.params.iter().map(|(type_param_id, constraint)| {
            (type_param_id.clone(), constraint)
        })
    }

    pub fn placeholder_type_var(&self, id: TypeParamId) -> TypeVarId {
        self.placeholder_variables.get(&id).unwrap().clone()
    }
}
