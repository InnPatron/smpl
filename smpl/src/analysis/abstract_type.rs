use std::collections::HashMap;

use crate::ast::{AstNode, Ident, TypeAnnotationRef, WidthConstraint};
use crate::span::Span;

use super::error::{AnalysisError, ApplicationError, TypeError as ATypeError};
use super::resolve_scope::ScopedData;
use super::semantic_data::{FieldId, TypeId, TypeParamId, TypeVarId, Universe};
use super::type_checker::TypingContext;
use super::type_resolver::resolve_types_static;
use super::type_cons::{TypeCons, TypeParams};

#[derive(Debug, Clone)]
pub enum AbstractType {
    Any(Span),

    Record {
        span: Span,
        type_id: TypeId,
        abstract_field_map: AbstractFieldMap,
    },

    App {
        span: Span,
        type_cons: TypeId,
        args: Vec<AbstractType>,
    },

    Array {
        span: Span,
        element_type: Box<AbstractType>,
        size: u64,
    },

    UncheckedFunction {
        span: Span,
        return_type: Box<AbstractType>,
    },

    Function {
        span: Span,
        parameters: Vec<AbstractType>,
        return_type: Box<AbstractType>,
    },

    WidthConstraint {
        span: Span, 
        width: AbstractWidthConstraint
    },

    Opaque {
        span: Span, 
        type_id: TypeId,
        args: Vec<AbstractType>,
    },

    TypeVar(TypeVarId),

    Int,
    Float,
    String,
    Bool,
    Unit,
}

impl AbstractType {
    pub fn type_cons(&self) -> Option<TypeId> {
        match *self {
            AbstractType::App {
                type_cons: ref tc, ..
            } => Some(tc.clone()),

            _ => None,
        }
    }

    pub fn substitute_with(
        &self,
        universe: &Universe,
        scoped_data: &ScopedData,
        typing_context: &TypingContext,
        map: &HashMap<TypeVarId, AbstractType>,
    ) -> Result<AbstractType, Vec<ATypeError>> {
        self.substitute_internal(universe, scoped_data, typing_context, map)
    }

    pub fn substitute(
        &self,
        universe: &Universe,
        scoped_data: &ScopedData,
        typing_context: &TypingContext,
    ) -> Result<AbstractType, Vec<ATypeError>> {
        match self {
            // TODO: Need to pass this span somewhere?
            AbstractType::App {
                span: ref _span,
                type_cons: _,
                args: _,
            } => {
                let no_sub_map = HashMap::new();
                self.substitute_internal(
                    universe,
                    scoped_data,
                    typing_context,
                    &no_sub_map,
                )
            }

            t => Ok(t.clone()),
        }
    }

    ///
    /// Takes a type constructor and applies its given arguments.
    ///
    /// Arguments are in the form of a map
    ///   from (placholder type variables) -> (type values)
    ///
    /// Assume all constraints are already met
    ///
    fn apply_internal(
        type_cons: &TypeCons,
        universe: &Universe,
        scoped_data: &ScopedData,
        typing_context: &TypingContext,
        map: &HashMap<TypeVarId, AbstractType>,
    ) -> Result<AbstractType, Vec<ATypeError>> {
        match type_cons {
            // TODO: Get span of declaration
            TypeCons::UncheckedFunction {
                ref return_type, ..
            } => Ok(AbstractType::UncheckedFunction {
                span: Span::dummy(),
                return_type: Box::new(return_type.substitute_internal(
                    universe,
                    scoped_data,
                    typing_context,
                    &map,
                )?),
            }),

            // TODO: Get span of declaration
            TypeCons::Function {
                ref parameters,
                ref return_type,
                ..
            } => Ok(AbstractType::Function {
                span: Span::dummy(),
                parameters: parameters
                    .iter()
                    .map(|p| {
                        p.substitute_internal(
                            universe,
                            scoped_data,
                            typing_context,
                            &map,
                        )
                    })
                    .collect::<Result<_, _>>()?,
                return_type: Box::new(return_type.substitute_internal(
                    universe,
                    scoped_data,
                    typing_context,
                    &map,
                )?),
            }),

            TypeCons::Record {
                ref type_id,
                type_params: _,
                ref fields,
                ref field_map,
                ..
            } => {
                let mut subbed_fields: HashMap<FieldId, AbstractType> =
                    HashMap::new();

                for (id, ty) in fields.iter() {
                    subbed_fields.insert(
                        id.clone(),
                        ty.substitute_internal(
                            universe,
                            scoped_data,
                            typing_context,
                            &map,
                        )?,
                    );
                }

                // TODO: Pass in AST span
                Ok(AbstractType::Record {
                    span: Span::dummy(),
                    type_id: type_id.clone(),
                    abstract_field_map: AbstractFieldMap {
                        fields: subbed_fields,
                        field_map: field_map.clone(),
                    },
                })
            }

            TypeCons::Opaque {
                type_id,
                ref type_params,
            } => {
                let ok_args = type_params
                    .iter()
                    .map(|(param_id, _)| {
                        map.get(&type_params.placeholder_type_var(param_id))
                            .expect("Missing placeholder type var in map")
                            .clone()
                    })
                    .collect();

                // TODO: Pass in AST span
                Ok(AbstractType::Opaque {
                    span: Span::dummy(),
                    type_id: type_id.clone(),
                    args: ok_args,
                })
            }

            TypeCons::Int => Ok(AbstractType::Int),
            TypeCons::Float => Ok(AbstractType::Float),
            TypeCons::Bool => Ok(AbstractType::Bool),
            TypeCons::String => Ok(AbstractType::String),
            TypeCons::Unit => Ok(AbstractType::Unit),
        }
    }

    /// Given a map of type variables to abstract types, recursively substitute
    ///   any type variables in the map with their abstract type.
    ///
    /// Only relies on scope/typing context for constraint resolution. They are NOT used for
    ///   substitution.
    ///
    /// Substitution literally replaces whatever the type variable with its mapped element
    ///  (or itself if it is not in the map)
    ///
    /// No AbstractType returned by substitute_internal() should have AbstractType::App() in its tree
    ///
    /// Type variable constraints are checked.
    ///
    fn substitute_internal(
        &self,
        universe: &Universe,
        scoped_data: &ScopedData,
        typing_context: &TypingContext,
        map: &HashMap<TypeVarId, AbstractType>,
    ) -> Result<AbstractType, Vec<ATypeError>> {
        match *self {
            // TODO: Use this span somewhere
            AbstractType::App {
                span: ref _span,
                type_cons: ref type_cons_id,
                args: ref type_args,
            } => {
                let (ok_args, err) = type_args
                    .iter()
                    .map(|at| {
                        at.substitute_internal(
                            universe,
                            scoped_data,
                            typing_context,
                            map,
                        )
                    })
                    .fold(
                        (Vec::new(), Vec::new()),
                        |(mut ok, mut err), apply_result| {
                            match apply_result {
                                Ok(app) => ok.push(app),
                                Err(mut e) => err.append(&mut e),
                            }

                            (ok, err)
                        },
                    );

                if err.len() != 0 {
                    return Err(err);
                }

                let type_cons = universe.get_type_cons(*type_cons_id);

                // Check if args match constraints
                if let Some(type_params) = type_cons.type_params() {
                    if ok_args.len() != type_params.len() {
                        return Err(vec![ATypeError::ApplicationError(
                            ApplicationError::Arity {
                                expected: type_params.len(),
                                found: ok_args.len(),
                            },
                        )]);
                    }

                    // Need to substitute all args into constraints
                    let (constraints, constraint_sub_map) = {
                        let mut constraints = Vec::new();
                        let mut constraint_errors = Vec::new();
                        let mut constraint_sub_map = HashMap::new();

                        // Build substitution map for constraints
                        for ((type_param_id, _), arg_type) in
                            type_params.iter().zip(ok_args.iter())
                        {
                            let placeholder_type_var =
                                type_params.placeholder_type_var(type_param_id);

                            constraint_sub_map
                                .insert(placeholder_type_var, arg_type.clone());
                        }

                        // Substitute constraint
                        for (_, constraint) in type_params.iter() {
                            let constraint = constraint.substitute_internal(
                                universe,
                                scoped_data,
                                typing_context,
                                &constraint_sub_map,
                            );

                            match constraint {
                                Ok(c) => constraints.push(c),
                                Err(mut e) => constraint_errors.append(&mut e),
                            }
                        }

                        if constraint_errors.len() != 0 {
                            return Err(constraint_errors);
                        } else {
                            (constraints, constraint_sub_map)
                        }
                    };

                    let mut arg_constraint_errors = Vec::new();
                    for (arg_type, constraint) in
                        ok_args.iter().zip(constraints.iter())
                    {
                        // TODO: Pass the correct Span
                        match super::type_resolver::resolve_types_static(
                            universe,
                            scoped_data,
                            typing_context,
                            arg_type,
                            &constraint,
                            crate::span::Span::dummy(),
                        ) {
                            Ok(_) => (),

                            Err(e) => arg_constraint_errors.push(e),
                        }
                    }

                    if arg_constraint_errors.len() != 0 {
                        return Err(arg_constraint_errors);
                    }

                    AbstractType::apply_internal(
                        type_cons,
                        universe,
                        scoped_data,
                        typing_context,
                        &constraint_sub_map,
                    )
                } else if ok_args.len() != 0 {
                    return Err(vec![ATypeError::ApplicationError(
                        ApplicationError::Arity {
                            expected: 0,
                            found: ok_args.len(),
                        },
                    )]);
                } else {
                    AbstractType::apply_internal(
                        type_cons,
                        universe,
                        scoped_data,
                        typing_context,
                        &HashMap::new(),
                    )
                }
            }

            AbstractType::Record {
                span: ref record_span,
                ref type_id,
                ref abstract_field_map,
            } => {
                let (ok_fields, err) = abstract_field_map
                    .fields
                    .iter()
                    .map(|(f_id, ty)| {
                        (
                            f_id.clone(),
                            ty.substitute_internal(
                                universe,
                                scoped_data,
                                typing_context,
                                map,
                            ),
                        )
                    })
                    .fold(
                        (HashMap::new(), Vec::new()),
                        |(mut ok, mut err), (f_id, result)| {
                            match result {
                                Ok(app) => {
                                    ok.insert(f_id, app);
                                }
                                Err(mut e) => err.append(&mut e),
                            }

                            (ok, err)
                        },
                    );

                if err.len() != 0 {
                    return Err(err);
                }

                Ok(AbstractType::Record {
                    span: record_span.clone(),
                    type_id: *type_id,
                    abstract_field_map: AbstractFieldMap {
                        fields: ok_fields,
                        field_map: abstract_field_map.field_map.clone(),
                    },
                })
            }

            AbstractType::Array {
                span: ref array_ty_span,
                ref element_type,
                ref size,
            } => Ok(AbstractType::Array {
                span: array_ty_span.clone(),
                element_type: Box::new(element_type.substitute_internal(
                    universe,
                    scoped_data,
                    typing_context,
                    map,
                )?),
                size: *size,
            }),

            AbstractType::Function {
                span: ref fn_ty_span,
                ref parameters,
                ref return_type,
            } => {
                let (ok_parameters, errors) = parameters
                    .iter()
                    .map(|p| {
                        p.substitute_internal(
                            universe,
                            scoped_data,
                            typing_context,
                            map,
                        )
                    })
                    .fold(
                        (Vec::new(), Vec::new()),
                        |(mut ok, mut err), result| {
                            match result {
                                Ok(app) => ok.push(app),
                                Err(mut e) => err.append(&mut e),
                            }

                            (ok, err)
                        },
                    );

                if errors.len() != 0 {
                    return Err(errors);
                }

                let new_return = return_type.substitute_internal(
                    universe,
                    scoped_data,
                    typing_context,
                    map,
                )?;

                Ok(AbstractType::Function {
                    span: fn_ty_span.clone(),
                    parameters: ok_parameters,
                    return_type: Box::new(new_return),
                })
            }

            AbstractType::UncheckedFunction { 
                ref span,
                ref return_type, 
            } => {
                let new_return = return_type.substitute_internal(
                    universe,
                    scoped_data,
                    typing_context,
                    map,
                )?;

                Ok(AbstractType::UncheckedFunction {
                    span: span.clone(),
                    return_type: Box::new(new_return),
                })
            }

            AbstractType::WidthConstraint {
                ref span,
                width: ref width_constraint
            } => {
                let (ok_field_types, errors) = width_constraint
                    .fields
                    .iter()
                    .map(|(ident, at)| {
                        at.substitute_internal(
                            universe,
                            scoped_data,
                            typing_context,
                            map,
                        )
                        .map(|r| (ident, r))
                    })
                    .fold(
                        (HashMap::new(), Vec::new()),
                        |(mut ok, mut err), apply_result| {
                            match apply_result {
                                Ok((ref_ident, at)) => {
                                    ok.insert(ref_ident.clone(), at);
                                }
                                Err(mut e) => err.append(&mut e),
                            };

                            (ok, err)
                        },
                    );

                if errors.len() != 0 {
                    return Err(errors);
                }

                let new_width = AbstractWidthConstraint {
                    fields: ok_field_types,
                };

                Ok(AbstractType::WidthConstraint {
                    span: span.clone(),
                    width: new_width,
                })
            }

            AbstractType::TypeVar(ref type_param_id) => {
                // Map not guaranteed to contain the type var b/c
                //   type var may not necessarily be something to substitute
                //   (e.g. may be a top-level type arg)
                let result = map
                    .get(type_param_id)
                    .map(|t| t.clone())
                    .unwrap_or(AbstractType::TypeVar(type_param_id.clone()));

                Ok(result)
            }

            AbstractType::Opaque { 
                ref span,
                type_id, 
                ref args 
            } => {
                let (ok_args, errors) = args
                    .iter()
                    .map(|a| {
                        a.substitute_internal(
                            universe,
                            scoped_data,
                            typing_context,
                            map,
                        )
                    })
                    .fold(
                        (Vec::new(), Vec::new()),
                        |(mut ok, mut err), result| {
                            match result {
                                Ok(app) => ok.push(app),
                                Err(mut e) => err.append(&mut e),
                            }

                            (ok, err)
                        },
                    );

                if errors.len() != 0 {
                    return Err(errors);
                }

                Ok(AbstractType::Opaque {
                    span: span.clone(),
                    type_id: type_id.clone(),
                    args: ok_args,
                })
            }

            AbstractType::Int => Ok(AbstractType::Int),
            AbstractType::Float => Ok(AbstractType::Float),
            AbstractType::String => Ok(AbstractType::String),
            AbstractType::Bool => Ok(AbstractType::Bool),
            AbstractType::Unit => Ok(AbstractType::Unit),
            AbstractType::Any(ref l) => Ok(AbstractType::Any(l.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AbstractFieldMap {
    pub fields: HashMap<FieldId, AbstractType>,
    pub field_map: HashMap<Ident, FieldId>,
}

impl AbstractFieldMap {
    pub fn get(&self, name: &Ident) -> Option<&AbstractType> {
        self.field_map.get(name).and_then(|id| self.fields.get(id))
    }
}

#[derive(Debug, Clone)]
pub struct AbstractWidthConstraint {
    pub fields: HashMap<Ident, AbstractType>,
}

pub fn type_from_ann<'a, 'b, 'c, 'd, T: Into<TypeAnnotationRef<'c>>>(
    universe: &'a Universe,
    scope: &'b ScopedData,
    typing_context: &'d TypingContext,
    anno: T,
) -> Result<AbstractType, AnalysisError> {
    match anno.into() {
        TypeAnnotationRef::Path(typed_path) => {
            // Check if path refers to type parameter
            // Assume naming conflicts detected at type parameter declaration
            if typed_path.module_path().0.len() == 1 {
                let ident = typed_path.module_path().0.get(0).unwrap().data();
                let type_var = scope.type_var(ident);

                // Found a type parameter
                if let Some(tv_id) = type_var {
                    // Do not allow type arguments on a type parameter
                    if typed_path.annotations().is_some() {
                        return Err(ATypeError::ParameterizedParameter {
                            ident: typed_path
                                .module_path()
                                .0
                                .get(0)
                                .unwrap()
                                .data()
                                .clone(),
                        }
                        .into());
                    }

                    assert!(typing_context.type_vars.contains_key(&tv_id));
                    return Ok(AbstractType::TypeVar(tv_id.clone()));
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
                    .collect(),
            );
            let type_cons = scope.type_cons(universe, &type_cons_path).ok_or(
                AnalysisError::UnknownType(typed_path.module_path().clone()),
            )?;

            let type_args = typed_path.annotations().map(|ref vec| {
                vec.iter()
                    .map(|anno| {
                        type_from_ann(universe, scope, typing_context, anno)
                    })
                    .collect::<Result<Vec<_>, _>>()
            });

            let type_args = match type_args {
                Some(dat) => dat?,

                None => Vec::new(),
            };

            // TODO: Get this span from annotation span
            Ok(AbstractType::App {
                span: Span::dummy(),
                type_cons: type_cons,
                args: type_args,
            })
        }

        TypeAnnotationRef::Array(element_type, size) => {
            let element_type_app = type_from_ann(
                universe,
                scope,
                typing_context,
                element_type.data(),
            )?;

            // TODO: Get this span from annotation span
            Ok(AbstractType::Array {
                span: Span::dummy(),
                element_type: Box::new(element_type_app),
                size: *size,
            })
        }

        TypeAnnotationRef::FnType(tp, params, return_type) => {
            let (_local_type_params, new_scope) = match tp {
                Some(_local_type_params) => {
                    return Err(ATypeError::FnAnnLocalTypeParameter.into());
                }

                None => (TypeParams::empty(), None),
            };

            let scope = new_scope.as_ref().unwrap_or(scope);

            let param_types = params
                .map(|slice| {
                    slice
                        .iter()
                        .map(|p| {
                            type_from_ann(
                                universe,
                                scope,
                                typing_context,
                                p.data(),
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()
                })
                .unwrap_or(Ok(Vec::new()))?;

            let return_type = return_type
                .map(|return_type| {
                    type_from_ann(
                        universe,
                        scope,
                        typing_context,
                        return_type.data(),
                    )
                })
                .unwrap_or(Ok(AbstractType::Unit))?;

            // TODO: Get this span from annotation span
            Ok(AbstractType::Function {
                span: Span::dummy(),
                parameters: param_types,
                return_type: Box::new(return_type),
            })
        }

        TypeAnnotationRef::WidthConstraint(ast_constraints) => {
            fuse_width_constraints(
                universe,
                scope,
                typing_context,
                ast_constraints,
            )
        }
    }
}

fn fuse_width_constraints(
    universe: &Universe,
    scope: &ScopedData,
    typing_context: &TypingContext,
    ast_constraints: &[AstNode<WidthConstraint>],
) -> Result<AbstractType, AnalysisError> {
    // TODO: Fuse constraints
    let mut field_constraints: HashMap<Ident, Vec<AbstractType>> =
        HashMap::new();

    // Map field to its (unfused) constraints
    for ast_constraint in ast_constraints {
        match ast_constraint.data() {
            // base Struct/WidthConstraint
            // Inspect the type and use it's field types as field constraints
            //   for a new WidthConstraint
            WidthConstraint::BaseStruct(ref ann) => {
                let ann_type =
                    type_from_ann(universe, scope, typing_context, ann.data())?
                        .substitute(universe, scope, typing_context)?;

                match ann_type {
                    AbstractType::Record {
                        abstract_field_map:
                            AbstractFieldMap {
                                ref fields,
                                ref field_map,
                            },
                        ..
                    } => {
                        for (field_name, field_id) in field_map.iter() {
                            let field_type = fields
                                .get(field_id)
                                .expect("Missing field id")
                                .clone();

                            field_constraints
                                .entry(field_name.clone())
                                .or_insert(Vec::new())
                                .push(field_type);
                        }
                    }

                    AbstractType::WidthConstraint {
                        ref span,
                        width: AbstractWidthConstraint { ref fields },
                    } => {
                        for (field_name, field_type) in fields {
                            field_constraints
                                .entry(field_name.clone())
                                .or_insert(vec![field_type.clone()])
                                .push(field_type.clone());
                        }
                    }

                    _ => unimplemented!("Non-record/width constraint base"),
                }
            }

            // A pseudo-width-constraint type (not actually a type)
            // Take the fields and use them as a new constraint
            WidthConstraint::Anonymous(ref ident_ann_pairs) => {
                for (ast_ident, ast_ann) in ident_ann_pairs.iter() {
                    let ann_type = type_from_ann(
                        universe,
                        scope,
                        typing_context,
                        ast_ann.data(),
                    )?;
                    field_constraints
                        .entry(ast_ident.data().clone())
                        .or_insert(vec![ann_type.clone()])
                        .push(ann_type);
                }
            }
        }
    }

    let (ok_constraints, errors) = field_constraints
        .iter()
        .map(|(name, field_constraints)| {
            let fuse = fuse_field_width_constraints(
                universe,
                scope,
                typing_context,
                field_constraints,
            );
            (name, fuse)
        })
        .fold(
            (HashMap::new(), Vec::new()),
            |(mut ok, mut err), (name, fuse_result)| {
                match fuse_result {
                    Ok(r) => {
                        ok.insert(name.clone(), r);
                    }

                    Err(e) => err.push(e),
                };

                (ok, err)
            },
        );

    if errors.len() != 0 {
        return Err(errors.into());
    }

    // TODO: Get span from declarations
    Ok(AbstractType::WidthConstraint {
        span: Span::dummy(),
        width: AbstractWidthConstraint {
            fields: ok_constraints,
        }
    })
}

/// Ensures that there are no conflicting constraints on a field
fn fuse_field_width_constraints(
    universe: &Universe,
    scope: &ScopedData,
    typing_context: &TypingContext,
    constraints: &[AbstractType],
) -> Result<AbstractType, AnalysisError> {
    use super::error::TypeError;

    let mut constraint_iter = constraints.into_iter();
    let first_constraint = constraint_iter
        .next()
        .expect("Always at least one constraint");

    let is_first_non_width_constraint = match first_constraint {
        AbstractType::Record { .. }
        | AbstractType::App { .. }
        | AbstractType::Array { .. }
        | AbstractType::Function { .. }
        | AbstractType::UncheckedFunction { .. }
        | AbstractType::Int
        | AbstractType::Float
        | AbstractType::String
        | AbstractType::Bool
        | AbstractType::Unit
        | AbstractType::Opaque { .. }
        | AbstractType::Any(_) => true,

        AbstractType::TypeVar(..) => true, // TODO: Check the type var in the context?

        AbstractType::WidthConstraint {..} => false,
    };

    let found_non_width_constraint = is_first_non_width_constraint;

    let mut internal_field_constraints: HashMap<Ident, Vec<AbstractType>> =
        HashMap::new();

    // TODO: Get span for constraints
    for constraint in constraint_iter {
        match constraint {
            AbstractType::WidthConstraint {
                ref span,
                width: AbstractWidthConstraint {
                    ref fields,
                }
            } => {
                if found_non_width_constraint {
                    // Error: found { foo: int } + { foo: { ... } }
                    // TODO: Make this collect only conflicting constraints
                    return Err(TypeError::ConflictingConstraints {
                        constraints: constraints
                            .iter()
                            .map(|c| c.clone())
                            .collect(),
                    }
                    .into());
                }

                // Gather internal field constraints to recurse later on
                for (field, field_type) in fields {
                    internal_field_constraints
                        .entry(field.clone())
                        .or_insert(Vec::new())
                        .push(field_type.clone());
                }
            }

            _ => {
                if !found_non_width_constraint {
                    // Error: found { foo: { ... } } + { foo: int }
                    // TODO: Make this collect only conflicting constraints
                    return Err(TypeError::ConflictingConstraints {
                        constraints: constraints
                            .iter()
                            .map(|c| c.clone())
                            .collect(),
                    }
                    .into());
                }

                // TODO: Pass span info
                resolve_types_static(
                    universe,
                    scope,
                    typing_context,
                    constraint,
                    first_constraint,
                    crate::span::Span::dummy(),
                )
                .map_err(|_e| {
                    TypeError::ConflictingConstraints {
                        constraints: constraints
                            .iter()
                            .map(|c| c.clone())
                            .collect(),
                    }
                })?;
            }
        }
    }

    // PRECONDITION:
    //  All current constraints scanned.
    if found_non_width_constraint {
        // No other constraint
        Ok(first_constraint.clone())
    } else {
        // Validate internal field constraints and fuse
        let mut final_internal_map = HashMap::new();
        for (field, constraints) in internal_field_constraints {
            let field_constraint = fuse_field_width_constraints(
                universe,
                scope,
                typing_context,
                &constraints,
            )?;
            if final_internal_map
                .insert(field.clone(), field_constraint)
                .is_some()
            {
                panic!("FUSE ERROR");
            }
        }

        // TODO: Get span of total width constraint
        Ok(AbstractType::WidthConstraint {
            span: Span::dummy(),
            width: AbstractWidthConstraint {
                fields: final_internal_map,
            }
        })
    }
}
