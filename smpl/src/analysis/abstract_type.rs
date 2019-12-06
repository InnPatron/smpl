use std::collections::HashMap;

use crate::ast::{AstNode, Ident, TypeAnnotation, WidthConstraint};
use crate::span::Span;

use super::error::{AnalysisError, ApplicationError, TypeError as ATypeError};
use super::resolve_scope::ScopedData;
use super::semantic_data::{FieldId, TypeId, TypeParamId, TypeVarId, Universe};
use super::type_checker::TypingContext;
use super::type_resolver::resolve_types_static;
use super::type_cons::{TypeCons, TypeParams};

pub type AbstractType = AbstractTypeX<Span>;
pub type AbstractFieldMap = AbstractFieldMapX<Span>;
pub type AbstractWidthConstraint = AbstractWidthConstraintX<Span>;

#[derive(Debug, Clone)]
pub enum AbstractTypeX<X> {
    Any(X),

    Record {
        data: X,
        type_id: TypeId,
        abstract_field_map: AbstractFieldMapX<X>,
    },

    App {
        data: X,
        type_cons: TypeId,
        args: Vec<AbstractTypeX<X>>,
    },

    Array {
        data: X,
        element_type: Box<AbstractTypeX<X>>,
        size: u64,
    },

    UncheckedFunction {
        data: X,
        return_type: Box<AbstractTypeX<X>>,
    },

    Function {
        data: X,
        parameters: Vec<AbstractTypeX<X>>,
        return_type: Box<AbstractTypeX<X>>,
    },

    WidthConstraint {
        data: X, 
        width: AbstractWidthConstraintX<X>
    },

    Opaque {
        data: X, 
        type_id: TypeId,
        args: Vec<AbstractTypeX<X>>,
    },

    TypeVar(X, TypeVarId),

    Int(X),
    Float(X),
    String(X),
    Bool(X),
    Unit(X),
}

impl<T> AbstractTypeX<T> {
    fn downcast(self) -> AbstractTypeX<()> {
        use self::AbstractTypeX::*;
        match self {
            Any(_) => Any(()),

            Record {
                type_id,
                abstract_field_map,
                ..
            } => {
                Record {
                    data: (),
                    type_id,
                    abstract_field_map: abstract_field_map.downcast(),
                }
            }

            App {
                type_cons,
                args,
                ..
            } => {
                App {
                    data: (),
                    type_cons,
                    args: args
                        .into_iter()
                        .map(|t| t.downcast())
                        .collect(),
                }
            }

            Array {
                element_type,
                size,
                ..
            } => {
                Array {
                    data: (),
                    element_type: Box::new(element_type.downcast()),
                    size,
                } 
            }

            UncheckedFunction {
                return_type,
                ..
            } => {
                UncheckedFunction {
                    data: (),
                    return_type: Box::new(return_type.downcast()),
                }
            }

            Function {
                parameters,
                return_type,
                ..
            } => {
                Function {
                    data: (),
                    parameters: parameters.into_iter()
                        .map(|t| t.downcast())
                        .collect(),
                    return_type: Box::new(return_type.downcast()),
                }
            }

            WidthConstraint {
                width,
                ..
            } => {
                WidthConstraint {
                    data: (),
                    width: width.downcast(),
                }
            }

            Opaque {
                type_id,
                args,
                ..
            } => {
                Opaque {
                    data: (), 
                    type_id,
                    args: args.into_iter()
                        .map(|t| t.downcast())
                        .collect(),
                }
            }

            TypeVar(_, type_var) => TypeVar((), type_var),

            Int(_) => Int(()),
            Float(_) => Float(()),
            String(_) => String(()),
            Bool(_) => Bool(()),
            Unit(_) => Unit(()),
        }
    }

    pub fn data(&self) -> &T {
        use self::AbstractTypeX::*;
        match *self {
            Any(ref data) => data,

            Record {
                ref data,
                ..
            } => data,

            App {
                ref data,
                ..
            } => data,

            Array {
                ref data,
                ..
            } => data,

            UncheckedFunction {
                ref data,
                ..
            } => data,

            Function {
                ref data,
                ..
            } => data,

            WidthConstraint {
                ref data,
                ..
            } => data,

            Opaque {
                ref data,
                ..
            } => data,

            TypeVar(ref data, _) => data,

            Int(ref data) => data,
            Float(ref data) => data,
            String(ref data) => data,
            Bool(ref data) => data,
            Unit(ref data) => data, 
        }
    }
}

impl AbstractTypeX<Span> { 

    pub fn span(&self) -> &Span {
        self.data()
    }

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
            AbstractType::App {
                type_cons: _,
                args: _,
                ..
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
        app_span: &Span,
    ) -> Result<AbstractType, Vec<ATypeError>> {
        match type_cons {
            TypeCons::UncheckedFunction {
                ref return_type, ..
            } => Ok(AbstractType::UncheckedFunction {
                data: app_span.clone(),
                return_type: Box::new(return_type.substitute_internal(
                    universe,
                    scoped_data,
                    typing_context,
                    &map,
                )?),
            }),

            TypeCons::Function {
                ref parameters,
                ref return_type,
                ..
            } => Ok(AbstractType::Function {
                data: app_span.clone(),
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

                Ok(AbstractType::Record {
                    data: app_span.clone(),
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

                Ok(AbstractType::Opaque {
                    data: app_span.clone(),
                    type_id: type_id.clone(),
                    args: ok_args,
                })
            }

            TypeCons::Int => Ok(AbstractType::Int(app_span.clone())),
            TypeCons::Float => Ok(AbstractType::Float(app_span.clone())),
            TypeCons::Bool => Ok(AbstractType::Bool(app_span.clone())),
            TypeCons::String => Ok(AbstractType::String(app_span.clone())),
            TypeCons::Unit => Ok(AbstractType::Unit(app_span.clone())),
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
            AbstractType::App {
                data: ref app_span,
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
                        app_span,
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
                        app_span,
                    )
                }
            }

            AbstractType::Record {
                data: ref record_span,
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
                    data: record_span.clone(),
                    type_id: *type_id,
                    abstract_field_map: AbstractFieldMap {
                        fields: ok_fields,
                        field_map: abstract_field_map.field_map.clone(),
                    },
                })
            }

            AbstractType::Array {
                data: ref array_ty_span,
                ref element_type,
                ref size,
            } => Ok(AbstractType::Array {
                data: array_ty_span.clone(),
                element_type: Box::new(element_type.substitute_internal(
                    universe,
                    scoped_data,
                    typing_context,
                    map,
                )?),
                size: *size,
            }),

            AbstractType::Function {
                data: ref fn_ty_span,
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
                    data: fn_ty_span.clone(),
                    parameters: ok_parameters,
                    return_type: Box::new(new_return),
                })
            }

            AbstractType::UncheckedFunction { 
                data: ref span,
                ref return_type, 
            } => {
                let new_return = return_type.substitute_internal(
                    universe,
                    scoped_data,
                    typing_context,
                    map,
                )?;

                Ok(AbstractType::UncheckedFunction {
                    data: span.clone(),
                    return_type: Box::new(new_return),
                })
            }

            AbstractType::WidthConstraint {
                data: ref span,
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
                    data: span.clone(),
                    width: new_width,
                })
            }

            AbstractType::TypeVar(ref span, ref type_param_id) => {
                // Map not guaranteed to contain the type var b/c
                //   type var may not necessarily be something to substitute
                //   (e.g. may be a top-level type arg)
                let result = map
                    .get(type_param_id)
                    .map(|t| t.clone())
                    .unwrap_or(AbstractType::TypeVar(span.clone(), type_param_id.clone()));

                Ok(result)
            }

            AbstractType::Opaque { 
                data: ref span,
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
                    data: span.clone(),
                    type_id: type_id.clone(),
                    args: ok_args,
                })
            }

            AbstractType::Int(ref s) => Ok(AbstractType::Int(s.clone())),
            AbstractType::Float(ref s) => Ok(AbstractType::Float(s.clone())),
            AbstractType::String(ref s) => Ok(AbstractType::String(s.clone())),
            AbstractType::Bool(ref s) => Ok(AbstractType::Bool(s.clone())),
            AbstractType::Unit(ref s) => Ok(AbstractType::Unit(s.clone())),
            AbstractType::Any(ref l) => Ok(AbstractType::Any(l.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AbstractFieldMapX<X> {
    pub fields: HashMap<FieldId, AbstractTypeX<X>>,
    pub field_map: HashMap<Ident, FieldId>,
}

impl<X> AbstractFieldMapX<X> {
    pub fn get(&self, name: &Ident) -> Option<&AbstractTypeX<X>> {
        self.field_map.get(name).and_then(|id| self.fields.get(id))
    }

    pub fn downcast(self) -> AbstractFieldMapX<()> {
        AbstractFieldMapX {
            fields: self.fields
                .into_iter()
                .map(|(id, at)| (id, at.downcast()))
                .collect(),
            field_map: self.field_map,
        }
    }
}

#[derive(Debug, Clone)]
enum WidthConstraintState<X> {
    Unevaluated(HashMap<Ident, Vec<AbstractTypeX<X>>>, Vec<AbstractTypeX<X>>),
    Evaluated(HashMap<Ident, AbstractTypeX<X>>),
}

#[derive(Debug, Clone)]
pub struct AbstractWidthConstraintX<X> {
    state: WidthConstraintState<X>
}

impl<X> AbstractWidthConstraintX<X> {

    pub(super) fn evaluate(self, universe: &Universe) -> Result<Self, AnalysisError> {
        match self.state {
            WidthConstraintState::Unevaluated(fields, struct_bases) => {
                unimplemented!();
            }

            WidthConstraintState::Evaluated(..) => Ok(self),
        }
    }

    pub fn downcast(self) -> AbstractWidthConstraintX<()> {

        let new_state = match self.state {
            WidthConstraintState::Unevaluated(fields, base_structs) => {
                WidthConstraintState::Unevaluated(
                    fields
                        .into_iter()
                        .map(|(i, vec)| {
                            let vec = vec.into_iter()
                                .map(|t| t.downcast())
                                .collect();
                            (i, vec)
                        })
                        .collect(),
                    base_structs
                        .into_iter()
                        .map(|t| t.downcast())
                        .collect()
                    )
                        
            }

            WidthConstraintState::Evaluated(fields) => {
                WidthConstraintState::Evaluated(
                    fields
                        .into_iter()
                        .map(|(i, t)| (i, t.downcast()))
                        .collect()
                )
            }
        };

        AbstractWidthConstraintX {
            state: new_state
        }
    }
}

pub fn type_from_ann(
    scope: &ScopedData,
    typing_context: &TypingContext,
    anno: &AstNode<TypeAnnotation>,
) -> Result<AbstractType, AnalysisError> 

{
    match anno.data() {
        TypeAnnotation::Path(ref typed_path) => {
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
                            span: anno.span(),
                        }
                        .into());
                    }

                    assert!(typing_context.type_vars.contains_key(&tv_id));
                    return Ok(AbstractType::TypeVar(anno.span(), tv_id.clone()));
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
            let type_cons = scope.type_cons(&type_cons_path).ok_or(
                AnalysisError::UnknownType(typed_path.module_path().clone(), anno.span()),
            )?;

            let type_args = typed_path.annotations().map(|ref vec| {
                vec.iter()
                    .map(|anno| {
                        type_from_ann(scope, typing_context, &*anno)
                    })
                    .collect::<Result<Vec<_>, _>>()
            });

            let type_args = match type_args {
                Some(dat) => dat?,

                None => Vec::new(),
            };

            Ok(AbstractType::App {
                data: anno.span(),
                type_cons: type_cons,
                args: type_args,
            })
        }

        TypeAnnotation::Array(ref element_type, size) => {
            let element_type_app = type_from_ann(
                scope,
                typing_context,
                element_type,
            )?;

            Ok(AbstractType::Array {
                data: anno.span(),
                element_type: Box::new(element_type_app),
                size: *size,
            })
        }

        TypeAnnotation::FnType(ref tp, ref params, ref return_type) => {
            let (_local_type_params, new_scope) = match tp {
                Some(_local_type_params) => {
                    return Err(ATypeError::FnAnnLocalTypeParameter.into());
                }

                None => (TypeParams::empty(), None),
            };

            let scope = new_scope.as_ref().unwrap_or(scope);

            let param_types = params
                .as_ref()
                .map(|slice| {
                    slice
                        .iter()
                        .map(|p| {
                            type_from_ann(
                                scope,
                                typing_context,
                                p,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()
                })
                .unwrap_or(Ok(Vec::new()))?;

            let return_type = return_type
                .as_ref()
                .map(|return_type| {
                    type_from_ann(
                        scope,
                        typing_context,
                        return_type,
                    )
                })
                .unwrap_or(Ok(AbstractType::Unit(anno.span())))?;

            Ok(AbstractType::Function {
                data: anno.span(),
                parameters: param_types,
                return_type: Box::new(return_type),
            })
        }

        TypeAnnotation::WidthConstraint(ref ast_constraints) => {
            fuse_width_constraints(
                scope,
                typing_context,
                ast_constraints,
            )
        }
    }
}

fn fuse_width_constraints(
    scope: &ScopedData,
    typing_context: &TypingContext,
    ast_constraints: &[AstNode<WidthConstraint>],
) -> Result<AbstractType, AnalysisError> {
    // TODO: Fuse constraints
    let mut constraint_span = None;
    let mut field_constraints: HashMap<Ident, Vec<AbstractType>> =
        HashMap::new();
    let mut base_structs: Vec<AbstractType> = Vec::new();

    // Map field to its (unfused) constraints
    for ast_constraint in ast_constraints {

        constraint_span = Some(constraint_span
            .map(|s| Span::combine(s, ast_constraint.span()))
            .unwrap_or(ast_constraint.span())
        );

        match ast_constraint.data() {
            // base Struct/WidthConstraint
            // Lazy store abstract type. Only inspect type during type checking
            //   Laziness required in order to remove Universe parameter on type_from_ann()
            WidthConstraint::BaseStruct(ref ann) => {
                let ann_type =
                    type_from_ann(scope, typing_context, ann)?;

                base_structs.push(ann_type);
            }

            // A pseudo-width-constraint type (not actually a type)
            // Take the fields and use them as a new constraint
            WidthConstraint::Anonymous(ref ident_ann_pairs) => {
                for (ast_ident, ast_ann) in ident_ann_pairs.iter() {
                    let ann_type = type_from_ann(
                        scope,
                        typing_context,
                        ast_ann,
                    )?;
                    field_constraints
                        .entry(ast_ident.data().clone())
                        .or_insert(vec![ann_type.clone()])
                        .push(ann_type);
                }
            }
        }
    }

    // Do not validate width constraints here
    //   Only validate during type checking
    //   Necessary to lift Universe parameter on type_from_ann()
    Ok(AbstractType::WidthConstraint {
        data: constraint_span
            .expect("Expect constraint span to be Some. Implies no constraints"),
        width: AbstractWidthConstraint {
            state: WidthConstraintState::Unevaluated(field_constraints, base_structs)
        }
    })
}

/*
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

    let mut constraint_span = first_constraint.span().clone();
    let is_first_non_width_constraint = match first_constraint {
        AbstractType::Record { .. }
        | AbstractType::App { .. }
        | AbstractType::Array { .. }
        | AbstractType::Function { .. }
        | AbstractType::UncheckedFunction { .. }
        | AbstractType::Int(_)
        | AbstractType::Float(_)
        | AbstractType::String(_)
        | AbstractType::Bool(_)
        | AbstractType::Unit(_)
        | AbstractType::Opaque { .. }
        | AbstractType::Any(_) => true,

        AbstractType::TypeVar(..) => true, // TODO: Check the type var in the context?

        AbstractType::WidthConstraint {..} => false,
    };

    let found_non_width_constraint = is_first_non_width_constraint;

    let mut internal_field_constraints: HashMap<Ident, Vec<AbstractType>> =
        HashMap::new();

    for constraint in constraint_iter {
        constraint_span = Span::combine(constraint_span, constraint.span().clone());

        match constraint {
            AbstractType::WidthConstraint {
                data: ref span,
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

                resolve_types_static(
                    universe,
                    scope,
                    typing_context,
                    constraint,
                    first_constraint,
                    constraint.span().clone(),
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

        Ok(AbstractType::WidthConstraint {
            data: constraint_span,
            width: AbstractWidthConstraint {
                fields: final_internal_map,
            }
        })
    }
}
*/
