use std::collections::HashMap;

use crate::ast::{Ident, TypeAnnotationRef, WidthConstraint, AstNode};

use super::error::{AnalysisError, ApplicationError, TypeError};
use super::semantic_data::{FieldId, ScopedData, TypeId, TypeParamId, Universe};
use super::type_resolver::resolve_types;

macro_rules! nill_check {
    ($type_args: expr) => {{
        if $type_args.is_some() {
            // TODO: error on type args to type cons int, bool, etc
            unimplemented!()
        }
    }};
}

/// Use during analysis
#[derive(Clone, Debug)]
pub enum Type {
    UncheckedFunction {
        return_type: Box<Type>,
    },

    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },

    Array {
        element_type: Box<Type>,
        size: u64,
    },

    Record {
        type_id: TypeId,
        fields: HashMap<FieldId, Type>,
        field_map: HashMap<Ident, FieldId>,
    },

    WidthConstraint {
        fields: HashMap<FieldId, Type>,
        field_map: HashMap<Ident, FieldId>,
    },

    Param(TypeParamId),

    Int,
    Float,
    String,
    Bool,
    Unit,
}

#[derive(Debug, Clone)]
pub struct TypeParams {
    params: Option<HashMap<TypeParamId, Option<AbstractWidthConstraint>>>,
}

impl TypeParams {

    pub fn new() -> TypeParams {
        TypeParams {
            params: Some(HashMap::new())
        }
    }

    pub fn empty() -> TypeParams {
        TypeParams {
            params: None
        }
    }

    pub fn add_param(&mut self, param: TypeParamId, constraint: Option<AbstractWidthConstraint>) {
        match self.params {
            Some(ref mut p) => {
                p.insert(param, constraint);
            }

            None => {
                let mut hm = HashMap::new();
                hm.insert(param, constraint);
                self.params = Some(hm);
            }
        }
    }

    pub fn len(&self) -> usize {
        self.params.as_ref().map_or(0, |tp| tp.len())
    }

    pub fn iter(&self) -> TypeParamsIter {
        TypeParamsIter {
            params: self.params.as_ref().map(|tp| tp.iter())
        }
    }
}

pub struct TypeParamsIter<'a> {
    params: Option<std::collections::hash_map::Iter<'a, 
        TypeParamId, 
        Option<AbstractWidthConstraint>
        >
    >
}

impl<'a> std::iter::Iterator for TypeParamsIter<'a> {
    type Item = (TypeParamId, Option<&'a AbstractWidthConstraint>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.params {
            Some(ref mut iter) => iter.next().map(|(id, wc)| (*id, wc.as_ref())),
            None => None,
        }
    }
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

    Array {
        element_type: AbstractType,
        size: u64,
    },

    Record {
        type_id: TypeId,
        type_params: TypeParams,
        fields: HashMap<FieldId, AbstractType>,
        field_map: HashMap<Ident, FieldId>,
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

    fn type_params(&self) -> Option<&TypeParams> {
        match *self {
            TypeCons::Function {
                ref type_params,
                ..
            } => Some(type_params),

            TypeCons::Record {
                ref type_params,
                ..
            } => Some(type_params),

            TypeCons::UncheckedFunction {
                ref type_params,
                ..
            } => Some(type_params),

            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AbstractWidthConstraint {
    base_types: Vec<AbstractType>,
    fields: HashMap<Ident, Vec<AbstractType>>,
}

#[derive(Debug, Clone)]
pub enum AbstractType {
    App {
        type_cons: TypeId,
        args: Option<Vec<AbstractType>>,
    },

    WidthConstraint(AbstractWidthConstraint),

    Param(TypeParamId),
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

    pub fn apply(&self, universe: &Universe, scope: &ScopedData) -> Result<Type, TypeError> {
        let param_map = scope
            .type_params()
            .map(|id| (id, AbstractType::Param(id)))
            .collect::<HashMap<_, _>>();

        self.apply_internal(universe, &param_map)
    }

    fn apply_internal(
        &self,
        universe: &Universe,
        param_map: &HashMap<TypeParamId, AbstractType>,
    ) -> Result<Type, TypeError> {
        match *self {
            AbstractType::App {
                ref type_cons,
                args: ref type_args,
            } => {
                let type_cons = universe.get_type_cons(*type_cons).unwrap();
                let new_param_map = match (type_cons.type_params(), type_args) {
                    (Some(ref type_params), Some(ref type_args)) => {
                        if type_params.len() != type_args.len() {
                            return Err(ApplicationError::Arity {
                                expected: type_params.len(),
                                found: type_args.len(),
                            }
                            .into());
                        }

                        let mut param_map = param_map.clone();

                        for ((param_id, constraint), type_arg) in type_params
                                .iter()
                                .zip(type_args.iter()) {

                            // TODO: Check width constraint
                            param_map.insert(param_id.clone(), type_arg.clone());
                        }

                        Some(param_map)
                    }

                    (Some(ref type_params), None) => {
                        return Err(ApplicationError::Arity {
                            expected: type_params.len(),
                            found: 0,
                        }
                        .into());
                    }

                    (None, Some(ref type_args)) => {
                        return Err(ApplicationError::Arity {
                            expected: 0,
                            found: type_args.len(),
                        }
                        .into());
                    }

                    (None, None) => None,
                };

                let param_map = new_param_map.as_ref().unwrap_or(param_map);

                match type_cons {
                    TypeCons::Function {
                        ref parameters,
                        ref return_type,
                        ..
                    } => {
                        let parameters = parameters
                            .iter()
                            .map(|app| app.apply_internal(universe, param_map))
                            .collect::<Result<Vec<_>, _>>()?;

                        let return_type = return_type.apply_internal(universe, param_map)?;

                        Ok(Type::Function {
                            parameters: parameters,
                            return_type: Box::new(return_type),
                        })
                    }

                    TypeCons::UncheckedFunction {
                        ref return_type,
                        ..
                    } => {
                        let return_type = return_type.apply_internal(universe, param_map)?;

                        Ok(Type::UncheckedFunction {
                            return_type: Box::new(return_type),
                        })
                    }

                    TypeCons::Array {
                        ref element_type,
                        size,
                    } => {
                        let element_type = element_type.apply_internal(universe, param_map)?;
                        Ok(Type::Array {
                            element_type: Box::new(element_type),
                            size: size,
                        })
                    }

                    TypeCons::Record {
                        type_id,
                        ref fields,
                        ref field_map,
                        ..
                    } => Ok(Type::Record {
                        type_id: type_id.clone(),
                        fields: fields
                            .iter()
                            .map(|(k, v)| match v.apply_internal(universe, param_map) {
                                Ok(v) => Ok((k.clone(), v)),
                                Err(e) => Err(e),
                            })
                            .collect::<Result<HashMap<_, _>, _>>()?,

                        field_map: field_map.clone(),
                    }),

                    TypeCons::Int => {
                        nill_check!(type_args);
                        Ok(Type::Int)
                    }

                    TypeCons::Float => {
                        nill_check!(type_args);
                        Ok(Type::Float)
                    }

                    TypeCons::Bool => {
                        nill_check!(type_args);
                        Ok(Type::Bool)
                    }

                    TypeCons::String => {
                        nill_check!(type_args);
                        Ok(Type::String)
                    }

                    TypeCons::Unit => {
                        nill_check!(type_args);
                        Ok(Type::Unit)
                    }
                }
            },

            AbstractType::WidthConstraint(AbstractWidthConstraint {
                ref base_types,
                ref fields,
            }) => {

                let mut concrete_constraints: HashMap<Ident, Vec<Type>> = HashMap::new();

                // Gather base constraints
                for base in base_types {
                    let concrete_base = base.apply_internal(universe, param_map)?;

                    match concrete_base {
                        Type::Record {
                            ref fields,
                            ref field_map,
                            ..
                        } => {
                            for (field, field_id) in field_map.iter() {
                                let field_type = fields.get(field_id).unwrap();
                                concrete_constraints.entry(field.clone())
                                    .and_modify(|vec| vec.push(field_type.clone()))
                                    .or_insert(vec![field_type.clone()]);
                            }
                        },

                        _ => {
                            unimplemented!()
                        },
                    }
                }

                // Gather constraints
                for (field, constraints) in fields.iter() {
                    for constraint in constraints {
                        let concrete = constraint.apply_internal(universe, param_map)?;

                        concrete_constraints.entry(field.clone())
                            .or_insert(Vec::new())
                            .push(concrete);
                    }
                    
                }

                // Perform width-constraint validation and fuse
                let mut final_map = HashMap::new();
                let mut final_constraints = HashMap::new();
                for (field, constraints) in concrete_constraints {
                    let field_id = universe.new_field_id();
                    let final_constraint = fuse_validate_concrete_field_constraints(universe,
                                                                                    &constraints)?;
                    if final_constraints.insert(field_id, final_constraint).is_some() {
                        panic!("FUSE ERROR")
                    }

                    final_map.insert(field, field_id);
                }

                Ok(Type::WidthConstraint { 
                    fields: final_constraints,
                    field_map: final_map,
                })
            }

            AbstractType::Param(ref param_id) => {
                let type_app = param_map.get(param_id).unwrap();
                match type_app {
                    AbstractType::Param(ref param_id) => Ok(Type::Param(param_id.clone())),
                    _ => type_app.apply_internal(universe, param_map),
                }
            }
        }
    }
}

pub fn type_app_from_annotation<'a, 'b, 'c, 'd, T: Into<TypeAnnotationRef<'c>>>(
    universe: &'a mut Universe,
    scope: &'b ScopedData,
    anno: T,
) -> Result<AbstractType, AnalysisError> {
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
                            ident: typed_path.module_path().0.get(0).unwrap().data().clone(),
                        }
                        .into());
                    }

                    return Ok(AbstractType::Param(tp_id));
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
            let type_cons = scope
                .type_cons(universe, &type_cons_path)
                .ok_or(AnalysisError::UnknownType(typed_path.module_path().clone()))?;

            let type_args = typed_path.annotations().map(|ref vec| {
                vec.iter()
                    .map(|anno| type_app_from_annotation(universe, scope, anno))
                    .collect::<Result<Vec<_>, _>>()
            });

            let type_args = match type_args {
                Some(dat) => Some(dat?),

                None => None,
            };

            Ok(AbstractType::App {
                type_cons: type_cons,
                args: type_args,
            })
        }

        TypeAnnotationRef::Array(element_type, size) => {
            let element_type_app = type_app_from_annotation(universe, scope, element_type.data())?;
            let cons = TypeCons::Array {
                element_type: element_type_app,
                size: *size,
            };

            let type_id = universe.insert_generated_type_cons(cons);

            Ok(AbstractType::App {
                type_cons: type_id,
                args: None,
            })
        }

        TypeAnnotationRef::FnType(tp, args, ret_type) => {
            let (local_type_params, new_scope) = match tp {
                Some(local_type_params) => {
                    let mut new_scope = scope.clone();
                    let mut local_param_ids = TypeParams::new();
                    let local_type_param_id = universe.new_type_param_id();

                    // Insert local type parameters into the current scope
                    for p in local_type_params.params.iter() {
                        if new_scope.insert_type_param(p.data().clone(), local_type_param_id) {
                            return Err(TypeError::TypeParameterNamingConflict {
                                ident: p.data().clone(),
                            }.into());
                        }

                        // TODO: Can function type annotations introduce new type parameters?
                        // If so, also need to implement constraint generation
                        local_param_ids.add_param(local_type_param_id, None);
                    }

                    (local_param_ids, Some(new_scope))
                },

                None => (TypeParams::empty(), None),
            };

            let scope = new_scope.as_ref().unwrap_or(scope);

            let arg_type_cons = match args.map(|slice| {
                slice
                    .iter()
                    .map(|arg| type_app_from_annotation(universe, scope, arg.data()))
                    .collect::<Result<Vec<_>, _>>()
            }) {
                Some(args) => Some(args?),
                None => None,
            };

            let return_type_cons = match ret_type
                .map(|ret_type| type_app_from_annotation(universe, scope, ret_type.data()))
            {
                Some(ret) => Some(ret?),
                None => None,
            };

            let cons = TypeCons::Function {
                type_params: local_type_params,
                parameters: arg_type_cons.unwrap_or(Vec::new()),
                return_type: return_type_cons.unwrap_or(AbstractType::App {
                    type_cons: universe.unit(),
                    args: None,
                }),
            };

            let type_id = universe.insert_generated_type_cons(cons);

            Ok(AbstractType::App {
                type_cons: type_id,
                args: None,
            })
        }

        TypeAnnotationRef::WidthConstraint(constraints) => {
            abstract_fuse_width_constraints(universe, scope, constraints)
        }
    }
}

fn abstract_fuse_width_constraints(universe: &mut Universe, 
                          scope: &ScopedData, 
                          constraints: &[AstNode<WidthConstraint>]) 
    -> Result<AbstractType, AnalysisError> {
    
    // Does NOT perform any validation
    // Validation is performed on type application

    let mut bases = Vec::new();
    let mut constrained_fields: HashMap<Ident, Vec<AbstractType>> = HashMap::new();
    for constraint in constraints {

        match constraint.data() {
            WidthConstraint::BaseStruct(ref base_constraint) => {
                let app = type_app_from_annotation(universe,
                                                   scope,
                                                   base_constraint.data())?;

                bases.push(app);
            }

            WidthConstraint::Anonymous(ref fields) => {
                for (name, ann) in fields.iter() {
                    let app = type_app_from_annotation(universe,
                                                       scope,
                                                       ann.data())?;

                    constrained_fields.entry(name.data().clone())
                        .or_insert(Vec::new())
                        .push(app);
                }


            },
        }
    }

    let width_constraint = AbstractType::WidthConstraint(AbstractWidthConstraint {
        base_types: bases,
        fields: constrained_fields,
    });

    Ok(width_constraint)
}

/// Validates type constraints
/// If all type constraints pass validation, then all type constraints can be fused into one
/// constraint
fn fuse_validate_concrete_field_constraints(universe: &Universe, constraints: &[Type]) -> Result<Type, TypeError> {

    let mut constraint_iter = constraints.into_iter();

    let mut internal_field_constraints = HashMap::new();
    let mut first_constraint = constraint_iter.next().unwrap();
    // Flag to see if first constraint is a concrete type (i.e. int or a width constraint)
    let is_first_concrete_constraint = match first_constraint {
        Type::WidthConstraint { 
            ref fields,
            ref field_map,
        } => {

            // Add first constraint to internal field constraints
            // Gather internal field constraints to recurse later on
            for (field, field_id) in field_map {
                let concrete_constraint = fields.get(field_id).unwrap();
                internal_field_constraints.entry(field)
                        .or_insert(Vec::new())
                        .push(concrete_constraint.clone());
            }
            false
        },
        _ => true,
    };

    // Flag to see if constraint is a concrete type (i.e. int or a width constraint)
    let found_base_type_constraint = is_first_concrete_constraint;


    // Check for invalid constraints like:
    //      { foo: int } + { foo: String }
    //      { foo: int, foo: String }
    //      { foo: int } + { foo: { ... } }
    //      { foo: { bar: int } } + { foo: { bar: String } }
    for constraint in constraint_iter {
        match constraint {
            Type::WidthConstraint {
                ref fields,
                ref field_map,
            } => {
                if found_base_type_constraint {
                    // Error: found { foo: int } + { foo: { ... } }
                    unimplemented!()
                }

                // Gather internal field constraints to recurse later on
                for (field, field_id) in field_map {
                    let concrete_constraint = fields.get(field_id).unwrap();
                    internal_field_constraints.entry(field)
                            .or_insert(Vec::new())
                            .push(concrete_constraint.clone());
                }

            }

            _ => {
                if !found_base_type_constraint {
                    // Error: found { foo: { ... } } + { foo: int }
                    unimplemented!()
                }

                if !resolve_types(constraint, first_constraint) {
                    // Error: found { foo: int } + { foo: String }
                    unimplemented!();
                }
            }
        }
    }

    if is_first_concrete_constraint {
        Ok(first_constraint.clone())
    } else {
        // Validate internal field constraints and fuse
        let mut final_internal_map = HashMap::new();
        let mut final_internal_constraints = HashMap::new();
        for (field, constraints) in internal_field_constraints {
            let field_id = universe.new_field_id();

            let field_constraint = fuse_validate_concrete_field_constraints(universe, &constraints)?;
            if final_internal_constraints.insert(field_id, field_constraint).is_some() {
                panic!("FUSE ERROR");
            }

            final_internal_map.insert(field.clone(), field_id);
        }

        Ok(Type::WidthConstraint { 
            fields: final_internal_constraints,
            field_map: final_internal_map,
        })
    }
}
