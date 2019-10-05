use std::collections::HashMap;

use crate::ast::{Ident, TypeAnnotationRef, WidthConstraint, AstNode};

use super::error::{AnalysisError, ApplicationError, TypeError as ATypeError};
use super::semantic_data::{FieldId, TypeId, TypeParamId, TypeVarId, Universe};
use super::resolve_scope::ScopedData;
use super::type_resolver::resolve_types_static;
use super::type_checker::TypingContext;

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
pub enum AbstractType {

    Any,

    Record {
        type_id: TypeId,
        abstract_field_map: AbstractFieldMap,
    },

    App {
        type_cons: TypeId,
        args: Vec<AbstractType>,
    },

    Array {
        element_type: Box<AbstractType>,
        size: u64,
    },

    UncheckedFunction {
        return_type: Box<AbstractType>,
    },

    Function {
        parameters: Vec<AbstractType>,
        return_type: Box<AbstractType>,
    },

    WidthConstraint(AbstractWidthConstraint),

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

    pub fn apply(&self, universe: &Universe, scope: &ScopedData, typing_context: &TypingContext) 
        -> Result<AbstractType, Vec<ATypeError>> {

        self.apply_internal(universe, &typing_context.type_vars)
    }

    /// Given an environment of type variables to abstract types, recursively substitute
    ///   any type variables in the map with their abstract type.
    ///
    /// No AbstractType returned by apply_internal() should have AbstractType::App() in its tree
    fn apply_internal(&self, universe: &Universe, map: &HashMap<TypeVarId, AbstractType>) 
        -> Result<AbstractType, Vec<ATypeError>> {

        macro_rules! primitive_apply {
            ($args: expr, $result: expr) => {{
                if $args.len() != 0 {
                    unimplemented!("Primitive type app");
                }

                Ok($result)
            }}
        }

        match *self {

            AbstractType::App {
                ref type_cons,
                args: ref type_args,
            } => {
                
                //
                // Type parameters of app's target type constructor are NOT in scope. Any type
                //   parameters within the type constructor are guaranteed to come from solely
                //   that type constructor.
                //
                // NOTE: If polymorphic modules are introduced, the above assumption is broken.
                // 
                // Still need to internally apply to type arguments in order to catch any nested
                //   type parameters
                //
                // TODO: Applying type arguments will probably cause an infinite loop with
                //   recursive types
                let (ok_args, err) = type_args
                            .iter()
                            .map(|at| at.apply_internal(universe, map))
                            .fold((Vec::new(), Vec::new()), | (mut ok, mut err), apply_result| {
                                match apply_result {
                                    Ok(app) => ok.push(app),
                                    Err(mut e) => err.append(&mut e),
                                }

                                (ok, err)
                            });

                if err.len() != 0 {
                    return Err(err);
                }

                let type_cons_id = *type_cons;
                let type_cons = universe.get_type_cons(type_cons_id);
                match type_cons {
                    TypeCons::Record {
                        ref type_params,
                        ref fields,
                        ref field_map,
                        ..
                    } => {
                        if type_params.len() != ok_args.len() {
                            if type_params.len() != ok_args.len() {
                                return Err(vec![ApplicationError::Arity {
                                    found: ok_args.len(),
                                    expected: type_params.len(),
                                }.into()]);
                            }
                        }

                        // Make sure type arguments are all applied and mapped into the typing
                        //  environment for field applications
                        let old_map = map;
                        let mut new_map = map.clone();
                        let mut errors = Vec::new();

                        type_params.map_args(&mut new_map, ok_args.into_iter())
                            .map_err(|e| vec![e])?;

                        let mut afm = AbstractFieldMap {
                            fields: HashMap::new(),
                            field_map: field_map.clone(),
                        };
                        for (field_id, field_abstract_type) in fields.iter() {
                            let field_app_result = 
                                field_abstract_type.apply_internal(universe, &new_map);
                            match field_app_result {

                                Ok(at) => {
                                    afm.fields.insert(field_id.clone(), at);
                                }

                                Err(mut es) => errors.append(&mut es),
                            }
                        }
                        Ok(AbstractType::Record {
                            type_id: type_cons_id,
                            abstract_field_map: afm,
                        })
                    }

                    TypeCons::Function {
                        ref type_params,
                        ref parameters,
                        ref return_type,
                    } => {

                        if type_params.len() != ok_args.len() {
                            if type_params.len() != ok_args.len() {
                                return Err(vec![ApplicationError::Arity {
                                    found: ok_args.len(),
                                    expected: type_params.len(),
                                }.into()]);
                            }
                        }

                        // Make sure type arguments are all applied and mapped into the typing
                        //  environment for field applications
                        let old_map = map;
                        let mut new_map = map.clone();

                        type_params.map_args(&mut new_map, ok_args.into_iter())
                            .map_err(|e| vec![e])?;
                        dbg!(&new_map, type_params);

                        let mut errors = Vec::new();
                        let mut new_params = Vec::new();
                        for param in parameters.iter() {
                            let param_app_result = param.apply_internal(universe, &new_map);
                            dbg!(param, &param_app_result);

                            match param_app_result {
                                Ok(p) => new_params.push(p),

                                Err(mut es) => errors.append(&mut es),
                            }
                        }

                        if errors.len() != 0 {
                            return Err(errors);
                        }

                        let return_type = return_type.apply_internal(universe, &new_map)?;

                        Ok(AbstractType::Function {
                            parameters: new_params,
                            return_type: Box::new(return_type),
                        })
                    }

                    TypeCons::UncheckedFunction {
                        ref type_params,
                        ref return_type,
                    } => {

                        if type_params.len() != ok_args.len() {
                            return Err(vec![ApplicationError::Arity {
                                found: ok_args.len(),
                                expected: type_params.len(),
                            }.into()]);
                        }

                        // Make sure type arguments are all applied and mapped into the typing
                        //  environment for field applications
                        let old_map = map;
                        let mut new_map = map.clone();
                        type_params.map_args(&mut new_map, ok_args.into_iter())
                            .map_err(|e| vec![e])?;

                        let return_type = return_type.apply_internal(universe, &new_map)?;

                        Ok(AbstractType::UncheckedFunction {
                            return_type: Box::new(return_type),
                        })
                    }

                    TypeCons::Int => primitive_apply!(ok_args, AbstractType::Int),
                    TypeCons::Float => primitive_apply!(ok_args, AbstractType::Float),
                    TypeCons::String => primitive_apply!(ok_args, AbstractType::String),
                    TypeCons::Bool => primitive_apply!(ok_args, AbstractType::Bool),
                    TypeCons::Unit => primitive_apply!(ok_args, AbstractType::Unit),
                }
            },

            AbstractType::Record {
                ref type_id,
                ref abstract_field_map,
            } => {
                let (ok_fields, err) = abstract_field_map.fields.iter()
                    .map(|(f_id, ty)| (f_id.clone(), ty.apply_internal(universe, map)))
                    .fold((HashMap::new(), Vec::new()), |(mut ok, mut err), (f_id, result)| {
                        match result {
                            Ok(app) => {
                                ok.insert(f_id, app);
                            }
                            Err(mut e) => err.append(&mut e),
                        }

                        (ok, err)
                });

                if err.len() != 0 {
                    return Err(err);
                }

                Ok(AbstractType::Record {
                    type_id: *type_id,
                    abstract_field_map: AbstractFieldMap {
                        fields: ok_fields,
                        field_map: abstract_field_map.field_map.clone()
                    }
                })
            }

            AbstractType::Array {
                ref element_type,
                ref size
            } => Ok(AbstractType::Array {
                element_type: Box::new(element_type.apply_internal(universe, map)?),
                size: *size,
            }),

            AbstractType::Function {
                ref parameters,
                ref return_type,
            } => {

                let (ok_parameters, errors) = parameters.iter()
                    .map(|p| p.apply_internal(universe, map))
                    .fold((Vec::new(), Vec::new()), |(mut ok, mut err), result| {
                        match result {
                            Ok(app) => ok.push(app),
                            Err(mut e) => err.append(&mut e),
                        }

                        (ok, err)
                });

                if errors.len() != 0 {
                    return Err(errors);
                }

                let new_return = return_type.apply_internal(universe, map)?;

                Ok(AbstractType::Function {
                    parameters: ok_parameters,
                    return_type: Box::new(new_return),
                })
            }

            AbstractType::UncheckedFunction {
                ref return_type,
            } => {

                let new_return = return_type.apply_internal(universe, map)?;

                Ok(AbstractType::UncheckedFunction {
                    return_type: Box::new(new_return),
                })
            }

            AbstractType::WidthConstraint(ref width_constraint) => {
                
                let (ok_field_types, errors) = width_constraint.fields
                    .iter()
                    .map(|(ident, at)| at.apply_internal(universe, map).map(|r| (ident, r)))
                    .fold((HashMap::new(), Vec::new()), |(mut ok, mut err), apply_result| {
                        match apply_result {
                            Ok((ref_ident, at)) => { ok.insert(ref_ident.clone(), at); },
                            Err(mut e) => err.append(&mut e),
                        };

                        (ok, err)
                });

                if errors.len() != 0 {
                    return Err(errors);
                }

                let new_width = AbstractWidthConstraint {
                    fields: ok_field_types,
                };

                Ok(AbstractType::WidthConstraint(new_width))
            },

            AbstractType::TypeVar(ref type_param_id) => {
                dbg!(type_param_id, map);
                assert!(map.contains_key(type_param_id));

                Ok(AbstractType::TypeVar(type_param_id.clone()))
            }

            AbstractType::Int => Ok(AbstractType::Int),
            AbstractType::Float => Ok(AbstractType::Float),
            AbstractType::String => Ok(AbstractType::String),
            AbstractType::Bool => Ok(AbstractType::Bool),
            AbstractType::Unit => Ok(AbstractType::Unit),
            AbstractType::Any => Ok(AbstractType::Any),
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
        self.field_map
            .get(name)
            .and_then(|id| self.fields.get(id))
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
                            ident: typed_path.module_path().0.get(0).unwrap().data().clone(),
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
            let type_cons = scope
                .type_cons(universe, &type_cons_path)
                .ok_or(AnalysisError::UnknownType(typed_path.module_path().clone()))?;

            let type_args = typed_path.annotations().map(|ref vec| {
                vec.iter()
                    .map(|anno| type_from_ann(universe, scope, typing_context, anno))
                    .collect::<Result<Vec<_>, _>>()
            });

            let type_args = match type_args {
                Some(dat) => dat?,

                None => Vec::new(),
            };

            Ok(AbstractType::App {
                type_cons: type_cons,
                args: type_args,
            })
        }

        TypeAnnotationRef::Array(element_type, size) => {
            let element_type_app = type_from_ann(universe, scope, typing_context, element_type.data())?;

            Ok(AbstractType::Array {
                element_type: Box::new(element_type_app),
                size: *size,
            })
        }

        TypeAnnotationRef::FnType(tp, params, return_type) => {
            let (local_type_params, new_scope) = match tp {
                Some(local_type_params) => {
                    return Err(ATypeError::FnAnnLocalTypeParameter.into());     
                },

                None => (TypeParams::empty(), None),
            };

            let scope = new_scope.as_ref().unwrap_or(scope);

            let param_types = params.map(|slice| {
                    slice
                        .iter()
                        .map(|p| type_from_ann(universe, scope, typing_context, p.data()))
                        .collect::<Result<Vec<_>, _>>()
                })
                .unwrap_or(Ok(Vec::new()))?;

            let return_type = return_type
                .map(|return_type| type_from_ann(universe, scope, typing_context, return_type.data()))
                .unwrap_or(Ok(AbstractType::Unit))?;


            Ok(AbstractType::Function {
                parameters: param_types,
                return_type: Box::new(return_type),
            })
        }

        TypeAnnotationRef::WidthConstraint(ast_constraints) =>  {
            fuse_width_constraints(universe, scope, typing_context, ast_constraints)
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeParams {
    params: Vec<(TypeParamId, Option<AbstractWidthConstraint>)>,
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

    pub fn add_param(&mut self, param: TypeParamId, 
        constraint: Option<AbstractWidthConstraint>, placeholder_var: TypeVarId) {
        self.params.push((param, constraint));
        self.placeholder_variables.insert(param, placeholder_var);
    }

    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn iter(&self) -> impl Iterator<Item=(TypeParamId, Option<&AbstractWidthConstraint>)> {
        self.params
            .iter()
            .map(|(type_param_id, constraint)| {
                (type_param_id.clone(), constraint.as_ref())
            })
    }

    pub fn placeholder_type_var(&self, id: TypeParamId) -> TypeVarId {
        self.placeholder_variables
            .get(&id)
            .unwrap()
            .clone()
    }

    // Assume arities are equal
    fn map_args<T>(&self, map: &mut HashMap<TypeVarId, AbstractType>, args: T) 
        -> Result<(), ATypeError>
        where T: Iterator<Item=AbstractType> {

        for (arg, (type_param_id, constraint)) in args.zip(self.iter()) {

            let type_var_id = self.placeholder_variables
                .get(&type_param_id)
                .unwrap();

            // TODO: Constraint checking
            map.insert(type_var_id.clone(), arg);
        }

        Ok(())
    }
}

fn fuse_width_constraints(universe: &Universe, scope: &ScopedData,
    typing_context: &TypingContext, ast_constraints: &[AstNode<WidthConstraint>]) 
    -> Result<AbstractType, AnalysisError> {

    // TODO: Fuse constraints
    let mut field_constraints: HashMap<Ident, Vec<AbstractType>> = HashMap::new();

    // Map field to its (unfused) constraints
    for ast_constraint in ast_constraints {
        match ast_constraint.data() {

            // base Struct/WidthConstraint
            // Inspect the type and use it's field types as field constraints
            //   for a new WidthConstraint
            WidthConstraint::BaseStruct(ref ann) => {

                let ann_type = 
                    type_from_ann(universe, scope, typing_context, ann.data())?
                    .apply(universe, scope, typing_context)?;

                match ann_type {
                    AbstractType::Record {
                        abstract_field_map: AbstractFieldMap {
                            ref fields,
                            ref field_map,
                        },
                        ..
                    } => {
                        for (field_name, (_field_id, field_type)) in
                            field_map.keys().zip(fields.iter()) {

                            field_constraints
                                .entry(field_name.clone())
                                .or_insert(vec![field_type.clone()])
                                .push(field_type.clone());
                        }
                    }

                    AbstractType::WidthConstraint(AbstractWidthConstraint {
                        ref fields,
                    }) => {
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
                    let ann_type = 
                        type_from_ann(universe, scope, typing_context, ast_ann.data())?;
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
                universe, scope,
                typing_context, field_constraints);
            (name, fuse)
        })
        .fold((HashMap::new(), Vec::new()), |(mut ok, mut err), (name, fuse_result)| {
            match fuse_result {
                Ok(r) => { 
                    ok.insert(name.clone(), r);
                }

                Err(e) => err.push(e),
            };

            (ok, err)
        });

    if errors.len() != 0 {
        return Err(errors.into());

    }

    Ok(AbstractType::WidthConstraint(AbstractWidthConstraint {
        fields: ok_constraints
    }))
}

/// Ensures that there are no conflicting constraints on a field
fn fuse_field_width_constraints(universe: &Universe, scope: &ScopedData,
    typing_context: &TypingContext, constraints: &[AbstractType]) 
    -> Result<AbstractType, AnalysisError> {

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
            | AbstractType::Any => true,
        
        AbstractType::TypeVar(..) => true,        // TODO: Check the type var in the context?

        AbstractType::WidthConstraint(..) => false,
    };

    let found_non_width_constraint = is_first_non_width_constraint;

    let mut internal_field_constraints: HashMap<Ident, Vec<AbstractType>> = HashMap::new();
    for constraint in constraint_iter {
        match constraint {
            AbstractType::WidthConstraint(AbstractWidthConstraint {
                ref fields,
            }) => {
                if found_non_width_constraint {
                    // Error: found { foo: int } + { foo: { ... } }
                    // TODO: Make this collect only conflicting constraints
                    return Err(TypeError::ConflictingConstraints {
                        constraints: constraints.iter().map(|c| c.clone()).collect()   
                    }.into());
                }

                // Gather internal field constraints to recurse later on
                for (field, field_type) in fields {
                    internal_field_constraints.entry(field.clone())
                            .or_insert(Vec::new())
                            .push(field_type.clone());
                }

            }

            _ => {
                if !found_non_width_constraint {
                    // Error: found { foo: { ... } } + { foo: int }
                    // TODO: Make this collect only conflicting constraints
                    return Err(TypeError::ConflictingConstraints {
                        constraints: constraints.iter().map(|c| c.clone()).collect()   
                    }.into());
                }

                // TODO: Pass span info
                resolve_types_static(universe, scope, typing_context,
                    constraint, first_constraint, crate::span::Span::dummy())
                    .map_err(|e| {
                        TypeError::ConflictingConstraints {
                            constraints: constraints.iter().map(|c| c.clone()).collect()   
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

            let field_constraint = 
                fuse_field_width_constraints(universe, scope, typing_context, &constraints)?;
            if final_internal_map.insert(field.clone(), field_constraint).is_some() {
                panic!("FUSE ERROR");
            }
        }

        Ok(AbstractType::WidthConstraint(AbstractWidthConstraint { 
            fields: final_internal_map,
        }))
    }
}
