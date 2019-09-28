use std::collections::HashMap;

use crate::ast::{Ident, TypeAnnotationRef, WidthConstraint, AstNode};

use super::error::{AnalysisError, ApplicationError, TypeError as ATypeError};
use super::semantic_data::{FieldId, TypeId, TypeParamId, Universe};
use super::resolve_scope::ScopedData;
use super::type_resolver::resolve_types;

macro_rules! nill_check {
    ($type_args: expr) => {{
        if $type_args.is_some() {
            // TODO: error on type args to type cons int, bool, etc
            unimplemented!()
        }
    }};
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
pub struct AbstractFieldMap {
    pub fields: HashMap<FieldId, AbstractType>,
    pub field_map: HashMap<Ident, FieldId>,
}

#[derive(Debug, Clone)]
pub struct AbstractWidthConstraint {
    pub fields: HashMap<Ident, AbstractType>,
}

#[derive(Debug, Clone)]
pub enum AbstractType {

    Record {
        type_id: TypeId,
        abstract_field_map: AbstractFieldMap,
    },

    App {
        type_cons: TypeId,
        args: Vec<AbstractType>,
    },

    Array {
        element_kind: Box<AbstractType>,
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

    Param(TypeParamId),
    ConstrainedParam(TypeParamId, Box<AbstractType>),       // Needed to carry nominal type information later

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

    pub fn apply(&self, universe: &Universe, scope: &ScopedData) 
        -> Result<AbstractType, Vec<ATypeError>> {

        // TODO: Use typing context instead of ScopedData
        /*
        let param_map = scope
            .type_params()
            .map(|(id, constraint)| {
                match constraint {
                    Some(constraint) => 
                        (id, AbstractType::ConstrainedParam(id, Box::new(constraint.clone()))),
                    None => (id, AbstractType::Param(id))
                }
            })
            .collect::<HashMap<_, _>>();

        self.apply_internal(universe, &param_map)
        */
        unimplemented!();

    }

    /// Given an environment of type variables to abstract types, recursively substitute
    ///   any type variables in the map with their abstract type.
    ///
    /// No AbstractType returned by apply_internal() should have AbstractType::App() in its tree
    fn apply_internal(&self, universe: &Universe, map: &HashMap<TypeParamId, AbstractType>) 
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
                ref args,
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
                let (ok_args, err) = args
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
                    unimplemented!()
                }

                let type_cons_id = *type_cons;
                let type_cons = universe.get_type_cons(type_cons_id).unwrap();
                match type_cons {
                    TypeCons::Record {
                        ref type_params,
                        ref fields,
                        ref field_map,
                        ..
                    } => {
                        if type_params.len() != ok_args.len() {
                            // TODO: Type app error
                            unimplemented!();
                        }

                        // Make sure type arguments are all applied and mapped into the typing
                        //  environment for field applications
                        let old_map = map;
                        let mut new_map = map.clone();
                        let mut errors = Vec::new();
                        for ((type_param, constraint), arg) in 
                            type_params.iter().zip(ok_args.into_iter()) {

                            // TODO: Constraint checking
                            new_map.insert(type_param, arg);
                        }

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

                        if type_params.len() != args.len() {
                            unimplemented!();
                        }

                        // Make sure type arguments are all applied and mapped into the typing
                        //  environment for field applications
                        let old_map = map;
                        let mut new_map = map.clone();
                        for ((type_param, constraint), arg) in 
                            type_params.iter().zip(ok_args.into_iter()) {

                            // TODO: Constraint checking
                            new_map.insert(type_param, arg);
                        }

                        let mut errors = Vec::new();
                        let mut new_params = Vec::new();
                        for param in parameters.iter() {
                            let param_app_result = param.apply_internal(universe, &new_map);

                            match param_app_result {
                                Ok(p) => new_params.push(p),

                                Err(mut es) => errors.append(&mut es),
                            }
                        }

                        if errors.len() != 0 {
                            unimplemented!();
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

                        if type_params.len() != args.len() {
                            unimplemented!();
                        }

                        // Make sure type arguments are all applied and mapped into the typing
                        //  environment for field applications
                        let old_map = map;
                        let mut new_map = map.clone();
                        for ((type_param, constraint), arg) in 
                            type_params.iter().zip(ok_args.into_iter()) {

                            // TODO: Constraint checking
                            new_map.insert(type_param, arg.clone());
                        }

                        let return_type = return_type.apply_internal(universe, &new_map)?;

                        Ok(AbstractType::UncheckedFunction {
                            return_type: Box::new(return_type),
                        })
                    }

                    TypeCons::Int => primitive_apply!(args, AbstractType::Int),
                    TypeCons::Float => primitive_apply!(args, AbstractType::Float),
                    TypeCons::String => primitive_apply!(args, AbstractType::String),
                    TypeCons::Bool => primitive_apply!(args, AbstractType::Bool),
                    TypeCons::Unit => primitive_apply!(args, AbstractType::Unit),
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
                    unimplemented!()
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
                ref element_kind,
                ref size
            } => Ok(AbstractType::Array {
                element_kind: Box::new(element_kind.apply_internal(universe, map)?),
                size: *size,
            }),

            AbstractType::Function {
                ref parameters,
                ref return_type,
            } => {

                let (ok_parameters, err) = parameters.iter()
                    .map(|p| p.apply_internal(universe, map))
                    .fold((Vec::new(), Vec::new()), |(mut ok, mut err), result| {
                        match result {
                            Ok(app) => ok.push(app),
                            Err(mut e) => err.append(&mut e),
                        }

                        (ok, err)
                });

                if err.len() != 0 {
                    unimplemented!()
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
                
                let (ok_field_types, err) = width_constraint.fields
                    .iter()
                    .map(|(ident, at)| at.apply_internal(universe, map).map(|r| (ident, r)))
                    .fold((HashMap::new(), Vec::new()), |(mut ok, mut err), apply_result| {
                        match apply_result {
                            Ok((ref_ident, at)) => { ok.insert(ref_ident.clone(), at); },
                            Err(mut e) => err.append(&mut e),
                        };

                        (ok, err)
                });

                if err.len() != 0 {
                    unimplemented!();
                }
                let new_width = AbstractWidthConstraint {
                    fields: ok_field_types,
                };

                Ok(AbstractType::WidthConstraint(new_width))
            },

            AbstractType::Param(ref type_param_id) => {
                Ok(map
                    .get(type_param_id)
                    .expect("Type parameter missing from scope")
                    .clone()
                )
            }

            AbstractType::ConstrainedParam(ref type_param_id, ref constraint) => {
                Ok(AbstractType::ConstrainedParam(type_param_id.clone(),
                    Box::new(constraint.apply_internal(universe, map)?))
                )
            }

            AbstractType::Int => Ok(AbstractType::Int),
            AbstractType::Float => Ok(AbstractType::Float),
            AbstractType::String => Ok(AbstractType::String),
            AbstractType::Bool => Ok(AbstractType::Bool),
            AbstractType::Unit => Ok(AbstractType::Unit),
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
                // TODO: Use typing context instead of ScopedData
                /*
                if let Some((tp_id, _constraint)) = type_param {
                    // Do not allow type arguments on a type parameter
                    if typed_path.annotations().is_some() {
                        return Err(ATypeError::ParameterizedParameter {
                            ident: typed_path.module_path().0.get(0).unwrap().data().clone(),
                        }
                        .into());
                    }

                    return Ok(AbstractType::Param(tp_id));
                }
                */
                unimplemented!();
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
                Some(dat) => dat?,

                None => Vec::new(),
            };

            Ok(AbstractType::App {
                type_cons: type_cons,
                args: type_args,
            })
        }

        TypeAnnotationRef::Array(element_type, size) => {
            let element_type_app = type_app_from_annotation(universe, scope, element_type.data())?;

            Ok(AbstractType::Array {
                element_kind: Box::new(element_type_app),
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
                        .map(|p| type_app_from_annotation(universe, scope, p.data()))
                        .collect::<Result<Vec<_>, _>>()
                })
                .unwrap_or(Ok(Vec::new()))?;

            let return_type = return_type
                .map(|return_type| type_app_from_annotation(universe, scope, return_type.data()))
                .unwrap_or(Ok(AbstractType::Unit))?;


            Ok(AbstractType::Function {
                parameters: param_types,
                return_type: Box::new(return_type),
            })
        }

        TypeAnnotationRef::WidthConstraint(constraints) => {
            // TODO: Fuse constraints
            unimplemented!();
        }
    }
}
