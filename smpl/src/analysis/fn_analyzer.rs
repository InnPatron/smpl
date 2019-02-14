use std::rc::Rc;
use std::collections::HashSet;
use petgraph::graph::NodeIndex;

use crate::feature::*;
use crate::ast;

use crate::span::Span;

use super::metadata::FnLayout;

use super::error::*;
use super::type_cons_gen::generate_anonymous_fn_type;
use super::type_cons::*;
use super::linear_cfg_traversal::*;
use super::control_flow::CFG;
use super::control_data::*;
use super::typed_ast::*;
use super::semantic_data::{BindingInfo, FnId, ModuleId, Program, ScopedData, TypeId, Universe, VarId};

struct FnAnalyzer<'a> {
    program: &'a mut Program,
    fn_return_type: Type,
    current_scope: ScopedData,
    scope_stack: Vec<ScopedData>,

    module_id: ModuleId,

    // metadata
    locals: Vec<(VarId, Type)>,
}

pub fn analyze_fn(
    program: &mut Program,
    global_scope: &ScopedData,
    fn_id: FnId,
    module_id: ModuleId,
) -> Result<(), AnalysisError> {

    let func = program.universe().get_fn(fn_id);
    let fn_type_id = func.fn_type();

    let fn_type = TypeApp::Applied {
        type_cons: fn_type_id,
        args: None,
    };

    let fn_type = fn_type.apply(program.universe())?;

    let (return_type, fn_params) = match fn_type {
        Type::Function {
            return_type: ref return_type,
            parameters: ref params,
        } => (return_type.clone(), params),

        ref t @ _ => panic!("{} not mapped to a function but a {:?}", fn_id, t),
    };

    let mut analyzer = FnAnalyzer {
        program: program,
        fn_return_type: *return_type,
        current_scope: global_scope.clone(),
        scope_stack: Vec::new(),
        locals: Vec::new(),
        module_id: module_id,
    };

    let mut param_types = Vec::new();

    // Add parameters to the current scope.
    for (param_type, meta) in fn_params
            .iter()
            .zip(analyzer.program.metadata().function_param_ids(fn_id).iter())
    {
        let v_id = meta.var_id();
        let p_name = meta.name();
        analyzer
            .current_scope
            .insert_var(p_name.clone(), v_id, param_type.clone());

        param_types.push((v_id, param_type.clone()));
    }


    let func = analyzer.program.universe().get_fn(fn_id);
    let cfg = func.cfg();

    // Restrain lifetime of traverser to move analyzer.locals
    {
        let traverser = Traverser::new(&*cfg, &mut analyzer);

        traverser.traverse()?;
    }

    // TODO: function layout
    /*
    analyzer.program.metadata_mut().insert_fn_layout(
        fn_id,
        FnLayout::new(analyzer.locals, param_types, fn_return_type_id),
    );
    */

    return_trace(&*cfg)
}

// TODO: maybe add reverse traverser?
fn return_trace(cfg: &CFG) -> Result<(), AnalysisError> {
    let end = cfg.end();
    let scope_exit = cfg.previous(end);

    let unknown = cfg.previous(scope_exit);

    let mut traced = HashSet::new();
    let mut node_stack = Vec::new();
    node_stack.push(unknown);

    for _ in 0..cfg.graph().node_count() {
        let to_trace = node_stack.pop();
        match to_trace {
            Some(id) => {
                if (traced.contains(&id)) == false {
                    traced.insert(id);

                    let more_to_trace = return_check_id(cfg, id)?;
                    if let Some(vec) = more_to_trace {
                        node_stack.extend(vec);
                    }
                }
            }

            None => return Ok(()),
        }
    }

    unreachable!();
}

fn return_check_id(cfg: &CFG, id: NodeIndex) -> Result<Option<Vec<NodeIndex>>, AnalysisError> {
    use super::control_data::Node;

    match *cfg.node_weight(id) {
        Node::Return(..) => Ok(None),

        Node::BranchMerge(_) => Ok(Some(cfg.before_branch_merge(id))),

        Node::ExitScope => Ok(Some(vec![cfg.previous(id)])),

        _ => return Err(ControlFlowError::MissingReturn.into()),
    }
}

fn resolve_bin_op(
    universe: &Universe,
    op: &ast::BinOp,
    lhs: Type,
    rhs: Type,
    span: Span,
) -> Result<Type, AnalysisError> {
    use crate::ast::BinOp::*;

    let expected_int = Type::Int;

    let expected_float = Type::Float;

    let expected_string = Type::String;

    let expected_bool = Type::Bool;

    let resolve_type = match *op {
        Add | Sub | Mul | Div | Mod => match (&lhs, &rhs) {
            (&Type::Int, &Type::Int) => Type::Int,
            (&Type::Float, &Type::Float) => Type::Float,

            _ => return Err(TypeError::BinOp {
                op: op.clone(),
                expected: vec![expected_int, expected_float],
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                span: span,
            }.into()),
        },

        LogicalAnd | LogicalOr => match (&lhs, &rhs) {
            (&Type::Bool, &Type::Bool) => Type::Bool,
            _ => return Err(TypeError::BinOp {
                op: op.clone(),
                expected: vec![expected_bool],
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                span: span,
            }.into()),
        },

        GreaterEq | LesserEq | Greater | Lesser => match (&lhs, &rhs) {
            (&Type::Int, &Type::Int) => Type::Bool,
            (&Type::Float, &Type::Float) => Type::Bool,

            _ => return Err(TypeError::BinOp {
                op: op.clone(),
                expected: vec![expected_int, expected_float],
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                span: span,
            }.into()),
        },

        Eq | InEq => {
            if lhs == rhs {
                Type::Bool 
            } else {
                return Err(TypeError::LhsRhsInEq(lhs.clone(), rhs.clone(), span).into());
            }
        }
    };

    Ok(resolve_type)
}

fn resolve_uni_op(
    universe: &Universe,
    op: &ast::UniOp,
    tmp_type: Type,
    span: Span,
) -> Result<Type, AnalysisError> {
    use crate::ast::UniOp::*;

    let expected_int = Type::Int;

    let expected_float = Type::Float;

    let expected_bool = Type::Bool;

    match *op {
        Negate => match tmp_type {
            Type::Int | Type::Float => Ok(tmp_type.clone()),
            _ => Err(TypeError::UniOp {
                op: op.clone(),
                expected: vec![expected_int, expected_float],
                expr: tmp_type.clone(),
                span: span,
            }.into()),
        },

        LogicalInvert => match tmp_type {
            Type::Bool => Ok(tmp_type.clone()),
            _ => Err(TypeError::UniOp {
                op: op.clone(),
                expected: vec![expected_bool],
                expr: tmp_type.clone(),
                span: span,
            }.into()),
        },

        _ => unimplemented!(),
    }
}

impl<'a> FnAnalyzer<'a> {
    fn resolve_field_access(
        &mut self,
        expr: &Expr,
        field_access: &FieldAccess,
        span: Span,
    ) -> Result<Type, AnalysisError> {

        let path = field_access.path();
        let path_iter = path.path().iter();

        let mut current_type;

        let (var_id, var_type) = self.current_scope.var_info(path.root_name())?;
        let root_var_type = var_type.clone();
        let root_var_id = var_id;

        path.set_root_var(root_var_id);
        path.set_root_var_type(root_var_type.clone());


        current_type = root_var_type.clone();

        if let Some(e) = path.root_indexing_expr() {
            let indexing_type = expr
                .get_tmp(e)
                .value()
                .get_type()
                .unwrap();

            match indexing_type {
                Type::Int => (),
                _ => {
                    return Err(TypeError::InvalidIndex {
                        found: indexing_type.clone(),
                        span: expr.get_tmp(e).span(),
                    }.into());
                }
            }

            match var_type {
                Type::Array { 
                    element_type: element_type,
                    ..
                } => {
                    current_type = *element_type;
                }
                _ => {
                    return Err(TypeError::NotAnArray {
                        found: var_type.clone(),
                        span: expr.get_tmp(e).span(),
                    }.into());
                }
            }
        }

        for (index, field) in path_iter.enumerate() {
            let next_type;
            match current_type {
                Type::Record {
                    fields: ref fields,
                    field_map: ref field_map,
                    ..
                } => {
                    match *field {
                        PathSegment::Ident(ref field) => {
                            let name = field.name();
                            let field_id = field_map.get(name).ok_or(TypeError::UnknownField {
                                name: name.clone(),
                                struct_type: current_type.clone(),
                                span: span,
                            })?;
                            next_type = fields.get(field_id).unwrap().clone();

                            field.set_field_id(field_id.clone());
                            field.set_field_type(current_type.clone());
                        }

                        PathSegment::Indexing(ref field, ref indexing) => {
                            let name = field.name();
                            let field_id = field_map.get(name).ok_or(TypeError::UnknownField {
                                name: name.clone(),
                                struct_type: current_type.clone(),
                                span: span,
                            })?;

                            let field_type = fields.get(field_id).unwrap();

                            field.set_field_id(field_id.clone());
                            field.set_field_type(field_type.clone());

                            let indexing_type = expr.get_tmp(*indexing)
                                .value()
                                .get_type()
                                .unwrap();

                            match indexing_type {
                                Type::Int => (),

                                _ => {
                                    return Err(TypeError::InvalidIndex {
                                        found: indexing_type.clone(),
                                        span: expr.get_tmp(*indexing).span(),
                                    }.into());
                                }
                            };

                            match *field_type {
                                Type::Array{
                                    element_type: ref element_type,
                                    size: _,
                                } => {
                                    next_type = *(element_type.clone());
                                }

                                _ => {
                                    return Err(TypeError::NotAnArray {
                                        found: field_type.clone(),
                                        span: span,
                                    }.into());
                                }
                            }
                        }
                    }
                },

                _ => {
                    return Err(TypeError::FieldAccessOnNonStruct {
                        path: field_access.raw_path().clone(),
                        index: index,
                        invalid_type: current_type,
                        root_type: root_var_type,
                        span: span,
                    }.into());
                }
            }

            current_type = next_type.clone();
        }

        let accessed_field_type = current_type;
        field_access.set_field_type(accessed_field_type.clone());

        Ok(accessed_field_type)
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<Type, AnalysisError> {
        let mut expr_type = None;

        for tmp_id in expr.execution_order() {
            let tmp = expr.get_tmp(*tmp_id);
            let tmp_type;
            match *tmp.value().data() {
                Value::Literal(ref literal) => {
                    use crate::ast::Literal;

                    let universe = self.program.universe();
                    let lit_type = match *literal {
                        Literal::Int(_) => Type::Int,
                        Literal::Float(_) => Type::Float,
                        Literal::String(_) => Type::String,
                        Literal::Bool(_) => Type::Bool,
                    };

                    tmp_type = lit_type;
                }

                Value::StructInit(ref init) => {
                    // Get type info
                    let type_name = init.type_name();
                    let tmp_type_name = type_name.clone().into();
                    let struct_type_id = self.current_scope
                        .type_cons(self.program.universe(), &tmp_type_name)
                        .ok_or(AnalysisError::UnknownType(type_name.clone()))?;

                    // TODO: Take into account type arguments
                    let struct_type = TypeApp::Applied {
                        type_cons: struct_type_id,
                        args: None,
                    }.apply(self.program.universe())?;

                    // Check if type is a struct.
                    let (struct_type_id, fields, field_map) = match struct_type {
                        Type::Record {
                            type_id: struct_type_id,
                            fields: ref fields,
                            field_map: ref field_map,
                            ..
                        } => (struct_type_id, fields, field_map),

                        _ => {
                            return Err(TypeError::NotAStruct {
                                type_name: type_name.clone(),
                                found: struct_type,
                                span: tmp.span(),
                            }.into());
                        }
                    };

                    // Check if the struct is an 'opaque' type (i.e. cannot be initialized by SMPL
                    // code)
                    if self.program.metadata().is_opaque(struct_type_id) {
                        return Err(TypeError::InitOpaqueType {
                            struct_type: struct_type,
                            span: tmp.span(),
                        }.into());
                    }

                    init.set_struct_type(struct_type.clone());
                    if let Err(unknown_fields) = init.set_field_init(self.program.universe()) {
                        // TODO: Allow for multiple errors
                        /*let ident = struct_type.get_ident(id);
                                return Err(TypeError::UnknownField {
                                    name: ident.clone(),
                                    struct_type: struct_type_id,
                                }.into());
                                */
                        // No field initializations but the struct type has fields
                        return Err(TypeError::StructNotFullyInitialized {
                            type_name: type_name.clone(),
                            struct_type: struct_type,
                            missing_fields: unknown_fields,
                            span: tmp.span(),
                        }.into());
                    }

                    match init.field_init() {
                        Some(init_list) => {
                            if init_list.len() != fields.len() {
                                // Missing fields -> struct is not fully initialized
                                return Err(TypeError::StructNotFullyInitialized {
                                    type_name: type_name.clone(),
                                    struct_type: struct_type.clone(),
                                    missing_fields: {
                                        let inits = init_list
                                            .iter()
                                            .map(|&(ref name, _)| name.clone())
                                            .collect::<Vec<_>>();

                                        field_map
                                            .iter()
                                            .filter(|&(_, ref id)| !inits.contains(id))
                                            .map(|(ident, _)| ident.clone())
                                            .collect::<Vec<_>>()
                                    },
                                    span: tmp.span(),
                                }.into());
                            }
                            // Go threw initialization list and check expressions
                            for (ref id, ref typed_tmp_id) in init_list {

                                let field_type = fields.get(id).unwrap();

                                let tmp = expr.get_tmp(*typed_tmp_id.data());
                                let tmp_type = tmp.value().get_type().unwrap();

                                typed_tmp_id.set_type(tmp_type.clone());

                                // Expression type the same as the field type?
                                if tmp_type != *field_type {
                                    return Err(TypeError::UnexpectedType {
                                        found: tmp_type,
                                        expected: field_type.clone(),
                                        span: tmp.span(),
                                    }.into());
                                }
                            }
                        }

                        None => {
                            if fields.len() != 0 {
                                // Missing fields -> struct is not fully initialized
                                return Err(TypeError::StructNotFullyInitialized {
                                    type_name: type_name.clone(),
                                    struct_type: unimplemented!(),
                                    missing_fields: {
                                        field_map
                                            .iter()
                                            .map(|(ident, _)| ident.clone())
                                            .collect::<Vec<_>>()
                                    },
                                    span: tmp.span(),
                                }.into());
                            }
                        }
                    }

                    tmp_type = struct_type;
                }

                Value::Binding(ref var) => match self.current_scope.binding_info(var.ident())? {
                    BindingInfo::Var(var_id, type_id) => {
                        var.set_id(var_id);
                        tmp_type = type_id;
                    }

                    BindingInfo::Fn(fn_id) => {
                        self.program.features_mut().add_feature(FUNCTION_VALUE);

                        // Bindings to unchecked functions are OK because:
                        // 1) Attempting to use the binding will trigger type checking
                        // 2) Cannot write out unchecked function types currently
                        /*
                        if self.program
                            .metadata_mut()
                            .is_builtin_params_unchecked(fn_id)
                        {
                            return Err(AnalysisError::UncheckedFunctionBinding(var.ident().clone()));
                        }
                        */

                        let fn_type_id = if self.program.metadata().is_builtin(fn_id) {
                            let f = self.program.universe().get_builtin_fn(fn_id);
                            f.fn_type().clone()
                        } else {
                            let f = self.program.universe().get_fn(fn_id);
                            f.fn_type().clone()
                        };

                        let fn_type = TypeApp::Applied {
                            type_cons: fn_type_id,
                            args: None,
                        }.apply(self.program.universe())?;

                        var.set_id(fn_id);
                        tmp_type = fn_type;
                    }
                },

                Value::FieldAccess(ref field_access) => {
                    let accessed_field_type_id =
                        self.resolve_field_access(expr, field_access, tmp.span())?;

                    tmp_type = accessed_field_type_id;
                }

                Value::BinExpr(ref op, ref lhs, ref rhs) => {
                    let lhs_type = expr
                        .get_tmp(*lhs.data())
                        .value()
                        .get_type()
                        .unwrap();

                    let rhs_type = 
                        expr
                        .get_tmp(*rhs.data())
                        .value()
                        .get_type()
                        .unwrap();

                    lhs.set_type(lhs_type.clone());
                    rhs.set_type(rhs_type.clone());

                    tmp_type = resolve_bin_op(
                        self.program.universe(),
                        op,
                        lhs_type,
                        rhs_type,
                        tmp.span(),
                    )?;
                }

                Value::UniExpr(ref op, ref uni_e) => {
                    let uni_tmp_type = expr
                        .get_tmp(*uni_e.data())
                        .value()
                        .get_type()
                        .unwrap();

                    uni_e.set_type(uni_tmp_type.clone());

                    tmp_type =
                        resolve_uni_op(self.program.universe(), 
                                       op, 
                                       uni_tmp_type, 
                                       tmp.span())?;
                }

                Value::FnCall(ref fn_call) => {
                    // Search for the function

                    let fn_value_tmp_type = expr
                        .get_tmp(fn_call.fn_value())
                        .value()
                        .get_type()
                        .unwrap();
                    
                    // Check args and parameters align
                    match fn_value_tmp_type {
                        Type::Function {
                            parameters: ref params,
                            return_type: ref return_type,
                            ..
                        } => {
                            tmp_type = *(return_type.clone());

                            let arg_type_ids = fn_call.args().map(|ref vec| {
                                vec.iter()
                                    .map(|ref tmp_id| {
                                        let tmp = expr.get_tmp(*tmp_id.data());
                                        let tmp_value = tmp.value();
                                        let tmp_value_type = tmp_value.get_type().unwrap();
                                        tmp_id.set_type(tmp_value_type.clone());
                                        tmp_value_type
                                    })
                                    .collect::<Vec<_>>()
                            });

                            match arg_type_ids {
                                Some(arg_type_ids) => {
                                    if params.len() != arg_type_ids.len() {
                                        return Err(TypeError::Arity {
                                            fn_type: fn_value_tmp_type.clone(),
                                            found_args: arg_type_ids.len(),
                                            expected_param: params.len(),
                                            span: tmp.span(),
                                        }.into());
                                    }

                                    let fn_param_type_ids = params.iter();

                                    for (index, (arg_type, param_type)) in
                                        arg_type_ids.iter().zip(fn_param_type_ids).enumerate()
                                    {
                                        if arg_type != param_type {
                                            return Err(
                                                TypeError::ArgMismatch {
                                                    fn_type: fn_value_tmp_type.clone(),
                                                    index: index,
                                                    arg: arg_type.clone(),
                                                    param: param_type.clone(),
                                                    span: tmp.span(),
                                                }.into(),
                                            );
                                        }
                                    }
                                }

                                None => {
                                    if params.len() != 0 {
                                        return Err(TypeError::Arity {
                                            fn_type: fn_value_tmp_type.clone(),
                                            found_args: 0,
                                            expected_param: params.len(),
                                            span: tmp.span(),
                                        }.into());
                                    }
                                }
                            }
                        },

                        Type::UncheckedFunction { 
                            return_type: return_type,
                            ..
                        } => {
                            tmp_type = *return_type;
                        },

                        t @ _ => panic!("Function call on a non-function type: {:?}", t),
                    };
                }

                Value::ArrayInit(ref init) => {
                    self.program.features_mut().add_feature(STATIC_ARRAY);
                    match *init {
                        ArrayInit::List(ref vec) => {
                            let size = vec.len() as u64;
                            let element_types = vec.iter().map(|ref tmp_id| {
                                let tmp = expr.get_tmp(*tmp_id.data());
                                let tmp_value = tmp.value();
                                let tmp_value_type = tmp_value.get_type().unwrap();
                                tmp_id.set_type(tmp_value_type.clone());
                                (tmp_value_type, tmp.span())
                            });

                            let mut expected_element_type = None;

                            for (i, (current_element_type, span)) in element_types.enumerate() {
                                
                                if expected_element_type.is_none() {
                                    expected_element_type = Some(current_element_type);
                                    continue;
                                }

                                let expected_element_type = expected_element_type
                                    .as_ref()
                                    .unwrap();

                                if current_element_type != *expected_element_type {
                                    return Err(TypeError::HeterogenousArray {
                                        expected: expected_element_type.clone(),
                                        found: current_element_type,
                                        index: i,
                                        span: span,
                                    }.into());
                                }
                            }

                            let array_type = Type::Array {
                                element_type: Box::new(expected_element_type.unwrap()),
                                size: size,
                            };

                            tmp_type = array_type;
                        }

                        ArrayInit::Value(ref val, size) => {
                            let tmp_val = expr.get_tmp(*val.data());
                            let tmp_concrete_value = tmp_val.value();
                            let array_tmp_type = tmp_concrete_value.get_type().unwrap();

                            val.set_type(array_tmp_type.clone());

                            let element_type = array_tmp_type;

                            let array_type = Type::Array {
                                element_type: Box::new(element_type),
                                size: size,
                            };

                            // TODO: Insert array type into metadata?
                            /*
                            self.program
                                .metadata_mut()
                                .insert_array_type(self.module_id, array_type);
                            */
                            tmp_type = array_type;
                        }
                    }
                }

                Value::Indexing(ref indexing) => {

                    let expected_element_type = {

                        // Check type of array
                        let tmp = expr.get_tmp(*indexing.array.data());
                        let tmp_value = tmp.value();
                        let tmp_type = tmp_value.get_type().unwrap();

                        indexing.array.set_type(tmp_type.clone());

                        match &tmp_type {
                            Type::Array {
                                element_type: ref element_type,
                                ..
                            } => element_type.clone(),

                            _ => {
                                return Err(TypeError::NotAnArray {
                                    found: tmp_type.clone(),
                                    span: tmp.span(),
                                }.into());
                            }
                        }
                    };

                    {
                        // Check type of indexer
                        let tmp = expr.get_tmp(*indexing.indexer.data());
                        let tmp_value = tmp.value();
                        let tmp_type = tmp_value.get_type().unwrap();

                        indexing.indexer.set_type(tmp_type.clone());

                        match &tmp_type {
                            Type::Int => (),

                            _ => {
                                return Err(TypeError::InvalidIndex {
                                    found: tmp_type.clone(),
                                    span: tmp.span(),
                                }.into());
                            }
                        }
                    }

                    tmp_type = *expected_element_type;
                }

                Value::ModAccess(ref access) => {
                    let fn_id = self.current_scope.get_fn(&access.path())?;
                    let func = self.program.universe().get_fn(fn_id);

                    let fn_type = func.fn_type();

                    tmp_type = TypeApp::Applied {
                        type_cons: fn_type.clone(),
                        args: None,
                    }.apply(self.program.universe())?;

                    self.program.features_mut().add_feature(MOD_ACCESS);
                }

                Value::AnonymousFn(ref a_fn) => {
                    let fn_id = self.program.universe().new_fn_id();
                    let func = a_fn.a_fn();

                    let (func_scope, fn_type) =
                        generate_anonymous_fn_type(self.program, &self.current_scope, fn_id, func)?;

                    let fn_type_id = self.program
                        .universe_mut()
                        .insert_type_cons(fn_type);

                    let fn_type = TypeApp::Applied {
                        type_cons: fn_type_id,
                        args: None,
                    }.apply(self.program.universe())?;

                    let cfg = CFG::generate(self.program.universe_mut(), func.body.clone(), &fn_type)?;

                    a_fn.set_fn_id(fn_id);

                    self.program
                        .universe_mut()
                        .insert_fn(fn_id, fn_type_id, func_scope, cfg);

                    // Since anonymous functions are ALWAYS inside another function
                    // Assume the global scope is at the bottom of the scope stack
                    analyze_fn(
                        self.program,
                        self.scope_stack.get(0).unwrap(),
                        fn_id,
                        self.module_id,
                    )?;

                    // TODO: Construct type directly
                    tmp_type = TypeApp::Applied {
                        type_cons: fn_type_id,
                        args: None
                    }.apply(self.program.universe())?;

                    self.program.features_mut().add_feature(ANONYMOUS_FN);
                },

                Value::TypeInst(ref type_inst) => {
                    let fn_id = self.current_scope
                        .get_fn(type_inst.path())?;

                    let func = self.program.universe().get_fn(fn_id);
                    let func_type_id = func.fn_type();

                    let type_args = type_inst.args()
                        .iter()
                        .map(|ann| {
                            type_app_from_annotation(self.program.universe_mut(),
                                &self.current_scope,
                                ann)
                        })
                    .collect::<Result<Vec<_>, _>>()?;

                    tmp_type = TypeApp::Applied {
                        type_cons: func_type_id,
                        args: Some(type_args),
                    }.apply(self.program.universe())?;
                },
            }

            tmp.value().set_type(tmp_type.clone());
            expr_type = Some(tmp_type);
        }

        Ok(expr_type.unwrap())
    }
}

impl<'a> Passenger<AnalysisError> for FnAnalyzer<'a> {
    fn start(&mut self, _id: NodeIndex) -> Result<(), AnalysisError> {
        Ok(())
    }

    fn end(&mut self, _id: NodeIndex) -> Result<(), AnalysisError> {
        Ok(())
    }

    fn branch_split(&mut self, _id: NodeIndex, _: &BranchingData) -> Result<(), AnalysisError> {
        Ok(())
    }

    fn branch_merge(&mut self, _id: NodeIndex, _: &BranchingData) -> Result<(), AnalysisError> {
        Ok(())
    }

    fn loop_head(&mut self, _id: NodeIndex, _ld: &LoopData) -> Result<(), AnalysisError> {
        Ok(())
    }

    fn loop_foot(&mut self, _id: NodeIndex, _ld: &LoopData) -> Result<(), AnalysisError> {
        Ok(())
    }

    fn cont(&mut self, _id: NodeIndex, _ld: &LoopData) -> Result<(), AnalysisError> {
        Ok(())
    }

    fn br(&mut self, _id: NodeIndex, _ld: &LoopData) -> Result<(), AnalysisError> {
        Ok(())
    }

    fn enter_scope(&mut self, _id: NodeIndex) -> Result<(), AnalysisError> {
        self.scope_stack.push(self.current_scope.clone());
        Ok(())
    }

    fn exit_scope(&mut self, _id: NodeIndex) -> Result<(), AnalysisError> {
        let popped = self.scope_stack.pop()
                                     .expect("If CFG was generated properly and the graph is being walked correctly, there should be a scope to pop");
        self.current_scope = popped;
        Ok(())
    }

    fn local_var_decl(&mut self, _id: NodeIndex, var_decl: &LocalVarDeclData) -> Result<(), AnalysisError> {
        let var_decl = &var_decl.decl;

        let expr_type = self.resolve_expr(var_decl.init_expr())?;

        let name = var_decl.var_name().clone();
        let var_id = var_decl.var_id();
        let var_type_annotation = var_decl.type_annotation();

        let var_type = var_decl
            .type_annotation()
            .map(|ann| {
                type_app_from_annotation(self.program.universe_mut(),
                    &self.current_scope,
                    ann)
            });

        let var_type = match var_type {
            Some(app_gen_result) => {
                app_gen_result?
                    .apply(self.program.universe())?
            },
    

            None => expr_type.clone(),
        };
        
        var_decl.set_type(var_type.clone());

        if var_type == expr_type {
            self.current_scope.insert_var(name, 
                                          var_id, 
                                          var_type.clone());
        } else {
            return Err(TypeError::LhsRhsInEq(var_type, expr_type, var_decl.span()).into());
        }

        // Local variable types metadata
        self.locals.push((var_id, var_type));

        Ok(())
    }

    fn assignment(&mut self, _id: NodeIndex, assignment: &AssignmentData) -> Result<(), AnalysisError> {
        let assignment = &assignment.assignment;
        let assignee = assignment.assignee();

        if assignment.access().order_length() > 0 {
            self.resolve_expr(assignment.access())?;
        }
        let assignee_type = self.resolve_field_access(assignment.access(), 
                                                      assignee, 
                                                      assignment.access_span())?;

        let expr_type = self.resolve_expr(assignment.value())?;

        let assignment_span = Span::combine(assignment.access_span(), assignment.value().span());

        if expr_type != assignee_type {
            return Err(TypeError::LhsRhsInEq(assignee_type, expr_type, assignment_span).into());
        }

        Ok(())
    }

    fn expr(&mut self, _id: NodeIndex, expr: &ExprData) -> Result<(), AnalysisError> {
        self.resolve_expr(&expr.expr).map(|_| ())
    }

    fn ret(&mut self, _id: NodeIndex, rdata: &ReturnData) -> Result<(), AnalysisError> {
        let expr = rdata.expr.as_ref();
        let span = rdata.span.clone();
        let expr_type = match expr {
            Some(ref expr) => self.resolve_expr(expr)?,

            None => Type::Unit
        };

        if expr_type != self.fn_return_type {
            return Err(TypeError::InEqFnReturn {
                expr: expr_type,
                fn_return: self.fn_return_type.clone(),
                return_span: span,
            }.into());
        }

        Ok(())
    }

    fn loop_condition(&mut self, _id: NodeIndex, condition: &ExprData) -> Result<(), AnalysisError> {
        let condition = &condition.expr;
        let expr_type = self.resolve_expr(condition)?;

        let expected = Type::Bool;

        if expr_type != expected {
            return Err(TypeError::UnexpectedType {
                found: expr_type,
                expected: expected,
                span: condition.span(),
            }.into());
        }

        Ok(())
    }

    fn loop_start_true_path(&mut self, _id: NodeIndex) -> Result<(), AnalysisError> {
        // Do nothing
        Ok(())
    }

    fn loop_end_true_path(&mut self, _id: NodeIndex) -> Result<(), AnalysisError> {
        // Do nothing
        Ok(())
    }

    fn branch_condition(&mut self, _id: NodeIndex, condition: &ExprData) -> Result<(), AnalysisError> {
        let condition = &condition.expr;
        let expr_type = self.resolve_expr(condition)?;
        
        let expected = Type::Bool;

        if expr_type != expected {
            return Err(TypeError::UnexpectedType {
                found: expr_type,
                expected: expected,
                span: condition.span(),
            }.into());
        }

        Ok(())
    }

    fn branch_start_true_path(&mut self, _id: NodeIndex) -> Result<(), AnalysisError> {
        // Do nothing
        Ok(())
    }

    fn branch_start_false_path(&mut self, _id: NodeIndex) -> Result<(), AnalysisError> {
        // Do nothing
        Ok(())
    }

    fn branch_end_true_path(&mut self, _id: NodeIndex, _: &BranchingData) -> Result<(), AnalysisError> {
        // Do nothing
        Ok(())
    }

    fn branch_end_false_path(&mut self, _id: NodeIndex, _: &BranchingData) -> Result<(), AnalysisError> {
        // Do nothing
        Ok(())
    }
}
