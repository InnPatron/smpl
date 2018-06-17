use std::rc::Rc;
use std::collections::HashSet;
use petgraph::graph::NodeIndex;

use feature::*;
use ast;
use err::*;

use span::Span;

use super::metadata::{Metadata, FnLayout};


use super::smpl_type::*;
use super::linear_cfg_traversal::*;
use super::control_flow::CFG;
use super::control_data::*;
use super::typed_ast::*;
use super::semantic_data::{VarId, FnId, ScopedData, TypeId, Universe, TypeConstructor, ModuleId, Program, BindingInfo};


struct FnAnalyzer<'a, 'b, 'c> {
    universe: &'a Universe,
    metadata: &'b mut Metadata,
    features: &'c mut PresentFeatures,
    fn_return_type: Rc<SmplType>,
    fn_return_type_id: TypeId,
    current_scope: ScopedData,
    scope_stack: Vec<ScopedData>,

    module_id: ModuleId,

    // metadata
    locals: Vec<(VarId, TypeId)>,
}

pub fn analyze_fn(
    program: &mut Program,
    global_scope: &ScopedData,
    fn_id: FnId,
    module_id: ModuleId,
) -> Result<(), Err> {
    let fn_return_type;
    let fn_return_type_id;
    let unknown_type = {
        let func = program.universe().get_fn(fn_id);
        program.universe().get_type(func.type_id())
    };

    let func_type;


    match *unknown_type {
        SmplType::Function(ref fn_type) => {
            fn_return_type = program.universe().get_type(fn_type.return_type.clone());
            fn_return_type_id = fn_type.return_type;
            func_type = fn_type;
        }

        ref t @ _ => panic!("{} not mapped to a function but a {:?}", fn_id, t),
    }

    let (u, m, f) = program.analysis_context();

    let cfg = u.get_fn(fn_id).cfg();

    let mut analyzer = FnAnalyzer {
        universe: u,
        metadata: m,
        features: f,
        fn_return_type: fn_return_type,
        fn_return_type_id: fn_return_type_id,
        current_scope: global_scope.clone(),
        scope_stack: Vec::new(),
        locals: Vec::new(),
        module_id: module_id,
    };

    let mut param_types = Vec::new();

    // Add parameters to the current scope.
    for (param_type_id, meta) in func_type.params.iter()
        .zip(analyzer.metadata.function_param_ids(fn_id).iter()) {
        let v_id = meta.var_id();
        let p_name = meta.name();
        let param_type_id = *param_type_id;
        analyzer
            .current_scope
            .insert_var(p_name.clone(), v_id, param_type_id);

        param_types.push((v_id, param_type_id));
    }

    // Restrain lifetime of traverser to move analyzer.locals
    {
        let traverser = Traverser::new(cfg, &mut analyzer);

        traverser.traverse()?;
    }

    analyzer.metadata.insert_fn_layout(fn_id, FnLayout::new(
            analyzer.locals, 
            param_types,
            fn_return_type_id));

    return_trace(cfg)
}

// TODO: maybe add reverse traverser?
fn return_trace(cfg: &CFG) -> Result<(), Err> {

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

fn return_check_id(cfg: &CFG, id: NodeIndex) -> Result<Option<Vec<NodeIndex>>, Err> {
    use super::control_data::Node;
 
    match *cfg.node_weight(id) {
        Node::Return(..) => Ok(None),

        Node::BranchMerge(_) => Ok(Some(cfg.before_branch_merge(id))),

        Node::ExitScope => Ok(Some(vec![cfg.previous(id)])),

        _ => return Err(ControlFlowErr::MissingReturn.into()),
    }
}

fn resolve_bin_op(
    universe: &Universe,
    op: &ast::BinOp,
    lhs: TypeId,
    rhs: TypeId,
    span: Span,
) -> Result<TypeId, Err> {
    use ast::BinOp::*;

    let lh_type = universe.get_type(lhs);
    let rh_type = universe.get_type(rhs);

    match *op {
        Add | Sub | Mul | Div | Mod => {
            match (&*lh_type, &*rh_type) {
                (&SmplType::Int, &SmplType::Int) => Ok(universe.int()),
                (&SmplType::Float, &SmplType::Float) => Ok(universe.float()),

                _ => Err(TypeErr::BinOp {
                    op: op.clone(),
                    expected: vec![universe.int(), universe.float()],
                    lhs: lhs,
                    rhs: rhs,
                    span: span,
                }.into()),
            }
        }

        LogicalAnd | LogicalOr => match (&*lh_type, &*rh_type) {
            (&SmplType::Bool, &SmplType::Bool) => Ok(universe.boolean()),
            _ => Err(TypeErr::BinOp {
                op: op.clone(),
                expected: vec![universe.boolean()],
                lhs: lhs,
                rhs: rhs,
                span: span,
            }.into()),
        },

        GreaterEq | LesserEq | Greater | Lesser => {
            match (&*lh_type, &*rh_type) {
                (&SmplType::Int, &SmplType::Int) => Ok(universe.boolean()),
                (&SmplType::Float, &SmplType::Float) => Ok(universe.boolean()),

                _ => Err(TypeErr::BinOp {
                    op: op.clone(),
                    expected: vec![universe.int(), universe.float()],
                    lhs: lhs,
                    rhs: rhs,
                    span: span,
                }.into()),
            }
        },

        Eq | InEq => {
            if *lh_type == *rh_type {
                Ok(universe.boolean())
            } else {
                Err(TypeErr::LhsRhsInEq(lhs, rhs, span).into())
            }
        }
    }
}

fn resolve_uni_op(
    universe: &Universe,
    op: &ast::UniOp,
    tmp_type_id: TypeId,
    span: Span,
) -> Result<TypeId, Err> {
    use ast::UniOp::*;

    let tmp_type = universe.get_type(tmp_type_id);

    match *op {
        Negate => match &*tmp_type {
            &SmplType::Int | &SmplType::Float => Ok(tmp_type_id),
            _ => Err(TypeErr::UniOp {
                op: op.clone(),
                expected: vec![universe.int(), universe.float()],
                expr: tmp_type_id,
                span: span,
            }.into()),
        },

        LogicalInvert => match &*tmp_type {
            &SmplType::Bool => Ok(tmp_type_id),
            _ => Err(TypeErr::UniOp {
                op: op.clone(),
                expected: vec![universe.boolean()],
                expr: tmp_type_id,
                span: span,
            }.into()),
        },

        _ => unimplemented!(),
    }
}

impl<'a, 'b, 'c> FnAnalyzer<'a, 'b, 'c> {

    fn resolve_field_access(
        &mut self,
        field_access: &FieldAccess,
        span: Span
    ) -> Result<TypeId, Err> {

        let path = field_access.path();
        let mut path_iter = path.path().iter();

        let root_var_id;
        let root_var_type_id;

        let mut current_type_id;
        let mut current_type;

        let (var_id, var_type_id) = self.current_scope.var_info(path.root_name())?;
        root_var_type_id = var_type_id;
        root_var_id = var_id;

        current_type_id = root_var_type_id;

        if let Some(e) = path.root_indexing_expr() {
            let indexing_type_id = self.resolve_expr(e)?;
            let indexing_type = self.universe.get_type(indexing_type_id);

            match *indexing_type {
                SmplType::Int => (),
                _ => {
                    return Err(TypeErr::InvalidIndex { 
                        found: indexing_type_id,
                        span: e.span(),
                    }.into());
                }
            }
            let var_type = self.universe.get_type(var_type_id);
            match *var_type {
                SmplType::Array(ref a) => {
                    current_type_id = a.base_type;
                }
                _ => {
                    return Err(TypeErr::NotAnArray { 
                        found: var_type_id,
                        span: e.span()
                    }.into());
                }
            }
        }

        path.set_root_var(root_var_id);
        path.set_root_var_type(root_var_type_id);

        current_type = self.universe.get_type(current_type_id);

        for (index, field) in path_iter.enumerate() {
            match *current_type {
                SmplType::Struct(ref struct_type) => {
                    match *field {
                        PathSegment::Ident(ref field) => {
                            let name = field.name();
                            let field_id = struct_type.field_id(name).ok_or(TypeErr::UnknownField {
                                name: name.clone(),
                                struct_type: current_type_id,
                                span: span,
                            })?;
                            current_type_id = struct_type.field_type(field_id).unwrap();

                            field.set_field_id(field_id);
                            field.set_field_type(current_type_id);
                        }

                        PathSegment::Indexing(ref field, ref indexing) => {
                            let name = field.name();
                            let field_id = struct_type.field_id(name).ok_or(TypeErr::UnknownField {
                                name: name.clone(),
                                struct_type: current_type_id,
                                span: span,
                            })?;

                            let field_type_id = struct_type.field_type(field_id).unwrap();
                            let field_type = self.universe.get_type(field_type_id);

                            field.set_field_id(field_id);
                            field.set_field_type(field_type_id);

                            let indexing_type_id = self.resolve_expr(indexing)?;
                            let indexing_type = self.universe.get_type(indexing_type_id);

                            match *indexing_type {
                                SmplType::Int => (),

                                _ => {
                                    return Err(TypeErr::InvalidIndex { 
                                        found: indexing_type_id,
                                        span: indexing.span(),
                                    }.into());
                                },
                            }
                            
                            match *field_type {
                                SmplType::Array(ref a) => {
                                    current_type_id = a.base_type;
                                }

                                _ => {
                                    return Err(TypeErr::NotAnArray { 
                                        found: field_type_id,
                                        span: span,
                                    }.into());
                                },
                            }
                        }
                    }
                }

                _ => {
                    return Err(TypeErr::FieldAccessOnNonStruct {
                        path: field_access.raw_path().clone(),
                        index: index,
                        invalid_type: current_type_id,
                        root_type: root_var_type_id,
                        span: span,
                    }.into());
                }
            }

            current_type = self.universe.get_type(current_type_id);
        }

        let accessed_field_type_id = current_type_id;
        field_access.set_field_type_id(accessed_field_type_id);

        Ok(accessed_field_type_id)
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<TypeId, Err> {
        let mut expr_type = None;

        for tmp_id in expr.execution_order() {
            let tmp = expr.get_tmp(*tmp_id);
            let tmp_type;
            match *tmp.value().data() {
                Value::Literal(ref literal) => {
                    use ast::Literal;
                    match *literal {
                        Literal::Int(_) => tmp_type = self.universe.int(),
                        Literal::Float(_) => tmp_type = self.universe.float(),
                        Literal::String(_) => tmp_type = self.universe.string(),
                        Literal::Bool(_) => tmp_type = self.universe.boolean(),
                    }
                }

                Value::StructInit(ref init) => {
                    // Get type info
                    let type_name = init.type_name();
                    let unknown_type_id = self.current_scope.type_id(self.universe, type_name.into())?;
                    let unknown_type = self.universe.get_type(unknown_type_id);

                    // Check if type is a struct.
                    let struct_type_id = unknown_type_id;
                    let struct_type;
                    match *unknown_type {
                        SmplType::Struct(ref t) => struct_type = t,
                        _ => {
                            return Err(TypeErr::NotAStruct {
                                type_name: type_name.clone(),
                                found: struct_type_id,
                                span: tmp.span(),
                            }.into());
                        }
                    }

                    init.set_struct_type(struct_type_id);
                    if let Err(unknown_fields) = init.set_field_init(self.universe) {
                         // TODO: Allow for multiple errors
                        /*let ident = struct_type.get_ident(id);
                                return Err(TypeErr::UnknownField {
                                    name: ident.clone(),
                                    struct_type: struct_type_id,
                                }.into());
                                */
                        // No field initializations but the struct type has fields
                        return Err(TypeErr::StructNotFullyInitialized {
                            type_name: type_name.clone(),
                            struct_type: struct_type_id,
                            missing_fields: unknown_fields,
                            span: tmp.span(),
                        }.into());
                    }

                    
                    match init.field_init() {
                        Some(init_list) => {

                            if init_list.len() != struct_type.fields.len() {
                                // Missing fields -> struct is not fully initialized
                                return Err(TypeErr::StructNotFullyInitialized {
                                    type_name: type_name.clone(),
                                    struct_type: struct_type_id,
                                    missing_fields: {
                                        let inits = init_list
                                            .iter()
                                            .map(|&(ref name, _)| name.clone())
                                            .collect::<Vec<_>>();

                                        struct_type
                                            .field_map
                                            .iter()
                                            .filter(|&(_, ref id)| {
                                                
                                                !inits.contains(id)
                                            })
                                            .map(|(ident, _)| ident.clone())
                                            .collect::<Vec<_>>()
                                    },
                                    span: tmp.span(),
                                }.into());
                            }
                            // Go threw initialization list and check expressions
                            for (ref id, ref typed_tmp_id) in init_list {
                                let field_type_id = struct_type.field_type(*id).unwrap();
                                let tmp = expr.get_tmp(*typed_tmp_id.data());
                                let tmp_type_id = tmp.value().type_id().unwrap();
                                typed_tmp_id.set_type_id(tmp_type_id);

                                // Expression type the same as the field type?
                                if self.universe.get_type(tmp_type_id)
                                    != self.universe.get_type(field_type_id)
                                {
                                    return Err(TypeErr::UnexpectedType {
                                        found: tmp_type_id,
                                        expected: field_type_id,
                                        span: tmp.span(),
                                    }.into());
                                }
                            }
                        }

                        None => {
                            if struct_type.fields.len() != 0 {
                                // Missing fields -> struct is not fully initialized
                                return Err(TypeErr::StructNotFullyInitialized {
                                    type_name: type_name.clone(),
                                    struct_type: struct_type_id,
                                    missing_fields: {
                                        struct_type
                                            .field_map
                                            .iter()
                                            .map(|(ident, _)| ident.clone())
                                            .collect::<Vec<_>>()
                                    },
                                    span: tmp.span(),
                                }.into());
                            }
                        }
                    } 

                    tmp_type = struct_type_id;
                }

                Value::Binding(ref var) => {
                    match self.current_scope.binding_info(var.ident())? {
                        BindingInfo::Var(var_id, type_id) => {
                            var.set_id(var_id);
                            tmp_type = type_id;
                        }

                        BindingInfo::Fn(fn_id) => {
                            self.features.add_feature(FUNCTION_VALUE);

                            if(self.metadata.is_builtin_params_unchecked(fn_id)) {
                                return Err(Err::UncheckedFunctionBinding(var.ident().clone()));
                            }

                            let f = self.universe.get_fn(fn_id);
                            let fn_type_id = f.type_id();

                            var.set_id(fn_id);
                            tmp_type = fn_type_id;
                        }
                    }
                }

                Value::FieldAccess(ref field_access) => {
                    let accessed_field_type_id =
                        self.resolve_field_access(field_access, tmp.span())?;

                    tmp_type = accessed_field_type_id;
                }

                Value::BinExpr(ref op, ref lhs, ref rhs) => {
                    let lhs_type_id = expr.get_tmp(*lhs.data()).value().type_id().unwrap();
                    let rhs_type_id = expr.get_tmp(*rhs.data()).value().type_id().unwrap();

                    lhs.set_type_id(lhs_type_id);
                    rhs.set_type_id(rhs_type_id);

                    tmp_type = resolve_bin_op(self.universe, op, lhs_type_id, rhs_type_id, tmp.span())?;
                }

                Value::UniExpr(ref op, ref uni_e) => {
                    let tmp_type_id = expr.get_tmp(*uni_e.data()).value().type_id().unwrap();

                    tmp_type = resolve_uni_op(self.universe, op, tmp_type_id, tmp.span())?;
                }

                Value::FnCall(ref fn_call) => {
                    // Search for the function
                    let mut skip_param_check = false;
                    let fn_type_id = if fn_call.path().0.len() == 1 {
                        let binding = self.current_scope.binding_info(fn_call.path()
                                                                      .0.get(0).unwrap().data());
                        if binding.is_ok() {
                            match binding.unwrap() {
                                BindingInfo::Fn(fn_id) => {
                                    fn_call.set_id(fn_id);
                                    skip_param_check = self.metadata.is_builtin_params_unchecked(fn_id);
                                    if self.metadata.is_builtin(fn_id)  {
                                        let func = self.universe.get_builtin_fn(fn_id);
                                        Some(func.type_id())
                                    } else {
                                        let func = self.universe.get_fn(fn_id);
                                        Some(func.type_id())
                                    }
                                }
                                BindingInfo::Var(v_id, v_type_id) => {
                                    // Function call on a local variable / parameter
                                    // Should be a functino type
                                    fn_call.set_id(v_id);
                                    self.features.add_feature(FUNCTION_VALUE);
                                    let v_type = self.universe.get_type(v_type_id);
                                    match *v_type {
                                        SmplType::Function(_) => Some(v_type_id),
                                        _ => unimplemented!(),
                                    }
                                },
                            }
                        } else {
                            None 
                        }
                    } else {
                        None
                    };

                    let fn_type_id = match fn_type_id {
                        Some(fn_type_id) => fn_type_id,
                        None => {
                            let fn_id = self.current_scope.get_fn(fn_call.path())?;
                            fn_call.set_id(fn_id);
                            if self.metadata.is_builtin(fn_id) {
                                let func = self.universe.get_builtin_fn(fn_id);
                                func.type_id()
                            } else {
                                let func = self.universe.get_fn(fn_id);
                                func.type_id()
                            }
                        }
                    };

                    let fn_type = self.universe.get_type(fn_type_id);
                    
                    // Check args and parameters align
                    if let SmplType::Function(ref fn_type) = *fn_type {
                        tmp_type = fn_type.return_type;
                        if skip_param_check == false {
                            let arg_type_ids = fn_call.args().map(|ref vec| {
                                vec.iter()
                                    .map(|ref tmp_id| {
                                        let tmp = expr.get_tmp(*tmp_id.data());
                                        let tmp_value = tmp.value();
                                        let tmp_value_type_id = tmp_value.type_id().unwrap();
                                        tmp_id.set_type_id(tmp_value_type_id);
                                        tmp_value_type_id
                                    })
                                    .collect::<Vec<_>>()
                            });

                            match arg_type_ids {
                                Some(arg_type_ids) => {
                                    if fn_type.params.len() != arg_type_ids.len() {
                                        return Err(TypeErr::Arity {
                                            fn_type: fn_type_id,
                                            found_args: arg_type_ids.len(),
                                            expected_param: fn_type.params.len(),
                                            span: tmp.span(),
                                        }.into());
                                    }

                                    let fn_param_type_ids = fn_type.params.iter();

                                    for (index, (arg, param)) in
                                        arg_type_ids.iter().zip(fn_param_type_ids).enumerate()
                                    {
                                        let arg_type = self.universe.get_type(*arg);
                                        let param_type = self.universe.get_type(*param);
                                        if arg_type != param_type {
                                            return Err(TypeErr::ArgMismatch {
                                                fn_type_id: fn_type_id,
                                                index: index,
                                                arg: *arg,
                                                param: param.clone(),
                                                span: tmp.span(),
                                            }.into());
                                        }
                                    }
                                }

                                None => {
                                    if fn_type.params.len() != 0 {
                                        return Err(TypeErr::Arity {
                                            fn_type: fn_type_id,
                                            found_args: 0,
                                            expected_param: fn_type.params.len(),
                                            span: tmp.span(),
                                        }.into());
                                    }
                                }
                            }
                        }
                    } else {
                        panic!( "{} was mapped to {}, which is not SmplType::Function but {:?}",
                            fn_type_id, fn_type_id, fn_type
                        );
                    }
                }

                Value::ArrayInit(ref init) => {
                    self.features.add_feature(STATIC_ARRAY);
                    match *init {
                        ArrayInit::List(ref vec) => {
                            let size = vec.len() as u64;
                            let element_type_ids = vec.iter()
                                .map(|ref tmp_id| {
                                    let tmp = expr.get_tmp(*tmp_id.data());
                                    let tmp_value = tmp.value();
                                    let tmp_value_type_id = tmp_value.type_id().unwrap();
                                    tmp_id.set_type_id(tmp_value_type_id);
                                    (tmp_value_type_id, tmp.span())
                                });

                            let mut expected_element_type_id = None;

                            for (i, (element_type_id, span)) in element_type_ids.enumerate() {
                                let current_element_type = self.universe.get_type(element_type_id);

                                if expected_element_type_id.is_none() {
                                    expected_element_type_id = Some(element_type_id);
                                    continue;
                                }

                                let expected_element_type = self.universe.get_type(
                                    expected_element_type_id.unwrap());

                                if expected_element_type != current_element_type {
                                    return Err(TypeErr::HeterogenousArray {
                                        expected: expected_element_type_id.unwrap(),
                                        found: element_type_id,
                                        index: i,
                                        span: span,
                                    }.into());
                                }
                            }

                            let array_type = TypeConstructor::construct_array_type(self.universe,
                                                                                   expected_element_type_id.unwrap(),
                                                                                   size);
                            self.metadata.insert_array_type(self.module_id, array_type);
                            tmp_type = array_type;
                        },

                        ArrayInit::Value(ref val, size) => {
                            let tmp_val = expr.get_tmp(*val.data());
                            let tmp_concrete_value = tmp_val.value();
                            let tmp_type_id = tmp_concrete_value.type_id().unwrap();
                            val.set_type_id(tmp_type_id);

                            let element_type_id = tmp_type_id;

                            let array_type = TypeConstructor::construct_array_type(self.universe,
                                                                                   element_type_id,
                                                                                   size);
                            self.metadata.insert_array_type(self.module_id, array_type);
                            tmp_type = array_type;
                        },
                    }
                }


                Value::Indexing(ref indexing) => {
                    let element_type;
                    {
                        // Check type of array
                        let tmp = expr.get_tmp(*indexing.array.data());
                        let tmp_value = tmp.value();
                        let tmp_type_id = tmp_value.type_id().unwrap();
                        indexing.array.set_type_id(tmp_type_id);

                        let tmp_type = self.universe.get_type(tmp_type_id);

                        match *tmp_type {
                            SmplType::Array(ref at) => {
                                element_type = at.base_type;
                            },

                            _ => {
                                return Err(TypeErr::NotAnArray { 
                                    found: tmp_type_id,
                                    span: tmp.span(),
                                }.into());
                            }
                        }
                    }

                    {
                        // Check type of array
                        let tmp = expr.get_tmp(*indexing.indexer.data());
                        let tmp_value = tmp.value();
                        let tmp_type_id = tmp_value.type_id().unwrap();
                        indexing.indexer.set_type_id(tmp_type_id);

                        let tmp_type = self.universe.get_type(tmp_type_id);

                        match *tmp_type {
                            SmplType::Int => (),
                            _ => {
                                return Err(TypeErr::InvalidIndex { 
                                    found: tmp_type_id,
                                    span: tmp.span(),
                                }.into());
                            }
                        }
                    }

                    tmp_type = element_type;
                }

                Value::ModAccess(ref access) => {
                    let fn_id = self.current_scope.get_fn(&access.path())?;
                    let func = self.universe.get_fn(fn_id);

                    let fn_type_id = func.type_id();

                    tmp_type = fn_type_id;

                    self.features.add_feature(MOD_ACCESS);
                }
            }

            tmp.value().set_type_id(tmp_type);
            expr_type = Some(tmp_type);
        }

        Ok(expr_type.unwrap())
    }
}

impl<'a, 'b, 'c> Passenger<Err> for FnAnalyzer<'a, 'b, 'c> {
    fn start(&mut self, _id: NodeIndex) -> Result<(), Err> {
        Ok(())
    }

    fn end(&mut self, _id: NodeIndex) -> Result<(), Err> {
        Ok(())
    }

    fn branch_split(&mut self, _id: NodeIndex, _: &BranchingData) -> Result<(), Err> {
        Ok(())
    }

    fn branch_merge(&mut self, _id: NodeIndex, _: &BranchingData) -> Result<(), Err> {
        Ok(())
    }

    fn loop_head(&mut self, _id: NodeIndex, ld: &LoopData) -> Result<(), Err> {
        Ok(())
    }

    fn loop_foot(&mut self, _id: NodeIndex, ld: &LoopData) -> Result<(), Err> {
        Ok(())
    }

    fn cont(&mut self, _id: NodeIndex, ld: &LoopData) -> Result<(), Err> {
        Ok(())
    }

    fn br(&mut self, _id: NodeIndex, ld: &LoopData) -> Result<(), Err> {
        Ok(())
    }

    fn enter_scope(&mut self, _id: NodeIndex) -> Result<(), Err> {
        self.scope_stack.push(self.current_scope.clone());
        Ok(())
    }

    fn exit_scope(&mut self, _id: NodeIndex) -> Result<(), Err> {
        let popped = self.scope_stack.pop()
                                     .expect("If CFG was generated properly and the graph is being walked correctly, there should be a scope to pop");
        self.current_scope = popped;
        Ok(())
    }

    fn local_var_decl(&mut self, _id: NodeIndex, var_decl: &LocalVarDeclData) -> Result<(), Err> {
        let var_decl = &var_decl.decl;

        let expr_type_id = self.resolve_expr(var_decl.init_expr())?;

        let name = var_decl.var_name().clone();
        let var_id = var_decl.var_id();
        let var_type_annotation = var_decl.type_annotation();
        let var_type_id = match var_type_annotation {
            Some(type_annotation) => self.current_scope.type_id(self.universe, type_annotation.into())?,

            None => expr_type_id,
        };

        let var_type = self.universe.get_type(var_type_id);
        let expr_type = self.universe.get_type(expr_type_id);

        var_decl.set_type_id(var_type_id);

        if var_type == expr_type {
            self.current_scope.insert_var(name, var_id, var_type_id);
        } else {
            return Err(TypeErr::LhsRhsInEq(var_type_id, expr_type_id, var_decl.span()).into());
        }
        
        // Local variable types metadata
        self.locals.push((var_id, var_type_id));

        Ok(())
    }

    fn assignment(&mut self, _id: NodeIndex, assignment: &AssignmentData) -> Result<(), Err> {
        let assignment = &assignment.assignment;
        let assignee = assignment.assignee();

        let assignee_type_id =
            self.resolve_field_access(assignee, assignment.access_span())?;

        let expr_type_id = self.resolve_expr(assignment.value())?;

        let assignee_type = self.universe.get_type(assignee_type_id);
        let expr_type = self.universe.get_type(expr_type_id);

        let assignment_span = Span::combine(assignment.access_span(), assignment.value().span());

        if assignee_type != expr_type {
            return Err(TypeErr::LhsRhsInEq(assignee_type_id, expr_type_id, assignment_span).into());
        }

        Ok(())
    }

    fn expr(&mut self, _id: NodeIndex, expr: &ExprData) -> Result<(), Err> {
        self.resolve_expr(&expr.expr).map(|_| ())
    }

    fn ret(&mut self, _id: NodeIndex, rdata: &ReturnData) -> Result<(), Err> {
        let expr = rdata.expr.as_ref();
        let span = rdata.span.clone();
        let expr_type_id = match expr {
            Some(ref expr) => self.resolve_expr(expr)?,

            None => self.universe.unit(),
        };

        if self.universe.get_type(expr_type_id) != self.fn_return_type {
            return Err(TypeErr::InEqFnReturn {
                expr: expr_type_id,
                fn_return: self.fn_return_type_id,
                return_span: span,
            }.into());
        }

        Ok(())
    }

    fn loop_condition(&mut self, _id: NodeIndex, condition: &ExprData) -> Result<(), Err> {
        let condition = &condition.expr;
        let expr_type_id = self.resolve_expr(condition)?;

        if *self.universe.get_type(expr_type_id) != SmplType::Bool {
            return Err(TypeErr::UnexpectedType {
                found: expr_type_id,
                expected: self.universe.boolean(),
                span: condition.span(),
            }.into());
        }

        Ok(())
    }

    fn loop_start_true_path(&mut self, _id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn loop_end_true_path(&mut self, _id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn branch_condition(&mut self, _id: NodeIndex, condition: &ExprData) -> Result<(), Err> {
        let condition = &condition.expr;
        let expr_type_id = self.resolve_expr(condition)?;

        if *self.universe.get_type(expr_type_id) != SmplType::Bool {
            return Err(TypeErr::UnexpectedType {
                found: expr_type_id,
                expected: self.universe.boolean(),
                span: condition.span(),
            }.into());
        }

        Ok(())
    }

    fn branch_start_true_path(&mut self, _id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn branch_start_false_path(&mut self, _id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn branch_end_true_path(&mut self, _id: NodeIndex, _: &BranchingData) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn branch_end_false_path(&mut self, _id: NodeIndex, _: &BranchingData) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }
}
