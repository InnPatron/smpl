use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::ast;
use crate::span::Span;

use super::unique_linear_cfg_traversal::*;
use super::control_data::*;
use super::control_flow::CFG;
use super::semantic_data::{TmpId, FieldId, FnId, VarId, TypeParamId, TypeId, Universe, ModulePath, BindingId};
use super::error::*;
use super::typed_ast::*;
use super::type_cons::*;
use super::resolve_scope::ScopedData;

struct TypeChecker {
    scopes: Vec<ScopedData>,
    typing_context: TypingContext,
}

impl TypeChecker {

    // TODO: Store function (return) type somwhere
    // TODO: Add function parameters somewhere
    pub fn new(inherited_scope: ScopedData) -> TypeChecker {
        let mut typing_context = TypingContext {
            type_params: HashMap::new(),
            var_type_map: HashMap::new(),
            fn_type_map: HashMap::new(),
            tmp_type_map: HashMap::new(),
        };

        TypeChecker {
            scopes: vec![inherited_scope],
            typing_context: typing_context,
        }
    }

    fn current(&self) -> &ScopedData {
        self.scopes
            .last()
            .expect("Should always have a scope")
    }

    fn current_mut(&mut self) -> &mut ScopedData {
        self.scopes
            .last_mut()
            .expect("Should always have a scope")
    }

    fn fork_current(&mut self) {
        let fork = self.current().clone();
        self.scopes.push(fork);
    }

    fn pop_current(&mut self) -> ScopedData {
        self.scopes
            .pop()
            .expect("Should always have a scope")
    }
}

type E = AnalysisError;
impl UniquePassenger<E> for TypeChecker {
    fn start(&mut self, id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn end(&mut self, id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn loop_head(&mut self, id: NodeIndex, ld: &mut LoopData, expr: &mut ExprData) 
        -> Result<(), E> {
        
        // TODO: Resolve types of expression to boolean
        unimplemented!();
        Ok(())
    }

    fn loop_foot(&mut self, id: NodeIndex, ld: &mut LoopData) -> Result<(), E> {
        Ok(())
    }

    fn cont(&mut self, id: NodeIndex, ld: &mut LoopData) -> Result<(), E> {
        Ok(())
    }

    fn br(&mut self, id: NodeIndex, ld: &mut LoopData) -> Result<(), E> {
        Ok(())
    }

    fn enter_scope(&mut self, id: NodeIndex) -> Result<(), E> {
        self.fork_current();
        Ok(())
    }

    fn exit_scope(&mut self, id: NodeIndex) -> Result<(), E> {
        let _old_scope = self.pop_current();
        Ok(())
    }

    fn local_var_decl(&mut self, id: NodeIndex, decl: &mut LocalVarDeclData) -> Result<(), E> {
        let var_decl = &mut decl.decl;

        // TODO: Resolve types of expression
        unimplemented!();
        Ok(())
    }

    fn assignment(&mut self, id: NodeIndex, assign: &mut AssignmentData) -> Result<(), E> {
        let assignment = &mut assign.assignment;

        // TODO: Resolve types of expression
        unimplemented!();

        Ok(())
    }

    fn expr(&mut self, id: NodeIndex, expr: &mut ExprData) -> Result<(), E> {
        // TODO: Resolve types of expression
        unimplemented!();
        Ok(())
    }

    fn ret(&mut self, id: NodeIndex, rdata: &mut ReturnData) -> Result<(), E> {
        // TODO: Resolve types of expression
        // TODO: Check if return type compatible
        unimplemented!();
        if let Some(ref mut return_data) = rdata.expr {
            
        }
        Ok(())
    }

    fn loop_start_true_path(&mut self, id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn loop_end_true_path(&mut self, id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn branch_split(&mut self, id: NodeIndex, b: &mut BranchingData, e: &mut ExprData) 
        -> Result<(), E> {
        // TODO: Resolve types of expression to boolean
        unimplemented!();
        Ok(())
    }

    fn branch_merge(&mut self, id: NodeIndex, b: &mut BranchingData) -> Result<(), E> {
        Ok(())
    }

    fn branch_start_true_path(&mut self, id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn branch_start_false_path(&mut self, id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn branch_end_true_path(&mut self, id: NodeIndex, b: &mut BranchingData) -> Result<(), E> {
        Ok(())
    }

    fn branch_end_false_path(&mut self, id: NodeIndex, b: &mut BranchingData) -> Result<(), E> {
        Ok(())
    }
}

pub struct TypingContext {
    pub type_params: HashMap<TypeParamId, AbstractType>,
    pub var_type_map: HashMap<VarId, AbstractType>,
    pub fn_type_map: HashMap<FnId, AbstractType>,
    pub tmp_type_map: HashMap<TmpId, AbstractType>,
}

impl TypingContext {
    pub fn get_type_param(&self, id: TypeParamId) -> Option<&AbstractType> {
        self.type_params
            .get(&id)
    }
}

fn resolve_expr(universe: &Universe, scope: &ScopedData, context: &mut TypingContext, expr: &Expr) 
    -> Result<AbstractType, AnalysisError> {

    let mut expr_type = None;
    for tmp_id in expr.execution_order() {
        let tmp = expr.get_tmp(tmp_id);

        let tmp_type = resolve_tmp(universe, scope, context, expr, tmp)?;
        let expr_type = Some(tmp_type.clone());

        if context.tmp_type_map
            .insert(tmp_id, tmp_type)
            .is_some() {
            panic!("Duplicate tmp ID"); 
        }
    }

    Ok(expr_type.unwrap())
}

fn resolve_tmp(universe: &Universe, scope: &ScopedData, 
    context: &mut TypingContext, expr: &Expr, tmp: &Tmp) 
    -> Result<AbstractType, AnalysisError> {

    let tmp_span = tmp.span();
    let tmp_value = tmp.value();
    let tmp_type = match tmp_value.data() {
        Value::Literal(ref literal) => {
            match *literal {
                Literal::Int(_) => AbstractType::Int,
                Literal::Float(_) => AbstractType::Float,
                Literal::String(_) => AbstractType::String,
                Literal::Bool(_) => AbstractType::Bool,
            }
        }

        Value::BinExpr(ref op, ref lhs, ref rhs) => {
            let lhs_type = context
                .tmp_type_map
                .get(lhs.data())
                .expect("Missing tmp");
            let rhs_type = context
                .tmp_type_map
                .get(rhs.data())
                .expect("Missing tmp");

            resolve_bin_op(scope, 
                context, 
                op, 
                lhs_type, 
                rhs_type,
                tmp_span)?
        }

        Value::UniExpr(ref op, ref uni_tmp) => {
            let uni_tmp_type = context
                .tmp_type_map
                .get(uni_tmp.data())
                .expect("Missing tmp");

            resolve_uni_op(scope, context, op, uni_tmp_type, tmp.span())?
        }

        Value::StructInit(ref init) => {
            resolve_struct_init(universe, scope, context, init, tmp.span())? 
        }

        Value::AnonStructInit(ref init) => {
            resolve_anon_struct_init(universe, scope, context, init, tmp.span())?
        }

        Value::Binding(ref binding) => {
            resolve_binding(universe, scope, context, binding, tmp.span())?
        }

        Value::ModAccess(ref access) => {
            resolve_mod_access(universe, scope, context, access, tmp.span())?
        }

        Value::FnCall(ref fn_call) => {
            resolve_fn_call(universe, scope, context, fn_call, tmp.span())?
        }

        Value::ArrayInit(ref init) => {
            resolve_array_init(universe, scope, context, init, tmp.span())?
        }

        Value::Indexing(ref indexing) => {
            resolve_indexing(universe, scope, context, indexing, tmp.span())?
        }

        Value::TypeInst(ref type_inst) => {
            resolve_type_inst(universe, scope, context, type_inst, tmp.span())?
        }

        Value::AnonymousFn(ref a_fn) => {
            resolve_anonymous_fn(universe, scope, context, a_fn, tmp.span())?
        }

        Value::FieldAccess(ref field_access) => {
            resolve_field_access(universe, 
                scope, 
                context, 
                expr,
                field_access, 
                tmp.span())?
        }
    }; 

    Ok(tmp_type)
}

fn resolve_bin_op(
    scope: &ScopedData,
    context: &TypingContext,
    op: &ast::BinOp,
    lhs: &AbstractType,
    rhs: &AbstractType,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    use crate::ast::BinOp::*;

    let expected_int = AbstractType::Int;
    let expected_float = AbstractType::Float;
    let expected_bool = AbstractType::Bool;

    // TODO: lhs/rhs already applied?
    let resolve_type = match *op {
        Add | Sub | Mul | Div | Mod => match (&lhs, &rhs) {
            (&AbstractType::Int, &AbstractType::Int) => AbstractType::Int,
            (&AbstractType::Float, &AbstractType::Float) => AbstractType::Float,

            _ => {
                return Err(TypeError::BinOp {
                    op: op.clone(),
                    expected: vec![expected_int, expected_float],
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                    span: span,
                }
                .into());
            }
        },

        LogicalAnd | LogicalOr => match (&lhs, &rhs) {
            (&AbstractType::Bool, &AbstractType::Bool) => AbstractType::Bool,
            _ => {
                return Err(TypeError::BinOp {
                    op: op.clone(),
                    expected: vec![expected_bool],
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                    span: span,
                }
                .into());
            }
        },

        GreaterEq | LesserEq | Greater | Lesser => match (&lhs, &rhs) {
            (&AbstractType::Int, &AbstractType::Int) => AbstractType::Bool,
            (&AbstractType::Float, &AbstractType::Float) => AbstractType::Bool,

            _ => {
                return Err(TypeError::BinOp {
                    op: op.clone(),
                    expected: vec![expected_int, expected_float],
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                    span: span,
                }
                .into());
            }
        },

        Eq | InEq => {

            // TODO: lhs/rhs already applied?
            // TODO: Check if rhs, lhs are equal
            unimplemented!()
            /*
            if resolve_types(&rhs, &lhs) {
                AbstractType::Bool
            } else {
                return Err(TypeError::LhsRhsInEq(lhs.clone(), rhs.clone(), span).into());
            }
            */
        }
    };

    Ok(resolve_type)
}

fn resolve_uni_op(
    scope: &ScopedData,
    context: &TypingContext,
    op: &ast::UniOp,
    tmp_type: &AbstractType,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    use crate::ast::UniOp::*;

    let expected_int = AbstractType::Int;

    let expected_float = AbstractType::Float;

    let expected_bool = AbstractType::Bool;

    // TODO: lhs/rhs already applied?
    match *op {
        Negate => match tmp_type {
            AbstractType::Int | AbstractType::Float => Ok(tmp_type.clone()),
            _ => Err(TypeError::UniOp {
                op: op.clone(),
                expected: vec![expected_int, expected_float],
                expr: tmp_type.clone(),
                span: span,
            }
            .into()),
        },

        LogicalInvert => match tmp_type {
            AbstractType::Bool => Ok(tmp_type.clone()),
            _ => Err(TypeError::UniOp {
                op: op.clone(),
                expected: vec![expected_bool],
                expr: tmp_type.clone(),
                span: span,
            }
            .into()),
        },

        _ => unimplemented!(),
    }
}

fn resolve_struct_init(universe: &Universe, scope: &ScopedData, 
    context: &TypingContext, init: &StructInit, span: Span) 
    -> Result<AbstractType, AnalysisError> {

    // Get type info
    let type_name = init.type_name();
    let tmp_type_name = type_name.clone().into();
    let struct_type_id = scope
        .type_cons(universe, &tmp_type_name)
        .ok_or(AnalysisError::UnknownType(type_name.clone()))?;

    let type_args = init.type_args()
        .map(|vec| {
            vec
            .iter()
            .map(|ann| {
                type_from_ann(
                    universe,
                    scope,
                    context,
                    ann,
                )
            })
            .collect::<Result<Vec<_>, _>>()
        })
        .unwrap_or(Ok(Vec::new()))?;

    // TODO: Take into account type arguments
    let struct_type = AbstractType::App {
        type_cons: struct_type_id,
        args: type_args,
    }
    .apply(universe, scope, context)?;

    // Check if type is a struct.
    let (struct_type_id, fields, field_map) = match struct_type {
        AbstractType::Record {
            type_id: struct_type_id,
            ref abstract_field_map,
            ..
        } => (struct_type_id, &abstract_field_map.fields, &abstract_field_map.field_map),

        _ => {
            return Err(TypeError::NotAStruct {
                type_name: type_name.clone(),
                found: struct_type,
                span: span,
            }
            .into());
        }
    };

    // Check if the struct is an 'opaque' type (i.e. cannot be initialized by SMPL
    // code)
    // TODO: Opaque check
    /*
    if self.program.metadata().is_opaque(struct_type_id) {
        return Err(TypeError::InitOpaqueType {
            struct_type: struct_type,
            span: tmp.span(),
        }
        .into());
    }
    */

    // Map init'd field to its type
    let mut init_expr_type_map: HashMap<FieldId, &'_ AbstractType> = HashMap::new();
    for (field_name, typed_tmp) in init.raw_field_init() {

        // Check if the struct type has the corresponding field
        let field_id = field_map
            .get(field_name)
            .ok_or(TypeError::UnknownField {
                name: field_name.clone(),
                struct_type: struct_type.clone(),
                span: span,
            })?;

        let tmp_type = context.tmp_type_map
            .get(typed_tmp.data())
            .expect("Missing tmp");

        if init_expr_type_map.insert(field_id.clone(), tmp_type).is_some() {
            panic!("Duplicate field init");
        }
    }

    // Not a full struct init
    if init_expr_type_map.len() != fields.len() {

        let missing_fields = field_map
            .iter()
            .filter(|(_, field_id)| init_expr_type_map.contains_key(field_id))
            .map(|(ident, _)| ident.clone())
            .collect();

        return Err(TypeError::StructNotFullyInitialized {
            type_name: type_name.clone(),
            struct_type: struct_type.clone(),
            missing_fields: missing_fields,
            span: span,
        }.into());
    }

    // SATISFIED CONDITIONS: 
    //   Field init expressions should be fully typed (tmps)
    //   Field names are all present and all valid

    // TODO: Check if field init expressions are of the correct type
    // Check if field init expressions are of the correct type
    for (field_id, field_type) in fields.iter() {
        let init_expr_type = init_expr_type_map
            .get(field_id)
            .unwrap();
        unimplemented!()
    }

    // SATISFIED CONDITIONS: 
    //   Field init expressions should be fully typed (tmps)
    //   Field names are all present and all valid
    //   Field init expressions are valid types for their corresponding fields

    Ok(struct_type)
}

/// Generates a WidthConstraint based on the types of its initializer expressions
fn resolve_anon_struct_init(universe: &Universe, scope: &ScopedData, 
    context: &TypingContext, init: &AnonStructInit, span: Span) 
    -> Result<AbstractType, AnalysisError> {

    let mut width_constraint = AbstractWidthConstraint {
        fields: HashMap::new(),
    };

    // Map init'd field to its type
    let mut duplicate_fields = Vec::new();
    for (field_name, typed_tmp) in init.raw_field_init() {

        let tmp_type = context.tmp_type_map
            .get(typed_tmp)
            .expect("Missing tmp");

        if width_constraint.fields.contains_key(field_name) {
            duplicate_fields.push(field_name.clone());
        } else {
            width_constraint.fields.insert(field_name.clone(), tmp_type.clone());
        }
    }

    if duplicate_fields.len() != 0 {
        return Err(TypeError::InvalidInitialization {
            fields: duplicate_fields,
            span: span,
        }.into());
    }

    let width_type = AbstractType::WidthConstraint(width_constraint);
    Ok(width_type)
}

fn resolve_binding(universe: &Universe, scope: &ScopedData,
    context: &TypingContext, binding: &Binding, span: Span)
    -> Result<AbstractType, AnalysisError> {

    match binding.get_id().unwrap() {
        BindingId::Var(var_id) => {
            Ok(context
                .var_type_map
                .get(&var_id)
                .expect("Missing VarId")
                .clone())
        }

        BindingId::Fn(fn_id) => {
            // self.program.features_mut().add_feature(FUNCTION_VALUE);

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

            // TODO: builtin check
            let fn_type_id = universe
                .get_fn(fn_id)
                .fn_type()
                .clone();
            /*
            let fn_type_id = if self.program.metadata().is_builtin(fn_id) {
                let f = self.program.universe().get_builtin_fn(fn_id);
                f.fn_type().clone()
            } else {
                let f = self.program.universe().get_fn(fn_id);
                f.fn_type().clone()
            };
            */

            let fn_type = AbstractType::App {
                type_cons: fn_type_id,
                args: Vec::new(),
            }
            .apply(universe, scope, context)?;

            Ok(fn_type)
        }
    }
}

fn resolve_mod_access(universe: &Universe, scope: &ScopedData,
    context: &TypingContext, mod_access: &ModAccess, span: Span)
    -> Result<AbstractType, AnalysisError> {

    let fn_id = mod_access.fn_id()
        .expect("Should be set by name resolution");

    // TODO: Builtin detection
    /*
    let fn_type_id = if self.program.metadata().is_builtin(fn_id) {
        let f = self.program.universe().get_builtin_fn(fn_id);
        f.fn_type().clone()
    } else {
        let f = self.program.universe().get_fn(fn_id);
        f.fn_type().clone()
    };
    */

    let fn_type_id = universe
        .get_fn(fn_id)
        .fn_type();

    let fn_type = AbstractType::App {
        type_cons: fn_type_id,
        args: Vec::new(),
    }
    .apply(universe, scope, context)?;

    Ok(fn_type)
}

fn resolve_fn_call(universe: &Universe, scope: &ScopedData, context: &TypingContext,
    fn_call: &FnCall, span: Span)
    -> Result<AbstractType, AnalysisError> {

    let fn_value = fn_call.fn_value();
    let fn_value_type = context.tmp_type_map
        .get(&fn_value)
        .expect("Missing TMP");

    // Check args and parameters align
    match fn_value_type {
        AbstractType::Function {
            parameters: ref params,
            ref return_type,
        } => {
            let arg_types = fn_call.args().map(|ref vec| {
                vec.iter()
                    .map(|ref tmp_id| {
                        context.tmp_type_map
                            .get(tmp_id.data())
                            .expect("Missing TMP")
                            .clone()
                    })
                    .collect::<Vec<_>>()
            });

            match arg_types {
                Some(arg_types) => {
                    if params.len() != arg_types.len() {
                        return Err(TypeError::Arity {
                            fn_type: fn_value_type.clone(),
                            found_args: arg_types.len(),
                            expected_param: params.len(),
                            span: span,
                        }
                        .into());
                    }

                    let fn_param_types = params.iter();

                    for (index, (arg_type, param_type)) in
                        arg_types.iter().zip(fn_param_types).enumerate()
                    {
                        let arg_type: &AbstractType = arg_type;
                        let param_type: &AbstractType = param_type;
                        // TODO: Check if types can resolve
                        /*
                        if !resolve_types(&arg_type, &param_type) {
                            return Err(TypeError::ArgMismatch {
                                fn_type: fn_value_type.clone(),
                                index: index,
                                arg: arg_type.clone(),
                                param: param_type.clone(),
                                span: span,
                            }
                            .into());
                        }
                        */
                    }

                    Ok(*(return_type.clone()))
                }

                None => {
                    if params.len() != 0 {
                        Err(TypeError::Arity {
                            fn_type: fn_value_type.clone(),
                            found_args: 0,
                            expected_param: params.len(),
                            span: span,
                        }
                        .into())
                    } else {
                        Ok(*(return_type.clone()))
                    }
                }
            }
        }

        AbstractType::UncheckedFunction {
            return_type,
            ..
        } => {
            Ok(*(return_type.clone()))
        }

        t @ _ => panic!("Function call on a non-function type: {:?}", t),
    }
}

fn resolve_array_init(universe: &Universe, scope: &ScopedData, context: &TypingContext,
    init: &ArrayInit, span: Span)
    -> Result<AbstractType, AnalysisError> {

    match *init {
        ArrayInit::List(ref vec) => {
            let size = vec.len() as u64;
            let element_types = vec.iter().map(|ref tmp_id| {
                let tmp_type = context.tmp_type_map
                    .get(tmp_id.data())
                    .expect("Missing TMP");
                (tmp_type, span)
            });

            let mut expected_element_type = None;

            for (i, (current_element_type, span)) in element_types.enumerate() {
                if expected_element_type.is_none() {
                    expected_element_type = Some(current_element_type);
                    continue;
                }

                let expected_element_type = expected_element_type.as_ref().unwrap();

                // TODO: Implement element consistency check
                /*
                if !resolve_types(&current_element_type, expected_element_type) {
                    return Err(TypeError::HeterogenousArray {
                        expected: expected_element_type.clone(),
                        found: current_element_type,
                        index: i,
                        span: span,
                    }
                    .into());
                }
                */
            }

            let array_type = AbstractType::Array {
                element_type: Box::new(expected_element_type.unwrap().clone()),
                size: size,
            };

            Ok(array_type)
        }

        ArrayInit::Value(ref val, size) => {
            let element_type = context.tmp_type_map
                .get(val.data())
                .expect("Missing TMP");

            let array_type = AbstractType::Array {
                element_type: Box::new(element_type.clone()),
                size: size,
            };

            // TODO: Insert array type into metadata?
            /*
            self.program
                .metadata_mut()
                .insert_array_type(self.module_id, array_type);
            */
            Ok(array_type)
        }
    }
}

fn resolve_indexing(universe: &Universe, scope: &ScopedData, context: &TypingContext,
    indexing: &Indexing, span: Span) 
    -> Result<AbstractType, AnalysisError> {

    let expected_element_type: AbstractType = {
        // Check type is array
        let tmp_type = context.tmp_type_map
            .get(indexing.array.data())
            .expect("Missing TMP");

        // TODO: Already applied?
        match &tmp_type {
            AbstractType::Array {
                ref element_type,
                ..
            } => *(element_type.clone()),

            _ => {
                return Err(TypeError::NotAnArray {
                    found: tmp_type.clone(),
                    span: span,
                }
                .into());
            }
        }
    };

    {
        // Check type of indexer
        let tmp_type = context.tmp_type_map
            .get(indexing.indexer.data())
            .expect("Missing TMP");

        // TODO: Already applied?
        match &tmp_type {
            AbstractType::Int => (),

            _ => {
                return Err(TypeError::InvalidIndex {
                    found: tmp_type.clone(),
                    span: span,
                }
                .into());
            }
        }
    }

    Ok(expected_element_type)
}

fn resolve_type_inst(universe: &Universe, scope: &ScopedData, context: &TypingContext,
    type_inst: &TypeInst, span: Span) 
    -> Result<AbstractType, AnalysisError> {

    let fn_id = type_inst
        .get_id()
        .expect("No FN ID. Should be caught in scope resolution");

    let fn_type_id = universe
        .get_fn(fn_id)
        .fn_type();

    let type_args = type_inst
        .args()
        .iter()
        .map(|ann| {
            type_from_ann(
                universe,
                scope,
                context,
                ann,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;

    let inst_type = AbstractType::App {
        type_cons: fn_type_id,
        args: type_args,
    }
    .apply(universe, scope, context)?;

    Ok(inst_type)
}

fn resolve_anonymous_fn(universe: &Universe, scope: &ScopedData, context: &TypingContext,
    a_fn: &AnonymousFn, span: Span)
    -> Result<AbstractType, AnalysisError> {

    let fn_id = a_fn.fn_id();
    let func = a_fn.a_fn();

    // TODO: For inserting later into the Universe, need to store the current scope
    //      and the type
    // TODO: Make sure scope ONLY has type parameter info
    let (func_scope, fn_type_cons) =
        super::type_cons_gen::generate_anonymous_fn_type(universe, scope, context, fn_id, func)?;

    let anon_fn_type = match fn_type_cons {
        TypeCons::Function {
            type_params,
            parameters,
            return_type,
        } => {

            if type_params.len() != 0 {
                // TODO: lift restrictions?
                unimplemented!("Anonymous functions are not allowed to have their own type params");
            }

            AbstractType::Function {
                parameters: parameters,
                return_type: Box::new(return_type),
            }
        },

        _ => unreachable!(),
    };


    // TODO: Generate and check anonymous functions in a later phase
    /*
    let cfg = CFG::generate(
        self.program.universe_mut(),
        func.body.clone(),
        &fn_type,
        &func_scope,
    )?;

    let fn_type_id = self.program.universe_mut().insert_type_cons(fn_type);


    self.program
        .universe_mut()
        .insert_fn(fn_id, fn_type_id, func_scope, cfg);

    // Since anonymous functions are ALWAYS inside another function
    // Assume the global scope is at the bottom of the scope stack
    analyze_fn(self.program, fn_id, self.module_id)?;
    */

    Ok(anon_fn_type)
}

fn resolve_field_access(
    universe: &Universe,
    scope: &ScopedData,
    context: &TypingContext,
    expr: &Expr,
    field_access: &FieldAccess,
    span: Span,
) -> Result<AbstractType, AnalysisError> {

    let path = field_access.path();
    let path_iter = path.path().iter();

    let root_var_id = path.root_var_id();
    let root_var_type = context.var_type_map
        .get(&root_var_id)
        .expect("Missing VAR");

    let mut current_type: AbstractType = root_var_type.clone();

    if let Some(e) = path.root_indexing_expr() {
        let indexing_type = context.tmp_type_map
            .get(&e)
            .expect("Missing TMP");

        match indexing_type {
            AbstractType::Int => (),
            _ => {
                return Err(TypeError::InvalidIndex {
                    found: indexing_type.clone(),
                    span: expr.get_tmp(e).span(),
                }
                .into());
            }
        }

        match root_var_type {
            AbstractType::Array {
                element_type,
                ..
            } => {
                current_type = *(element_type.clone());
            }
            _ => {
                return Err(TypeError::NotAnArray {
                    found: root_var_type.clone(),
                    span: expr.get_tmp(e).span(),
                }
                .into());
            }
        }
    }

    for (index, field) in path_iter.enumerate() {
        let next_type: AbstractType;
        let field_type_retriever: 
            Box<dyn Fn(&crate::ast::Ident) -> Result<AbstractType, AnalysisError>> = 
            match current_type {

            AbstractType::WidthConstraint(awc) => {
                Box::new(move |name| {
                    awc
                        .fields
                        .get(name)
                        .map(|t| t.clone())
                        .ok_or(TypeError::UnknownField {
                            name: name.clone(),
                            struct_type: AbstractType::WidthConstraint(awc.clone()),
                            span: span,
                        }.into())
                })
            }

            AbstractType::Record {
                type_id,
                abstract_field_map: afm,
            } => {
                let fields = afm.fields;
                let field_map = afm.field_map;

                Box::new(move |name| {
                    let field_id = field_map
                        .get(name)
                        .map(|t| t.clone())
                        .ok_or(TypeError::UnknownField {
                            name: name.clone(),
                            struct_type: AbstractType::Record {
                                type_id: type_id,
                                abstract_field_map: AbstractFieldMap {
                                    fields: fields.clone(),
                                    field_map: field_map.clone(),
                                }
                            },
                            span: span,
                        })?;

                    let field_type = fields
                        .get(&field_id)
                        .map(|t| t.clone())
                        .unwrap();

                    Ok(field_type)
                })
            }

            _ => {
                return Err(TypeError::FieldAccessOnNonStruct {
                    path: field_access.raw_path().clone(),
                    index: index,
                    invalid_type: current_type,
                    root_type: root_var_type.clone(),
                    span: span,
                }
                .into());
            }
        };

        match *field {
            PathSegment::Ident(ref field) => {
                next_type = field_type_retriever(field.name())?
                    .clone();
            }

            PathSegment::Indexing(ref field, ref indexing) => {

                let field_type = field_type_retriever(field.name())?;

                let indexing_type = context.tmp_type_map
                    .get(indexing)
                    .expect("Missing TMP");

                // TODO: Application?
                match indexing_type {
                    AbstractType::Int => (),

                    _ => {
                        return Err(TypeError::InvalidIndex {
                            found: indexing_type.clone(),
                            span: expr.get_tmp(*indexing).span(),
                        }
                        .into());
                    }
                };

                // TODO: Application?
                match field_type {
                    AbstractType::Array {
                        element_type,
                        size: _,
                    } => {
                        next_type = *element_type;
                    }

                    _ => {
                        return Err(TypeError::NotAnArray {
                            found: field_type.clone(),
                            span: span,
                        }
                        .into());
                    }
                }
            }
        }

        current_type = next_type.clone();
    }

    let accessed_field_type = current_type;

    Ok(accessed_field_type)
}
