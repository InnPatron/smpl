use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::ast;
use crate::span::Span;

use super::control_data::*;
use super::linear_cfg_traversal::*;

use super::analysis_helpers;
use super::error::*;
use super::metadata::Metadata;
use super::resolve_scope::ScopedData;
use super::semantic_data::Function;
use super::semantic_data::*;
use super::abstract_type::*;
use super::type_resolver;
use super::typed_ast::*;

pub fn type_check(
    universe: &mut Universe,
    metadata: &mut Metadata,
    module_id: ModuleId,
    fn_id: FnId,
) -> Result<(), AnalysisError> {
    use super::semantic_data::Function;

    let cfg = {
        let fn_to_resolve = universe.get_fn(fn_id);
        match fn_to_resolve {
            Function::SMPL(ref smpl_fn) => smpl_fn.cfg(),
            Function::Anonymous(ref afn) => match afn {
                AnonymousFunction::Reserved(_) => {
                    panic!("Anonymous function should be resolved")
                }

                AnonymousFunction::Resolved { ref cfg, .. } => cfg.clone(),
            },

            _ => panic!("Not a function with a type-checkable body"),
        }
    };

    let mut type_checker = TypeChecker::new(universe, metadata, module_id, fn_id)?;
    let cfg = cfg.borrow();
    let traverser = Traverser::new(&*cfg, &mut type_checker);
    traverser.traverse()?;

    // Update the typing context
    let typing_context = type_checker.typing_context;
    {
        let resolved_fn = universe.get_fn_mut(fn_id);
        match resolved_fn {
            Function::SMPL(ref mut smpl_fn) => {
                smpl_fn
                    .analysis_context_mut()
                    .set_typing_context(typing_context);
            }
            Function::Anonymous(ref mut afn) => match afn {
                AnonymousFunction::Reserved(_) => unreachable!(),

                AnonymousFunction::Resolved {
                    ref mut analysis_context,
                    ..
                } => {
                    analysis_context.set_typing_context(typing_context);
                }
            },

            _ => unreachable!(),
        }
    };

    Ok(())
}

struct TypeChecker<'a> {
    universe: &'a mut Universe,
    metadata: &'a mut Metadata,
    module_id: ModuleId,
    scopes: Vec<ScopedData>,
    typing_context: TypingContext,
    return_type: AbstractType,
}

impl<'a> TypeChecker<'a> {
    // TODO: Store function (return) type somwhere
    // TODO: Add function parameters somewhere
    // TODO: Put formal parameters into function scope within Universe
    pub fn new<'b>(
        universe: &'b mut Universe,
        metadata: &'b mut Metadata,
        module_id: ModuleId,
        fn_id: FnId,
    ) -> Result<TypeChecker<'b>, AnalysisError> {
        use super::semantic_data::Function;

        match universe.get_fn(fn_id) {
            Function::Builtin(_) => unimplemented!(),

            Function::Anonymous(anonymous_fn) => {
                match anonymous_fn {
                    AnonymousFunction::Reserved(..) => {
                        panic!("Expected anonymous functions to already be resolved");
                    }

                    AnonymousFunction::Resolved {
                        ref fn_type,
                        ref analysis_context,
                        ..
                    } => {
                        let typing_context =
                            analysis_context.typing_context().clone();
                        let fn_scope = analysis_context.fn_scope().clone();

                        let return_type: AbstractType = {
                            let type_id = fn_type.clone();

                            let fn_type = AbstractType::App {
                                data: fn_type.span(),
                                type_cons: type_id.data().clone(),
                                args: analysis_context
                                    .existential_type_vars()
                                    .iter()
                                    .map(|id| AbstractType::TypeVar(Span::dummy(), id.clone()))
                                    .collect::<Vec<_>>(),
                            }
                            .substitute(universe, &fn_scope, &typing_context)?;
                            // TODO: Should this be the module context/scope

                            match fn_type {
                                AbstractType::Function {
                                    ref return_type,
                                    ..
                                } => *return_type.clone(),

                                _ => panic!("Non-function type constructor for function"),
                            }
                        };

                        Ok(TypeChecker {
                            universe: universe,
                            metadata: metadata,
                            module_id: module_id,
                            scopes: vec![fn_scope],
                            typing_context: typing_context,
                            return_type: return_type,
                        })
                    }
                }
            }

            Function::SMPL(smpl_function) => {
                let typing_context =
                    smpl_function.analysis_context().typing_context().clone();
                let fn_scope =
                    smpl_function.analysis_context().fn_scope().clone();

                let return_type: AbstractType = {
                    let type_id = smpl_function.fn_type();

                    let decl_span = smpl_function.span();
                    let fn_type = AbstractType::App {
                        data: decl_span.clone(),
                        type_cons: type_id,
                        args: smpl_function
                            .analysis_context()
                            .existential_type_vars()
                            .iter()
                            .map(|id| AbstractType::TypeVar(decl_span.clone(), id.clone()))
                            .collect::<Vec<_>>(),
                    }
                    .substitute(
                        universe,
                        &fn_scope,
                        &typing_context,
                    )?;
                    // TODO: Should this be the module context/scope

                    match fn_type {
                        AbstractType::Function {
                            ref return_type, ..
                        } => *return_type.clone(),

                        _ => {
                            panic!("Non-function type constructor for function")
                        }
                    }
                };

                Ok(TypeChecker {
                    scopes: vec![fn_scope],
                    typing_context: typing_context,
                    module_id: module_id,
                    universe: universe,
                    metadata: metadata,
                    return_type: return_type,
                })
            }
        }
    }

    fn current(&self) -> &ScopedData {
        self.scopes.last().expect("Should always have a scope")
    }

    fn current_mut(&mut self) -> &mut ScopedData {
        self.scopes.last_mut().expect("Should always have a scope")
    }

    fn fork_current(&mut self) {
        let fork = self.current().clone();
        self.scopes.push(fork);
    }

    fn pop_current(&mut self) -> ScopedData {
        self.scopes.pop().expect("Should always have a scope")
    }
}

macro_rules! expr_type {
    ($self: expr, $expr: expr) => {{
        resolve_expr(
            $self.universe,
            $self.metadata,
            $self.module_id.clone(),
            $self.scopes.last().expect("Should always have a scope"),
            &mut $self.typing_context,
            $expr,
        )
    }};
}

macro_rules! resolve {
    ($self: expr, $synthesis: expr, $constraint: expr, $span: expr) => {{
        use super::type_resolver;
        type_resolver::resolve_types(
            $self.universe,
            $self.scopes.last().expect("Should always have a scope"),
            &mut $self.typing_context,
            $synthesis,
            $constraint,
            $span,
        )
    }};
}

macro_rules! ann_to_type {
    ($self: expr, $ann: expr) => {{
        use super::abstract_type;
        abstract_type::type_from_ann(
            $self.scopes.last().expect("Should always have a scope"),
            &$self.typing_context,
            $ann,
        )
    }};
}

type E = AnalysisError;
impl<'a> Passenger<E> for TypeChecker<'a> {
    fn start(&mut self, _id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn end(&mut self, _id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn loop_head(
        &mut self,
        _id: NodeIndex,
        _ld: &LoopData,
        expr: &ExprData,
    ) -> Result<(), E> {
        let expr_type = expr_type!(self, &expr.expr)?;
        resolve!(self, &expr_type, &AbstractType::Bool(expr.span.clone()), expr.span.clone())?;

        Ok(())
    }

    fn loop_foot(&mut self, _id: NodeIndex, _ld: &LoopData) -> Result<(), E> {
        Ok(())
    }

    fn cont(&mut self, _id: NodeIndex, _ld: &LoopData) -> Result<(), E> {
        Ok(())
    }

    fn br(&mut self, _id: NodeIndex, _ld: &LoopData) -> Result<(), E> {
        Ok(())
    }

    fn enter_scope(&mut self, _id: NodeIndex) -> Result<(), E> {
        self.fork_current();
        Ok(())
    }

    fn exit_scope(&mut self, _id: NodeIndex) -> Result<(), E> {
        let _old_scope = self.pop_current();
        Ok(())
    }

    fn local_var_decl(
        &mut self,
        _id: NodeIndex,
        decl: &LocalVarDeclData,
    ) -> Result<(), E> {
        let var_decl = &decl.decl;

        let expr_type = expr_type!(self, var_decl.init_expr())?;

        let var_type = match var_decl.type_annotation() {
            Some(ann) => {
                let ann_type = ann_to_type!(self, ann)?;
                resolve!(self, &expr_type, &ann_type, decl.span.clone()).map_err(
                    |_| {
                        let name = var_decl.var_name();
                        TypeError::IncompatibleLocal {
                            name: name.clone(),
                            local_type: ann_type.clone(),
                            found_type: expr_type,
                            span: decl.span.clone(),
                        }
                    },
                )?;

                ann_type
            }

            None => {
                // No type annotation
                // Default to the RHS type
                expr_type
            }
        }
        .substitute(
            self.universe,
            self.scopes.last().expect("Should always have a scope"),
            &self.typing_context,
        )?;

        self.typing_context
            .var_type_map
            .insert(var_decl.var_id(), var_type);

        Ok(())
    }

    fn assignment(
        &mut self,
        _id: NodeIndex,
        assign: &AssignmentData,
    ) -> Result<(), E> {
        let assignment = &assign.assignment;

        let value_type = expr_type!(self, assignment.value())?;

        let assignee_type = resolve_field_access(
            self.universe,
            self.metadata,
            self.module_id.clone(),
            self.scopes.last().expect("Should always have a scope"),
            &mut self.typing_context,
            assignment.assignee(),
            assignment.access_span(),
        )?;

        resolve!(self, &value_type, &assignee_type, assign.span.clone())?;

        Ok(())
    }

    fn expr(&mut self, _id: NodeIndex, expr: &ExprData) -> Result<(), E> {
        let _expr_type = expr_type!(self, &expr.expr)?;

        Ok(())
    }

    fn ret(&mut self, _id: NodeIndex, rdata: &ReturnData) -> Result<(), E> {
        // TODO: Resolve types of expression
        // TODO: Check if return type compatible

        match rdata.expr {
            Some(ref expr) => {
                let expr_type = expr_type!(self, expr)?;
                resolve!(self, &expr_type, &self.return_type, rdata.span.clone())
                    .map_err(|e| e.into())
            }

            None => resolve!(
                self,
                &AbstractType::Unit(rdata.span.clone()),
                &self.return_type,
                rdata.span.clone()
            )
            .map_err(|e| e.into()),
        }
    }

    fn loop_start_true_path(&mut self, _id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn loop_end_true_path(&mut self, _id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn branch_split(
        &mut self,
        _id: NodeIndex,
        _b: &BranchingData,
        e: &ExprData,
    ) -> Result<(), E> {
        let expr_type = expr_type!(self, &e.expr)?;
        resolve!(self, &expr_type, &AbstractType::Bool(e.span.clone()), e.span.clone())?;

        Ok(())
    }

    fn branch_merge(
        &mut self,
        _id: NodeIndex,
        _b: &BranchingData,
    ) -> Result<(), E> {
        Ok(())
    }

    fn branch_start_true_path(&mut self, _id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn branch_start_false_path(&mut self, _id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn branch_end_true_path(
        &mut self,
        _id: NodeIndex,
        _b: &BranchingData,
    ) -> Result<(), E> {
        Ok(())
    }

    fn branch_end_false_path(
        &mut self,
        _id: NodeIndex,
        _b: &BranchingData,
    ) -> Result<(), E> {
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct TypingContext {
    pub type_vars: HashMap<TypeVarId, AbstractType>,
    pub var_type_map: HashMap<VarId, AbstractType>,
    pub fn_type_map: HashMap<FnId, AbstractType>,
    pub tmp_type_map: HashMap<TmpId, AbstractType>,
}

impl TypingContext {
    pub fn empty() -> TypingContext {
        TypingContext {
            type_vars: HashMap::new(),
            var_type_map: HashMap::new(),
            fn_type_map: HashMap::new(),
            tmp_type_map: HashMap::new(),
        }
    }

    pub fn get_type_var(&self, id: TypeVarId) -> Option<&AbstractType> {
        self.type_vars.get(&id)
    }

    pub fn tmp_type(&self, tmp_id: TmpId) -> &AbstractType {
        self.tmp_type_map
            .get(&tmp_id)
            .expect("Missing type for tmp")
    }
}

fn resolve_expr(
    universe: &mut Universe,
    metadata: &mut Metadata,
    module_id: ModuleId,
    scope: &ScopedData,
    context: &mut TypingContext,
    expr: &Expr,
) -> Result<AbstractType, AnalysisError> {
    let mut expr_type = None;
    for tmp_id in expr.execution_order() {
        let tmp = expr.get_tmp(tmp_id);

        let tmp_type =
            resolve_tmp(universe, metadata, module_id, scope, context, expr, tmp)?;
        expr_type = Some(tmp_type.clone());

        if context.tmp_type_map.insert(tmp_id, tmp_type).is_some() {
            panic!("Duplicate tmp ID");
        }
    }

    Ok(expr_type.unwrap())
}

fn resolve_tmp(
    universe: &mut Universe,
    metadata: &mut Metadata,
    module_id: ModuleId,
    scope: &ScopedData,
    context: &mut TypingContext,
    _expr: &Expr,
    tmp: &Tmp,
) -> Result<AbstractType, AnalysisError> {
    let tmp_span = tmp.span();
    let tmp_value = tmp.value();
    let tmp_type = match tmp_value.data() {
        Value::Literal(ref literal) => match *literal {
            Literal::Int(_) => AbstractType::Int(tmp_span.clone()),
            Literal::Float(_) => AbstractType::Float(tmp_span.clone()),
            Literal::String(_) => AbstractType::String(tmp_span.clone()),
            Literal::Bool(_) => AbstractType::Bool(tmp_span.clone()),
        },

        Value::BinExpr(ref op, ref lhs, ref rhs) => {
            // TODO: These clones are necessary b/c typing context may mutate
            //  If type inference becomes a thing, the function will need to be
            //    re-typechecked. No type inference ATM so nothing to do
            let lhs_type = context
                .tmp_type_map
                .get(lhs.data())
                .expect("Missing tmp")
                .substitute(universe, scope, context)?;
            let rhs_type = context
                .tmp_type_map
                .get(rhs.data())
                .expect("Missing tmp")
                .substitute(universe, scope, context)?;

            resolve_bin_op(
                universe, scope, context, op, &lhs_type, &rhs_type, tmp_span,
            )?
        }

        Value::UniExpr(ref op, ref uni_tmp) => {
            let uni_tmp_type = context
                .tmp_type_map
                .get(uni_tmp.data())
                .expect("Missing tmp")
                .substitute(universe, scope, context)?;

            resolve_uni_op(scope, context, op, &uni_tmp_type, tmp.span())?
        }

        Value::StructInit(ref init) => {
            resolve_struct_init(universe, scope, context, init, tmp.span())?
        }

        Value::AnonStructInit(ref init) => resolve_anon_struct_init(
            universe,
            scope,
            context,
            init,
            tmp.span(),
        )?,

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

        Value::AnonymousFn(ref a_fn) => resolve_anonymous_fn(
            universe,
            metadata,
            module_id,
            scope,
            context,
            a_fn,
            tmp.span(),
        )?,

        Value::FieldAccess(ref field_access) => resolve_field_access(
            universe,
            metadata,
            module_id,
            scope,
            context,
            field_access,
            tmp.span(),
        )?,
    };

    Ok(tmp_type)
}

/// Assume types are already applied
fn resolve_bin_op(
    universe: &Universe,
    scope: &ScopedData,
    context: &mut TypingContext,
    op: &ast::BinOp,
    lhs: &AbstractType,
    rhs: &AbstractType,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    use crate::ast::BinOp::*;

    let expected_int = AbstractType::Int(span.clone());
    let expected_float = AbstractType::Float(span.clone());
    let expected_bool = AbstractType::Bool(span.clone());

    let resolve_type = match *op {
        Add | Sub | Mul | Div | Mod => match (&lhs, &rhs) {
            (&AbstractType::Int(_), &AbstractType::Int(_)) => AbstractType::Int(span.clone()),
            (&AbstractType::Float(_), &AbstractType::Float(_)) => AbstractType::Float(span.clone()),

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
            (&AbstractType::Bool(_), &AbstractType::Bool(_)) => AbstractType::Bool(span.clone()),
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
            (&AbstractType::Int(_), &AbstractType::Int(_)) => AbstractType::Bool(span.clone()),
            (&AbstractType::Float(_), &AbstractType::Float(_)) => AbstractType::Bool(span.clone()),

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
            // TODO: Stricter equality check?
            type_resolver::resolve_types(
                universe, scope, context, lhs, rhs, span.clone(),
            )?;

            AbstractType::Bool(span.clone())
        }
    };

    Ok(resolve_type)
}

/// Assume types are already applied
fn resolve_uni_op(
    _scope: &ScopedData,
    _context: &TypingContext,
    op: &ast::UniOp,
    tmp_type: &AbstractType,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    use crate::ast::UniOp::*;

    let expected_int = AbstractType::Int(span.clone());

    let expected_float = AbstractType::Float(span.clone());

    let expected_bool = AbstractType::Bool(span.clone());

    match *op {
        Negate => match tmp_type {
            AbstractType::Int(_) | AbstractType::Float(_) => Ok(tmp_type.clone()),
            _ => Err(TypeError::UniOp {
                op: op.clone(),
                expected: vec![expected_int, expected_float],
                expr: tmp_type.clone(),
                span: span,
            }
            .into()),
        },

        LogicalInvert => match tmp_type {
            AbstractType::Bool(_) => Ok(tmp_type.clone()),
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

fn resolve_struct_init(
    universe: &Universe,
    scope: &ScopedData,
    context: &mut TypingContext,
    init: &StructInit,
    init_span: Span,
) -> Result<AbstractType, AnalysisError> {
    // Get type info
    let type_name = init.type_name();
    let tmp_type_name = type_name.clone().into();
    let struct_type_id = scope
        .type_cons(&tmp_type_name)
        .ok_or(AnalysisError::UnknownType(type_name.clone(), init_span.clone()))?;

    let type_args = init
        .type_args()
        .map(|vec| {
            vec.iter()
                .map(|ann| type_from_ann(scope, context, ann))
                .collect::<Result<Vec<_>, _>>()
        })
        .unwrap_or(Ok(Vec::new()))?;

    // TODO: Take into account type arguments
    let struct_type = AbstractType::App {
        data: init_span.clone(),
        type_cons: struct_type_id,
        args: type_args,
    }
    .substitute(universe, scope, context)?;

    // Check if type is a struct.
    // Opaque types are represented using AbstractType::Opaque
    let (_struct_type_id, fields, field_map) = match struct_type {
        AbstractType::Record {
            type_id: struct_type_id,
            ref abstract_field_map,
            ..
        } => (
            struct_type_id,
            &abstract_field_map.fields,
            &abstract_field_map.field_map,
        ),

        AbstractType::Opaque { .. } => {
            return Err(TypeError::InitOpaqueType {
                struct_type: struct_type,
                span: init_span,
            }
            .into());
        }

        _ => {
            return Err(TypeError::NotAStruct {
                type_name: type_name.clone(),
                found: struct_type,
                span: init_span,
            }
            .into());
        }
    };

    // Map init'd field to its type
    let mut init_expr_type_map: HashMap<FieldId, AbstractType> = HashMap::new();
    for (field_name, typed_tmp) in init.raw_field_init() {
        // Check if the struct type has the corresponding field
        let field_id =
            field_map.get(field_name).ok_or(TypeError::UnknownField {
                name: field_name.clone(),
                struct_type: struct_type.clone(),
                span: init_span.clone(),
            })?;

        let tmp_type = context
            .tmp_type_map
            .get(typed_tmp.data())
            .expect("Missing tmp")
            .clone();

        if init_expr_type_map
            .insert(field_id.clone(), tmp_type)
            .is_some()
        {
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
            span: init_span,
        }
        .into());
    }

    // SATISFIED CONDITIONS:
    //   Field init expressions should be fully typed (tmps)
    //   Field names are all present and all valid

    // Check if field init expressions are of the correct type
    for (field_id, field_type) in fields.iter() {
        let init_expr_type = init_expr_type_map.get(field_id).unwrap();

        // TODO: If type inference is implemented, another pass needs to check
        //   that all types are still valid
        type_resolver::resolve_types(
            universe,
            scope,
            context,
            init_expr_type,
            field_type,
            init_span.clone(),
        )?;
    }

    // SATISFIED CONDITIONS:
    //   Field init expressions should be fully typed (tmps)
    //   Field names are all present and all valid
    //   Field init expressions are valid types for their corresponding fields

    Ok(struct_type)
}

/// Generates a WidthConstraint based on the types of its initializer expressions
fn resolve_anon_struct_init(
    _universe: &Universe,
    _scope: &ScopedData,
    context: &TypingContext,
    init: &AnonStructInit,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    
    let mut fields: HashMap<_, AbstractType> = HashMap::new();
    // Map init'd field to its type
    let mut duplicate_fields = Vec::new();
    for (field_name, typed_tmp) in init.raw_field_init() {
        let tmp_type =
            context.tmp_type_map.get(typed_tmp).expect("Missing tmp");

        if fields.contains_key(field_name) {
            duplicate_fields.push(field_name.clone());
        } else {
            fields
                .insert(field_name.clone(), tmp_type.clone());
        }
    }

    if duplicate_fields.len() != 0 {
        return Err(TypeError::InvalidInitialization {
            fields: duplicate_fields,
            span: span,
        }
        .into());
    }

    let width_type = AbstractType::WidthConstraint {
        data: span,
        width: AbstractWidthConstraint::new_evaluated(fields),
    };
    Ok(width_type)
}

fn resolve_binding(
    universe: &Universe,
    scope: &ScopedData,
    context: &TypingContext,
    binding: &Binding,
    bind_span: Span,
) -> Result<AbstractType, AnalysisError> {
    match binding.get_id().unwrap() {
        BindingId::Var(var_id) => Ok(context
            .var_type_map
            .get(&var_id)
            .expect("Missing VarId")
            .clone()),

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
                .expect(
                    "Expect anonymous function types to already be resolved",
                )
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
                data: bind_span,
                type_cons: fn_type_id,
                args: Vec::new(),
            }
            .substitute(universe, scope, context)?;

            Ok(fn_type)
        }
    }
}

fn resolve_mod_access(
    universe: &Universe,
    scope: &ScopedData,
    context: &TypingContext,
    mod_access: &ModAccess,
    access_span: Span,
) -> Result<AbstractType, AnalysisError> {
    let fn_id = mod_access
        .fn_id()
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
        .fn_type()
        .expect("Expect anonymous functions to already be resolved");

    let fn_type = AbstractType::App {
        data: access_span,
        type_cons: fn_type_id,
        args: Vec::new(),
    }
    .substitute(universe, scope, context)?;

    Ok(fn_type)
}

fn resolve_fn_call(
    _universe: &Universe,
    _scope: &ScopedData,
    context: &TypingContext,
    fn_call: &FnCall,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    let fn_value = fn_call.fn_value();
    let fn_value_type =
        context.tmp_type_map.get(&fn_value).expect("Missing TMP");

    // Check args and parameters align
    match fn_value_type {
        AbstractType::Function {
            data: ref _fn_type_span,
            parameters: ref params,
            ref return_type,
        } => {
            let arg_types = fn_call.args().map(|ref vec| {
                vec.iter()
                    .map(|ref tmp_id| {
                        context
                            .tmp_type_map
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

                    for (_index, (arg_type, param_type)) in
                        arg_types.iter().zip(fn_param_types).enumerate()
                    {
                        let _arg_type: &AbstractType = arg_type;
                        let _param_type: &AbstractType = param_type;
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

        AbstractType::UncheckedFunction { return_type, .. } => {
            Ok(*(return_type.clone()))
        }

        t @ _ => panic!("Function call on a non-function type: {:?}", t),
    }
}

fn resolve_array_init(
    universe: &Universe,
    scope: &ScopedData,
    context: &mut TypingContext,
    init: &ArrayInit,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    match *init {
        ArrayInit::List(ref vec) => {
            let size = vec.len() as u64;
            let element_types = vec
                .iter()
                .map(|ref tmp_id| {
                    let tmp_type = context
                        .tmp_type_map
                        .get(tmp_id.data())
                        .expect("Missing TMP")
                        .clone();
                    (tmp_type, span.clone())
                })
                .collect::<Vec<_>>();

            let mut expected_element_type = None;

            for (i, (current_element_type, span)) in
                element_types.into_iter().enumerate()
            {
                if expected_element_type.is_none() {
                    expected_element_type = Some(
                        current_element_type
                            .substitute(universe, scope, context)?,
                    );
                    continue;
                }

                let expected_element_type =
                    expected_element_type.as_ref().unwrap();

                type_resolver::resolve_types(
                    universe,
                    scope,
                    context,
                    &current_element_type,
                    expected_element_type,
                    span.clone(),
                )
                .map_err(|_| TypeError::HeterogenousArray {
                    expected: expected_element_type.clone(),
                    found: current_element_type.clone(),
                    index: i,
                    span: span,
                })?;
            }

            let array_type = AbstractType::Array {
                data: span,
                element_type: Box::new(expected_element_type.unwrap().clone()),
                size: size,
            };

            Ok(array_type)
        }

        ArrayInit::Value(ref val, size) => {
            let element_type =
                context.tmp_type_map.get(val.data()).expect("Missing TMP");

            let array_type = AbstractType::Array {
                data: span,
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

fn resolve_indexing(
    universe: &Universe,
    scope: &ScopedData,
    context: &TypingContext,
    indexing: &Indexing,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    let expected_element_type: AbstractType = {
        // Check type is array
        let tmp_type = context
            .tmp_type_map
            .get(indexing.array.data())
            .expect("Missing TMP");

        // TODO: Already applied?
        match &tmp_type {
            AbstractType::Array {
                ref element_type, ..
            } => element_type.substitute(universe, scope, context)?,

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
        let tmp_type = context
            .tmp_type_map
            .get(indexing.indexer.data())
            .expect("Missing TMP");

        // TODO: Already applied?
        match &tmp_type {
            AbstractType::Int(_) => (),

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

fn resolve_type_inst(
    universe: &Universe,
    scope: &ScopedData,
    context: &TypingContext,
    type_inst: &TypeInst,
    inst_span: Span,
) -> Result<AbstractType, AnalysisError> {
    let fn_id = type_inst
        .get_id()
        .expect("No FN ID. Should be caught in scope resolution");

    let fn_type_id = universe
        .get_fn(fn_id)
        .fn_type()
        .expect("Expect anonymous functions to already be resolved");

    let type_args = type_inst
        .args()
        .iter()
        .map(|ann| type_from_ann(scope, context, ann))
        .collect::<Result<Vec<_>, _>>()?;

    let inst_type = AbstractType::App {
        data: inst_span,
        type_cons: fn_type_id,
        args: type_args,
    }
    .substitute(universe, scope, context)?;

    Ok(inst_type)
}

fn resolve_anonymous_fn(
    universe: &mut Universe,
    metadata: &mut Metadata,
    module_id: ModuleId,
    scope: &ScopedData,
    context: &TypingContext,
    a_fn: &AnonymousFn,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    use std::cell::RefCell;
    use std::rc::Rc;

    let fn_id = a_fn.fn_id();

    let mut resolved = None;
    let mut carry_data = None;
    if let Function::Anonymous(ref afn) = universe.get_fn(fn_id) {
        if let AnonymousFunction::Reserved(ref ast_anonymous_fn) = afn {
            let fn_type_cons =
                super::type_cons_gen::generate_anonymous_fn_type(
                    universe,
                    metadata,
                    scope,
                    context,
                    fn_id,
                    ast_anonymous_fn.data(),
                )?;

            let anon_span = ast_anonymous_fn.span();

            let analysis_context = analysis_helpers::generate_fn_analysis_data(
                universe,
                scope,
                context,
                &fn_type_cons,
                ast_anonymous_fn.data(),
            )?;

            let body = ast_anonymous_fn.data().body.clone();

            // Needed to get around possible future borrow checker error
            // See issue #59159 (mutable_borrow_reservation_conflict)
            carry_data = Some((body, analysis_context, fn_type_cons, anon_span)); 
        }
    } else {
        panic!("FN ID did not refer to an anonymous function");
    }

    // Needed to get around possible future borrow checker error
    // See issue #59159 (mutable_borrow_reservation_conflict)
    if let Some((body, analysis_context, fn_type_cons, anon_span)) = carry_data {
        let cfg = super::control_flow::CFG::generate(
            universe,
            body,
            &fn_type_cons,
            &analysis_context,
        )?;

        let fn_type_id = universe.insert_type_cons(fn_type_cons);

        resolved = Some(AnonymousFunction::Resolved {
            fn_type: crate::ast::AstNode::new(fn_type_id, anon_span),
            analysis_context: analysis_context,
            cfg: Rc::new(RefCell::new(cfg)),
        });

        // Mark the function as owned by a module
        let module = universe.get_module_mut(module_id);
        module.owned_fns.insert(fn_id);
    }

    if let Some(resolved) = resolved {
        *universe.get_fn_mut(fn_id) = Function::Anonymous(resolved);
        
        // Make anonymous function owned by SMPL module
        universe.get_module_mut(module_id.clone()).owned_fns.insert(fn_id);

        analysis_helpers::analyze_fn(universe, metadata, module_id, fn_id)?;
    }

    let fn_type = if let Function::Anonymous(ref afn) = universe.get_fn(fn_id) {
        if let AnonymousFunction::Resolved { ref fn_type, .. } = afn {
            let fn_type_cons = fn_type.data().clone();

            // TODO: Anonymous functions are not allowed to have type parameters
            AbstractType::App {
                data: fn_type.span(),
                type_cons: fn_type_cons,
                args: Vec::new(),
            }
            .substitute(universe, scope, context)?
        } else {
            panic!("Anonymous function should be resolved.");
        }
    } else {
        panic!("FN ID did not refer to an anonymous function");
    };

    Ok(fn_type)
}

/// Assumes that all previous temporaries in Expr are already typed
fn resolve_field_access(
    universe: &mut Universe,
    metadata: &mut Metadata,
    module_id: ModuleId,
    scope: &ScopedData,
    context: &mut TypingContext,
    field_access: &FieldAccess,
    span: Span,
) -> Result<AbstractType, AnalysisError> {
    fn generate_field_retriever(
        universe: &Universe,
        scope: &ScopedData,
        context: &TypingContext,
        current_type: AbstractType,
        index: usize,
        field_access: &FieldAccess,
        root_var_type: &AbstractType,
        span: Span,
    ) -> Result<
        Box<dyn Fn(&crate::ast::Ident) -> Result<AbstractType, AnalysisError>>,
        AnalysisError,
    > {
        match current_type.substitute(universe, scope, context)? {
            AbstractType::WidthConstraint {
                data: width_span,
                width: awc,
            } => Ok(Box::new(move |name| {
                awc.fields().get(name).map(|t| t.clone()).ok_or(
                    TypeError::UnknownField {
                        name: name.clone(),
                        struct_type: AbstractType::WidthConstraint {
                            data: width_span.clone(),
                            width: awc.clone(),
                        },
                        span: span.clone(),
                    }
                    .into(),
                )
            })),

            AbstractType::Record {
                data: record_span,
                type_id,
                abstract_field_map: afm,
            } => {
                let fields = afm.fields;
                let field_map = afm.field_map;

                Ok(Box::new(move |name| {
                    let field_id = field_map
                        .get(name)
                        .map(|t| t.clone())
                        .ok_or(TypeError::UnknownField {
                            name: name.clone(),
                            struct_type: AbstractType::Record {
                                data: record_span.clone(),
                                type_id: type_id,
                                abstract_field_map: AbstractFieldMap {
                                    fields: fields.clone(),
                                    field_map: field_map.clone(),
                                },
                            },
                            span: span.clone(),
                        })?;

                    let field_type =
                        fields.get(&field_id).map(|t| t.clone()).unwrap();

                    Ok(field_type)
                }))
            }

            AbstractType::TypeVar(ref span, ref type_var) => {
                let type_var_value = context
                    .get_type_var(*type_var)
                    .expect(&format!("Missing type variable: {}", type_var));

                generate_field_retriever(
                    universe,
                    scope,
                    context,
                    type_var_value.substitute(universe, scope, context)?,
                    index,
                    field_access,
                    root_var_type,
                    span.clone(),
                )
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
        }
    }

    let path = field_access.path();
    let path_iter = path.path().iter();

    let root_var_id = path.root_var_id();
    let root_var_type = context
        .var_type_map
        .get(&root_var_id)
        .expect("Missing VAR")
        .clone();

    let mut current_type: AbstractType = root_var_type.clone();

    if let Some(expr) = path.root_indexing_expr() {
        let indexing_type =
            resolve_expr(universe, metadata, module_id.clone(), scope, context, expr)?;

        match indexing_type.substitute(universe, scope, context)? {
            AbstractType::Int(_) => (),
            _ => {
                return Err(TypeError::InvalidIndex {
                    found: indexing_type.clone(),
                    span: expr.get_tmp(expr.last()).span(),
                }
                .into());
            }
        }

        match current_type.substitute(universe, scope, context)? {
            AbstractType::Array { element_type, .. } => {
                current_type = *(element_type.clone());
            }
            _ => {
                return Err(TypeError::NotAnArray {
                    found: root_var_type.clone(),
                    span: expr.get_tmp(expr.last()).span(),
                }
                .into());
            }
        }
    }

    for (index, field) in path_iter.enumerate() {
        let next_type: AbstractType;
        let field_type_retriever: Box<
            dyn Fn(&crate::ast::Ident) -> Result<AbstractType, AnalysisError>,
        > = generate_field_retriever(
            universe,
            scope,
            context,
            current_type,
            index,
            field_access,
            &root_var_type,
            span.clone(),
        )?;

        match *field {
            PathSegment::Ident(ref field) => {
                next_type = field_type_retriever(field.name())?.clone();
            }

            PathSegment::Indexing(ref field, ref indexing) => {
                let field_type = field_type_retriever(field.name())?;

                let indexing_type =
                    resolve_expr(universe, metadata, module_id, scope, context, indexing)?;

                // TODO: Application?
                match indexing_type {
                    AbstractType::Int(_) => (),

                    _ => {
                        return Err(TypeError::InvalidIndex {
                            found: indexing_type.clone(),
                            span: indexing.get_tmp(indexing.last()).span(),
                        }
                        .into());
                    }
                };

                // TODO: Application?
                match field_type {
                    AbstractType::Array {
                        data,
                        element_type,
                        size: _,
                    } => {
                        next_type = *element_type;
                    }

                    ref t => {
                        return Err(TypeError::NotAnArray {
                            found: field_type.clone(),
                            span: t.span().clone(),
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
