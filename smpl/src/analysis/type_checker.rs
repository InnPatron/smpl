use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::ast;
use crate::span::Span;

use super::unique_linear_cfg_traversal::*;
use super::control_data::*;
use super::control_flow::CFG;
use super::semantic_data::{TmpId, FieldId, FnId, VarId, TypeParamId, TypeId, Universe, ModulePath};
use super::error::*;
use super::typed_ast::*;
use super::type_cons::*;
use super::resolve_scope::ScopedData;

struct TypeChecker {
    scopes: Vec<ScopedData>,
}

impl TypeChecker {

    // TODO: Store function (return) type somwhere
    // TODO: Add function parameters somewhere
    pub fn new(inherited_scope: ScopedData) -> TypeChecker {
        TypeChecker {
            scopes: vec![inherited_scope]
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

struct TypingContext {
    type_params: HashMap<TypeParamId, AbstractType>,
    var_type_map: HashMap<VarId, AbstractType>,
    fn_type_map: HashMap<FnId, AbstractType>,
    tmp_type_map: HashMap<TmpId, AbstractType>,
}

fn resolve_expr(universe: &Universe, scope: &ScopedData, context: &mut TypingContext, expr: &Expr) 
    -> Result<AbstractType, AnalysisError> {

    let mut expr_type = None;
    for tmp_id in expr.execution_order() {
        let tmp = expr.get_tmp(tmp_id);

        let tmp_type = resolve_tmp(universe, scope, context, tmp)?;
        let expr_type = Some(tmp_type.clone());

        if context.tmp_type_map
            .insert(tmp_id, tmp_type)
            .is_some() {
            panic!("Duplicate tmp ID"); 
        }
    }

    Ok(expr_type.unwrap())
}

fn resolve_tmp(universe: &Universe, scope: &ScopedData, context: &mut TypingContext, tmp: &Tmp) 
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

        _ => unimplemented!(),

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
    .apply(universe, scope)?;

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
