use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::ast;
use crate::span::Span;

use super::unique_linear_cfg_traversal::*;
use super::control_data::*;
use super::control_flow::CFG;
use super::semantic_data::{TmpId, FnId, VarId, TypeParamId, TypeId, Universe, ModulePath};
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

fn resolve_expr(scope: &ScopedData, context: &mut TypingContext, expr: &Expr) 
    -> Result<AbstractType, AnalysisError> {

    let mut expr_type = None;
    for tmp_id in expr.execution_order() {
        let tmp = expr.get_tmp(tmp_id);

        let tmp_type = resolve_tmp(scope, context, tmp)?;
        let expr_type = Some(tmp_type.clone());

        if context.tmp_type_map
            .insert(tmp_id, tmp_type)
            .is_some() {
            panic!("Duplicate tmp ID"); 
        }
    }

    Ok(expr_type.unwrap())
}

fn resolve_tmp(scope: &ScopedData, context: &mut TypingContext, tmp: &Tmp) 
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
