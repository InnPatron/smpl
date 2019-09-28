use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use super::unique_linear_cfg_traversal::*;
use super::control_data::*;
use super::control_flow::CFG;
use super::semantic_data::{FnId, VarId, TypeParamId, TypeId, Universe, ModulePath};
use super::error::AnalysisError;
use super::typed_ast::*;
use super::type_cons::*;
use super::resolve_scope::ScopedData;

struct TypeChecker {
    scopes: Vec<ScopedData>,
}

impl TypeChecker {

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
}
