use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::ast::{AstNode, Ident, ModulePath as AstModulePath};

use super::control_data::*;
use super::unique_linear_cfg_traversal::*;

use super::error::AnalysisError;
use super::semantic_data::{
    FnId, ModulePath, TypeId, TypeVarId, VarId, AnonymousFn as ResolvedAnonymousFn,
};
use super::analysis_context::{
    AnalyzableFn, AnalyzableAnonymousFn as AnonymousFn,
};
use super::typed_ast::*;
use super::anon_storage::AnonStorage;

pub fn resolve(to_resolve: &mut AnalyzableFn)
    -> Result<AnonStorage<ScopedData>, AnalysisError> {

    let mut scope_resolver = ScopeResolver::new(to_resolve);

    let result: Result<(), _> = match to_resolve {
        AnalyzableFn::SMPL(ref mut smpl_fn) => {
            let cfg = &mut smpl_fn.cfg;
            traverse(cfg, &mut scope_resolver)
        }

        AnalyzableFn::Anonymous(ref mut anon_fn) => match anon_fn {
            AnonymousFn::Reserved(..) => {
                panic!("Anonymous function should be resolved")
            }
            AnonymousFn::Resolved(ResolvedAnonymousFn { ref mut cfg, .. }) => {
                traverse(cfg, &mut scope_resolver)
            }
        },

        AnalyzableFn::Builtin(..) => {
            panic!("Unable to resolve scope of builtin functions")
        }
    };

    result.map(|_| scope_resolver.anon_scope_storage)
}

struct ScopeResolver {
    anon_scope_storage: AnonStorage<ScopedData>,
    scopes: Vec<ScopedData>,
}

impl ScopeResolver {

    // Formal parameters should already be in the function scope
    //  (in generate_fn_type())
    pub fn new(to_resolve: &AnalyzableFn) -> ScopeResolver {

        match to_resolve {
            AnalyzableFn::Builtin(_) => unimplemented!(),
            AnalyzableFn::Anonymous(ref anonymous_fn) => {
                let fn_scope = match anonymous_fn {
                    AnonymousFn::Reserved(..) => {
                        panic!("Expected anonymous functions to already be resolved");
                    }

                    AnonymousFn::Resolved(ResolvedAnonymousFn {
                        ref analysis_context,
                        ..
                    }) => analysis_context.parent_scope().clone(),
                };

                ScopeResolver {
                    anon_scope_storage: AnonStorage::new(),
                    scopes: vec![fn_scope],
                }
            }

            AnalyzableFn::SMPL(ref smpl_function) => ScopeResolver {
                anon_scope_storage: AnonStorage::new(),
                scopes: vec![smpl_function
                    .analysis_context()
                    .parent_scope()
                    .clone()],
            },
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

    fn resolve_expr_scope(&mut self,
        expr: &mut Expr,
    ) -> Result<(), AnalysisError> {

        for tmp_id in expr.execution_order() {
            let tmp = expr.get_tmp_mut(tmp_id);
            match tmp.value_mut().data_mut() {
                Value::Literal(ref mut _literal) => (),

                Value::StructInit(ref mut _init) => (),

                Value::AnonStructInit(ref mut _init) => (),

                Value::Binding(ref mut var) => {
                    let current_scope = self.current();

                    match current_scope.binding_info(var.ident())? {
                        BindingInfo::Var(var_id) => {
                            var.set_id(var_id);
                        }

                        BindingInfo::Fn(fn_id) => {
                            var.set_id(fn_id);
                        }
                    }
                }

                // TODO: FieldID resolution depends on types
                Value::FieldAccess(ref mut field_access) => {
                    let path = field_access.path_mut();

                    for segment in path.path_mut() {
                        match segment {
                            PathSegment::Indexing(_, ref mut e)
                                => self.resolve_expr_scope(e)?,

                            _ => (),
                        }
                    }

                    let current_scope = self.current();
                    let var_id = current_scope.var_id(path.root_name())?;
                    path.set_root_var(var_id);
                }

                Value::BinExpr(..) => (),

                Value::UniExpr(..) => (),

                Value::FnCall(..) => (),

                Value::ArrayInit(..) => (),

                Value::Indexing(..) => (),

                Value::ModAccess(ref mut access) => {
                    let current_scope = self.current();
                    let fn_id = current_scope.get_fn(&access.path())?;
                    access.set_fn_id(fn_id);
                }

                // TODO: Generate anonymous functions in a separate phase
                //  Relies on type information
                // FnId is generated in expr_flow
                Value::AnonymousFn(anon_fn) => {
                    let parent_scope = self.current().clone();
                    let anon_fn_id = anon_fn.fn_id();
                    self.anon_scope_storage
                        .insert(anon_fn_id, parent_scope);
                }

                Value::TypeInst(ref mut type_inst) => {
                    let current_scope = self.current();
                    let fn_id = current_scope.get_fn(type_inst.path())?;

                    type_inst.set_id(fn_id);
                }
            }
        }

        Ok(())
    }
}

type E = AnalysisError;

impl UniquePassenger<E> for ScopeResolver {
    fn start(&mut self, _id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn end(&mut self, _id: NodeIndex) -> Result<(), E> {
        Ok(())
    }

    fn loop_head(
        &mut self,
        _id: NodeIndex,
        _ld: &mut LoopData,
        expr: &mut ExprData,
    ) -> Result<(), E> {
        self.resolve_expr_scope(&mut expr.expr)?;
        Ok(())
    }

    fn loop_foot(
        &mut self,
        _id: NodeIndex,
        _ld: &mut LoopData,
    ) -> Result<(), E> {
        Ok(())
    }

    fn cont(&mut self, _id: NodeIndex, _ld: &mut LoopData) -> Result<(), E> {
        Ok(())
    }

    fn br(&mut self, _id: NodeIndex, _ld: &mut LoopData) -> Result<(), E> {
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
        decl: &mut LocalVarDeclData,
    ) -> Result<(), E> {
        let var_decl = &mut decl.decl;

        self.resolve_expr_scope(var_decl.init_expr_mut())?;

        let name = var_decl.var_name().clone();
        let var_id = var_decl.var_id();
        self.current_mut().insert_var(name, var_id);

        Ok(())
    }

    fn assignment(
        &mut self,
        _id: NodeIndex,
        assign: &mut AssignmentData,
    ) -> Result<(), E> {
        let assignment = &mut assign.assignment;

        let path = assignment.assignee_mut().path_mut();

        let var_id = self.current().var_id(path.root_name())?;
        path.set_root_var(var_id);

        // Resolve expression scopes for root indexing expressions
        if let Some(root_index) = path.root_indexing_expr_mut() {
            self.resolve_expr_scope(root_index)?;
        }

        // Resolve expression scopes for inner indexing expressions
        for segment in path.path_mut() {
            if let PathSegment::Indexing(_, ref mut index_expr) = segment {
                self.resolve_expr_scope(index_expr)?;
            }
        }

        self.resolve_expr_scope(assignment.value_mut())?;
        Ok(())
    }

    fn expr(&mut self, _id: NodeIndex, expr: &mut ExprData) -> Result<(), E> {
        self.resolve_expr_scope(&mut expr.expr)?;
        Ok(())
    }

    fn ret(&mut self, _id: NodeIndex, rdata: &mut ReturnData) -> Result<(), E> {
        if let Some(ref mut return_data) = rdata.expr {
            self.resolve_expr_scope(return_data)?;
        }
        Ok(())
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
        _b: &mut BranchingData,
        e: &mut ExprData,
    ) -> Result<(), E> {
        self.resolve_expr_scope(&mut e.expr)?;
        Ok(())
    }

    fn branch_merge(
        &mut self,
        _id: NodeIndex,
        _b: &mut BranchingData,
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
        _b: &mut BranchingData,
    ) -> Result<(), E> {
        Ok(())
    }

    fn branch_end_false_path(
        &mut self,
        _id: NodeIndex,
        _b: &mut BranchingData,
    ) -> Result<(), E> {
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct ScopedData {
    type_cons_map: HashMap<ModulePath, TypeId>,
    var_map: HashMap<Ident, VarId>,
    fn_map: HashMap<ModulePath, FnId>,
    type_param_map: HashMap<Ident, TypeVarId>,
}

impl ScopedData {
    pub fn new(type_cons_map: HashMap<ModulePath, TypeId>) -> ScopedData {
        ScopedData {
            type_cons_map: type_cons_map,
            var_map: HashMap::new(),
            fn_map: HashMap::new(),
            type_param_map: HashMap::new(),
        }
    }

    pub fn clear_scoped_vars(&mut self) {
        self.var_map.clear();
    }

    pub fn insert_fn(&mut self, name: ModulePath, fn_id: FnId) {
        // TODO: Fn name override behaviour?
        self.fn_map.insert(name, fn_id);
    }

    pub fn unmap_fn(&mut self, name: &ModulePath) {
        self.fn_map.remove(&name).unwrap();
    }

    pub fn type_cons<'a, 'b, 'c>(
        &'a self,
        path: &'c ModulePath,
    ) -> Option<TypeId> {
        self.type_cons_map.get(path).map(|id| id.clone())
    }

    pub fn insert_type_cons(
        &mut self,
        path: ModulePath,
        id: TypeId,
    ) -> Option<TypeId> {
        self.type_cons_map.insert(path, id)
    }

    pub fn binding_info(
        &self,
        name: &AstNode<Ident>,
    ) -> Result<BindingInfo, AnalysisError> {
        match self.var_map.get(name.data()) {
            Some(v_id) => Ok(BindingInfo::Var(v_id.clone())),

            None => {
                let p = ModulePath(vec![name.data().clone()]);
                self.fn_map
                    .get(&p)
                    .map(|f| BindingInfo::Fn(f.clone()))
                    .ok_or(AnalysisError::UnknownBinding(
                        name.data().clone(),
                        name.span(),
                    ))
            }
        }
    }

    pub fn var_id(
        &self,
        name: &AstNode<Ident>,
    ) -> Result<VarId, AnalysisError> {
        let var_id = self
            .var_map
            .get(name.data())
            .ok_or(AnalysisError::UnknownBinding(
                name.data().clone(),
                name.span(),
            ))?
            .clone();

        Ok(var_id)
    }

    pub fn insert_var(&mut self, name: Ident, id: VarId) -> Option<VarId> {
        self.var_map.insert(name, id)
    }

    pub fn get_fn(&self, path: &AstModulePath) -> Result<FnId, AnalysisError> {
        self.fn_map
            .get(&path.clone().into())
            .map(|id| id.clone())
            .ok_or(AnalysisError::UnknownFn(path.clone(), path.span()))
    }

    pub fn insert_type_var(&mut self, ident: Ident, id: TypeVarId) -> bool {
        self.type_param_map.insert(ident, id).is_some()
    }

    pub fn type_var<'a, 'b>(&'a self, ident: &'b Ident) -> Option<TypeVarId> {
        self.type_param_map.get(ident).map(|id| id.clone())
    }

    pub fn type_vars<'a>(&'a self) -> impl Iterator<Item = TypeVarId> + 'a {
        self.type_param_map.values().map(|id| id.clone())
    }

    pub fn all_types(&self) -> impl Iterator<Item = (&ModulePath, TypeId)> {
        self.type_cons_map
            .iter()
            .map(|(path, id)| (path, id.clone()))
    }

    pub fn all_fns(&self) -> impl Iterator<Item = (&ModulePath, FnId)> {
        self.fn_map.iter().map(|(path, id)| (path, id.clone()))
    }
}

pub enum BindingInfo {
    Var(VarId),
    Fn(FnId),
}
