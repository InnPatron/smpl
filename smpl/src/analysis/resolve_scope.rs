use std::collections::HashMap;

use crate::ast::ModulePath as AstModulePath;
use crate::ast::*;

use super::semantic_data::{FnId, VarId, TypeParamId, TypeId, Universe, ModulePath};
use super::error::AnalysisError;
use super::metadata::Metadata;

#[derive(Clone, Debug)]
pub struct ScopedData {
    type_cons_map: HashMap<ModulePath, TypeId>,
    var_map: HashMap<Ident, VarId>,
    fn_map: HashMap<ModulePath, FnId>,
    type_param_map: HashMap<Ident, TypeParamId>,
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

    pub fn insert_fn(&mut self, name: ModulePath, fn_id: FnId) {
        // TODO: Fn name override behaviour?
        self.fn_map.insert(name, fn_id);
    }

    pub fn unmap_fn(&mut self, name: &ModulePath) {
        self.fn_map.remove(&name).unwrap();
    }

    pub fn type_cons<'a, 'b, 'c>(
        &'a self,
        _universe: &'b Universe,
        path: &'c ModulePath,
    ) -> Option<TypeId> {
        self.type_cons_map.get(path).map(|id| id.clone())
    }

    pub fn insert_type_cons(&mut self, path: ModulePath, id: TypeId) -> Option<TypeId> {
        self.type_cons_map.insert(path, id)
    }

    pub fn binding_info(&self, name: &AstNode<Ident>) -> Result<BindingInfo, AnalysisError> {
        match self.var_map.get(name.data()) {
            Some(v_id) => Ok(BindingInfo::Var(
                v_id.clone(),
            )),

            None => {
                let p = ModulePath(vec![name.data().clone()]);
                self.fn_map
                    .get(&p)
                    .map(|f| BindingInfo::Fn(f.clone()))
                    .ok_or(AnalysisError::UnknownBinding(name.data().clone(), name.span()))
            }
        }
    }

    pub fn var_id(&self, name: &AstNode<Ident>) -> Result<VarId, AnalysisError> {
        let var_id = self
            .var_map
            .get(name.data())
            .ok_or(AnalysisError::UnknownBinding(name.data().clone(), name.span()))?
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
            .ok_or(AnalysisError::UnknownFn(path.clone()))
    }

    pub fn insert_type_param(&mut self, 
                             ident: Ident, 
                             id: TypeParamId) -> bool {
        self.type_param_map.insert(ident, id).is_some()
    }

    pub fn type_param<'a, 'b>(&'a self, ident: &'b Ident) -> Option<TypeParamId> {
        self.type_param_map
            .get(ident)
            .map(|id| id.clone())
    }

    pub fn type_params<'a>(&'a self) 
        -> impl Iterator<Item = TypeParamId> + 'a {
        self.type_param_map
            .values()
            .map(|id| id.clone())
    }

    pub fn all_types(&self) -> impl Iterator<Item=(&ModulePath, TypeId)> {
        self.type_cons_map
            .iter()
            .map(|(path, id)| (path, id.clone()))
    }

    pub fn all_fns(&self) -> impl Iterator<Item=(&ModulePath, FnId)> {
        self.fn_map
            .iter()
            .map(|(path, id)| (path, id.clone()))
    }
}

pub enum BindingInfo {
    Var(VarId),
    Fn(FnId),
}
