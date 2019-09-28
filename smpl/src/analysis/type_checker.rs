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


struct TypingContext {
    type_params: HashMap<TypeParamId, AbstractType>,
    var_type_map: HashMap<VarId, AbstractType>,
    fn_type_map: HashMap<FnId, AbstractType>,
}
