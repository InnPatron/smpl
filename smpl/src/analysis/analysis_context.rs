use std::cell::Cell;
use std::collections::HashMap;

use crate::ast::{Ident, AstNode, AnonymousFn as AstAnonymousFn};
use crate::span::Span;

use super::semantic_data::*;
use super::control_flow::CFG;
use super::type_checker::TypingContext;
use super::resolve_scope::ScopedData;
use super::semantic_data::{
    FieldId, FnId, Program, TypeId, TypeParamId, TypeVarId, LoopId,
    BranchingId, TmpId, VarId,
};

pub struct GlobalData {
    id_counter: Cell<u64>,
    int_type_id: TypeId,
    float_type_id: TypeId,
    bool_type_id: TypeId,
    string_type_id: TypeId,
    unit_type_id: TypeId,
}

impl GlobalData {

    pub fn new() -> GlobalData {
        let mut g = GlobalData {
            id_counter: Cell::new(0),
            int_type_id: TypeId(0),
            float_type_id: TypeId(0),
            bool_type_id: TypeId(0),
            string_type_id: TypeId(0),
            unit_type_id: TypeId(0),
        };

        g.unit_type_id = g.new_type_id();
        g.int_type_id = g.new_type_id();
        g.float_type_id = g.new_type_id();
        g.string_type_id = g.new_type_id();
        g.bool_type_id = g.new_type_id();

        g
    }

    pub fn int_type_id(&self) -> TypeId {
        self.int_type_id.clone()
    }

    pub fn float_type_id(&self) -> TypeId {
        self.float_type_id.clone()
    }

    pub fn bool_type_id(&self) -> TypeId {
        self.bool_type_id.clone()
    }

    pub fn string_type_id(&self) -> TypeId {
        self.string_type_id.clone()
    }

    pub fn unit_type_id(&self) -> TypeId {
        self.unit_type_id.clone()
    }

    fn inc_counter(&self) -> u64 {
        let curr = self.id_counter.get();
        let next = curr + 1;
        self.id_counter.set(next);

        curr
    }

    pub fn new_type_id(&self) -> TypeId {
        TypeId(self.inc_counter())
    }

    pub fn new_type_param_id(&self) -> TypeParamId {
        TypeParamId(self.inc_counter())
    }

    pub fn new_type_var_id(&self) -> TypeVarId {
        TypeVarId(self.inc_counter())
    }

    pub fn new_field_id(&self) -> FieldId {
        FieldId(self.inc_counter())
    }

    pub fn new_fn_id(&self) -> FnId {
        FnId(self.inc_counter())
    }
}

pub struct LocalData {
    id_counter: Cell<u64>,
}

impl LocalData {

    pub fn new() -> Self {
        LocalData {
            id_counter: Cell::new(0)
        }
    }

    fn inc_counter(&self) -> u64 {
        let curr = self.id_counter.get();
        let next = curr + 1;
        self.id_counter.set(next);

        curr
    }

    pub fn new_var_id(&self) -> VarId {
        VarId(self.inc_counter())
    }

    pub fn new_tmp_id(&self) -> TmpId {
        TmpId(self.inc_counter())
    }

    pub fn new_loop_id(&self) -> LoopId {
        LoopId(self.inc_counter())
    }

    pub fn new_branching_id(&self) -> BranchingId {
        BranchingId(self.inc_counter())
    }

}

#[derive(Clone, Debug)]
pub struct AnalysisContext {
    parent_scope: ScopedData,
    typing_context: TypingContext,
    existential_type_vars: Vec<TypeVarId>,
}

impl AnalysisContext {
    pub fn new(
        parent_scope: ScopedData,
        typing_context: TypingContext,
        existential_type_vars: Vec<TypeVarId>,
    ) -> AnalysisContext {
        AnalysisContext {
            parent_scope,
            typing_context,
            existential_type_vars,
        }
    }

    pub fn parent_scope(&self) -> &ScopedData {
        &self.parent_scope
    }

    pub fn typing_context(&self) -> &TypingContext {
        &self.typing_context
    }

    // TODO: Restructure program to remove this function
    pub fn set_typing_context(&mut self, tc: TypingContext) {
        self.typing_context = tc;
    }

    pub fn existential_type_vars(&self) -> &[TypeVarId] {
        &self.existential_type_vars
    }
}
