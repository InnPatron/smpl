use std::cell::Cell;
use std::collections::HashMap;

use super::semantic_data::ModuleId;
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

    pub fn new_var_id(&self) -> VarId {
        VarId(self.inc_counter())
    }

    pub fn new_fn_id(&self) -> FnId {
        FnId(self.inc_counter())
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

pub struct AnalyzableModule {
}

pub struct AnalyzableFn {
   cfg: CFG, 
   parent_typing_context: Option<TypingContext>,
   parent_scope: Option<ScopedData>,
}
