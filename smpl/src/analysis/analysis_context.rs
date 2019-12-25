use std::cell::Cell;
use std::collections::{HashSet, HashMap};

use crate::ast::*;
use crate::ast::{Ident, AstNode, AnonymousFn as AstAnonymousFn, ModulePath as AstModulePath};
use crate::span::Span;

use super::semantic_data::*;
use super::control_flow::CFG;
use super::type_checker::TypingContext;
use super::resolve_scope::ScopedData;
use super::type_cons::TypeCons;
use super::semantic_data::{
    FieldId, FnId, Program, TypeId, TypeParamId, TypeVarId, LoopId,
    BranchingId, TmpId, VarId, Module, ModuleId, ModulePath,
    AnonymousFn as ResolvedAnonymousFn, SMPLFunction, BuiltinFunction
};

pub enum UniverseFn {
    SMPL(SMPLFunction),
    Anonymous(AnalyzableAnonymousFn),
    Builtin(BuiltinFunction),
}

pub enum AnalyzableAnonymousFn {
    Reserved(ReservedAnonymousFn),
    Resolved(ResolvedAnonymousFn),
}

impl AnonymousFn {
    pub fn name(&self) -> Option<&Ident> {
        None
    }

    pub fn type_id(&self) -> Option<TypeId> {
        match self {
            AnonymousFn::Reserved(..) => None,
            AnonymousFn::Resolved { type_id, .. } => {
                Some(type_id.clone())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ReservedAnonymousFn {
    pub(super) fn_id: FnId,
    pub(super) ast: AstNode<AstAnonymousFn>,
}

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

#[derive(Clone, Debug)]
pub struct AnalysisUniverse {
    type_cons_map: HashMap<TypeId, TypeCons>,
    fn_type_cons: HashMap<FnId, TypeCons>,
    anon_fn_map: HashMap<FnId, AnonymousFn>,
    builtin_fn_set: HashSet<FnId>,
    module_map: HashMap<ModuleId, Module>,
    module_name: HashMap<Ident, ModuleId>,
    std_scope: ScopedData,
    unit: TypeId,
    int: TypeId,
    float: TypeId,
    string: TypeId,
    boolean: TypeId,
}

impl AnalysisUniverse {
    pub fn std(global_data: &GlobalData) -> Self {
        let unit =
            (global_data.unit_type_id(), internal_module_path!(UNIT_TYPE), TypeCons::Unit);
        let int =
            (global_data.int_type_id(), internal_module_path!(INT_TYPE), TypeCons::Int);
        let float = (
            global_data.float_type_id(),
            internal_module_path!(FLOAT_TYPE),
            TypeCons::Float,
        );
        let string = (
            global_data.string_type_id(),
            internal_module_path!(STRING_TYPE),
            TypeCons::String,
        );
        let boolean =
            (global_data.bool_type_id(), internal_module_path!(BOOL_TYPE), TypeCons::Bool);

        let type_map = vec![
            unit.clone(),
            int.clone(),
            float.clone(),
            string.clone(),
            boolean.clone(),
        ];

        AnalysisUniverse {
            type_cons_map: type_map
                .clone()
                .into_iter()
                .map(|(id, _, tc)| (id, tc))
                .collect(),
            fn_type_cons: HashMap::new(),
            anon_fn_map: HashMap::new(),
            builtin_fn_set: HashSet::new(),
            module_map: HashMap::new(),
            module_name: HashMap::new(),
            std_scope: ScopedData::new(
                type_map
                    .clone()
                    .into_iter()
                    .map(|(id, path, _)| (path, id))
                    .collect(),
            ),
            unit: unit.0,
            int: int.0,
            float: float.0,
            string: string.0,
            boolean: boolean.0,
        }
    }

    pub fn std_scope(&self) -> ScopedData {
        self.std_scope.clone()
    }

    pub fn unit(&self) -> TypeId {
        self.unit
    }

    pub fn int(&self) -> TypeId {
        self.int
    }

    pub fn float(&self) -> TypeId {
        self.float
    }

    pub fn string(&self) -> TypeId {
        self.string
    }

    pub fn boolean(&self) -> TypeId {
        self.boolean
    }

    pub fn map_module(
        &mut self,
        mod_id: ModuleId,
        name: Ident,
        module: Module,
    ) {
        if self.module_name.insert(name, mod_id).is_some() {
            unimplemented!("Overriding module with the same name.");
        }

        self.module_map.insert(mod_id, module);
    }

    pub fn get_module(&self, id: ModuleId) -> &Module {
        self.module_map.get(&id).unwrap()
    }

    pub(crate) fn get_module_mut(&mut self, id: ModuleId) -> &mut Module {
        self.module_map.get_mut(&id).unwrap()
    }

    pub fn module_id(&self, name: &Ident) -> Option<ModuleId> {
        self.module_name.get(name).map(|id| id.clone())
    }

    pub fn manual_insert_type_cons(&mut self, type_id: TypeId, cons: TypeCons) {
        if self.type_cons_map.insert(type_id, cons).is_some() {
            panic!("Duplicate type constructor for type id");
        }
    }

    pub fn insert_fn_type_cons(&mut self, fn_id: FnId, type_id: TypeId, cons: TypeCons) {
        self.manual_insert_type_cons(type_id, cons.clone());
        if self.fn_type_cons.insert(fn_id, cons).is_some() {
            panic!("Duplicate type constructor for fn id");
        }
    }

    pub fn get_fn_type_cons(&self, fn_id: FnId) -> Option<&TypeCons> {
        self.fn_type_cons.get(&fn_id)
    }

    pub fn get_type_cons(&self, id: TypeId) -> &TypeCons {
        self.type_cons_map
            .get(&id)
            .expect("Expected TypeID to always resolve to a TypeCons")
    }

    pub fn insert_anon_fn(&mut self, id: FnId, func: AnonymousFn) {
        if self.anon_fn_map.insert(id, func).is_some() {
            panic!("Overwriting function");
        }
    }

    pub fn get_anon_fn(&self, id: FnId) -> &AnonymousFn {
        self.anon_fn_map.get(&id).unwrap()
    }

    pub fn get_anon_fn_mut(&mut self, id: FnId) -> &mut AnonymousFn {
        self.anon_fn_map.get_mut(&id).unwrap()
    }

    pub fn is_builtin_fn(&self, id: FnId) -> bool {
        self.builtin_fn_set.contains(&id)
    }

    pub fn all_modules(&self) -> impl Iterator<Item = (&Ident, ModuleId)> {
        self.module_name
            .iter()
            .map(|(name, id)| (name, id.clone()))
    }
}
