use std::cell::{Cell, RefCell};
use std::collections::{HashSet, HashMap};
use std::fmt;
use std::rc::Rc;
use std::slice::Iter;

use uuid::Uuid;

use crate::ast::{ AnonymousFn as AstAnonymousFn, ModulePath as AstModulePath };
use crate::ast::*;
use crate::feature::PresentFeatures;

use super::resolve_scope::ScopedData;
use super::type_checker::TypingContext;
use super::control_flow::CFG;

use super::metadata::Metadata;
use super::type_cons::*;

pub const UNIT_TYPE: &'static str = "Unit";
pub const INT_TYPE: &'static str = "int";
pub const FLOAT_TYPE: &'static str = "float";
pub const STRING_TYPE: &'static str = "String";
pub const BOOL_TYPE: &'static str = "bool";

pub struct Program {
    universe: Universe,
    metadata: Metadata,
    features: PresentFeatures,
}

impl Program {
    pub(super) fn new(universe: Universe, metadata: Metadata, features: PresentFeatures) -> Program {
        Program {
            universe: universe,
            metadata: metadata,
            features: features,
        }
    }

    pub(super) fn analysis_context<'a>(
        &'a mut self,
    ) -> (&'a mut Universe, &'a mut Metadata, &'a mut PresentFeatures) {
        (&mut self.universe, &mut self.metadata, &mut self.features)
    }

    pub(super) fn universe(&self) -> &Universe {
        &self.universe
    }

    pub fn all_fns(&self) -> impl Iterator<Item=(FnId, &Function)> {
        self.universe.all_fns()
    }

    pub fn smpl_fns(&self) -> impl Iterator<Item=(FnId, &SMPLFunction)> {
        self.universe
            .all_fns()
            .filter(|(_, f)| {
                match f {
                    Function::SMPL(_) => true,
                    _ => false
                }
            })
            .map(|(f_id, f)| {
                match f {
                    Function::SMPL(f) => (f_id, f),
                    _ => unreachable!(),
                }
            })
    }

    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }

    pub fn features(&self) -> &PresentFeatures {
        &self.features
    }

    pub(super) fn universe_mut(&mut self) -> &mut Universe {
        &mut self.universe
    }

    pub(super) fn metadata_mut(&mut self) -> &mut Metadata {
        &mut self.metadata
    }

    pub(super) fn features_mut(&mut self) -> &mut PresentFeatures {
        &mut self.features
    }
}

#[derive(Clone, Debug)]
pub struct Universe {
    type_cons_map: HashMap<TypeId, TypeCons>,
    fn_map: HashMap<FnId, Function>,
    builtin_fn_set: HashSet<FnId>,
    module_map: HashMap<ModuleId, Module>,
    module_name: HashMap<Ident, ModuleId>,
    id_counter: Cell<u64>,
    std_scope: ScopedData,
    unit: TypeId,
    int: TypeId,
    float: TypeId,
    string: TypeId,
    boolean: TypeId,
}

impl Universe {
    pub fn std() -> Universe {
        let unit = (TypeId(0), internal_module_path!(UNIT_TYPE), TypeCons::Unit);
        let int = (TypeId(1), internal_module_path!(INT_TYPE), TypeCons::Int);
        let float = (
            TypeId(2),
            internal_module_path!(FLOAT_TYPE),
            TypeCons::Float,
        );
        let string = (
            TypeId(3),
            internal_module_path!(STRING_TYPE),
            TypeCons::String,
        );
        let boolean = (TypeId(4), internal_module_path!(BOOL_TYPE), TypeCons::Bool);

        let type_map = vec![
            unit.clone(),
            int.clone(),
            float.clone(),
            string.clone(),
            boolean.clone(),
        ];

        Universe {
            type_cons_map: type_map
                .clone()
                .into_iter()
                .map(|(id, _, tc)| (id, tc))
                .collect(),
            fn_map: HashMap::new(),
            builtin_fn_set: HashSet::new(),
            module_map: HashMap::new(),
            module_name: HashMap::new(),
            id_counter: Cell::new(5),
            std_scope: ScopedData::new(type_map
                    .clone()
                    .into_iter()
                    .map(|(id, path, _)| (path, id))
                    .collect()),
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

    pub fn map_module(&mut self, mod_id: ModuleId, name: Ident, module: Module) {
        if self.module_name.insert(name, mod_id).is_some() {
            unimplemented!("Overriding module with the same name.");
        }

        self.module_map.insert(mod_id, module);
    }

    pub fn get_module(&self, id: ModuleId) -> &Module {
        self.module_map.get(&id).unwrap()
    }

    pub fn module_id(&self, name: &Ident) -> Option<ModuleId> {
        self.module_name.get(name).map(|id| id.clone())
    }

    /// Only used when function analysis returns a recoverable error.
    /// Unmap to not have a partial function.
    pub fn unmap_fn(&mut self, fn_id: FnId) {
        self.fn_map.remove(&fn_id);
    }

    pub fn insert_fn(&mut self, fn_id: FnId, name: Ident,
        type_id: TypeId, analysis_context: AnalysisContext, cfg: CFG) {
        let function = SMPLFunction {
            name: name,
            fn_type: type_id,
            cfg: Rc::new(RefCell::new(cfg)),
            analysis_context: analysis_context,
        };

        if self.fn_map.insert(fn_id, Function::SMPL(function)).is_some() {
            panic!(
                "Attempting to override Function with FnId {} in the Universe",
                fn_id.0
            );
        }
    }

    pub fn insert_builtin_fn(&mut self, fn_id: FnId, name: Ident, fn_type: TypeId) {
        let builtin = BuiltinFunction { 
            name: name,
            fn_type: fn_type,
        };

        self.builtin_fn_set.insert(fn_id);

        if self.fn_map.insert(fn_id, Function::Builtin(builtin)).is_some() {
            panic!(
                "Attempting to override builtin function with FnId {} in the Universe",
                fn_id.0
            );
        }
    }

    pub fn reserve_anonymous_fn(&mut self, fn_id: FnId, ast_fn: AstAnonymousFn) {
        let anon_fn = AnonymousFunction::Reserved(ast_fn);

        if self.fn_map.insert(fn_id, Function::Anonymous(anon_fn)).is_some() {
            panic!(
                "Attempting to override function with FnId {} in the Universe",
                fn_id.0
            );
        }
    }

    pub fn manual_insert_type_cons(&mut self, type_id: TypeId, cons: TypeCons) {
        if self.type_cons_map.insert(type_id, cons).is_some() {
            panic!("Duplicate type constructor for type id");
        }
    }

    pub fn insert_type_cons(&mut self, cons: TypeCons) -> TypeId {
        let type_id = self.new_type_id();
        self.manual_insert_type_cons(type_id, cons);
        type_id
    }

    pub fn get_type_cons(&self, id: TypeId) -> &TypeCons {
        self.type_cons_map
            .get(&id)
            .expect("Expected TypeID to always resolve to a TypeCons")
    }

    pub fn get_fn(&self, id: FnId) -> &Function {
        self.fn_map.get(&id).unwrap()
    }

    pub fn get_fn_mut(&mut self, id: FnId) -> &mut Function {
        self.fn_map.get_mut(&id).unwrap()
    }

    pub fn is_builtin_fn(&self, id: FnId) -> bool {
        self.builtin_fn_set.contains(&id)
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

    pub fn static_types(&self) -> Vec<(TypeId, TypeCons)> {
        self.type_cons_map
            .iter()
            .map(|(id, cons)| (id.clone(), cons.clone()))
            .collect()
    }

    pub fn all_fns(&self) -> impl Iterator<Item=(FnId, &Function)> {
        self.fn_map
            .iter()
            .map(|(id, f)| (id.clone(), f))
    }

    pub fn all_modules(&self) -> Vec<(&Ident, &ModuleId)> {
        self.module_name.iter().collect()
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    id: ModuleId,
    module_scope: ScopedData,
    owned_types: Vec<TypeId>,
    owned_fns: Vec<FnId>,
    dependencies: Vec<ModuleId>,
}

impl Module {
    pub fn new(
        module_scope: ScopedData,
        owned_t: Vec<TypeId>,
        owned_fns: Vec<FnId>,
        dependencies: Vec<ModuleId>,
        id: ModuleId,
    ) -> Module {
        Module {
            id: id,
            module_scope: module_scope,
            owned_types: owned_t,
            owned_fns: owned_fns,
            dependencies: dependencies,
        }
    }

    pub fn module_id(&self) -> ModuleId {
        self.id
    }

    pub fn module_scope(&self) -> &ScopedData {
        &self.module_scope
    }

    pub fn owned_types(&self) -> &[TypeId] {
        &self.owned_types
    }

    pub fn owned_fns(&self) -> &[FnId] {
        &self.owned_fns
    }

    pub fn dependencies(&self) -> &[ModuleId] {
        &self.dependencies
    }
}

pub enum BindingInfo {
    Var(VarId, AbstractType),
    Fn(FnId),
}

#[derive(Clone, Debug)]
pub struct AnalysisContext {
    fn_scope: ScopedData,
    typing_context: TypingContext,
    existential_type_vars: Vec<TypeVarId>,
}

impl AnalysisContext {
    pub fn new(fn_scope: ScopedData, typing_context: TypingContext, 
        existential_type_vars: Vec<TypeVarId>) -> AnalysisContext {

        AnalysisContext {
            fn_scope: fn_scope,
            typing_context: typing_context,
            existential_type_vars: existential_type_vars
        }
    }

    pub fn fn_scope(&self) -> &ScopedData {
        &self.fn_scope
    }

    pub fn typing_context(&self) -> &TypingContext {
        &self.typing_context
    }

    pub fn set_typing_context(&mut self, tc: TypingContext) {
        self.typing_context = tc;
    }

    pub fn existential_type_vars(&self) -> &[TypeVarId] {
        &self.existential_type_vars
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    Builtin(BuiltinFunction),
    SMPL(SMPLFunction),
    Anonymous(AnonymousFunction),
}

impl Function {
    pub fn fn_type(&self) -> Option<TypeId> {
        match self {
            Function::Builtin(ref bf) => Some(bf.fn_type()),
            Function::SMPL(ref sf) => Some(sf.fn_type()),
            Function::Anonymous(ref af) => af.fn_type(),
        }
    }

    pub fn name(&self) -> Option<&Ident> {
        match self {
            Function::Builtin(ref bf) => Some(bf.name()),
            Function::SMPL(ref func) => Some(func.name()),
            Function::Anonymous(ref anon) => anon.name(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AnonymousFunction {
    Reserved(AstAnonymousFn),
    Resolved {
        fn_type: TypeId,
        cfg: Rc<RefCell<CFG>>,
        analysis_context: AnalysisContext,
    }
}

impl AnonymousFunction {

    pub fn name(&self) -> Option<&Ident> {
        None
    }

    pub fn fn_type(&self) -> Option<TypeId> {

        match self {
            AnonymousFunction::Reserved(_) => None,
            AnonymousFunction::Resolved { fn_type, .. } => Some(fn_type.clone()),
        }
    }
}


#[derive(Clone, Debug)]
pub struct BuiltinFunction {
    name: Ident,
    fn_type: TypeId,
}

impl BuiltinFunction {

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn fn_type(&self) -> TypeId {
        self.fn_type
    }
}

#[derive(Clone, Debug)]
pub struct SMPLFunction {
    name: Ident,
    fn_type: TypeId,
    cfg: Rc<RefCell<CFG>>,
    analysis_context: AnalysisContext
}

impl SMPLFunction {

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn fn_type(&self) -> TypeId {
        self.fn_type
    }

    pub fn cfg(&self) -> Rc<RefCell<CFG>> {
        self.cfg.clone()
    }

    pub(crate) fn analysis_context(&self) -> &AnalysisContext {
        &self.analysis_context
    }

    pub(super) fn analysis_context_mut(&mut self) -> &mut AnalysisContext {
        &mut self.analysis_context
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BindingId {
    Var(VarId),
    Fn(FnId),
}

impl From<VarId> for BindingId {
    fn from(id: VarId) -> BindingId {
        BindingId::Var(id)
    }
}

impl From<FnId> for BindingId {
    fn from(id: FnId) -> BindingId {
        BindingId::Fn(id)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(u64);

impl ::std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "TypeId[{}]", self.0)
    }
}

impl TypeId {
    pub fn raw(&self) -> u64 {
        self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParamId(u64);

impl ::std::fmt::Display for TypeParamId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "TypeParamId[{}]", self.0)
    }
}

impl TypeParamId {
    pub fn raw(&self) -> u64 {
        self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVarId(u64);

impl ::std::fmt::Display for TypeVarId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "TypeVarId[{}]", self.0)
    }
}

impl TypeVarId {
    pub fn raw(&self) -> u64 {
        self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldId(u64);

impl ::std::fmt::Display for FieldId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "FieldId[{}]", self.0)
    }
}

impl FieldId {
    pub fn raw(&self) -> u64 {
        self.0
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DataId {
    VarId(VarId),
    TmpId(TmpId),
}

impl ::std::fmt::Display for DataId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            DataId::VarId(ref id) => id.fmt(f),
            DataId::TmpId(ref id) => id.fmt(f),
        }
    }
}

impl From<VarId> for DataId {
    fn from(id: VarId) -> DataId {
        DataId::VarId(id)
    }
}

impl From<TmpId> for DataId {
    fn from(id: TmpId) -> DataId {
        DataId::TmpId(id)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(u64);

impl ::std::fmt::Display for VarId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "VarId[{}]", self.0)
    }
}

impl VarId {
    pub fn raw(&self) -> u64 {
        self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnId(u64);

impl ::std::fmt::Display for FnId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "FnId[{}]", self.0)
    }
}

impl FnId {
    pub fn raw(&self) -> u64 {
        self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TmpId(u64);

impl ::std::fmt::Display for TmpId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "TmpId[{}]", self.0)
    }
}

impl TmpId {
    pub fn raw(&self) -> u64 {
        self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoopId(u64);

impl ::std::fmt::Display for LoopId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "LoopId[{}]", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BranchingId(u64);

impl ::std::fmt::Display for BranchingId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "BranchingId[{}]", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(Uuid);

impl ::std::fmt::Display for ModuleId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "ModuleId[{}]", self.0)
    }
}

impl ModuleId {
    pub fn new() -> ModuleId {
        ModuleId(Uuid::new_v4())
    }

    pub fn raw(&self) -> Uuid {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModulePath(pub Vec<Ident>);

impl ModulePath {
    pub fn new(v: Vec<Ident>) -> ModulePath {
        ModulePath(v)
    }

    pub fn iter(&self) -> Iter<Ident> {
        self.0.iter()
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let buffer = self.0.iter().fold(String::new(), |mut buffer, ref item| {
            buffer.push_str(&item.0);
            buffer
        });
        write!(f, "{}", buffer)
    }
}

impl From<AstModulePath> for ModulePath {
    fn from(p: AstModulePath) -> ModulePath {
        ModulePath::new(p.0.into_iter().map(|node| node.data().clone()).collect())
    }
}

impl From<Ident> for ModulePath {
    fn from(i: Ident) -> ModulePath {
        let mut v = Vec::with_capacity(1);
        v.push(i);
        ModulePath(v)
    }
}
