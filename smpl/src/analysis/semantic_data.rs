use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::slice::Iter;

use uuid::Uuid;

use crate::ast::ModulePath as AstModulePath;
use crate::ast::*;
use crate::feature::PresentFeatures;

use super::control_flow::CFG;
use super::error::AnalysisError;
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

    pub fn all_fns(&self) -> Vec<(FnId, &Function)> {
        self.universe.all_fns()
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
    generated_type_cons_map: RefCell<HashMap<TypeId, TypeCons>>,
    type_cons_map: HashMap<TypeId, TypeCons>,
    fn_map: HashMap<FnId, Function>,
    builtin_fn_map: HashMap<FnId, BuiltinFunction>,
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
            generated_type_cons_map: RefCell::new(HashMap::new()),
            type_cons_map: type_map
                .clone()
                .into_iter()
                .map(|(id, _, tc)| (id, tc))
                .collect(),
            fn_map: HashMap::new(),
            builtin_fn_map: HashMap::new(),
            module_map: HashMap::new(),
            module_name: HashMap::new(),
            id_counter: Cell::new(5),
            std_scope: ScopedData {
                type_cons_map: type_map
                    .clone()
                    .into_iter()
                    .map(|(id, path, _)| (path, id))
                    .collect(),
                var_map: HashMap::new(),
                var_type_map: HashMap::new(),
                fn_map: HashMap::new(),
                type_param_map: HashMap::new(),
            },
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

    pub fn insert_fn(&mut self, fn_id: FnId, type_id: TypeId, fn_scope: ScopedData, cfg: CFG) {
        let function = Function {
            fn_type: type_id,
            cfg: Rc::new(cfg),
            fn_scope: fn_scope,
        };

        if self.fn_map.insert(fn_id, function).is_some() {
            panic!(
                "Attempting to override Function with FnId {} in the Universe",
                fn_id.0
            );
        }
    }

    pub fn insert_builtin_fn(&mut self, fn_id: FnId, fn_type: TypeId) {
        let builtin = BuiltinFunction { fn_type: fn_type };

        if self.builtin_fn_map.insert(fn_id, builtin).is_some() {
            panic!(
                "Attempting to override builtin function with FnId {} in the Universe",
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

    pub fn insert_generated_type_cons(&self, cons: TypeCons) -> TypeId {
        let type_id = self.new_type_id();
        let mut borrow = self.generated_type_cons_map.borrow_mut();

        if borrow.insert(type_id, cons).is_some() {
            panic!("Duplicate type constructor for type id");
        }

        type_id
    }

    pub fn get_type_cons(&self, id: TypeId) -> Option<TypeCons> {
        self.type_cons_map.get(&id).map(|cons| cons.clone()).or({
            let borrow = self.generated_type_cons_map.borrow();
            borrow.get(&id).map(|cons| cons.clone())
        })
    }

    pub fn get_fn(&self, id: FnId) -> &Function {
        self.fn_map.get(&id).unwrap()
    }

    pub fn get_builtin_fn(&self, id: FnId) -> &BuiltinFunction {
        self.builtin_fn_map.get(&id).unwrap()
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

    pub fn all_fns(&self) -> Vec<(FnId, &Function)> {
        self.fn_map
            .iter()
            .map(|(id, f)| (id.clone(), f))
            .collect()
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

#[derive(Clone, Debug)]
pub struct ScopedData {
    type_cons_map: HashMap<ModulePath, TypeId>,
    var_map: HashMap<Ident, VarId>,
    var_type_map: HashMap<VarId, Type>,
    fn_map: HashMap<ModulePath, FnId>,
    type_param_map: HashMap<Ident, (TypeParamId, Option<AbstractType>)>,
}

impl ScopedData {
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

    pub fn binding_info(&self, name: &Ident) -> Result<BindingInfo, AnalysisError> {
        match self.var_map.get(name) {
            Some(v_id) => Ok(BindingInfo::Var(
                v_id.clone(),
                self.var_type_map.get(v_id).unwrap().clone(),
            )),
            None => {
                let p = ModulePath(vec![name.clone()]);
                self.fn_map
                    .get(&p)
                    .map(|f| BindingInfo::Fn(f.clone()))
                    .ok_or(AnalysisError::UnknownBinding(name.clone()))
            }
        }
    }

    pub fn var_info(&self, name: &Ident) -> Result<(VarId, Type), AnalysisError> {
        let var_id = self
            .var_map
            .get(name)
            .ok_or(AnalysisError::UnknownBinding(name.clone()))?
            .clone();
        let type_id = self.var_type_map.get(&var_id).unwrap().clone();

        Ok((var_id, type_id))
    }

    pub fn insert_var(&mut self, name: Ident, id: VarId, var_type: Type) {
        self.var_map.insert(name, id);

        if self.var_type_map.insert(id, var_type).is_some() {
            panic!("Attempting to override variable {} with a different type. Shadowing should produce a new variable id.", id);
        }
    }

    pub fn get_fn(&self, path: &AstModulePath) -> Result<FnId, AnalysisError> {
        self.fn_map
            .get(&path.clone().into())
            .map(|id| id.clone())
            .ok_or(AnalysisError::UnknownFn(path.clone()))
    }

    pub fn insert_type_param(&mut self, 
                             ident: Ident, 
                             id: TypeParamId, 
                             constraint: Option<AbstractType>) -> bool {
        self.type_param_map.insert(ident, (id, constraint)).is_some()
    }

    pub fn type_param<'a, 'b>(&'a self, ident: &'b Ident) 
        -> Option<(TypeParamId, Option<&'a AbstractType>)> {
        self.type_param_map
            .get(ident)
            .map(|(id, constraint)| (id.clone(), constraint.as_ref()))
    }

    pub fn type_params<'a>(&'a self) 
        -> impl Iterator<Item = (TypeParamId, Option<&'a AbstractType>)> + 'a {
        self.type_param_map
            .values()
            .map(|(id, constraint)| (id.clone(), constraint.as_ref()))
    }

    pub fn all_types(&self) -> Vec<(&ModulePath, &TypeId)> {
        self.type_cons_map.iter().collect()
    }

    pub fn all_fns(&self) -> Vec<(&ModulePath, &FnId)> {
        self.fn_map.iter().collect()
    }
}

pub enum BindingInfo {
    Var(VarId, Type),
    Fn(FnId),
}

#[derive(Clone, Debug)]
pub struct BuiltinFunction {
    fn_type: TypeId,
}

impl BuiltinFunction {
    pub fn fn_type(&self) -> TypeId {
        self.fn_type
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    fn_type: TypeId,
    cfg: Rc<CFG>,
    fn_scope: ScopedData,
}

impl Function {
    pub fn fn_type(&self) -> TypeId {
        self.fn_type
    }

    pub fn cfg(&self) -> Rc<CFG> {
        self.cfg.clone()
    }

    pub(super) fn fn_scope(&self) -> &ScopedData {
        &self.fn_scope
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
