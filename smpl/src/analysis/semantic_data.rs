use std::collections::HashMap;
use std::cell::{RefCell, Cell};
use std::rc::Rc;

use err::Err;
use ast::*;
use ast::{Module as AstModule, Function as AstFunction};
use feature::PresentFeatures;

use super::metadata::Metadata;
use super::smpl_type::*;
use super::control_flow::CFG;

pub struct Program {
    universe: Universe,
    metadata: Metadata,
    features: PresentFeatures,
}

impl Program {

    pub fn new(universe: Universe, metadata: Metadata, features: PresentFeatures) -> Program {
        Program {
            universe: universe,
            metadata: metadata,
            features: features, 
        }
    }

    pub fn analysis_context<'a>(&'a mut self) -> (&'a Universe, &'a mut Metadata, &'a mut PresentFeatures) {
        (&self.universe, &mut self.metadata, &mut self.features)
    }

    pub fn universe(&self) -> &Universe {
        &self.universe
    }

    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }

    pub fn features(&self) -> &PresentFeatures {
        &self.features
    }

    pub fn universe_mut(&mut self) -> &mut Universe {
        &mut self.universe
    }

    pub fn metadata_mut(&mut self) -> &mut Metadata {
        &mut self.metadata
    }

    pub fn features_mut(&mut self) -> &mut PresentFeatures {
        &mut self.features
    }
}

#[derive(Clone, Debug)]
pub struct Universe {
    type_constructor: TypeConstructor,
    types: HashMap<TypeId, Rc<SmplType>>,
    fn_map: HashMap<FnId, Function>,
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

        let unit = (TypeId(0), type_path!("Unit"), SmplType::Unit);
        let int = (TypeId(1), type_path!("i32"), SmplType::Int);
        let float = (TypeId(2), type_path!("f32"), SmplType::Float);
        let string = (TypeId(3), type_path!("String"), SmplType::String);
        let boolean = (TypeId(4), type_path!("bool"), SmplType::Bool);

        let type_map = vec![
            unit.clone(),
            int.clone(),
            float.clone(),
            string.clone(),
            boolean.clone(),
        ];

        Universe {
            type_constructor: TypeConstructor::new(),
            types: type_map.clone().into_iter().map(|(id, _, t)| (id, Rc::new(t))).collect(),
            fn_map: HashMap::new(),
            module_map: HashMap::new(),
            module_name: HashMap::new(),
            id_counter: Cell::new(5),
            std_scope: ScopedData {
                type_map: type_map.into_iter().map(|(id, path, _)| (path, id)).collect(),
                var_map: HashMap::new(),
                var_type_map: HashMap::new(),
                fn_map: HashMap::new(),
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

    pub fn insert_fn(&mut self, fn_id: FnId, type_id: TypeId, fn_t: FunctionType, cfg: CFG) {
        self.insert_type(type_id, SmplType::Function(fn_t));

        let function = Function {
            fn_type: type_id,
            cfg: cfg,
        };

        if self.fn_map.insert(fn_id, function).is_some() {
            panic!("Attempting to override Function with FnId {} in the Universe", fn_id.0);
        }
    }

    pub fn insert_type(&mut self, id: TypeId, t: SmplType) {
        if self.types.insert(id, Rc::new(t)).is_some() {
            panic!("Attempting to override type with TypeId {} in the Universe", id.0);
        }
    }

    pub fn get_type(&self, id: TypeId) -> Rc<SmplType> {
        match self.types
            .get(&id)
            .map(|t| t.clone())
            .or(self.type_constructor.get_type(id)) {
            Some(t) => t,
            None => panic!("Type with TypeId {} does not exist.", id.0),
        }
    }

    pub fn get_fn(&self, id: FnId) -> &Function {
        self.fn_map.get(&id).unwrap()
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

    pub fn new_module_id(&self) -> ModuleId {
        ModuleId(self.inc_counter())
    }

    pub fn all_types(&self) -> Vec<(TypeId, Rc<SmplType>)> {
        self.types.iter()
            .map(|(id, t)| (id.clone(), t.clone()))
            .chain(self.type_constructor.all_types())
            .collect()
    }

    pub fn all_fns(&self) -> Vec<(FnId, &Function)> {
        self.fn_map.iter().map(|(id, f)| (id.clone(), f)).collect()
    }

    pub fn all_modules(&self) -> Vec<(&Ident, &ModuleId)> {
        self.module_name.iter().collect()
    }
}

#[derive(Debug, Clone)]
pub struct TypeConstructor {
    map: RefCell<HashMap<ConstructedType, TypeId>>,
    constructed: RefCell<HashMap<TypeId, Rc<SmplType>>>,
}

impl TypeConstructor {

    fn new() -> TypeConstructor {
        TypeConstructor {
            map: RefCell::new(HashMap::new()),
            constructed: RefCell::new(HashMap::new()),
        }
    }

    fn all_types(&self) -> Vec<(TypeId, Rc<SmplType>)> {
        let b = self.constructed.borrow();
        b.iter().map(|(id, t)| (id.clone(), t.clone())).collect()
    }

    fn get_type(&self, id: TypeId) -> Option<Rc<SmplType>> {
        self.constructed.borrow().get(&id).map(|rc| rc.clone())
    }

    fn map(&self, t: ConstructedType, id: TypeId) {
        let mut b = self.map.borrow_mut();

        if b.contains_key(&t) == false {
            b.insert(t.clone(), id);
        }

        match t {
            ConstructedType::Array(at) => {
                let mut b = self.constructed.borrow_mut();
                b.insert(id, Rc::new(SmplType::Array(at)));
            }

            ConstructedType::Function(ft) => {
                let mut b = self.constructed.borrow_mut();
                b.insert(id, Rc::new(SmplType::Function(ft)));
            }
        }
    }

    fn contains(&self, t: &ConstructedType) -> Option<TypeId> {
        let b = self.map.borrow();

        b.get(t).map(|id| id.clone())
    }

    pub fn construct_array_type(universe: &Universe, base_type: TypeId, size: u64) -> TypeId {
        let at = ArrayType {
            base_type: base_type,
            size: size,
        };

        let at = ConstructedType::Array(at.clone());

        match universe.type_constructor.contains(&at) {
            Some(id) => id,
            None => {
                let id = universe.new_type_id();
                universe.type_constructor.map(at, id);
                id
            }
        }
    }

    pub fn construct_fn_type(universe: &Universe, params: Vec<TypeId>, return_t: TypeId) -> TypeId {

        let ft = FunctionType {
            params: params,
            return_type: return_t,
        };

        let ft = ConstructedType::Function(ft);

        match universe.type_constructor.contains(&ft) {
            Some(id) => id,
            None => {
                let id = universe.new_type_id();
                universe.type_constructor.map(ft, id);
                id
            }
        }
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
    pub fn new(module_scope: ScopedData, owned_t: Vec<TypeId>, 
               owned_fns: Vec<FnId>, dependencies: Vec<ModuleId>,
               id: ModuleId) -> Module {
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
    type_map: HashMap<ModulePath, TypeId>,
    var_map: HashMap<Ident, VarId>,
    var_type_map: HashMap<VarId, TypeId>,
    fn_map: HashMap<ModulePath, FnId>,
}

impl ScopedData {

    pub fn insert_fn(&mut self, name: ModulePath, fn_id: FnId) {
        // TODO: Fn name override behaviour?
        self.fn_map.insert(name, fn_id);
    }

    pub fn unmap_fn(&mut self, name: &ModulePath) {
        self.fn_map.remove(&name).unwrap();
    }

    pub fn type_id<'a, 'b, 'c>(&'a self, universe: &'c Universe, type_annotation: TypeAnnotationRef<'b>) -> Result<TypeId, Err> {
        match type_annotation {
            TypeAnnotationRef::Path(path) => {
                self.type_map.get(path)
                    .map(|id| id.clone())
                    .ok_or(Err::UnknownType(type_annotation.into()))
            }

            TypeAnnotationRef::Array(base_type, size) => {
                let base_type = ScopedData::type_id(self, universe, base_type.into())?;
                let type_id = TypeConstructor::construct_array_type(universe, 
                                                                   base_type, 
                                                                   size.clone());
                Ok(type_id)
            },

            TypeAnnotationRef::FnType(params, return_t) => {
                let param_types = match params {
                    Some(ref v) => {
                        let mut new_params = Vec::new();
                        for p in v.iter() {
                            let p_type = ScopedData::type_id(self, universe, p.into())?;
                            new_params.push(p_type);
                        }

                        new_params
                    }

                    None => Vec::with_capacity(0),
                };

                let return_t = match return_t {
                    Some(r) => ScopedData::type_id(self, universe, r.into())?,
                    None => universe.unit(),
                };

                Ok(TypeConstructor::construct_fn_type(universe, param_types, return_t))
            }
        }
    }

    pub fn insert_type(&mut self, path: ModulePath, id: TypeId) -> Option<TypeId> {
        self.type_map.insert(path, id)
    }

    pub fn binding_info(&self, name: &Ident) -> Result<BindingInfo, Err> {
        match self.var_map.get(name) {
            Some(v_id) => {
                Ok(BindingInfo::Var(v_id.clone(), 
                                    self.var_type_map.get(v_id).unwrap().clone()))
            }
            None => {
                let p = ModulePath(vec![name.clone()]);
                self.fn_map.get(&p)
                    .map(|f| BindingInfo::Fn(f.clone()))
                    .ok_or(Err::UnknownBinding(name.clone()))
            }
        }
    }

    pub fn var_info(&self, name: &Ident) -> Result<(VarId, TypeId), Err> {
        let var_id = self.var_map.get(name)
                         .ok_or(Err::UnknownBinding(name.clone()))?
                         .clone();
        let type_id = self.var_type_map
                          .get(&var_id)
                          .unwrap()
                          .clone();

        Ok((var_id, type_id))
    }

    pub fn insert_var(&mut self, name: Ident, id: VarId, type_id: TypeId) {
        self.var_map.insert(name, id);

        if self.var_type_map.insert(id, type_id).is_some() {
            panic!("Attempting to override variable {} with a different type. Shadowing should produce a new variable id.", id);
        }
    }

    pub fn get_fn(&self, path: &ModulePath) -> Result<FnId, Err> {
        self.fn_map.get(path)
                   .map(|id| id.clone())
                   .ok_or(Err::UnknownFn(path.clone()))
    }

    pub fn all_types(&self) -> Vec<(&ModulePath, &TypeId)> {
        self.type_map.iter().collect()
    }

    pub fn all_fns(&self) -> Vec<(&ModulePath, &FnId)> {
        self.fn_map.iter().collect()
    }
}

pub enum BindingInfo {
    Var(VarId, TypeId),
    Fn(FnId),
}

#[derive(Clone, Debug)]
pub struct Function {
    fn_type: TypeId,
    cfg: CFG,
}

impl Function {
    pub fn type_id(&self) -> TypeId {
        self.fn_type
    }

    pub fn cfg(&self) -> &CFG {
        &self.cfg
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
pub struct ModuleId(u64);

impl ::std::fmt::Display for ModuleId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "ModuleId[{}]", self.0)
    }
}

impl ModuleId {
    pub fn raw(&self) -> u64 {
        self.0
    }
}

pub struct ModuleCkData {
    pub name: Ident,
    pub unresolved_module_uses: Vec<UseDecl>,
    pub unresolved_module_structs: Vec<Struct>,
    pub unresolved_module_fns: Vec<AstFunction>,
    pub module_scope: ScopedData,
    pub owned_types: Vec<TypeId>,
    pub owned_fns: Vec<FnId>,
    pub dependencies: Vec<ModuleId>,
}

impl ModuleCkData {
    pub fn new(universe: &Universe, module: AstModule) -> Result<ModuleCkData, Err> {
        let mut module_uses = Vec::new();
        let mut module_structs = Vec::new();
        let mut module_fns = Vec::new();

        for decl_stmt in module.1.into_iter() {
            match decl_stmt {
                DeclStmt::Struct(d) => module_structs.push(d),
                DeclStmt::Function(d) => module_fns.push(d),
                DeclStmt::Use(d) => module_uses.push(d),
            }
        }

        Ok(ModuleCkData {
            name: module.0.ok_or(Err::MissingModName)?,
            unresolved_module_uses: module_uses,
            unresolved_module_structs: module_structs,
            unresolved_module_fns: module_fns,
            module_scope: universe.std_scope(),
            owned_types: Vec::new(),
            owned_fns: Vec::new(),
            dependencies: Vec::new(),
        })
    }
}

pub enum ModuleCkSignal {
    Defer(ModuleCkData),
    Success,
}
