use std::collections::HashMap;
use std::cell::Cell;
use std::rc::Rc;

use err::Err;
use ast::*;
use ast::{Module as AstModule, Function as AstFunction};

use super::smpl_type::*;
use super::smpl_type::FnParameter;
use super::control_flow::CFG;

pub struct Program {
    universe: Universe,
    main: Option<FnId>,
}

impl Program {

    pub fn new(universe: Universe, main: Option<FnId>) -> Program {
        Program {
            universe: universe,
            main: main,
        }
    }

    pub fn universe(&self) -> &Universe {
        &self.universe
    }

    pub fn main(&self) -> Option<FnId> {
        self.main
    }
}

#[derive(Clone, Debug)]
pub struct Universe {
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

        let unit = (TypeId(0), path!("Unit"), SmplType::Unit);
        let int = (TypeId(1), path!("i32"), SmplType::Int);
        let float = (TypeId(2), path!("f32"), SmplType::Float);
        let string = (TypeId(3), path!("String"), SmplType::String);
        let boolean = (TypeId(4), path!("bool"), SmplType::Bool);

        let type_map = vec![
            unit.clone(),
            int.clone(),
            float.clone(),
            string.clone(),
            boolean.clone(),
        ];

        Universe {
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
                module_names: HashMap::new(),
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
        match self.types.get(&id).map(|t| t.clone()) {
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

    pub fn new_module_id(&self) -> ModuleId {
        ModuleId(self.inc_counter())
    }

    pub fn all_types(&self) -> Vec<(TypeId, Rc<SmplType>)> {
        self.types.iter().map(|(id, t)| (id.clone(), t.clone())).collect()
    }

    pub fn all_fns(&self) -> Vec<(FnId, &Function)> {
        self.fn_map.iter().map(|(id, f)| (id.clone(), f)).collect()
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
}

impl Module {
    pub fn new(module_scope: ScopedData, owned_t: Vec<TypeId>, owned_fns: Vec<FnId>, id: ModuleId) -> Module {
        Module {
            id: id,
            module_scope: module_scope,
            owned_types: owned_t,
            owned_fns: owned_fns,
        }
    }

    pub fn module_id(&self) -> ModuleId {
        self.id
    }

    pub fn module_scope(&self) -> &ScopedData {
        &self.module_scope
    }
}

#[derive(Clone, Debug)]
pub struct ScopedData {
    type_map: HashMap<Path, TypeId>,
    var_map: HashMap<Ident, VarId>,
    var_type_map: HashMap<VarId, TypeId>,
    fn_map: HashMap<Path, FnId>,
    module_names: HashMap<Ident, ModuleId>,
}

impl ScopedData {

    pub fn insert_fn(&mut self, name: Path, fn_id: FnId) {
        // TODO: Fn name override behaviour?
        self.fn_map.insert(name, fn_id);
    }

    pub fn type_id(&self, path: &Path) -> Result<TypeId, Err> {
        self.type_map.get(path)
            .map(|id| id.clone())
            .ok_or(Err::UnknownType(path.clone()))
    }

    pub fn get_type(&self, universe: &Universe, path: &Path) -> Result<Rc<SmplType>, Err> {
        let id = self.type_map.get(path).ok_or(Err::UnknownType(path.clone()))?;
        let t = universe.types.get(id).expect(&format!("Missing TypeId: {}. All TypeId's should be valid if retrieven from ScopedData.type_map", id.0));
        Ok(t.clone())
    }

    pub fn insert_type(&mut self, path: Path, id: TypeId) -> Option<TypeId> {
        self.type_map.insert(path, id)
    }

    pub fn var_info(&self, name: &Ident) -> Result<(VarId, TypeId), Err> {
        let var_id = self.var_map.get(name)
                         .ok_or(Err::UnknownVar(name.clone()))?
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

    pub fn get_fn(&self, path: &Path) -> Result<FnId, Err> {
        self.fn_map.get(path)
                   .map(|id| id.clone())
                   .ok_or(Err::UnknownFn(path.clone()))
    }

    pub fn map_module(&mut self, name: Ident, id: ModuleId) {
        if self.module_names.insert(name, id).is_some() {
            unimplemented!("Overriding module with the same name.");
        }
    }

    pub fn get_module(&self, name: &Ident) -> Option<ModuleId> {
        self.module_names.get(name).map(|id| id.clone())
    }

    pub fn all_types(&self) -> Vec<(&Path, &TypeId)> {
        self.type_map.iter().collect()
    }

    pub fn all_fns(&self) -> Vec<(&Path, &FnId)> {
        self.fn_map.iter().collect()
    }
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
        })
    }
}

pub enum ModuleCkSignal {
    Defer(ModuleCkData),
    Success,
}
