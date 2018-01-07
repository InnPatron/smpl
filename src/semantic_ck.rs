#[macro_use]
use std::collections::HashMap;
use std::str::FromStr;
use std::borrow::{Borrow, BorrowMut};
use std::cell::Cell;
use std::rc::Rc;
use std::fmt::Debug;

use ascii::*;

use control_flow::Err as ControlFlowErr;
use control_flow::CFG;
use smpl_type::*;
use ast::*;
use ast::Function as AstFunction;

pub fn check(mut program: Program) -> Result<(), Err> {
    let mut universe = Universe::std();
    let mut global_scope = universe.std_scope.clone();

    for decl_stmt in program.0.into_iter() {
        match decl_stmt {
            DeclStmt::Struct(struct_def) => {
                let struct_t = generate_struct_type(&global_scope, &universe, struct_def)?;
                let id = universe.new_type_id();
                
                global_scope.insert_type(struct_t.name.clone().into(), id);
                universe.insert_type(id, SmplType::Struct(struct_t));
            },

            DeclStmt::Function(fn_def) => {
                let name = fn_def.name.clone().into();

                let type_id = universe.new_type_id();

                let fn_type = generate_fn_type(&global_scope, &universe, &fn_def)?;
                let cfg = CFG::generate(&universe, fn_def, &fn_type)?;

                let fn_id = universe.insert_fn(type_id, fn_type, cfg);
                global_scope.insert_fn(name, fn_id);
            },
        }
    }
    Ok(())
}

fn generate_fn_type(scope: &ScopedData, universe: &Universe, fn_def: &AstFunction) -> Result<FunctionType, Err> {
    let ret_type = match fn_def.return_type {
        Some(ref path) => scope.get_type(universe, path)?,
        None => Rc::new(SmplType::Unit),
    };

    let args: Vec<_> = match fn_def.args {
        Some(ref args) => args.iter()
                              .map(|ref fn_param| scope.get_type(universe, &fn_param.arg_type))
                              .collect::<Result<Vec<Rc<SmplType>>, Err>>()?,

        None => Vec::new(),
    };

    Ok(FunctionType {
        args: args,
        return_type: ret_type,
    })
}

fn generate_struct_type(scope: &ScopedData, universe: &Universe, struct_def: Struct) -> Result<StructType, Err> {
    let struct_name = struct_def.name;
    let mut fields = HashMap::new();
    if let Some(body) = struct_def.body.0 {
        for field in body.into_iter() {
            let f_name = field.name;
            let field_type = scope.get_type(universe, &f_name.clone().into())?.clone();
            fields.insert(f_name, field_type);
        }
    } 

    let struct_t = StructType {
        name: struct_name,
        fields: fields,
    };

    Ok(struct_t)
}

#[derive(Clone, Debug)]
pub enum Err {
    ControlFlowErr(ControlFlowErr),
    UnknownType(Path),
    UnknownVar(AsciiString),
}

impl From<ControlFlowErr> for Err {
    fn from(err: ControlFlowErr) -> Err {
        Err::ControlFlowErr(err)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(u64);

impl ::std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "TypeId[{}]", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(u64);

impl ::std::fmt::Display for VarId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "VarId[{}]", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnId(u64);

impl ::std::fmt::Display for FnId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "FnId[{}]", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TmpId(u64);

impl ::std::fmt::Display for TmpId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "TmpId[{}]", self.0)
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    fn_type: TypeId,
    cfg: CFG,
}

#[derive(Clone, Debug)]
pub struct Universe {
    types: HashMap<TypeId, Rc<SmplType>>,
    fn_map: HashMap<FnId, Function>,
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
        let float = (TypeId(2), path!("float"), SmplType::Float);
        let string = (TypeId(3), path!("String"), SmplType::String);
        let boolean = (TypeId(4), path!("bool"), SmplType::Bool);

        let mut type_map = vec![
            unit.clone(),
            int.clone(),
            float.clone(),
            string.clone(),
            boolean.clone(),
        ];

        Universe {
            types: type_map.clone().into_iter().map(|(id, _, t)| (id, Rc::new(t))).collect(),
            fn_map: HashMap::new(),
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

    fn insert_fn(&mut self, type_id: TypeId, fn_t: FunctionType, cfg: CFG) -> FnId {
        let fn_id = self.new_fn_id();
        self.insert_type(type_id, SmplType::Function(fn_t));

        let function = Function {
            fn_type: type_id,
            cfg: cfg,
        };

        if self.fn_map.insert(fn_id, function).is_some() {
            panic!("Attempting to override Function with FnId {} in the Universe", fn_id.0);
        }

        fn_id
    }

    fn insert_type(&mut self, id: TypeId, t: SmplType) {
        if self.types.insert(id, Rc::new(t)).is_some() {
            panic!("Attempting to override type with TypeId {} in the Universe", id.0);
        }
    }

    fn get_type(&self, id: &TypeId) -> Rc<SmplType> {
        match self.types.get(id).map(|t| t.clone()) {
            Some(t) => t,
            None => panic!("Type with TypeId {} does not exist.", id.0),
        }
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
}

#[derive(Clone, Debug)]
pub struct ScopedData {
    type_map: HashMap<Path, TypeId>,
    var_map: HashMap<AsciiString, VarId>,
    var_type_map: HashMap<VarId, TypeId>,
    fn_map: HashMap<Path, FnId>,
}

impl ScopedData {

    fn insert_fn(&mut self, name: Path, fn_id: FnId) {
        // TODO: Fn name override behaviour?
        self.fn_map.insert(name, fn_id);
    }

    pub fn type_id(&self, path: &Path) -> Result<TypeId, Err> {
        self.type_map.get(path)
            .map(|id| id.clone())
            .ok_or(Err::UnknownType(path.clone()))
    }

    fn get_type(&self, universe: &Universe, path: &Path) -> Result<Rc<SmplType>, Err> {
        let id = self.type_map.get(path).ok_or(Err::UnknownType(path.clone()))?;
        let t = universe.types.get(id).expect(&format!("Missing TypeId: {}. All TypeId's should be valid if retrieven from ScopedData.type_map", id.0));
        Ok(t.clone())
    }

    fn insert_type(&mut self, path: Path, id: TypeId) -> Option<TypeId> {
        self.type_map.insert(path, id)
    }

    pub fn var_type_id(&self, name: &AsciiStr) -> Result<TypeId, Err> {
        self.var_map.get(name)
            .map(|id| self.var_type_map.get(id).unwrap().clone())
            .ok_or(Err::UnknownVar(name.to_owned()))
    }

    pub fn insert_var(&mut self, name: AsciiString, id: VarId, type_id: TypeId) {
        self.var_map.insert(name, id);

        if self.var_type_map.insert(id, type_id).is_some() {
            panic!("Attempting to override variable {} with a different type. Shadowing should produce a new variable id.", id);
        }
    }
}
