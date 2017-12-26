#[macro_use]
use std::collections::HashMap;
use std::str::FromStr;
use std::borrow::{Borrow, BorrowMut};
use std::cell::Cell;
use std::fmt::Debug;

use ascii::*;

use control_flow::Err as ControlFlowErr;
use control_flow::CFG;
use smpl_type::*;
use ast::*;

pub fn check(mut program: Program) -> Result<(), Err> {
    

    Ok(())
}

#[derive(Clone, Debug)]
pub enum Err {
    ControlFlowErr(ControlFlowErr),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub u64);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnId(pub u64);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(pub u64);


#[derive(Clone, Debug)]
pub struct Function {
    fn_type: FunctionType,
    cfg: CFG
}

#[derive(Clone, Debug)]
struct GlobalData {
    types: HashMap<TypeId, SmplType>,
    fns: HashMap<FnId, Function>,
    id_counter: Cell<u64>,
}

impl GlobalData {

    fn std() -> GlobalData {
        let mut data = GlobalData {
            types: HashMap::new(),
            fns: HashMap::new(),
            id_counter: Cell::new(0), 
        };

        let mut map = HashMap::new();
        map.insert(data.new_type_id(), SmplType::Unit);
        map.insert(data.new_type_id(), SmplType::Int);
        map.insert(data.new_type_id(), SmplType::Float);
        map.insert(data.new_type_id(), SmplType::String);
        map.insert(data.new_type_id(), SmplType::Bool);

        data.types = map;

        data
    }

    pub fn new_fn_id(&self) -> FnId {
        let id = self.id_counter.get();
        let fn_id = FnId(id);
        self.id_counter.set(id + 1);

        fn_id
    }

    pub fn new_type_id(&self) -> TypeId {
        let id = self.id_counter.get();
        let type_id = TypeId(id);
        self.id_counter.set(id + 1);

        type_id
    }

    pub fn new_var_id(&self) -> VarId {
        let id = self.id_counter.get();
        let type_id = VarId(id);
        self.id_counter.set(id + 1);

        type_id
    }
}

#[derive(Clone, Debug)]
struct ScopedData {
    types: HashMap<Path, TypeId>,
    fns: HashMap<Ident, FnId>,
    vars: HashMap<Ident, VarId>,
    var_map: HashMap<VarId, SmplType>,
}
