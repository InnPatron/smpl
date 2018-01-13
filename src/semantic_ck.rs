use std::collections::HashMap;
use std::cell::Cell;
use std::str::FromStr;
use std::rc::Rc;

use ascii::*;

use err::Err;
use control_flow::CFG;
use fn_analyzer::analyze_fn;
use ast::*;
use ast::{Function as AstFunction, Program as AstProgram};
use smpl_type::*;
use smpl_type::FnParameter as FnParameter;

pub struct Program {
    universe: Universe,
    global_scope: ScopedData,
    main: Option<FnId>,
}

impl Program {
    pub fn universe(&self) -> &Universe {
        &self.universe
    }

    pub fn global_scope(&self) -> &ScopedData {
        &self.global_scope
    }

    pub fn main(&self) -> Option<FnId> {
        self.main
    }
}

pub fn check(program: AstProgram) -> Result<Program, Err> {
    let mut universe = Universe::std();
    let mut global_scope = universe.std_scope.clone();

    let mut main = None;

    for decl_stmt in program.0.into_iter() {
        match decl_stmt {
            DeclStmt::Struct(struct_def) => {
                let struct_t = generate_struct_type(&global_scope, struct_def)?;
                let id = universe.new_type_id();
                
                global_scope.insert_type(struct_t.name.clone().into(), id);
                universe.insert_type(id, SmplType::Struct(struct_t));
            },

            DeclStmt::Function(fn_def) => {
                let name: Path = fn_def.name.clone().into();

                let type_id = universe.new_type_id();

                let fn_type = generate_fn_type(&global_scope, &universe, &fn_def)?;
                let cfg = CFG::generate(&universe, fn_def, &fn_type)?;

                let fn_id = universe.new_fn_id();
                universe.insert_fn(fn_id, type_id, fn_type, cfg);
                global_scope.insert_fn(name.clone(), fn_id);

                let func = universe.get_fn(fn_id);
                analyze_fn(&universe, &global_scope, func.cfg(), fn_id)?;

                if name == path!("main") {
                    if main.is_some() {
                        return Err(Err::MultipleMainFns);
                    } else {
                        main = Some(fn_id);
                    }
                }
            },
        }
    }
    
    Ok( Program {
        universe: universe,
        global_scope: global_scope,
        main: main,
    })
}

fn generate_fn_type(scope: &ScopedData, universe: &Universe, fn_def: &AstFunction) -> Result<FunctionType, Err> {
    let ret_type = match fn_def.return_type {
        Some(ref path) => scope.type_id(path)?,
        None => universe.unit(),
    };

    let params: Vec<_> = match fn_def.params {
        Some(ref params) => params.iter()
                              .map(|ref fn_param| {
                                  scope.type_id(&fn_param.param_type)
                                       .map(|id| FnParameter::new(fn_param.name.clone(), id))
                              })
                              .collect::<Result<Vec<_>, Err>>()?,

        None => Vec::new(),
    };

    Ok(FunctionType {
        params: params,
        return_type: ret_type,
    })
}

fn generate_struct_type(scope: &ScopedData, struct_def: Struct) -> Result<StructType, Err> {
    let struct_name = struct_def.name;
    let mut fields = HashMap::new();
    if let Some(body) = struct_def.body.0 {
        for field in body.into_iter() {
            let f_name = field.name;
            let f_type_path = field.field_type;
            let field_type = scope.type_id(&f_type_path)?;
            fields.insert(f_name, field_type);
        }
    } 

    let struct_t = StructType {
        name: struct_name,
        fields: fields,
    };

    Ok(struct_t)
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

    pub fn all_types(&self) -> Vec<Rc<SmplType>> {
        self.types.values().cloned().collect()
    }

    pub fn all_fns(&self) -> Vec<&Function> {
        self.fn_map.values().collect()
    }
}

#[derive(Clone, Debug)]
pub struct ScopedData {
    type_map: HashMap<Path, TypeId>,
    var_map: HashMap<Ident, VarId>,
    var_type_map: HashMap<VarId, TypeId>,
    fn_map: HashMap<Path, FnId>,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;

    #[test]
    fn basic_test_semantic_analysis() {
        let program =
"struct Test {
    field_1: i32,
    field_2: float,
    field_3: String,
    field_4: bool
}

fn main() {
    let truthy: bool = true;
    if true {
        truthy = false;
    } else {
        truthy = true;
    }
}
";
        let program = parse_Program(program).unwrap();
        let program = check(program).unwrap();

        let universe = program.universe();

        let main = universe.get_fn(program.main().unwrap());
        let main_type = universe.get_type(main.type_id());
        if let SmplType::Function(ref fn_type) = *main_type {
            assert_eq!(SmplType::Unit, *universe.get_type(fn_type.return_type));
        } else {
            panic!("main()'s TypeId was not mapped to a SmplType::Function");
        }

    }
}
