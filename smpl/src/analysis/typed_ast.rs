use std::cell::Cell;
use std::collections::HashMap;
use std::slice::Iter;

pub use ast::BinOp;
pub use ast::UniOp;
pub use ast::Literal;
use ast;


use super::smpl_type::*;
use super::semantic_data::*;
use super::expr_flow;

#[derive(Debug, Clone, PartialEq)]
pub struct Typed<T> where T: ::std::fmt::Debug + Clone + PartialEq {
    data: T,
    data_type: Cell<Option<TypeId>>
}

impl<T> Typed<T> where T: ::std::fmt::Debug + Clone + PartialEq {

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn untyped(data: T) -> Typed<T> {
        Typed {
            data: data,
            data_type: Cell::new(None) 
        }
    }

    pub fn typed(data: T, t: TypeId) -> Typed<T> {
        Typed {
            data: data,
            data_type: Cell::new(Some(t)),
        }
    }

    pub fn set_type_id(&self, t: TypeId) {
        // TODO: Handle type override
        if self.data_type.get().is_some() {
            panic!("Attempting to overwrite the type of this node ({:?})", self);
        } else {
            self.data_type.set(Some(t));
        }
    }

    pub fn type_id(&self) -> Option<TypeId> {
        self.data_type.get()
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    name: ast::Path,
    value: self::Expr,
    type_id: Cell<Option<TypeId>>,
    var_id: Cell<Option<VarId>>,
}

impl Assignment {
    pub fn new(universe: &Universe, assignment: ast::Assignment) -> Assignment {
        Assignment {
            name: assignment.name,
            value: expr_flow::flatten(universe, assignment.value),
            type_id: Cell::new(None),
            var_id: Cell::new(None),
        }
    }

    pub fn name(&self) -> &ast::Path {
        &self.name
    }

    pub fn value(&self) -> &self::Expr {
        &self.value
    }

    pub fn set_type_id(&self, id: TypeId) {
        if self.type_id.get().is_some() {
            panic!("Attempting to override {} for local variable declarration {:?}", id, self);
        } else {
            self.type_id.set(Some(id));
        }
    }

    pub fn type_id(&self) -> Option<TypeId> {
        self.type_id.get()
    }

    pub fn set_var_id(&self, id: VarId) {
        if self.var_id.get().is_some() {
            panic!("Attempting to override {} for local variable declarration {:?}", id, self);
        } else {
            self.var_id.set(Some(id));
        }
    }

    pub fn var_id(&self) -> Option<VarId> {
        self.var_id.get()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalVarDecl {
    var_type: ast::Path,
    var_name: ast::Ident,
    var_init: self::Expr,
    type_id: Cell<Option<TypeId>>,
    var_id: VarId,
}

impl LocalVarDecl {
    pub fn new(universe: &Universe, decl: ast::LocalVarDecl) -> LocalVarDecl {
        LocalVarDecl {
            var_type: decl.var_type,
            var_name: decl.var_name,
            var_init: expr_flow::flatten(universe, decl.var_init),
            type_id: Cell::new(None),
            var_id: universe.new_var_id(),
        }
    }

    pub fn type_path(&self) -> &ast::Path {
        &self.var_type
    }

    pub fn var_name(&self) -> &ast::Ident {
        &self.var_name
    }

    pub fn set_type_id(&self, id: TypeId) {
        if self.type_id.get().is_some() {
            panic!("Attempting to override {} for local variable declarration {:?}", id, self);
        } else {
            self.type_id.set(Some(id));
        }
    }

    pub fn type_id(&self) -> Option<TypeId> {
        self.type_id.get()
    }

    pub fn var_id(&self) -> VarId {
        self.var_id
    }

    pub fn init_expr(&self) -> &self::Expr {
        &self.var_init
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    map: HashMap<TmpId, Tmp>,
    execution_order: Vec<TmpId>,
}

impl Expr {

    pub fn new() -> Expr {
        Expr {
            map: HashMap::new(),
            execution_order: Vec::new(),
        }
    }

    pub fn get_tmp(&self, id: TmpId) -> &Tmp {
        self.map.get(&id).expect("Given ID should always be valid if taken from the correct Expr")
    }

    pub fn execution_order(&self) -> Iter<TmpId> {
        self.execution_order.iter()
    }

    pub fn map_tmp(&mut self, universe: &Universe, val: Value) -> TmpId {
        let tmp = Tmp {
            id: universe.new_tmp_id(),
            value: Typed {
                data: val,
                data_type: Cell::new(None),
            }
        };
        let id = tmp.id;

        if self.map.insert(id, tmp).is_some() {
            panic!("Attempting to override {}", id);
        }

        self.execution_order.push(id);

        id
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tmp {
    id: TmpId,
    value: Typed<Value>,
}

impl Tmp {
    pub fn id(&self) -> TmpId {
        self.id
    }

    pub fn value(&self) -> &Typed<Value> {
        &self.value
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Literal(ast::Literal),
    Variable(self::Variable),
    FieldAccess(self::FieldAccess),
    FnCall(self::FnCall),
    BinExpr(ast::BinOp, Typed<TmpId>, Typed<TmpId>),
    UniExpr(ast::UniOp, Typed<TmpId>),
    StructInit(StructInit),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInit {
    struct_type_name: ast::Path,
    field_init: Option<Vec<(ast::Ident, Typed<TmpId>)>>,
    struct_type: Cell<Option<TypeId>>,
}

impl StructInit {
    pub fn new(struct_type_name: ast::Path, field_init: Option<Vec<(ast::Ident, Typed<TmpId>)>>) -> StructInit {
        StructInit {
            struct_type_name: struct_type_name,
            struct_type: Cell::new(None),
            field_init: field_init,
        }
    }

    pub fn type_name(&self) -> &ast::Path {
        &self.struct_type_name
    }

    pub fn set_struct_type(&self, id: TypeId) {
        if self.struct_type.get().is_some() {
            panic!("Attempting to overwrite {} of the struct init with {}", self.struct_type.get().unwrap(), id);
        } else {
            self.struct_type.set(Some(id));
        }
    }

    pub fn field_init(&self, universe: &Universe) -> Option<Vec<(FieldId, Typed<TmpId>)>> {
        let t = universe.get_type(self.struct_type.get().unwrap());
        let t = match *t {
            SmplType::Struct(ref t) => t,
            _ => unreachable!(),
        };

        self.field_init.map(|vec| vec.iter().map(|&(k, v)| {
            (t.field_id(&k), v.clone())
        }).collect::<Vec<_>>())

    }

    pub fn struct_type(&self) -> Option<TypeId> {
        self.struct_type.get()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    path: ast::Path,
    root_var: Cell<Option<VarId>>,
    field_type_id: Cell<Option<TypeId>>
}

impl FieldAccess {
    pub fn new(path: ast::Path) -> FieldAccess {
        FieldAccess {
            path: path,
            root_var: Cell::new(None),
            field_type_id: Cell::new(None),
        }
    }

    pub fn path(&self) -> &ast::Path {
        &self.path
    }

    pub fn set_root_var_id(&self, id: VarId) {
        if self.root_var.get().is_some() {
            panic!("Attempting to overwrite root variable {} of the FieldAccess {:?} with {}", self.root_var.get().unwrap(), self.path, id);
        } else {
            self.root_var.set(Some(id));
        }
    }

    pub fn get_root_var_id(&self) -> Option<VarId> {
        self.root_var.get()
    }

    pub fn set_field_type_id(&self, id: TypeId) {
        if self.field_type_id.get().is_some() {
            panic!("Attempting to override {} for local variable declarration {:?}", id, self);
        } else {
            self.field_type_id.set(Some(id));
        }
    }

    pub fn field_type_id(&self) -> Option<TypeId> {
        self.field_type_id.get()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    ident: ast::Ident,
    var_id: Cell<Option<VarId>>,
}

impl Variable {
    pub fn new(ident: ast::Ident) -> Variable {
        Variable {
            ident: ident,
            var_id: Cell::new(None),
        }
    }

    pub fn ident(&self) -> &ast::Ident {
        &self.ident
    }

    pub fn set_id(&self, id: VarId) {
        if self.var_id.get().is_some() {
            panic!("Attempting to overwrite {} of the Ident {:?} with {}", self.var_id.get().unwrap(), self.ident, id);
        } else {
            self.var_id.set(Some(id));
        }
    }

    pub fn get_id(&self) -> Option<VarId> {
        self.var_id.get()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCall {
    path: ast::Path,
    args: Option<Vec<Typed<TmpId>>>,
    fn_id: Cell<Option<FnId>>,
}

impl FnCall {
    pub fn new(path: ast::Path, args: Option<Vec<Typed<TmpId>>>) -> FnCall {
        FnCall {
            path: path,
            args: args,
            fn_id: Cell::new(None),
        }
    }

    pub fn path(&self) -> &ast::Path {
        &self.path
    }

    pub fn args(&self) -> Option<&Vec<Typed<TmpId>>> {
        self.args.as_ref()
    }

    pub fn set_id(&self, id: FnId) {
        if self.fn_id.get().is_some() {
            panic!("Attempting to overwrite {} of the FnCall {:?}", self.fn_id.get().unwrap(), self.path);
        } else {
            self.fn_id.set(Some(id));
        }
    }

    pub fn get_id(&self) -> Option<FnId> {
        self.fn_id.get()
    }
}
