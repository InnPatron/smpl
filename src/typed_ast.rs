pub use ast::BinOp;
pub use ast::UniOp;
pub use ast::Literal;

use std::cell::Cell;
use std::collections::HashMap;
use std::slice::Iter;

use semantic_ck::*;
use expr_flow;
use smpl_type::{ SmplType, FunctionType };
use ast;
use ascii::{AsciiString, AsciiStr};

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

    pub fn map_tmp(&mut self, universe: &Universe, val: Value, t: Option<TypeId>) -> TmpId {
        let tmp = Tmp {
            id: universe.new_tmp_id(),
            value: Typed {
                data: val,
                data_type: Cell::new(t),
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
    FnCall(self::FnCall),
    BinExpr(ast::BinOp, Typed<TmpId>, Typed<TmpId>),
    UniExpr(ast::UniOp, Typed<TmpId>),
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
    name: ast::Ident,
    args: Option<Vec<Typed<TmpId>>>,
    fn_id: Cell<Option<FnId>>,
}

impl FnCall {
    pub fn new(name: ast::Ident, args: Option<Vec<Typed<TmpId>>>) -> FnCall {
        FnCall {
            name: name,
            args: args,
            fn_id: Cell::new(None),
        }
    }

    pub fn name(&self) -> &ast::Ident {
        &self.name
    }

    pub fn args(&self) -> Option<&Vec<Typed<TmpId>>> {
        self.args.as_ref()
    }

    pub fn set_id(&self, id: FnId) {
        if self.fn_id.get().is_some() {
            panic!("Attempting to overwrite {} of the FnCall {:?}", self.fn_id.get().unwrap(), self.name);
        } else {
            self.fn_id.set(Some(id));
        }
    }

    pub fn get_id(&self) -> Option<FnId> {
        self.fn_id.get()
    }
}
