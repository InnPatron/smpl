use std::cell::Cell;
use std::collections::HashMap;
use std::slice::Iter;

use semantic_ck::{Universe, FnId, TypeId, VarId, TmpId};
use ast::{FnCall as AstFnCall, Ident as AstIdent, Literal, UniOp, BinOp, Expr as AstExpr};

pub fn flatten(universe: &Universe, e: AstExpr) -> Expr {
    let mut expr = Expr {
        map: HashMap::new(),
        execution_order: Vec::new(),
    };

    flatten_expr(universe, &mut expr, e);

    expr
}

pub fn flatten_expr(universe: &Universe, scope: &mut Expr, e: AstExpr) -> TmpId {
    match e {
        AstExpr::Bin(bin) => {
            let lhs = flatten_expr(universe, scope, *bin.lhs);
            let rhs = flatten_expr(universe, scope, *bin.rhs);
            scope.map_tmp(universe, Value::BinExpr(Typed::untyped(bin.op), 
                                                   Typed::untyped(lhs),
                                                   Typed::untyped(rhs)))
        }

        AstExpr::Uni(uni) => {
            let expr = flatten_expr(universe, scope, *uni.expr);
            scope.map_tmp(universe, Value::UniExpr(Typed::untyped(uni.op),
                                                   Typed::untyped(expr)))
        }

        AstExpr::Literal(literal) => {
            let lit_type = match literal {
                Literal::String(_) => universe.string(),
                Literal::Number(ref num) => {
                    unimplemented!("Might need to fix parser to distinguish between ints and floats (require floats to have a full stop)");
                },
                Literal::Bool(_) => universe.boolean(),
            };

            scope.map_tmp(universe, Value::Literal(Typed::typed(literal, lit_type)))
        }

        AstExpr::Ident(ident) => scope.map_tmp(universe, Value::Ident(Typed::untyped(Ident::new(ident)))),

        AstExpr::FnCall(fn_call) => {
            let name = fn_call.name;
            let args = fn_call.args.map(|vec| vec.into_iter().map(|e| Typed::untyped(flatten_expr(universe, scope, e))).collect::<Vec<_>>());

            let fn_call = FnCall::new(name, args);

            scope.map_tmp(universe, Value::FnCall(Typed::untyped(fn_call)))
        }
    }
    
}

#[derive(Debug)]
pub struct Typed<T> where T: ::std::fmt::Debug {
    data: T,
    data_type: Cell<Option<TypeId>>
}

impl<T> Typed<T> where T: ::std::fmt::Debug {
    fn untyped(data: T) -> Typed<T> {
        Typed {
            data: data,
            data_type: Cell::new(None) 
        }
    }

    fn typed(data: T, t: TypeId) -> Typed<T> {
        Typed {
            data: data,
            data_type: Cell::new(Some(t)),
        }
    }

    pub fn set_type(&self, t: TypeId) {
        // TODO: Handle type override
        if self.data_type.get().is_some() {
            panic!("Attempting to overwrite the type of this node ({:?})", self);
        } else {
            self.data_type.set(Some(t));
        }
    }

    pub fn get_type(&self) -> Option<TypeId> {
        self.data_type.get()
    }

    pub fn data(&self) -> &T {
        &self.data
    }
}

pub struct Expr {
    map: HashMap<TmpId, Tmp>,
    execution_order: Vec<TmpId>,
}

impl Expr {

    pub fn execution_order(&self) -> Iter<TmpId> {
        self.execution_order.iter()
    }

    fn map_tmp(&mut self, universe: &Universe, val: Value) -> TmpId {
        let tmp = Tmp {
            id: universe.new_tmp_id(),
            value: val,
        };
        let id = tmp.id;

        if self.map.insert(id, tmp).is_some() {
            panic!("Attempting to override {}", id);
        }

        self.execution_order.push(id);

        id
    }
}

#[derive(Debug)]
pub struct Tmp {
    id: TmpId,
    value: Value,
}

#[derive(Debug)]
pub enum Value {
    Literal(Typed<Literal>),
    Ident(Typed<Ident>),
    FnCall(Typed<FnCall>),
    BinExpr(Typed<BinOp>, Typed<TmpId>, Typed<TmpId>),
    UniExpr(Typed<UniOp>, Typed<TmpId>),
}

#[derive(Debug)]
pub struct Ident {
    ident: AstIdent,
    var_id: Cell<Option<VarId>>,
}

impl Ident {
    fn new(ident: AstIdent) -> Ident {
        Ident {
            ident: ident,
            var_id: Cell::new(None),
        }
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

#[derive(Debug)]
pub struct FnCall {
    name: AstIdent,
    args: Option<Vec<Typed<TmpId>>>,
    fn_id: Cell<Option<FnId>>,
}

impl FnCall {
    fn new(name: AstIdent, args: Option<Vec<Typed<TmpId>>>) -> FnCall {
        FnCall {
            name: name,
            args: args,
            fn_id: Cell::new(None),
        }
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
