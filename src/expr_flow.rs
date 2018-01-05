use std::cell::Cell;
use std::collections::HashMap;

use semantic_ck::{Universe, FnId, TypeId, VarId, TmpId};
use ast::{FnCall as AstFnCall, Ident as AstIdent, Literal, UniOp, BinOp, Expr};

pub struct ExprScope {
    map: HashMap<TmpId, Tmp>,
}

#[derive(Debug)]
pub struct Tmp {
    id: TmpId,
    value: Value,
}

#[derive(Debug)]
pub enum Op {
    BinOp(BinOp),
    UniOp(UniOp),
    FnCall(usize),
}

#[derive(Debug)]
pub enum Value {
    Literal(Typed<Literal>),
    Ident(Typed<Ident>),
    FnCall(Typed<FnCall>),
    BinExpr(Typed<BinOp>, Box<Typed<Value>>, Box<Typed<Value>>),
    UniExpr(Typed<UniOp>, Box<Typed<Value>>),
}

pub fn flatten_expr(universe: &Universe, e: Expr) {
    let mut scope = ExprScope {
        map: HashMap::new(),
    };

    let mut ops = Vec::new();
    let mut values = Vec::new();
    let mut exprs = Vec::new();

    exprs.push(e);

    while let Some(e) = exprs.pop() {
        match e {
            Expr::Bin(bin) => {
                ops.push(Op::BinOp(bin.op));
                //TODO: Might need to switch order
                exprs.push(*bin.lhs);
                exprs.push(*bin.rhs);
            }

            Expr::Uni(uni) => {
                ops.push(Op::UniOp(uni.op));
                exprs.push(*uni.expr);
            }

            Expr::Literal(literal) => {
                let lit_type = match literal {
                    Literal::String(_) => universe.string(),
                    Literal::Number(ref num) => {
                        unimplemented!("Might need to fix parser to distinguish between ints and floats (require floats to have a full stop)");
                    },
                    Literal::Bool(_) => universe.boolean(),
                };

                let tmp = Tmp {
                    id: universe.new_tmp_id(),
                    value: Value::Literal(Typed::typed(literal, lit_type))
                };

                values.push(map_tmp(&mut scope, tmp));
            }

            Expr::Ident(ident) => {
                let tmp = Tmp {
                    id: universe.new_tmp_id(),
                    value: Value::Ident(Typed::untyped(Ident::new(ident)))
                };

                values.push(map_tmp(&mut scope, tmp));
            }

            Expr::FnCall(fn_call) => {

            }
        }
    }
}

fn map_tmp(scope: &mut ExprScope, tmp: Tmp) -> TmpId {
    let id = tmp.id;
    scope.map.insert(id, tmp);
    id
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
    fn_call: AstFnCall,
    fn_id: Cell<Option<FnId>>,
}

impl FnCall {
    fn new(call: AstFnCall) -> FnCall {
        FnCall {
            fn_call: call,
            fn_id: Cell::new(None),
        }
    }

    pub fn set_id(&self, id: FnId) {
        if self.fn_id.get().is_some() {
            panic!("Attempting to overwrite {} of the FnCall {:?} with {}", self.fn_id.get().unwrap(), self.fn_call, id);
        } else {
            self.fn_id.set(Some(id));
        }
    }

    pub fn get_id(&self) -> Option<FnId> {
        self.fn_id.get()
    }
}
