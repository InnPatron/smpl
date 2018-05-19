use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use petgraph::graph;
use petgraph::graph::NodeIndex;
use petgraph::Direction;
use petgraph::visit::EdgeRef;

use feature::*;

use ast::{Ident, BinOp, UniOp};

use analysis::*;
use analysis::smpl_type::*;
use analysis::metadata::*;

use super::value::{Value, FnHandle};

pub struct VM {
    program: Program
}

impl VM {
    pub fn new(program: Program) -> VM {
        VM {
            program: program
        }
    }

    pub fn eval_fn(&self, handle: FnHandle) -> Value {
        let mut fn_env = FnEnv::new(&self.program, handle.id(), None);
        fn_env.eval()
    }

    pub fn eval_fn_args(&self, handle: FnHandle, args: Vec<Value>) -> Value {
        let mut fn_env = FnEnv::new(&self.program, handle.id(), Some(args));
        fn_env.eval()
    }
}

#[derive(Debug, Clone)]
struct Env {
    env: HashMap<String, Rc<RefCell<Value>>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            env: HashMap::new()
        }
    }

    pub fn map_var(&mut self, var: VarId, value: Value) -> Option<Value> {
        self.map_value(Env::var_id(var), value)
    }

    pub fn map_tmp(&mut self, tmp: TmpId, value: Value) -> Option<Value> {
        self.map_value(Env::tmp_id(tmp), value)
    }

    pub fn map_value(&mut self, name: String, value: Value) -> Option<Value> {
        self.env.insert(name, Rc::new(RefCell::new(value))).map(|rc| rc.borrow().clone())
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.env.get(name).map(|r| (*r.borrow()).clone())
    }

    pub fn get_var(&self, id: VarId) -> Option<Value> {
        self.get(&Env::var_id(id))
    }

    pub fn get_tmp(&self, id: TmpId) -> Option<Value> {
        self.get(&Env::tmp_id(id))
    }

    pub fn ref_value(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.env.get(name).map(|r| r.clone())
    }

    pub fn ref_var(&self, id: VarId) -> Option<Rc<RefCell<Value>>> {
        self.ref_value(&Env::var_id(id))
    }

    pub fn ref_tmp(&self, id: TmpId) -> Option<Rc<RefCell<Value>>> {
        self.ref_value(&Env::tmp_id(id))
    }

    fn tmp_id(id: TmpId) -> String {
        format!("_tmp_{}", id.raw())
    }

    fn var_id(id: VarId) -> String {
        format!("_var_{}", id.raw())
    }
}

struct FnEnv<'a> {
    program: &'a Program,
    graph: &'a CFG,
    env: Env,
    loop_heads: HashMap<LoopId, NodeIndex>,
    loop_result: HashMap<LoopId, bool>,
    previous_is_loop_head: bool,
    loop_stack: Vec<LoopId>, 
}

enum NodeEval {
    Next(NodeIndex),
    Return(Value),
}

impl<'a> FnEnv<'a> {

    fn new(program: &Program, fn_id: FnId, args: Option<Vec<Value>>) -> FnEnv {
        let mut env = Env::new();

        if let Some(args) = args {
            for (arg, param_info) in args.into_iter()
                .zip(program.metadata().function_param_ids(fn_id)) {
                    env.map_var(param_info.var_id(), arg);
            }
        }

        let f = program.universe().get_fn(fn_id);

        FnEnv {
            program: program,
            graph: f.cfg(),
            env: env,
            loop_heads: HashMap::new(),
            loop_result: HashMap::new(),
            previous_is_loop_head: false,
            loop_stack: Vec::new(),
        }
    }

    fn eval(&mut self) -> Value {
        let mut next_node = Some(self.graph.start());

        while let Some(next) = next_node {
            match self.eval_node(next).unwrap() {
                NodeEval::Next(n) => next_node = Some(n),
                NodeEval::Return(v) => return v,
            }
        }

        unreachable!()
    }

    fn pop_loop_stack(&mut self) -> LoopId {
        self.loop_stack.pop().unwrap()
    }

    fn get_loop_result(&self, id: LoopId) -> bool {
        self.loop_result.get(&id).unwrap().clone()
    }

    fn get_loop_head(&self, id: LoopId) -> NodeIndex {
        self.loop_heads.get(&id).unwrap().clone()
    }

    fn eval_node(&mut self, current: NodeIndex) -> Result<NodeEval, ()> {
        match *self.graph.node_weight(current) {
            Node::End => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Return(Value::Unit))
            }

            Node::Start => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.graph.next(current)))
            }

            Node::BranchSplit(_) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.graph.next(current)))
            }

            Node::BranchMerge(_) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.graph.next(current)))
            }

            Node::LoopHead(ref data) => {
                self.previous_is_loop_head = true;

                self.loop_stack.push(data.loop_id);
                self.loop_heads.insert(data.loop_id, current);

                Ok(NodeEval::Next(self.graph.next(current)))
            }

            Node::LoopFoot(_) => {
                self.previous_is_loop_head = false;

                let loop_id = self.pop_loop_stack();
                let loop_result = self.get_loop_result(loop_id);

                if loop_result {
                    return Ok(NodeEval::Next(self.get_loop_head(loop_id)));
                } else {
                    let neighbors = neighbors!(self.graph, current);
                    for n in neighbors {
                        match *node_w!(self.graph, n) {
                            Node::LoopHead(_) => continue,
                            _ => return Ok(NodeEval::Next(n)),
                        }
                    }
                }

                unreachable!();
            }

            Node::Continue(_) => {
                self.previous_is_loop_head = false;
                let loop_id = self.pop_loop_stack();
                Ok(NodeEval::Next(self.get_loop_head(loop_id)))
            }

            Node::Break(_) => {
                self.previous_is_loop_head = false;

                let neighbors = neighbors!(self.graph, current);
                for n in neighbors {
                    match *node_w!(self.graph, current) {
                        Node::LoopFoot(_) => return Ok(NodeEval::Next(n)),
                        _ => continue,
                    }
                }

                unreachable!();
            }

            Node::EnterScope => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.graph.next(current)))
            }

            Node::ExitScope => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.graph.next(current)))
            }

            Node::LocalVarDecl(ref data) => {
                self.previous_is_loop_head = false;
                let value = Expr::eval_expr(self.program, &self.env, data.decl.init_expr());
                self.env.map_var(data.decl.var_id(), value);
                Ok(NodeEval::Next(self.graph.next(current)))
            }

            Node::Assignment(ref data) => {
                self.previous_is_loop_head = false;
                let path = data.assignment.assignee().path();

                let root_var = path.root_var_id();
                let root_var = self.env.ref_var(root_var).unwrap();

                let mut value = root_var;
                if let Some(ref e) = path.root_indexing_expr() {
                    value = {
                        let borrow = value.borrow();
                        let indexer = Expr::eval_expr(self.program, &self.env, e);
                        let indexer = irmatch!(indexer; Value::Int(i) => i);
                        let array = irmatch!(*borrow; Value::Array(ref a) => a);
                        array.get(indexer as usize).unwrap().clone()
                    };
                }

                for ps in path.path() {
                    match *ps {
                        PathSegment::Ident(ref f) => {
                            value = {
                                let value = value.borrow();
                                let struct_value = irmatch!(*value; Value::Struct(ref s) => s);
                                struct_value.ref_field(f.field_id()).unwrap()
                            };
                        }

                        PathSegment::Indexing(ref f, ref indexer) => {
                            value = {
                                let value = value.borrow();
                                let struct_value = irmatch!(*value; Value::Struct(ref s) => s);
                                let field_to_index = struct_value.ref_field(f.field_id()).unwrap();
                                let field_to_index = field_to_index.borrow();
                                let field = irmatch!(*field_to_index; Value::Array(ref a) => a);

                                let indexer = Expr::eval_expr(self.program, &self.env, indexer);
                                let indexer = irmatch!(indexer; Value::Int(i) => i);
                                field.get(indexer as usize).unwrap().clone()
                            };
                        }
                    }
                }

                let mut borrow = value.borrow_mut();
                *borrow = Expr::eval_expr(self.program, &self.env, data.assignment.value());

                Ok(NodeEval::Next(self.graph.next(current)))
            }

            Node::Expr(ref data) => {
                self.previous_is_loop_head = false;
                Expr::eval_expr(self.program, &self.env, &data.expr);
                Ok(NodeEval::Next(self.graph.next(current)))
            }

            Node::Return(ref data) => {
                self.previous_is_loop_head = false;
                let value = match data.expr {
                    Some(ref expr) => Expr::eval_expr(self.program, &self.env, expr),
                    None => Value::Unit
                };
                Ok(NodeEval::Return(value))
            }

            Node::Condition(ref data) => {
                let value = Expr::eval_expr(self.program, &self.env, &data.expr);
                let value = irmatch!(value; Value::Bool(b) => b);
                let (t_b, f_b) = self.graph.after_condition(current);
                let next = if value {
                    t_b
                } else {
                    f_b
                };

                if self.previous_is_loop_head {
                    let id = self.pop_loop_stack();
                    self.loop_result.insert(id, value);
                    self.loop_stack.push(id);
                }


                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.graph.next(next)))
            }
        }
    }
}

mod Expr {
    use std::ops::{Add, Sub, Div, Mul, BitAnd, BitOr, Neg, Not};

    use ast::{Literal, BinOp, UniOp};
    use analysis::{Program, Expr, Tmp, Value as AbstractValue, BindingId, ArrayInit, PathSegment};
    use analysis::smpl_type::SmplType;
    use super::*;
    use super::super::value::*;

    pub(in super) fn eval_expr(program: &Program, host_env: &Env, expr: &Expr) -> Value {
        let mut expr_env = Env::new();
        let mut last = None;
        for id in expr.execution_order() {
            let tmp = expr.get_tmp(id.clone());

            let result = eval_tmp(program, host_env, &expr_env, expr, tmp);
            expr_env.map_tmp(*id, result.clone());
            last = Some(result);
        }

        last.unwrap()
    }

    fn eval_tmp(program: &Program, host_env: &Env, expr_env: &Env, expr: &Expr, tmp: &Tmp) -> Value {
        match *tmp.value().data() {
            AbstractValue::Literal(ref literal) => {
                match *literal {
                    Literal::Bool(b) => Value::Bool(b),
                    Literal::Int(i) => Value::Int(i as i32),
                    Literal::Float(f) => Value::Float(f as f32),
                    Literal::String(ref s) => Value::String(s.to_string()),
                }
            },

            AbstractValue::Binding(ref binding) => {
                let id = binding.get_id().unwrap();
                match id {
                    BindingId::Var(id) => host_env.get_var(id).map(|v| v.clone()).unwrap(),
                    BindingId::Fn(id) => Value::Function(id.into()),
                }
            }

            AbstractValue::FieldAccess(ref access) => {
                let path = access.path();

                let root_var = path.root_var_id();
                let root_var = host_env.ref_var(root_var).unwrap();

                let mut value = root_var;

                if let Some(ref e) = path.root_indexing_expr() {
                    value = {
                        let borrow = value.borrow();
                        let indexer = eval_expr(program, host_env, e);
                        let indexer = irmatch!(indexer; Value::Int(i) => i);
                        let array = irmatch!(*borrow; Value::Array(ref a) => a);
                        array.get(indexer as usize).unwrap().clone()
                    };
                }

                for ps in path.path() {
                    match *ps {
                        PathSegment::Ident(ref f) => {
                            value = {
                                let value = value.borrow();
                                let struct_value = irmatch!(*value; Value::Struct(ref s) => s);
                                struct_value.ref_field(f.field_id()).unwrap()
                            };
                        }

                        PathSegment::Indexing(ref f, ref indexer) => {
                            value = {
                                let value = value.borrow();
                                let struct_value = irmatch!(*value; Value::Struct(ref s) => s);
                                let field_to_index = struct_value.ref_field(f.field_id()).unwrap();
                                let field_to_index = field_to_index.borrow();
                                let field = irmatch!(*field_to_index; Value::Array(ref a) => a);

                                let indexer = eval_expr(program, host_env, indexer);
                                let indexer = irmatch!(indexer; Value::Int(i) => i);
                                field.get(indexer as usize).unwrap().clone()
                            };
                        }
                    }
                }
                let borrow = value.borrow();
                let ret = borrow.clone();

                ret
            },

            AbstractValue::FnCall(ref call) => {
                let fn_id = match call.get_id().unwrap() {
                    BindingId::Var(var) => {
                        let var = host_env.get_var(var).unwrap();
                        let function = irmatch!(var; Value::Function(fn_id) => fn_id);
                        function.id()
                    }

                    BindingId::Fn(fn_id) => fn_id,
                };

                let args = call.args().map(|v| {
                    v.iter().map(|tmp| {
                        expr_env.get_tmp(tmp.data().clone()).unwrap().clone()
                    }).collect()
                });

                let mut fn_env = FnEnv::new(program, fn_id, args);
                fn_env.eval()
            }

            AbstractValue::BinExpr(ref op, ref lhs, ref rhs) => {
                let lh_id = lhs.data().clone();
                let rh_id = rhs.data().clone();

                let lh_v = expr_env.get_tmp(lh_id).unwrap();
                let rh_v = expr_env.get_tmp(rh_id).unwrap();

                match *program.universe().get_type(lhs.type_id().unwrap()) {
                    SmplType::Int => {
                        let lhs = irmatch!(lh_v; Value::Int(i) => i);
                        let rhs = irmatch!(rh_v; Value::Int(i) => i);


                        if is_math(op.clone()) {
                            let result = math_op(op.clone(), lhs, rhs);
                            Value::Int(result)
                        } else {
                            let result = cmp(op.clone(), lhs, rhs);
                            Value::Bool(result)
                        }
                    }

                    SmplType::Float => {
                        let lhs = irmatch!(lh_v; Value::Float(f) => f);
                        let rhs = irmatch!(rh_v; Value::Float(f) => f);


                        if is_math(op.clone()) {
                            let result = math_op(op.clone(), lhs, rhs);
                            Value::Float(result)
                        } else {
                            let result = cmp(op.clone(), lhs, rhs);
                            Value::Bool(result)
                        }
                    }
                    
                    SmplType::Bool => {
                        let lhs = irmatch!(lh_v; Value::Bool(b) => b);
                        let rhs = irmatch!(rh_v; Value::Bool(b) => b);


                        if is_logical(op.clone()) {
                            let result = logical(op.clone(), lhs, rhs);
                            Value::Bool(result)
                        } else {
                            let result = cmp(op.clone(), lhs, rhs);
                            Value::Bool(result)
                        }
                    }

                    _ => {
                        Value::Bool(partial_cmp(op.clone(), lh_v, rh_v))
                    }
                }
            }

            AbstractValue::UniExpr(ref op, ref t) => {
                let t_id = t.data().clone();
                let t_v = expr_env.get_tmp(t_id).unwrap();

                irmatch!(*program.universe().get_type(t.type_id().unwrap());
                         SmplType::Float => {
                             let f = irmatch!(t_v; Value::Float(f) => f);
                             Value::Float(negate(f))
                         },

                         SmplType::Int => {
                             let i = irmatch!(t_v; Value::Int(i) => i);
                             Value::Int(negate(i))
                         },

                         SmplType::Bool => {
                             let b = irmatch!(t_v; Value::Bool(b) => b);
                             Value::Bool(not(b))
                         }
                 )
            }

            AbstractValue::StructInit(ref init) => {
                let mut s = Struct::new();

                match init.field_init() {
                    Some(ref v) => {
                        for &(ref field_id, ref tmp) in v.iter() {
                            let field_value = expr_env.get_tmp(tmp.data().clone()).unwrap();
                            s.set_field(field_id.clone(), field_value.clone());
                        }
                    }

                    None => ()
                }

                Value::Struct(s)
            }

            AbstractValue::ArrayInit(ref init) => {
                match *init {
                    ArrayInit::List(ref v) => {
                        Value::Array(v.iter().map(|element| {
                            let element_id = element.data().clone();
                            Rc::new(RefCell::new(expr_env.get_tmp(element_id).unwrap().clone()))
                        }).collect())
                    }

                    ArrayInit::Value(ref v, size) => {
                        let element = expr_env.get_tmp(v.data().clone()).unwrap();
                        Value::Array((0..size).into_iter().map(|_| Rc::new(RefCell::new(element.clone()))).collect())
                    }
                }
            }

            AbstractValue::Indexing(ref indexing) => {
                let array = expr_env.ref_tmp(indexing.array.data().clone()).unwrap();
                let array = array.borrow();
                let array = irmatch!(*array; Value::Array(ref v) => v);

                let indexer = expr_env.get_tmp(indexing.indexer.data().clone()).unwrap();
                let indexer = irmatch!(indexer; Value::Int(i) => i);

                let indexed_value = array.get(indexer as usize).unwrap();
                let indexed_value = indexed_value.borrow();
                indexed_value.clone()
            }

            AbstractValue::ModAccess(ref access) => {
                let fn_id = access.fn_id().unwrap();
                Value::Function(fn_id.into())
            }
        }
    }

    fn not<T: Not<Output=T>>(t: T) -> T {
        !t
    }

    fn negate<T: Neg<Output=T>>(t: T) -> T {
        -t
    }

    fn is_logical(op: BinOp) -> bool {
        match op {
            BinOp::LogicalAnd | BinOp::LogicalOr => true,
            _ => false,
        }
    }

    fn is_math(op: BinOp) -> bool {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => true,
            _ => false
        }
    }

    fn math_op<T: Add<Output=T> + Sub<Output=T> + Div<Output=T> + Mul<Output=T>>(op: BinOp, lhs: T, rhs: T) -> T {
        irmatch!(op;
            BinOp::Add => lhs + rhs,
            BinOp::Sub => lhs - rhs,
            BinOp::Mul => lhs * rhs,
            BinOp::Div => lhs / rhs
        )
    }

    fn partial_cmp<T: PartialEq>(op: BinOp, lhs: T, rhs: T) -> bool {
        irmatch!(op;
                 BinOp::Eq => lhs == rhs,
                 BinOp::InEq => lhs != rhs
        )
    }

    fn cmp<T: PartialOrd>(op: BinOp, lhs: T, rhs: T) -> bool {
        irmatch!(op;
            BinOp::Eq => lhs == rhs,
            BinOp::InEq => lhs != rhs,
            BinOp::GreaterEq => lhs >= rhs,
            BinOp::LesserEq => lhs <= rhs,
            BinOp::Lesser => lhs < rhs,
            BinOp::Greater => lhs > rhs
        )
    }

    fn logical<T: BitAnd<Output=T> + BitOr<Output=T>>(op: BinOp, lhs: T, rhs: T) -> T {
        irmatch!(op;
                 BinOp::LogicalAnd => lhs & rhs,
                 BinOp::LogicalOr => lhs | rhs
        )
    }
}
