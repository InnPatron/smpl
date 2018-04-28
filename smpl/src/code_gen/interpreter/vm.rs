use std::collections::HashMap;

use petgraph::graph;
use petgraph::graph::NodeIndex;
use petgraph::Direction;
use petgraph::visit::EdgeRef;

use feature::*;

use ast::{Ident, BinOp, UniOp};

use analysis::*;
use analysis::smpl_type::*;
use analysis::metadata::*;

use super::value::Value;

pub struct VM {

}

#[derive(Debug, Clone)]
struct Env {
    env: HashMap<String, Value>,
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
        self.env.insert(name, value)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.env.get_mut(name)
    }

    pub fn get_var_mut(&mut self, id: VarId) -> Option<&mut Value> {
        self.env.get_mut(&Env::var_id(id))
    }

    pub fn get_tmp_mut(&mut self, id: TmpId) -> Option<&mut Value> {
        self.env.get_mut(&Env::tmp_id(id))
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.env.get(name)
    }

    pub fn get_var(&self, id: VarId) -> Option<&Value> {
        self.env.get(&Env::var_id(id))
    }

    pub fn get_tmp(&self, id: TmpId) -> Option<&Value> {
        self.env.get(&Env::tmp_id(id))
    }

    fn tmp_id(id: TmpId) -> String {
        format!("_tmp_{}", id.raw())
    }

    fn var_id(id: VarId) -> String {
        format!("_var_{}", id.raw())
    }
}

struct FnEnv<'a> {
    universe: &'a Universe,
    env: Env,
    loop_heads: HashMap<LoopId, NodeIndex>,
    loop_result: HashMap<LoopId, bool>,
    previous_is_loop_head: bool,
    loop_stack: Vec<LoopId>, 
}

enum NodeEval {
    Value(Value),
    Next(NodeIndex),
    End,
}

impl<'a> FnEnv<'a> {

    fn pop_loop_stack(&mut self) -> LoopId {
        self.loop_stack.pop().unwrap()
    }

    fn get_loop_result(&self, id: LoopId) -> bool {
        self.loop_result.get(&id).unwrap().clone()
    }

    fn get_loop_head(&self, id: LoopId) -> NodeIndex {
        self.loop_heads.get(&id).unwrap().clone()
    }

    fn eval_node(&mut self, graph: &CFG, current: NodeIndex) -> Result<NodeEval, ()> {
        match *graph.node_weight(current) {
            Node::End => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::End)
            }

            Node::Start => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::BranchSplit(id) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::BranchMerge(id) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::LoopHead(id) => {
                self.previous_is_loop_head = true;

                self.loop_stack.push(id);
                self.loop_heads.insert(id, current);

                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::LoopFoot(id) => {
                self.previous_is_loop_head = false;

                let loop_id = self.pop_loop_stack();
                let loop_result = self.get_loop_result(loop_id);

                if loop_result {
                    return Ok(NodeEval::Next(self.get_loop_head(loop_id)));
                } else {
                    let neighbors = neighbors!(graph, current);
                    for n in neighbors {
                        match *node_w!(graph, n) {
                            Node::LoopHead(_) => continue,
                            _ => return Ok(NodeEval::Next(n)),
                        }
                    }
                }

                unreachable!();
            }

            Node::Continue(id) => {
                self.previous_is_loop_head = false;
                let loop_id = self.pop_loop_stack();
                Ok(NodeEval::Next(self.get_loop_head(loop_id)))
            }

            Node::Break(_) => {
                self.previous_is_loop_head = false;

                let neighbors = neighbors!(graph, current);
                for n in neighbors {
                    match *node_w!(graph, current) {
                        Node::LoopFoot(_) => return Ok(NodeEval::Next(n)),
                        _ => continue,
                    }
                }

                unreachable!();
            }

            Node::EnterScope => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::ExitScope => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::LocalVarDecl(ref decl) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::Assignment(ref assign) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::Expr(ref expr) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::Return(ref ret_expr) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::Condition(ref condition) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }
        }
    }
}

mod Expr {
    use std::ops::{Add, Sub, Div, Mul, BitAnd, BitOr, Neg, Not};

    use ast::{Literal, BinOp, UniOp};
    use analysis::{Universe, Expr, Tmp, Value as AbstractValue, BindingId};
    use analysis::smpl_type::SmplType;
    use super::Env;
    use super::super::value::Value;

    fn eval_expr(universe: &Universe, host_env: &Env, expr: &Expr) -> Value {
        let mut expr_env = Env::new();
        let mut last = None;
        for id in expr.execution_order() {
            let tmp = expr.get_tmp(id.clone());

            let result = eval_tmp(universe, host_env, &expr_env, expr, tmp);
            expr_env.map_tmp(*id, result.clone());
            last = Some(result);
        }

        last.unwrap()
    }

    fn eval_tmp(universe: &Universe, host_env: &Env, expr_env: &Env, expr: &Expr, tmp: &Tmp) -> Value {
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
                    BindingId::Fn(id) => Value::Function(id),
                }
            }

            AbstractValue::FieldAccess(ref access) => {
                unimplemented!()
            },

            AbstractValue::FnCall(ref call) => {
                unimplemented!()
            }

            AbstractValue::BinExpr(ref op, ref lhs, ref rhs) => {
                let lh_id = lhs.data().clone();
                let rh_id = rhs.data().clone();

                let lh_v = expr_env.get_tmp(lh_id).unwrap();
                let rh_v = expr_env.get_tmp(rh_id).unwrap();

                match *universe.get_type(lhs.type_id().unwrap()) {
                    SmplType::Int => {
                        let lhs = irmatch!(*lh_v; Value::Int(i) => i);
                        let rhs = irmatch!(*rh_v; Value::Int(i) => i);


                        if is_math(op.clone()) {
                            let result = math_op(op.clone(), lhs, rhs);
                            Value::Int(result)
                        } else {
                            let result = cmp(op.clone(), lhs, rhs);
                            Value::Bool(result)
                        }
                    }

                    SmplType::Float => {
                        let lhs = irmatch!(*lh_v; Value::Float(f) => f);
                        let rhs = irmatch!(*rh_v; Value::Float(f) => f);


                        if is_math(op.clone()) {
                            let result = math_op(op.clone(), lhs, rhs);
                            Value::Float(result)
                        } else {
                            let result = cmp(op.clone(), lhs, rhs);
                            Value::Bool(result)
                        }
                    }
                    
                    SmplType::Bool => {
                        let lhs = irmatch!(*lh_v; Value::Bool(b) => b);
                        let rhs = irmatch!(*rh_v; Value::Bool(b) => b);


                        if is_logical(op.clone()) {
                            let result = logical(op.clone(), lhs, rhs);
                            Value::Bool(result)
                        } else {
                            let result = cmp(op.clone(), lhs, rhs);
                            Value::Bool(result)
                        }
                    }

                    _ => {
                        Value::Bool(cmp(op.clone(), lh_v, rh_v))
                    }
                }
            }

            AbstractValue::UniExpr(ref op, ref t) => {
                let t_id = t.data().clone();
                let t_v = expr_env.get_tmp(t_id).unwrap();

                irmatch!(*universe.get_type(t.type_id().unwrap());
                         SmplType::Float => {
                             let f = irmatch!(*t_v; Value::Float(f) => f);
                             Value::Float(negate(f))
                         },

                         SmplType::Int => {
                             let i = irmatch!(*t_v; Value::Int(i) => i);
                             Value::Int(negate(i))
                         },

                         SmplType::Bool => {
                             let b = irmatch!(*t_v; Value::Bool(b) => b);
                             Value::Bool(not(b))
                         }
                 )
            }

            AbstractValue::StructInit(ref init) => {
unimplemented!()
            }

            AbstractValue::ArrayInit(ref init) => {
unimplemented!()
            }

            AbstractValue::Indexing(ref indexing) => {
unimplemented!()
            }

            AbstractValue::ModAccess(ref access) => {
unimplemented!()
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
