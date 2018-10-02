use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use petgraph::graph::NodeIndex;
use petgraph::Direction;

use ast::{Ident, Module};

use err::Err;

use analysis::*;

use super::BuiltinMap;
use super::loader;
use super::vm_i::*;
use super::value::Value;
use super::env::Env;

pub struct VM {
    program: Program,
    builtins: HashMap<FnId, Box<BuiltinFn>>,
}

impl VM {
    pub fn new(user_modules: Vec<Module>) -> Result<VM, Err> {
        let modules = loader::include(user_modules);
        let program = check_program(modules)?;
        let mut vm = VM {
            program: program,
            builtins: HashMap::new(),
        };

        loader::load(&mut vm);

        Ok(vm)
    }

    pub fn eval_fn(&self, handle: FnHandle) -> Value {
        let id = handle.id();
        if self.program.metadata().is_builtin(id) {
            self.builtins
                .get(&id)
                .expect("Missing a built-in")
                .execute(None)
        } else {
            let mut fn_env = FnEnv::new(&self, id, None);
            fn_env.eval()
        }
    }

    pub fn eval_fn_args(&self, handle: FnHandle, args: Option<Vec<Value>>) -> Value {
        let id = handle.id();
        if self.program.metadata().is_builtin(id) {
            self.builtins
                .get(&id)
                .expect("Missing a built-in")
                .execute(args)
        } else {
            let mut fn_env = FnEnv::new(self, id, args);
            fn_env.eval()
        }
    }

    pub fn query_module(&self, module: &str, name: &str) -> Result<Option<FnHandle>, String> {
        let module = Ident(module.to_string());
        let name = Ident(name.to_string());
        let mod_id = self.program.universe().module_id(&module);

        match mod_id {
            Some(mod_id) => Ok(self.program
                .metadata()
                .module_fn(mod_id, name)
                .map(|fn_id| fn_id.into())),

            None => Err(format!("Module '{}' does not exist", module)),
        }
    }

    fn program(&self) -> &Program {
        &self.program
    }
}

impl BuiltinMap for VM {
    fn insert_builtin(
        &mut self,
        module_str: &str,
        name_str: &str,
        builtin: Box<BuiltinFn>,
    ) -> Result<Option<Box<BuiltinFn>>, String> {
        let module = Ident(module_str.to_string());
        let name = Ident(name_str.to_string());
        let mod_id = self.program.universe().module_id(&module);

        match mod_id {
            Some(mod_id) => match self.program.metadata().module_fn(mod_id, name) {
                Some(fn_id) => {
                    if self.program.metadata().is_builtin(fn_id) {
                        Ok(self.builtins.insert(fn_id, builtin))
                    } else {
                        Err(format!(
                            "{}::{} is not a valid builtin function",
                            module_str, name_str
                        ))
                    }
                }

                None => Err(format!("{} is not a function in {}", name_str, module_str)),
            },

            None => Err(format!("Module '{}' does not exist", module_str)),
        }
    }
}

struct FnEnv<'a> {
    vm: &'a VM,
    func: Rc<Function>,
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
    fn new(vm: &VM, fn_id: FnId, args: Option<Vec<Value>>) -> FnEnv {
        let mut env = Env::new();

        if let Some(args) = args {
            for (arg, param_info) in args.into_iter()
                .zip(vm.program().metadata().function_param_ids(fn_id))
            {
                env.map_var(param_info.var_id(), arg);
            }
        }

        let f = vm.program().universe().get_fn(fn_id);

        FnEnv {
            vm: vm,
            func: f,
            env: env,
            loop_heads: HashMap::new(),
            loop_result: HashMap::new(),
            previous_is_loop_head: false,
            loop_stack: Vec::new(),
        }
    }

    fn eval(&mut self) -> Value {
        let mut next_node = Some(self.func.cfg().start());

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
        match *self.func.cfg().node_weight(current) {
            Node::End => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Return(Value::Unit))
            }

            Node::Start => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.func.cfg().next(current)))
            }

            Node::BranchSplit(_) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.func.cfg().next(current)))
            }

            Node::BranchMerge(_) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.func.cfg().next(current)))
            }

            Node::LoopHead(ref data) => {
                self.previous_is_loop_head = true;

                self.loop_stack.push(data.loop_id);
                self.loop_heads.insert(data.loop_id, current);

                Ok(NodeEval::Next(self.func.cfg().next(current)))
            }

            Node::LoopFoot(_) => {
                self.previous_is_loop_head = false;

                let loop_id = self.pop_loop_stack();
                let loop_result = self.get_loop_result(loop_id);

                if loop_result {
                    return Ok(NodeEval::Next(self.get_loop_head(loop_id)));
                } else {
                    let cfg = self.func.cfg();
                    let neighbors = neighbors!(&*cfg, current);
                    for n in neighbors {
                        match *node_w!(self.func.cfg(), n) {
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

                let cfg = self.func.cfg();
                let neighbors = neighbors!(&*cfg, current);
                for n in neighbors {
                    match *node_w!(self.func.cfg(), current) {
                        Node::LoopFoot(_) => return Ok(NodeEval::Next(n)),
                        _ => continue,
                    }
                }

                unreachable!();
            }

            Node::EnterScope => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.func.cfg().next(current)))
            }

            Node::ExitScope => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.func.cfg().next(current)))
            }

            Node::LocalVarDecl(ref data) => {
                self.previous_is_loop_head = false;
                let value = Expr::eval_expr(self.vm, &self.env, data.decl.init_expr());
                self.env.map_var(data.decl.var_id(), value);
                Ok(NodeEval::Next(self.func.cfg().next(current)))
            }

            Node::Assignment(ref data) => {
                self.previous_is_loop_head = false;
                let path = data.assignment.assignee().path();

                let root_var = path.root_var_id();
                let root_var = self.env.ref_var(root_var).unwrap();

                let mut value = root_var;
                if let Some(tmp) = path.root_indexing_expr() {
                    value = {
                        let borrow = value.borrow();
                        let indexer = self.env.get_tmp(tmp).unwrap();
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
                                struct_value.ref_field(f.name().as_str()).unwrap()
                            };
                        }

                        PathSegment::Indexing(ref f, ref indexer) => {
                            value = {
                                let value = value.borrow();
                                let struct_value = irmatch!(*value; Value::Struct(ref s) => s);
                                let field_to_index =
                                    struct_value.ref_field(f.name().as_str()).unwrap();
                                let field_to_index = field_to_index.borrow();
                                let field = irmatch!(*field_to_index; Value::Array(ref a) => a);

                                
                                let indexer = self.env.get_tmp(*indexer).unwrap();
                                let indexer = irmatch!(indexer; Value::Int(i) => i);
                                field.get(indexer as usize).unwrap().clone()
                            };
                        }
                    }
                }

                let result = Expr::eval_expr(self.vm, &self.env, data.assignment.value());

                let mut borrow = value.borrow_mut();
                *borrow = result;

                Ok(NodeEval::Next(self.func.cfg().next(current)))
            }

            Node::Expr(ref data) => {
                self.previous_is_loop_head = false;
                Expr::eval_expr(self.vm, &self.env, &data.expr);
                Ok(NodeEval::Next(self.func.cfg().next(current)))
            }

            Node::Return(ref data) => {
                self.previous_is_loop_head = false;
                let value = match data.expr {
                    Some(ref expr) => Expr::eval_expr(self.vm, &self.env, expr),
                    None => Value::Unit,
                };
                Ok(NodeEval::Return(value))
            }

            Node::Condition(ref data) => {
                let value = Expr::eval_expr(self.vm, &self.env, &data.expr);
                let value = irmatch!(value; Value::Bool(b) => b);
                let (t_b, f_b) = self.func.cfg().after_condition(current);
                let next = if value { t_b } else { f_b };

                if self.previous_is_loop_head {
                    let id = self.pop_loop_stack();
                    self.loop_result.insert(id, value);
                    self.loop_stack.push(id);
                }

                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(self.func.cfg().next(next)))
            }
        }
    }
}

mod Expr {
    use ast::Literal;
    use analysis::{ArrayInit, BindingId, Expr, PathSegment, Tmp, Value as AbstractValue};
    use analysis::smpl_type::SmplType;
    use super::*;
    use super::super::value::*;
    use super::super::comp::*;

    pub(super) fn eval_expr(vm: &VM, host_env: &Env, expr: &Expr) -> Value {
        let mut expr_env = Env::new();
        let mut last = None;
        for id in expr.execution_order() {
            let tmp = expr.get_tmp(id.clone());

            let result = eval_tmp(vm, host_env, &expr_env, expr, tmp);
            expr_env.map_tmp(*id, result.clone());
            last = Some(result);
        }

        last.unwrap()
    }

    fn eval_tmp(vm: &VM, host_env: &Env, expr_env: &Env, _expr: &Expr, tmp: &Tmp) -> Value {
        match *tmp.value().data() {
            AbstractValue::Literal(ref literal) => match *literal {
                Literal::Bool(b) => Value::Bool(b),
                Literal::Int(i) => Value::Int(i as i32),
                Literal::Float(f) => Value::Float(f as f32),
                Literal::String(ref s) => Value::String(s.to_string()),
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

                if let Some(indexer) = path.root_indexing_expr() {
                    value = {
                        let borrow = value.borrow();
                        let indexer = host_env.get_tmp(indexer).unwrap();
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
                                struct_value.ref_field(f.name().as_str()).unwrap()
                            };
                        }

                        PathSegment::Indexing(ref f, ref indexer) => {
                            value = {
                                let value = value.borrow();
                                let struct_value = irmatch!(*value; Value::Struct(ref s) => s);
                                let field_to_index =
                                    struct_value.ref_field(f.name().as_str()).unwrap();
                                let field_to_index = field_to_index.borrow();
                                let field = irmatch!(*field_to_index; Value::Array(ref a) => a);

                                let indexer = host_env.get_tmp(*indexer).unwrap();
                                let indexer = irmatch!(indexer; Value::Int(i) => i);
                                field.get(indexer as usize).unwrap().clone()
                            };
                        }
                    }
                }
                let borrow = value.borrow();
                let ret = borrow.clone();

                ret
            }

            AbstractValue::FnCall(ref call) => {
                let fn_id = match call.get_id().unwrap() {
                    BindingId::Var(var) => {
                        let var = host_env.get_var(var).unwrap();
                        let function = irmatch!(var; Value::Function(fn_id) => fn_id);
                        function.id()
                    }

                    BindingId::Fn(fn_id) => fn_id,
                };

                let args: Option<Vec<_>> = call.args().map(|v| {
                    v.iter()
                        .map(|tmp| expr_env.get_tmp(tmp.data().clone()).unwrap().clone())
                        .collect()
                });

                vm.eval_fn_args(fn_id.into(), args)
            }

            AbstractValue::BinExpr(ref op, ref lhs, ref rhs) => {
                let lh_id = lhs.data().clone();
                let rh_id = rhs.data().clone();

                let lh_v = expr_env.get_tmp(lh_id).unwrap();
                let rh_v = expr_env.get_tmp(rh_id).unwrap();

                match *vm.program().universe().get_type(lhs.type_id().unwrap()) {
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

                    _ => Value::Bool(partial_cmp(op.clone(), lh_v, rh_v)),
                }
            }

            AbstractValue::UniExpr(ref _op, ref t) => {
                let t_id = t.data().clone();
                let t_v = expr_env.get_tmp(t_id).unwrap();

                irmatch!(*vm.program().universe().get_type(t.type_id().unwrap());
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
                        let init_order = init.init_order().unwrap();
                        for (ident, (_, ref tmp)) in init_order.into_iter().zip(v.iter()) {
                            let field_value = expr_env.get_tmp(tmp.data().clone()).unwrap();
                            s.set_field(ident.as_str().to_string(), field_value.clone());
                        }
                    }

                    None => (),
                }

                Value::Struct(s)
            }

            AbstractValue::ArrayInit(ref init) => match *init {
                ArrayInit::List(ref v) => Value::Array(
                    v.iter()
                        .map(|element| {
                            let element_id = element.data().clone();
                            Rc::new(RefCell::new(expr_env.get_tmp(element_id).unwrap().clone()))
                        })
                        .collect(),
                ),

                ArrayInit::Value(ref v, size) => {
                    let element = expr_env.get_tmp(v.data().clone()).unwrap();
                    Value::Array(
                        (0..size)
                            .into_iter()
                            .map(|_| Rc::new(RefCell::new(element.clone())))
                            .collect(),
                    )
                }
            },

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

            AbstractValue::AnonymousFn(ref a_fn) => {
                let fn_id = a_fn.fn_id();
                Value::Function(fn_id.into())
            }
        }
    }
}
