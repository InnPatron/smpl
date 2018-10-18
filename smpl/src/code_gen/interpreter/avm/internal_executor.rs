use std::collections::HashMap;

use failure::Fail;

use analysis::*;

use code_gen::interpreter::value::Value;
use code_gen::interpreter::env::Env;

use code_gen::interpreter::vm_i::*;

use super::vm::*;
use super::node_fetch::*;
use super::expr_eval::*;
use super::node_eval::*;

pub struct InternalExecutor<'a> {
    pub context: ExecutionContext,
    pub program: &'a Program,
    pub builtins: &'a HashMap<FnId, Box<BuiltinFn>>,
}

impl<'a> InternalExecutor<'a> {
    pub fn step(&mut self) -> ExecResult<Value, Box<Fail>> {
        match self.context.top().exec_state {
            ExecutorState::Fetch(node_index) => {
                let node_fetch = match node_fetch(&mut self.context.top_mut().fn_context, 
                                                  self.program, 
                                                  node_index) {
                    Ok(d) => d,
                    Err(e) => return ExecResult::Err(e),

                };
                match node_fetch {
                    FetchResult::Next(next) => self.context.top_mut().exec_state = ExecutorState::Fetch(next),
                    FetchResult::Expr(expr_phase) => { 
                        self.context.top_mut().exec_state = ExecutorState::Expr {
                            node: node_index,
                            tmp_index: 0,
                            expr_phase: expr_phase,
                        };
                    },
                    FetchResult::Return(v) => {
                        let stack = self.context.stack_mut();

                        let _returning_fn = stack.pop();

                        match stack.last_mut() {
                            Some(ref mut top) => top.fn_context.return_store = Some(v),

                            None => return ExecResult::Ok(v),
                        }
                    },
                }
            }

            ExecutorState::Expr {
                node,
                tmp_index,
                expr_phase,
            } => {
                match eval_node_tmp(self.program, 
                                    &mut self.context, 
                                    node, 
                                    tmp_index, 
                                    expr_phase) {
                    ExprEvalResult::Value(v, tmp_id) => {
                        self.context.top_mut().exec_state = ExecutorState::Expr {
                            node: node,
                            tmp_index: tmp_index + 1, // Go to the next tmp
                            expr_phase: expr_phase,
                        };

                        // Map the tmp
                        self.context.top_mut().func_env.map_tmp(tmp_id, v);
                    },

                    ExprEvalResult::PhaseChange(v, tmp_id) => {
                        self.context.top_mut().exec_state = ExecutorState::Expr {
                            node: node,
                            tmp_index: 0, // Go to the start of the next phase
                            expr_phase: expr_phase + 1, // Go to the next phase
                        };

                        // Map the tmp
                        self.context.top_mut().func_env.map_tmp(tmp_id, v);
                    },

                    ExprEvalResult::FnCall(fn_id, args) => {
                        // Check if builtin first
                        if self.program.metadata().is_builtin(fn_id) {
                            let result = self.builtins
                                                .get(&fn_id)
                                                .expect("Missing a built-in")
                                                .execute(args);

                            let v = match result {
                                Ok(v) => v,
                                Err(e) => return ExecResult::Err(e),
                            };

                            match self.context.stack_mut().last_mut() {
                                // Store return value in return store
                                Some(ref mut top) => {
                                    top.fn_context.return_store = Some(v);
                                    return ExecResult::Pending;

                                }

                                None => unreachable!(),
                            }
                        }


                        let fn_context = FnContext::new(fn_id);
                        let start = fn_context.get_fn(self.program).cfg().start();

                        // Set up arguments
                        let mut env = Env::new();
                        if let Some(args) = args {
                            for (arg, param_info) in args.into_iter()
                                .zip(self.program.metadata().function_param_ids(fn_id))
                            {
                               env.map_var(param_info.var_id(), arg);
                            }
                        }

                        // Push a StackInfo onto the stack, diverting the control flow to that
                        // function.
                        self.context.push_info(StackInfo {
                            func: fn_id,
                            func_env: env,
                            fn_context: fn_context,
                            exec_state: ExecutorState::Fetch(start),
                        });
                    },

                    ExprEvalResult::Finished(v, tmp_id) => {
                        // All expressions have been evaluated. Perform any node post-processing
                        // eval
                        self.context.top_mut().exec_state = ExecutorState::Eval(node);

                        // Map the tmp
                        self.context.top_mut().func_env.map_tmp(tmp_id, v);
                    },
                }
            }

            ExecutorState::Eval(current_node) => {
                let result = eval_node(self.context.top_mut(), self.program, current_node);

                let result = match result {
                    Ok(r) => r,
                    Err(e) => {
                        return ExecResult::Err(e);
                    }
                };

                match result {

                    NodeEval::Next(next_node) => {
                        self.context.top_mut().exec_state = ExecutorState::Fetch(next_node);
                    }

                    NodeEval::Return(value) => {
                        let stack = self.context.stack_mut();

                        let _returning_fn = stack.pop();

                        match stack.last_mut() {
                            Some(ref mut top) => top.fn_context.return_store = Some(value),

                            None => return ExecResult::Ok(value),
                        }
                    },
                }
            }
        }

        ExecResult::Pending
    }
}
