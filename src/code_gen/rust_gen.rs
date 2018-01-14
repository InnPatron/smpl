use semantic_ck::*;
use typed_ast::*;
use control_flow::*;
use smpl_type::*;
use ast::{Ident, Path, BinOp, UniOp};

use petgraph::graph::NodeIndex;

pub struct RustGen {
    output: String,
    shift: u32,
}

// Misc
impl RustGen {
    pub fn new() -> RustGen {
        RustGen {
            output: String::new(),
            shift: 0,
        }
    }

    fn line_pad(&mut self) {
        self.output.push('\n');
    }

    fn emit(&mut self, str: &str) {
        self.output.push_str(str);
    }

    fn emit_line(&mut self, line: &str) {
        self.emit_shift();
        self.output.push_str(line);
        self.output.push('\n');
    }

    fn emit_line_with_padding(&mut self, line: &str, pre: u32, post: u32) {
        for i in 0..pre {
            self.line_pad();
        }

        self.emit_shift();
        self.output.push_str(line);

        for i in 0..pre {
            self.line_pad();
        }
    }

    fn emit_shift(&mut self) {
        for i in 0..self.shift {
            self.output.push('\t');
        }
    }

    fn shift_right(&mut self) {
        self.shift = self.shift + 1;
    }

    fn shift_left(&mut self) {
        if self.shift > 0 {
            self.shift = self.shift - 1;
        }
    }

    fn tmp_id(id: TmpId) -> String {
        format!("_tmp{}", id.raw())
    }

    fn var_id(id: VarId) -> String {
        format!("_var{}", id.raw())
    }

    fn fn_id(id: FnId) -> String {
        format!("_fn{}", id.raw())
    }

    fn type_id(id: TypeId) -> String {
        format!("_type{}", id.raw())
    }
}

// Code generation
impl RustGen {
    pub fn generate(&mut self, program: &Program) {
        self.prelude();

        self.emit_line("//#### START STRUCT DEFINITIONS ####");

        // Emit struct definitions
        for (id, t) in program.universe().all_types() {
            if let SmplType::Struct(ref struct_t) = *t {
                self.emit_struct_type(program.universe(), id, struct_t);
                self.line_pad();
            }
        }

        self.emit_line("//#### END  STRUCT DEFINITIONS ####");
        self.emit_line("//#### START FUNCTION DEFINITIONS ####");

        // Emit function definitions
        for (fn_id, ref func) in program.universe().all_fns() {
            let func_type = program.universe().get_type(func.type_id());
            let name = RustGen::fn_id(fn_id);

            let return_type_id;
            let mut args = String::new();
            if let SmplType::Function(ref fn_type) = *func_type {
                // Get return type
                return_type_id = fn_type.return_type;

                // Gather parameters 
                for param in fn_type.params.iter() {
                    let param_type = param.param_type;
                    args.push_str(&format!("{}: {}, ", 
                                           RustGen::var_id(param.var_id().unwrap()),
                                           RustGen::type_id(param_type)));
                }
            } else {
                panic!("{} did not map to a function type", func.type_id());
            }
            let return_type = RustGen::type_id(return_type_id);

            // Emit fn signature
            self.emit(&format!("fn {} (", name));
            self.emit(&args);
            self.emit(&format!(") -> {}", return_type));

            // Emit CFG
            let mut to_check = func.cfg().after_start();
            loop {
                match self.emit_node(func.cfg(), to_check) {
                    Some(next) => to_check = next,
                    None => break,
                }
            }
            self.line_pad();
        }
        self.emit_line("//#### END FUNCTION DEFINITIONS ####");
    }

    fn prelude(&mut self) {
        self.output.push_str("use std::cell::RefCell;\n");
    }

    fn emit_node(&mut self, cfg: &CFG, to_check: NodeIndex) -> Option<NodeIndex> {
        match *node_w!(cfg, to_check) {
            Node::End => None,
            Node::Start | Node::BranchMerge | Node::LoopHead(_) => {
                Some(cfg.next(to_check))
            }

            Node::LoopFoot(_) => Some(cfg.after_loop_foot(to_check)),
            Node::Continue(_) => {
                self.emit_line("continue;");
                Some(cfg.after_continue(to_check))
            }

            Node::Break(_) => { 
                self.emit_line("break;");
                Some(cfg.after_break(to_check))
            }

            Node::EnterScope => {
                self.emit_line("{");
                self.shift_right();
                Some(cfg.next(to_check))
            }
            Node::ExitScope => {
                self.emit_line("}");
                self.shift_left();
                Some(cfg.next(to_check))
            }

            Node::LocalVarDecl(ref var_decl) => {
                let var_id = var_decl.var_id();
                let type_id = var_decl.type_id().unwrap();
                let expr = self.emit_expr(var_decl.init_expr());

                let name = RustGen::var_id(var_id);
                let var_type = RustGen::type_id(type_id);
                let expr = RustGen::tmp_id(expr);

                self.emit_line(&format!("let {}: {} = {};",
                                        name, var_type, expr));
                Some(cfg.next(to_check))
            }

            Node::Assignment(ref assignment) => {
                let var_id = assignment.var_id().unwrap();
                let expr = self.emit_expr(assignment.value());

                self.emit_line(&format!("{} = {};",
                                        RustGen::var_id(var_id),
                                        RustGen::tmp_id(expr)));

                Some(cfg.next(to_check))
            },

            Node::Expr(ref expr) => {
                self.emit_expr(expr);
                Some(cfg.next(to_check))
            }

            Node::Return(ref return_expr) => {
                match *return_expr {
                    Some(ref expr) => {
                        let expr = self.emit_expr(expr);
                        self.emit_line(&format!("return {};", 
                                                RustGen::tmp_id(expr)));
                    }

                    None => self.emit_line("return;"),

                }
                Some(cfg.next(to_check))
            }

            Node::Condition(ref condition_expr) => {
                let expr = self.emit_expr(condition_expr);
                self.emit_line(&format!("if {} ", RustGen::tmp_id(expr)));


                let mut merge_node = None;
                let (true_branch_head, false_branch_head) = cfg.after_condition(to_check);
                let mut current_true_node = true_branch_head;
                let mut current_false_node = false_branch_head;

                // Go through all the nodes in each branch until each branch hits the
                // Node::BranchMerge.
                //
                // Can continue going through the CFG linearly afterwords.
                loop {
                    match *node_w!(cfg, current_true_node) {
                        Node::BranchMerge => {
                            merge_node = Some(current_true_node);
                            break;
                        }

                        _ => (),
                    }
                    match self.emit_node(cfg, current_true_node) {
                        Some(next) => current_true_node = next,
                        None => return None,
                    }
                }

                self.emit(" else ");
                loop {
                    match *node_w!(cfg, current_false_node) {
                        Node::BranchMerge => {
                            merge_node = Some(current_false_node);
                            break; 
                        }

                        _ => (),
                    }
                    match self.emit_node(cfg, current_false_node) {
                        Some(next) => current_false_node = next,
                        None => return None,
                    }
                }

                let merge_node = merge_node.unwrap();
                Some(merge_node)
            }
        }
    }

    fn emit_expr(&mut self, expr: &Expr) -> TmpId {
        let execution_order = expr.execution_order();

        let mut last_tmp = None;
        for tmp in execution_order {
            self.emit_tmp(expr.get_tmp(*tmp));
            last_tmp = Some(tmp);
        }

        *last_tmp.unwrap()
    }

    fn emit_tmp(&mut self, tmp: &Tmp) {
        let id = tmp.id();
        let value = tmp.value();

        let lhs = RustGen::tmp_id(id);
        let rhs = match *value.data() {
            Value::Literal(ref lit) => {
                match *lit {
                    Literal::String(ref string) => {
                        let lit = format!("\"{}\"", string);
                        lit
                    }

                    Literal::Int(int) => int.to_string(), 

                    Literal::Float(float) => float.to_string(),

                    Literal::Bool(boolean) => boolean.to_string(),
                }
            },

            Value::Variable(ref var) => {
                let var_id = RustGen::var_id(var.get_id()
                                             .expect("If the program passed semantic analysis, all IDs should be filled in."));
                var_id
            }

            Value::FieldAccess(ref access) => {
                let var_id = RustGen::var_id(access.get_root_var_id()
                                             .expect("If the program passed semantic analysis, all IDs should be filled in."));
                let mut result = var_id;
                let mut path = access.path().iter();

                path.next();        // Remove root ident

                for field in path {
                    result.push_str(&format!(".{}", field));
                }

                result
            }

            Value::BinExpr(ref op, ref lhs, ref rhs) => {
                format!("{} {} {}", 
                        RustGen::tmp_id(*lhs.data()),
                        RustGen::bin_op(op),
                        RustGen::tmp_id(*rhs.data()))
            }

            Value::UniExpr(ref op, ref tmp) => {
                format!("{}{}",
                        RustGen::uni_op(op),
                        RustGen::tmp_id(*tmp.data()))
            }

            Value::FnCall(ref fn_call) => {
                let fn_id = fn_call.get_id().unwrap();
                
                // Gather argument expressions
                let mut arg_string = String::new();
                match fn_call.args() {
                    Some(ref args) => {
                        for a in args.iter() {
                            arg_string.push_str(&format!("{}, ", 
                                                         RustGen::tmp_id(*a.data())));
                        }
                    }

                    None => (),
                }

                format!("{}({})", 
                        RustGen::fn_id(fn_id),
                        arg_string)
            },

            Value::StructInit(ref struct_init) => {
                let struct_id = struct_init.struct_type().unwrap();

                let mut field_init = String::new();
                match struct_init.field_init() {
                    Some(init_list) => {
                        for &(ref field, ref typed_tmp) in init_list {
                            field_init.push_str(&format!("{}: {},\n",
                                                         field.to_string(),
                                                         RustGen::tmp_id(*typed_tmp.data())));
                        }
                    },
                    
                    None => (),
                }

                format!("{} {{ {} }}",
                        RustGen::type_id(struct_id),
                        field_init)
            }
        };

        self.emit_line(&format!("let {} = {};\n",
                                lhs,
                                rhs));
    }

    fn uni_op(op: &UniOp) -> String {
        use self::UniOp::*;

        match *op {
            Ref => unimplemented!(),
            Deref => unimplemented!(),
            Negate => "-".to_string(),
            LogicalInvert => "!".to_string(),
        }
    }

    fn bin_op(op: &BinOp) -> String {
        use self::BinOp::*;
        match *op {
            Add => "+".to_string(),
            Sub => "-".to_string(),
            Mul => "*".to_string(),
            Div => "/".to_string(),
            Mod => "%".to_string(),

            LogicalAnd => "&&".to_string(),
            LogicalOr => "||".to_string(),
            GreaterEq => ">=".to_string(),
            LesserEq => "<=".to_string(),
            Greater => ">".to_string(),
            Lesser => "<".to_string(),
            Eq => "==".to_string(),
            InEq => "!=".to_string(),
        }
    }

    fn emit_struct_type(&mut self, universe: &Universe, id: TypeId, struct_type: &StructType) {
        let name = RustGen::type_id(id);
        let fields = struct_type.fields.iter()
                                .map(|(name, id)| (name.clone(), RustGen::type_id(*id)));

        
        self.emit_line("#[derive(Clone, Debug, PartialEq)]");
        self.emit_line(&format!("struct {} {{", name));
        for (name, string_type) in fields {
            self.emit_line(&format!("\t{}: RefCell<{}>", name, string_type));
        }
        self.emit_line("}");
    }
}
