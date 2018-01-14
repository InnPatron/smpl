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
            self.output.push('\n');
        }

        self.emit_shift();
        self.output.push_str(line);

        for i in 0..pre {
            self.output.push('\n');
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
}

// Code generation
impl RustGen {
    pub fn generate(&mut self, program: &Program) {
        self.prelude();

        for t in program.universe().all_types() {
            if let SmplType::Struct(ref t) = *t {
                self.emit_struct_type(program.universe(), t);
            }
        }
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
                self.output.push_str("continue;\n");
                Some(cfg.after_continue(to_check))
            }

            Node::Break(_) => { 
                self.output.push_str("break;\n");
                Some(cfg.after_break(to_check))
            }

            Node::EnterScope => {
                self.output.push_str("{\n");
                Some(cfg.next(to_check))
            }
            Node::ExitScope => {
                self.output.push_str("}\n");
                Some(cfg.next(to_check))
            }

            Node::LocalVarDecl(ref var_decl) => {
                unimplemented!();
                Some(cfg.next(to_check))
            }

            Node::Assignment(ref assignment) => {
                let var_id = assignment.var_id().unwrap();
                let expr = self.emit_expr(assignment.value());

                self.output.push_str(&format!("{} = {};\n",
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
                        self.output.push_str(&format!("return {};\n", 
                                                      RustGen::tmp_id(expr)));
                    }

                    None => self.output.push_str("return;\n"),

                }
                Some(cfg.next(to_check))
            }

            Node::Condition(ref condition_expr) => {
                let expr = self.emit_expr(condition_expr);
                self.output.push_str(&format!("if {} \n", RustGen::tmp_id(expr)));


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

                self.output.push_str("else");
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

            Value::FnCall(_) => unimplemented!(),
            Value::StructInit(_) => unimplemented!(),
        };

        self.output.push_str(&format!("let {} = {};\n",
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

    fn tmp_id(id: TmpId) -> String {
        format!("_tmp{}", id.raw())
    }

    fn var_id(id: VarId) -> String {
        format!("_var{}", id.raw())
    }

    fn emit_struct_type(&mut self, universe: &Universe, struct_type: &StructType) {
        let name = struct_type.name.to_string();
        let fields = struct_type.fields.iter()
                                .map(|(name, id)| (name.clone(), RustGen::string_repr(&*universe.get_type(*id))));

        let mut output = String::new();
        
        output.push_str("#[derive(Clone, Debug, PartialEq)]\n");
        output.push_str(&format!("struct {} {{\n", name));
        for (name, string_type) in fields {
            output.push_str(&format!("\t{}: RefCell<{}>,\n", name, string_type));
        }
        output.push_str("}\n");

        self.output.push_str(&output);
    }

    fn string_repr(t: &SmplType) -> String {
        match *t {
            SmplType::Int => "i64".to_string(),
            SmplType::Float => "f64".to_string(),
            SmplType::String => "String".to_string(),
            SmplType::Bool => "bool".to_string(),
            SmplType::Unit => "()".to_string(),
            SmplType::Struct(ref struct_t) => struct_t.name.to_string(),
            SmplType::Function(_) => unimplemented!(),
        }
    }
}
