use semantic_ck::*;

use linear_cfg_traversal::*;
use typed_ast::*;
use control_flow::*;
use smpl_type::*;
use ast::{BinOp, UniOp};
use petgraph::graph::NodeIndex;

pub struct RustGen {
    output: String,
    shift: u32,
}

impl RustGen {
    pub fn new() -> RustGen {
        RustGen {
            output: String::new(),
            shift: 0,
        }
    }

    pub fn program(&self) -> &str {
        &self.output
    }

    pub fn emit_program(&mut self, program: &Program) {
        self.prelude(program.universe());

        self.emit_line("//#### START STRUCT DEFINITIONS ####");

        // Emit struct definitions
        for (id, t) in program.universe().all_types() {
            if let SmplType::Struct(ref struct_t) = *t {
                self.emit_struct_type(id, struct_t);
                self.line_pad();
            }
        }

        self.emit_line("//#### END  STRUCT DEFINITIONS ####");
        self.line_pad();
        self.emit_line("//#### START FUNCTION DEFINITIONS ####");

        if let Some(id) = program.main() {
            self.emit_line(&format!("fn main() {{ {}(); }}", RustFnGen::fn_id(id)));
        }

        // Emit function definitions
        for (fn_id, ref func) in program.universe().all_fns() {
            let func_type = program.universe().get_type(func.type_id());
            let name = RustFnGen::fn_id(fn_id);

            let return_type_id;
            let mut args = String::new();
            if let SmplType::Function(ref fn_type) = *func_type {
                // Get return type
                return_type_id = fn_type.return_type;

                // Gather parameters
                for param in fn_type.params.iter() {
                    let param_type = param.param_type;
                    let param_type = RustFnGen::type_id(param_type);
                    let param_type = RustFnGen::rustify_type(param_type);

                    args.push_str(&format!(
                        "{}: {}, ",
                        RustFnGen::var_id(param.var_id().unwrap()),
                        param_type
                    ));
                }
            } else {
                panic!("{} did not map to a function type", func.type_id());
            }
            let return_type = RustFnGen::type_id(return_type_id);
            let return_type = RustFnGen::rustify_type(return_type);

            // Emit fn signature
            match *program.universe().get_type(return_type_id) {
                SmplType::Unit => {
                    self.emit(&format!("fn {} ({})", name, args));
                }

                _ => {
                    self.emit(&format!("fn {} (", name));
                    self.emit(&args);
                    self.emit(&format!(") -> {} ", return_type));
                }
            }

            // Emit CFG
            let mut cfg = func.cfg();
            let mut fn_gen = RustFnGen::new(&cfg);

            {
                let traverser = Traverser::new(cfg, &mut fn_gen);
                traverser.traverse();
            }

            let fn_string = fn_gen.function();
            self.emit(&fn_string);

            self.line_pad();
        }
        self.emit_line("//#### END FUNCTION DEFINITIONS ####");
    }

    fn prelude(&mut self, universe: &Universe) {
        self.emit_line("use std::cell::RefCell;");
        self.emit_line("use std::rc::Rc;");
        self.line_pad();

        // Create TypeId aliases for primitives
        self.emit_line(&format!(
            "type {} = ();",
            RustFnGen::type_id(universe.unit())
        ));
        self.emit_line(&format!(
            "type {} = i64;",
            RustFnGen::type_id(universe.int())
        ));

        self.emit_line(&format!(
            "type {} = f64;",
            RustFnGen::type_id(universe.float())
        ));

        self.emit_line(&format!(
            "type {} = bool;",
            RustFnGen::type_id(universe.boolean())
        ));

        self.emit_line(&format!(
            "type {} = String;",
            RustFnGen::type_id(universe.string())
        ));

        self.line_pad();
    }

    fn emit_struct_type(&mut self, id: TypeId, struct_type: &StructType) {
        let name = RustFnGen::type_id(id);
        let fields = struct_type
            .fields
            .iter()
            .map(|(name, id)| (name.clone(), RustFnGen::type_id(*id)));

        self.emit_line("#[derive(Clone, Debug, PartialEq)]");
        self.emit_line(&format!("struct {} {{", name));
        self.shift_right();
        for (name, string_type) in fields {
            let name = name;
            let field_type = RustGen::rustify_type(string_type);
            self.emit_line(&format!("{}: {},", name, field_type));
        }
        self.shift_left();
        self.emit_line("}");
    }
}

pub struct RustFnGen<'a> {
    output: String,
    shift: u32,
    cfg: &'a CFG,
}

// Misc
impl<'a> RustFnGen<'a> {
    pub fn new(cfg: &'a CFG) -> RustFnGen<'a> {
        RustFnGen {
            output: String::new(),
            shift: 0,
            cfg: cfg,
        }
    }

    pub fn function(&self) -> &str {
        &self.output
    }
}

// Code generation
impl<'a> RustFnGen<'a> {

    fn emit_condition(&mut self, e: &Expr) {
        self.emit_line("if {");
        self.shift_right();
        let expr = self.emit_expr(e);
        self.emit_line(&format!("let condition = {}.borrow();", RustFnGen::tmp_id(expr)));
        self.emit_line("*condition }");
        self.shift_left();
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

        let lhs = RustFnGen::tmp_id(id);
        let rhs = match *value.data() {
            Value::Literal(ref lit) => match *lit {
                Literal::String(ref string) => {
                    let lit = format!("\"{}\".to_string()", string);
                    RustFnGen::new_value(lit)
                }

                Literal::Int(int) => RustFnGen::new_value(int.to_string()),

                Literal::Float(float) => RustFnGen::new_value(float.to_string()),

                Literal::Bool(boolean) => RustFnGen::new_value(boolean.to_string()),
            },

            Value::Variable(ref var) => {
                let var_id = RustFnGen::var_id(var.get_id().expect(
                    "If the program passed semantic analysis, all IDs should be filled in.",
                ));
                RustFnGen::clone_value(var_id)
            }

            Value::FieldAccess(ref access) => {
                let var_id = RustFnGen::borrow(RustFnGen::var_id(access.get_root_var_id().expect(
                    "If the program passed semantic analysis, all IDs should be filled in.",
                )));
                let mut result = var_id;
                let mut path = access.path().iter();

                path.next(); // Remove root ident

                for field in path {
                    result.push_str(&format!(".{}", field));
                }

                RustFnGen::clone_value(result)
            }

            Value::BinExpr(ref op, ref lhs, ref rhs) => RustFnGen::new_value(format!(
                "{} {} {}",
                RustFnGen::borrow(RustFnGen::tmp_id(*lhs.data())),
                RustFnGen::bin_op(op),
                RustFnGen::borrow(RustFnGen::tmp_id(*rhs.data())),
            )),

            Value::UniExpr(ref op, ref tmp) => format!(
                "{}{}",
                RustFnGen::uni_op(op),
                RustFnGen::tmp_id(*tmp.data())
            ),

            Value::FnCall(ref fn_call) => {
                let fn_id = fn_call.get_id().unwrap();

                // Gather argument expressions
                let mut arg_string = String::new();
                match fn_call.args() {
                    Some(ref args) => for a in args.iter() {
                        arg_string.push_str(&format!("{}, ", RustFnGen::tmp_id(*a.data())));
                    },

                    None => (),
                }

                format!("{}({})", RustFnGen::fn_id(fn_id), arg_string)
            }

            Value::StructInit(ref struct_init) => {
                let struct_id = struct_init.struct_type().unwrap();

                let mut field_init = String::new();
                match struct_init.field_init() {
                    Some(init_list) => for &(ref field, ref typed_tmp) in init_list {
                        field_init.push_str(&format!(
                            "{}: {},",
                            field.to_string(),
                            RustFnGen::tmp_id(*typed_tmp.data())
                        ));
                    },

                    None => (),
                }

                RustFnGen::new_value(format!("{} {{ {} }}", RustFnGen::type_id(struct_id), field_init))
            }
        };

        self.emit_line(&format!("let {} = {};", lhs, rhs));
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
}

impl<'a> Passenger<()> for RustFnGen<'a> {
    fn start(&mut self, _id: NodeIndex) -> Result<(), ()> {
        // Emit nothing
        Ok(())
    }

    fn end(&mut self, _id: NodeIndex) -> Result<(), ()> {
        // Emit nothing
        Ok(())
    }

    fn branch_merge(&mut self, _id: NodeIndex) -> Result<(), ()> {
        // Emit nothing
        Ok(())
    }

    fn loop_head(&mut self, _id: NodeIndex) -> Result<(), ()> {
        self.emit_line("loop {");
        self.shift_right();
        Ok(())
    }

    fn loop_foot(&mut self, _id: NodeIndex) -> Result<(), ()> {
        /*
        if self.previous_is_loop_head {
            self.emit_line("{ break; }");
            self.previous_is_loop_head = false;
        }
        // Can't be used; is it necessary?
        */

        self.shift_left();
        self.emit_line("}");
        self.line_pad();
        Ok(())
    }

    fn cont(&mut self, _id: NodeIndex) -> Result<(), ()> {
        self.emit_line("continue;");
        self.line_pad();
        Ok(())
    }

    fn br(&mut self, _id: NodeIndex) -> Result<(), ()> {
        self.emit_line("break;");
        self.line_pad();
        Ok(())
    }

    fn enter_scope(&mut self, _id: NodeIndex) -> Result<(), ()> {
        self.emit_line("{");
        self.shift_right();
        Ok(())
    }

    fn exit_scope(&mut self, _id: NodeIndex) -> Result<(), ()> {
        self.shift_left();
        self.emit_line("}");
        Ok(())
    }

    fn local_var_decl(&mut self, _id: NodeIndex, var_decl: &LocalVarDecl) -> Result<(), ()> {
        let var_id = var_decl.var_id();
        let type_id = var_decl.type_id().unwrap();
        let expr = self.emit_expr(var_decl.init_expr());

        let name = RustFnGen::var_id(var_id);
        let var_type = RustFnGen::type_id(type_id);
        let expr = RustFnGen::tmp_id(expr);

        let var_type = RustFnGen::rustify_type(var_type);

        self.emit_line(&format!("let mut {}: {} = {};", name, var_type, expr));
        self.line_pad();

        Ok(())
    }

    fn assignment(&mut self, _id: NodeIndex, assignment: &Assignment) -> Result<(), ()> {
        let var_id = assignment.var_id().unwrap();
        let expr = self.emit_expr(assignment.value());


        let path = assignment.name().clone();
        let path_count = path.0.len();
        let mut path = path.0.into_iter();
        path.next();        // Get rid of root variable ident

        let lhs = {
            let mut access_path = if path_count > 1 {
                RustFnGen::borrow_mut(RustFnGen::var_id(var_id))
            } else {
                RustFnGen::var_id(var_id)
            };
            
            for ident in path {
                access_path.push_str(&format!(".{}", ident));
            }
            access_path
        };


        let rhs = {
            let tmp = RustFnGen::tmp_id(expr);
            //RustFnGen::borrow(tmp)
            tmp
        };
        self.emit_line(&format!(
            "{} = {};",
            lhs,
            rhs
        ));

        self.line_pad();
        Ok(())
    }

    fn expr(&mut self, _id: NodeIndex, expr: &Expr) -> Result<(), ()> {
        self.emit_expr(expr);
        self.line_pad();
        Ok(())
    }

    fn ret(&mut self, _id: NodeIndex, expr: Option<&Expr>) -> Result<(), ()> {
        match expr {
            Some(ref expr) => {
                let expr = self.emit_expr(expr);
                self.emit_line(&format!("return {};", RustFnGen::tmp_id(expr)));
            }

            None => self.emit_line("return;"),
        }

        self.line_pad();
        Ok(())
    }

    fn loop_condition(&mut self, _id: NodeIndex, condition_expr: &Expr) -> Result<(), ()> {
        self.emit_condition(condition_expr);

        Ok(())
    }

    fn loop_start_true_path(&mut self, _id: NodeIndex) -> Result<(), ()> {
        Ok(())
    }

    fn loop_end_true_path(&mut self, _id: NodeIndex) -> Result<(), ()> {
        self.emit_line("else { break; }");
        self.shift_left();
        self.emit_line("}");
        Ok(())
    }

    fn branch_condition(&mut self, _id: NodeIndex, condition_expr: &Expr) -> Result<(), ()> {
        self.emit_condition(condition_expr);

        Ok(())
    }

    fn branch_start_true_path(&mut self, id: NodeIndex) -> Result<(), ()> {
        if let Node::BranchMerge = *self.cfg.node_weight(id) {
            self.emit_line("{ /* EMPTY */}");
        }

        Ok(())
    }

    fn branch_end_true_path(&mut self, _id: NodeIndex) -> Result<(), ()> {
        // Emit nothing
        Ok(())
    }

    fn branch_start_false_path(&mut self, id: NodeIndex) -> Result<(), ()> {
        if let Node::BranchMerge = *self.cfg.node_weight(id) {
            self.emit_line("else { /* EMPTY */}");
            self.line_pad();
        } else {
            self.emit_fmt(" else ");
        }

        Ok(())
    }

    fn branch_end_false_path(&mut self, _id: NodeIndex) -> Result<(), ()> {
        // Emit nothing
        Ok(())
    }
}

impl<'a> RustGenFmt for RustFnGen<'a> {
    fn output(&mut self) -> &mut String {
        &mut self.output
    }

    fn shift(&self) -> u32 {
        self.shift
    }

    fn set_shift(&mut self, shift: u32) {
        self.shift = shift;
    }
}

impl RustGenFmt for RustGen {
    fn output(&mut self) -> &mut String {
        &mut self.output
    }

    fn shift(&self) -> u32 {
        self.shift
    }

    fn set_shift(&mut self, shift: u32) {
        self.shift = shift;
    }
}

trait RustGenFmt {
    fn output(&mut self) -> &mut String;

    fn shift(&self) -> u32;

    fn set_shift(&mut self, shift: u32);

    fn line_pad(&mut self) {
        self.output().push('\n');
    }

    fn emit(&mut self, str: &str) {
        self.output().push_str(str);
    }

    fn emit_fmt(&mut self, str: &str) {
        self.emit_shift();
        self.output().push_str(str);
    }

    fn emit_line(&mut self, line: &str) {
        self.emit_shift();
        self.output().push_str(line);
        self.output().push('\n');
    }

    fn emit_shift(&mut self) {
        for _ in 0..self.shift() {
            self.output().push('\t');
        }
    }

    fn shift_right(&mut self) {
        let shift = self.shift() + 1;
        self.set_shift(shift);
    }

    fn shift_left(&mut self) {
        if self.shift() > 0 {
            let shift = self.shift() - 1;
            self.set_shift(shift);
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

    fn new_value(str: String) -> String {
        format!("Rc::new(RefCell::new({}))", str)
    }

    fn clone_value(str: String) -> String {
        format!("({}).clone()", str)
    }

    fn borrow_mut(str: String) -> String {
        format!("(*(({}).borrow_mut()))", str)
    }

    fn rustify_type(str: String) -> String {
        format!("Rc<RefCell<{}>>", str)
    }

    fn borrow(str: String) -> String {
        format!("(*(({}).borrow()))", str)
    }
}
