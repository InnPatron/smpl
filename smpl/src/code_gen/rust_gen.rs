use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use ast::{Ident, BinOp, UniOp};

use analysis::*;
use analysis::smpl_type::*;

pub struct RustBackend {
    mods: Vec<(Ident, ModuleId, String)>,
    mod_wrap: bool
}

impl RustBackend {
    pub fn new() -> RustBackend {
        RustBackend {
            mods: Vec::new(),
            mod_wrap: false,
        }
    }

    pub fn wrap_mod(mut self) -> RustBackend {
        self.mod_wrap = true;
        self
    }

    pub fn generate(mut self, program: &Program) -> RustBackend {
        for &(ident, id) in program.universe().all_modules().iter() {
            let mut gen = RustModGen::new();

            if self.mod_wrap {
                gen.emit_line(&format!("mod {} {{", RustModGen::mod_id(*id)));
                gen.shift_right();
            }

            gen.emit_mod(program.universe(), program.universe().get_module(*id));

            if self.mod_wrap {
                gen.shift_left();
                gen.emit_line("}");
            }

            self.mods.push((ident.clone(), id.clone(), gen.module().to_string()));
        }

        self
    }

    pub fn mods(&self) -> &[(Ident, ModuleId, String)] {
        &self.mods
    }
}

struct RustModGen {
    output: String,
    shift: u32,
}

impl RustModGen {
    fn new() -> RustModGen {
        RustModGen {
            output: String::new(),
            shift: 0,
        }
    }

    fn module(&self) -> &str {
        &self.output
    }

    fn emit_mod(&mut self, universe: &Universe, module: &Module) {
        self.prelude(universe);

        self.emit_line("//#### START STRUCT DEFINITIONS ####");

        // Emit struct definitions
        for id in module.owned_types() {
            let id = *id;
            let t = universe.get_type(id);
            if let SmplType::Struct(ref struct_t) = *t {
                self.emit_struct_type(id, struct_t);
                self.line_pad();
            }
        }

        self.emit_line("//#### END  STRUCT DEFINITIONS ####");
        self.line_pad();
        self.emit_line("//#### START FUNCTION DEFINITIONS ####");

        // Emit function definitions
        for fn_id in module.owned_fns() {
            let fn_id = *fn_id;
            let func = universe.get_fn(fn_id);
            let func_type = universe.get_type(func.type_id());
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
            match *universe.get_type(return_type_id) {
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
            .map(|(name, id)| (name.clone(), RustFnGen::type_id(*id)))
            .collect::<Vec<_>>();

        self.emit_line("#[derive(Debug, PartialEq)]");
        self.emit_line(&format!("struct {} {{", name));
        self.shift_right();
        for &(ref name, ref string_type) in fields.iter() {
            let name = name;
            let field_type = RustModGen::rustify_type(string_type.to_string());
            self.emit_line(&format!("{}: {},", name, field_type));
        }
        self.shift_left();
        self.emit_line("}");

        self.line_pad();

        // Emit Clone impl
        self.emit_line(&format!("impl Clone for {} {{", name));
        self.shift_right();
        self.emit_line(&format!("fn clone(&self) -> Self {{"));

        self.shift_right();
        self.emit_line(&format!("{} {{", name));
        self.shift_right();
        for &(ref name, _) in fields.iter() {
            let value = RustFnGen::new_value("Default::default()".to_string());
            self.emit_line(&format!("{}: {},", name, value));
        }
        self.emit_line("}");
        self.shift_left();
        self.shift_left();

        self.emit_line("}");
        self.shift_left();
        self.emit_line("}");

        // Emit Default impl
        self.emit_line(&format!("impl Default for {} {{", name));
        self.shift_right();
        self.emit_line(&format!("fn default() -> Self {{"));

        self.shift_right();
        self.emit_line(&format!("{} {{", name));
        self.shift_right();
        for &(ref name, _) in fields.iter() {
            let value = RustFnGen::new_value("Default::default()".to_string());
            self.emit_line(&format!("{}: {},", name, value));
        }
        self.emit_line("}");
        self.shift_left();
        self.shift_left();

        self.emit_line("}");
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
        self.emit_line(&format!("{} }}", RustFnGen::tmp_id(expr)));
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
                    lit
                }

                Literal::Int(int) => int.to_string(),

                Literal::Float(float) => float.to_string(),

                Literal::Bool(boolean) => boolean.to_string(),
            },

            Value::Variable(ref var) => {
                let var_id = RustFnGen::var_id(var.get_id().expect(
                    "If the program passed semantic analysis, all IDs should be filled in.",
                ));

                let inner = RustFnGen::borrow(var_id);
                RustFnGen::clone_value(inner)
            }

            Value::FieldAccess(ref access) => {
                let var_id = RustFnGen::var_id(access.get_root_var_id().unwrap());
                let mut borrow_chain = format!("let _borrow_{} = {};\n", var_id,
                                               RustFnGen::borrow_ref(var_id.clone()));
                
                let mut previous = var_id;
                let mut path = access.path().iter();
                path.next(); // Remove root ident

                for field in path {
                    let borrow = RustFnGen::borrow_ref(format!("_borrow_{}.{}", previous, field));
                    borrow_chain.push_str(&format!(
                            "let _borrow_{} = {};", field, borrow));
                    previous = field.to_string();
                }

                let value = RustFnGen::clone_value(format!("_borrow_{}", previous));
                let result = format!("{{ {} {} }}", borrow_chain, value);

                result
            }

            Value::BinExpr(ref op, ref lhs, ref rhs) => format!(
                "{} {} {}",
                RustFnGen::tmp_id(*lhs.data()),
                RustFnGen::bin_op(op),
                RustFnGen::tmp_id(*rhs.data()),
            ),

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
                        let arg = RustFnGen::tmp_id(*a.data());
                        arg_string.push_str(&format!("{}, ", RustFnGen::new_value(arg)));
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
                            RustFnGen::new_value(RustFnGen::tmp_id(*typed_tmp.data())),
                        ));
                    },

                    None => (),
                }

                format!("{} {{ {} }}", RustFnGen::type_id(struct_id), field_init)
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
        let expr = RustFnGen::new_value(RustFnGen::tmp_id(expr));

        let var_type = RustFnGen::rustify_type(var_type);

        self.emit_line(&format!("let mut {}: {} = {};", name, var_type, expr));
        self.line_pad();

        Ok(())
    }

    fn assignment(&mut self, _id: NodeIndex, assignment: &Assignment) -> Result<(), ()> {
        
        let var_id = RustFnGen::var_id(assignment.var_id().unwrap());
        self.emit_line("{");
        self.shift_right();

        let expr = self.emit_expr(assignment.value());
        self.emit_line(&format!("let mut _borrow_{} = {};", var_id,
                               RustFnGen::borrow_ref_mut(var_id.clone())));
        
        let mut previous = var_id;
        let mut path = assignment.name().iter();
        path.next(); // Remove root ident

        for field in path {
            let borrow = RustFnGen::borrow_ref_mut(format!("_borrow_{}.{}", previous, field));
            self.emit_line(&format!("let mut _borrow_{} = {};", field, borrow));
            previous = field.to_string();
        }
        let assignee = format!("*_borrow_{}", previous);
        let rhs = {
            let tmp = RustFnGen::tmp_id(expr);
            tmp
        };
        let result = format!("{} = {};", assignee, rhs);
        self.emit_line(&result);
        self.shift_left();
        self.emit_line("}");
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

impl RustGenFmt for RustModGen {
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

    fn mod_id(id: ModuleId) -> String {
        format!("_mod{}", id.raw())
    }

    fn new_value(str: String) -> String {
        format!("Rc::new(RefCell::new({}))", str)
    }

    fn clone_value(str: String) -> String {
        format!("({}).clone()", str)
    }

    fn borrow_ref_mut(str: String) -> String {
        format!("({}).borrow_mut()", str)
    }

    fn borrow_ref(str: String) -> String {
        format!("({}).borrow()", str)
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
