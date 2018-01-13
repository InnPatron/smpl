use std::cell::RefCell;
use semantic_ck::*;
use typed_ast::*;
use control_flow::*;
use smpl_type::*;
use ast::{Ident, Path, BinOp, UniOp};

pub struct RustGen {
    output: RefCell<String>,
}

// Config
impl RustGen {
    pub fn new() -> RustGen {
        RustGen {
            output: RefCell::new(String::new()),
        }
    }
}

// Code generation
impl RustGen {
    pub fn generate(&self, program: &Program) {
        self.prelude();

        for t in program.universe().all_types() {
            if let SmplType::Struct(ref t) = *t {
                self.emit_struct_type(program.universe(), t);
            }
        }
    }

    fn prelude(&self) {
        let mut output = self.output.borrow_mut();
        output.push_str("use std::cell::RefCell;\n");
    }

    fn emit_expr(&self, universe: &Universe, expr: &Expr) -> TmpId {
        let execution_order = expr.execution_order();

        let mut last_tmp = None;
        for tmp in execution_order {
            self.emit_tmp(expr.get_tmp(*tmp));
            last_tmp = Some(tmp);
        }

        *last_tmp.unwrap()
    }

    fn emit_tmp(&self, tmp: &Tmp) {
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

        self.output.borrow_mut().push_str(&format!("let {} = {};",
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

    fn emit_struct_type(&self, universe: &Universe, struct_type: &StructType) {
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

        self.output.borrow_mut().push_str(&output);
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
