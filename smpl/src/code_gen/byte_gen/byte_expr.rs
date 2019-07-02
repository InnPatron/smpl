use crate::analysis::*;
use super::byte_code::*;

pub fn translate_expr(expr: &Expr) -> Vec<Instruction> {
    let execution_order = expr.execution_order();

    let mut translated = Vec::new();
    for tmp in execution_order {
        translated.push(translate_tmp(expr.get_tmp(*tmp)));
    }

    translated
}

fn translate_tmp(tmp: &Tmp) -> Instruction {
    use super::byte_code::Instruction::*;
    use super::byte_code::Arg;

    let id = tmp.id();
    let value = tmp.value();

    let store = tmp_id(id);

    match *value.data() {
        Value::Literal(ref lit) => match *lit {
            Literal::String(ref string) => Store(Location(store), Arg::String(string.to_string())),

            Literal::Int(int) => Store(Location(store), Arg::Int(int)),

            Literal::Float(float) => Store(Location(store), Arg::Float(float)),

            Literal::Bool(boolean) => Store(Location(store), Arg::Bool(boolean)),
        },

        Value::Binding(ref var) => {
            let value = match var.get_id()
                .expect("If the program passed semantic analysis, all IDs should be filled in.")
            {
                BindingId::Fn(id) => fn_id(id),
                BindingId::Var(id) => var_id(id),
            };

            Store(Location(store), Arg::Location(Location(value)))
        },

        Value::FieldAccess(ref access) => {
            unimplemented!()
        }

        Value::BinExpr(ref op, ref lhs, ref rhs) => {
            use crate::ast::BinOp;

            let lhs = Arg::Location(Location(tmp_id(*lhs.data())));
            let rhs = Arg::Location(Location(tmp_id(*rhs.data())));
            match op {
                BinOp::Add => Add(Location(store), lhs, rhs),
                BinOp::Sub => Sub(Location(store), lhs, rhs),
                BinOp::Mul => Mul(Location(store), lhs, rhs),
                BinOp::Div => Div(Location(store), lhs, rhs),
                BinOp::Mod => Mod(Location(store), lhs, rhs),

                BinOp::LogicalAnd => And(Location(store), lhs, rhs),
                BinOp::LogicalOr => Or(Location(store), lhs, rhs),
                BinOp::GreaterEq => GEq(Location(store), lhs, rhs),
                BinOp::LesserEq => LEq(Location(store), lhs, rhs),
                BinOp::Greater => GE(Location(store), lhs, rhs),
                BinOp::Lesser => LE(Location(store), lhs, rhs),

                BinOp::Eq => Eq(Location(store), lhs, rhs),
                BinOp::InEq => InEq(Location(store), lhs, rhs),
            }
        }

        Value::UniExpr(ref op, ref tmp) => {
            use crate::ast::UniOp;

            let tmp = Arg::Location(Location(tmp_id(*tmp.data())));
            match op {
                UniOp::Negate => Negate(Location(store), tmp),
                UniOp::LogicalInvert => Invert(Location(store), tmp),

                _ => unimplemented!(),
            }
        }

        Value::FnCall(ref fn_call) => {
            
            let args = match fn_call.args() {
                Some(args) => args.iter().map(|tmp| {
                    Arg::Location(Location(tmp_id(*tmp.data())))
                }).collect(),
                None => Vec::new(),
            };

            let to_call = Location(tmp_id(fn_call.fn_value()));

            FnCall(Location(store), to_call, args)
        }

        Value::StructInit(ref struct_init) => unimplemented!(),

        Value::ArrayInit(ref a) => unimplemented!(),

        Value::Indexing(ref indexing) => unimplemented!(),

        Value::ModAccess(_) => unimplemented!(),

        Value::AnonymousFn(_) => unimplemented!(),

        Value::AnonStructInit(_) => unimplemented!(),

        Value::TypeInst(_) => unimplemented!(),
    }
}

fn field_id(id: FieldId) -> String {
    format!("_field{}", id.raw())
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
