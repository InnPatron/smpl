use std::collections::HashMap;

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
            Literal::String(ref string) => Store(Location::Tmp(store), Arg::String(string.to_string())),

            Literal::Int(int) => Store(Location::Tmp(store), Arg::Int(int)),

            Literal::Float(float) => Store(Location::Tmp(store), Arg::Float(float)),

            Literal::Bool(boolean) => Store(Location::Tmp(store), Arg::Bool(boolean)),
        },

        Value::Binding(ref var) => {
            let value = match var.get_id()
                .expect("If the program passed semantic analysis, all IDs should be filled in.")
            {
                BindingId::Fn(id) => fn_id(id),
                BindingId::Var(id) => var_id(id),
            };

            Store(Location::Tmp(store), Arg::Location(Location::Namespace(value)))
        },

        Value::FieldAccess(ref access) => {
            let internal_path = access.path();
            let root_var = 
                var_id(internal_path.root_var_id());
            let root_indexing_expr = internal_path.root_indexing_expr()
                .map(|tmp| tmp_id(tmp));
            let path: Vec<_> = internal_path
                .path()
                .iter()
                .map(|path_segment| {
                    match path_segment {
                        PathSegment::Ident(ref field) => {
                            super::byte_code::FieldAccess::Field(field.name().to_string())
                        }

                        PathSegment::Indexing(ref field, ref index_tmp) => {
                            super::byte_code::FieldAccess::FieldIndex {
                                field: field.name().to_string(),
                                index_tmp: tmp_id(*index_tmp),
                            }
                        }
                    }
                }).collect();

            // If assignment has field accesses or indexing, location is Location::Compound
            let field_access_location = if path.len() > 0 || root_indexing_expr.is_some() {
                Location::Compound {
                    root: root_var,
                    root_index: root_indexing_expr,
                    path: path,
                }
            } else {
                Location::Namespace(root_var)
            };

            Store(Location::Tmp(store), Arg::Location(field_access_location))
        }

        Value::BinExpr(ref op, ref lhs, ref rhs) => {
            use crate::ast::BinOp;

            let lhs = Arg::Location(Location::Tmp(tmp_id(*lhs.data())));
            let rhs = Arg::Location(Location::Tmp(tmp_id(*rhs.data())));
            match op {
                BinOp::Add => Add(Location::Tmp(store), lhs, rhs),
                BinOp::Sub => Sub(Location::Tmp(store), lhs, rhs),
                BinOp::Mul => Mul(Location::Tmp(store), lhs, rhs),
                BinOp::Div => Div(Location::Tmp(store), lhs, rhs),
                BinOp::Mod => Mod(Location::Tmp(store), lhs, rhs),

                BinOp::LogicalAnd => And(Location::Tmp(store), lhs, rhs),
                BinOp::LogicalOr => Or(Location::Tmp(store), lhs, rhs),
                BinOp::GreaterEq => GEq(Location::Tmp(store), lhs, rhs),
                BinOp::LesserEq => LEq(Location::Tmp(store), lhs, rhs),
                BinOp::Greater => GE(Location::Tmp(store), lhs, rhs),
                BinOp::Lesser => LE(Location::Tmp(store), lhs, rhs),

                BinOp::Eq => Eq(Location::Tmp(store), lhs, rhs),
                BinOp::InEq => InEq(Location::Tmp(store), lhs, rhs),
            }
        }

        Value::UniExpr(ref op, ref tmp) => {
            use crate::ast::UniOp;

            let tmp = Arg::Location(Location::Tmp(tmp_id(*tmp.data())));
            match op {
                UniOp::Negate => Negate(Location::Tmp(store), tmp),
                UniOp::LogicalInvert => Invert(Location::Tmp(store), tmp),

                _ => unimplemented!(),
            }
        }

        Value::FnCall(ref fn_call) => {
            
            let args = match fn_call.args() {
                Some(args) => args.iter().map(|tmp| {
                    Arg::Location(Location::Tmp(tmp_id(*tmp.data())))
                }).collect(),
                None => Vec::new(),
            };

            let to_call = Location::Tmp(tmp_id(fn_call.fn_value()));

            FnCall(Location::Tmp(store), to_call, args)
        }

        Value::StructInit(ref struct_init) => {
            let field_init = struct_init.field_init().unwrap();

            let mut map = HashMap::new();
            for (field, typed_tmp_id) in field_init.into_iter() {
                let tmp = typed_tmp_id.data().clone();
                map.insert(field_id(field), Arg::Location(Location::Tmp(tmp_id(tmp))));
            }

            StoreStructure(Location::Tmp(store), map)
        },

        Value::ArrayInit(ref array_init) => {
            match array_init {
                ArrayInit::List(ref typed_tmp_vec) => {
                   let list = typed_tmp_vec
                       .iter() 
                       .map(|typed_tmp| {
                           let tmp_id = tmp_id(typed_tmp.data().clone());
                           Arg::Location(Location::Tmp(tmp_id))
                       })
                       .collect();
                   StoreArray1(Location::Tmp(store), list)
                }

                ArrayInit::Value(ref typed_tmp, ref number) => {
                    let init_value = tmp_id(typed_tmp.data().clone());

                    StoreArray2(Location::Tmp(store), 
                                Arg::Location(Location::Tmp(init_value)), 
                                *number)
                }
            }
        }

        Value::Indexing(ref indexing) => {
            let array_tmp = tmp_id(indexing.array.data().clone());
            let indexer_tmp = tmp_id(indexing.indexer.data().clone());

            let access_location = Location::Compound {
                root: array_tmp,
                root_index: Some(indexer_tmp),
                path: Vec::with_capacity(0),
            };

            Store(Location::Tmp(store), Arg::Location(access_location))
        }

        Value::ModAccess(ref mod_access) => {
            let func = fn_id(mod_access.fn_id().unwrap());
            let location = Location::Namespace(func);

            Store(Location::Tmp(store), Arg::Location(location))
        }

        Value::AnonymousFn(ref anon_fn) => {
            // Anonymous functions should already be emitted
            let func = fn_id(anon_fn.fn_id());
            let location = Location::Namespace(func);

            Store(Location::Tmp(store), Arg::Location(location))
        }

        Value::AnonStructInit(ref struct_init) => {
            let field_init = struct_init.field_init().unwrap();

            let mut map = HashMap::new();
            for (field, typed_tmp_id) in field_init.into_iter() {
                let tmp = typed_tmp_id.data().clone();
                map.insert(field_id(field), Arg::Location(Location::Tmp(tmp_id(tmp))));
            }

            StoreStructure(Location::Tmp(store), map)
        }

        Value::TypeInst(_) => unimplemented!(),
    }
}

pub fn field_id(id: FieldId) -> String {
    format!("_field{}", id.raw())
}

pub fn tmp_id(id: TmpId) -> String {
    format!("_tmp{}", id.raw())
}

pub fn var_id(id: VarId) -> String {
    format!("_var{}", id.raw())
}

pub fn fn_id(id: FnId) -> String {
    format!("_fn{}", id.raw())
}
