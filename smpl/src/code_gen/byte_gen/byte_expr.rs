use std::collections::HashMap;

use crate::analysis::*;
use super::byte_code::*;

pub fn translate_expr(expr: &Expr, typing_context: &TypingContext) -> Vec<Instruction> {
    let execution_order = expr.execution_order();

    let mut translated = Vec::new();
    for tmp in execution_order {
        translated.extend(translate_tmp(expr.get_tmp(tmp), typing_context));
    }

    translated
}

fn translate_tmp(tmp: &Tmp, typing_context: &TypingContext) -> Vec<Instruction> {
    use super::byte_code::Instruction::*;

    let id = tmp.id();
    let value = tmp.value();

    let store = tmp_id(id);

    let single = match *value.data() {
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
                BindingId::Var(_) => var.ident().data().as_str().to_owned(),
            };

            Store(Location::Tmp(store), Arg::Location(Location::Namespace(value)))
        },

        Value::FieldAccess(ref access) => {

            let mut instruction_buffer: Vec<Instruction> = Vec::new();

            let internal_path = access.path();
            let root_var = 
                internal_path.root_name().data().as_str().to_owned();
            let root_indexing_expr = internal_path.root_indexing_expr();

            if let Some(root_indexing_expr) = root_indexing_expr {
                instruction_buffer.extend(translate_expr(root_indexing_expr, typing_context));
            }

            let path: Vec<_> = internal_path
                .path()
                .iter()
                .map(|path_segment| {
                    match path_segment {
                        PathSegment::Ident(ref field) => {
                            super::byte_code::FieldAccess::Field(field.name().to_string())
                        }

                        PathSegment::Indexing(ref field, ref index_expr) => {
                            instruction_buffer.extend(translate_expr(index_expr, typing_context));
                            super::byte_code::FieldAccess::FieldIndex {
                                field: field.name().to_string(),
                                index_tmp: tmp_id(index_expr.last()),
                            }
                        }
                    }
                }).collect();

            // If assignment has field accesses or indexing, location is Location::Compound
            let field_access_location = if path.len() > 0 || root_indexing_expr.is_some() {
                Location::Compound {
                    root: root_var,
                    root_index: root_indexing_expr.map(|expr| tmp_id(expr.last())),
                    path: path,
                }
            } else {
                Location::Namespace(root_var)
            };

            let access_instr = 
                Store(Location::Tmp(store), Arg::Location(field_access_location));

            instruction_buffer.push(access_instr);
            return instruction_buffer;
        }

        Value::BinExpr(ref op, ref lhs, ref rhs) => {
            use crate::analysis::type_cons::AbstractType;

            macro_rules! specific_math_op {
                ($ty: expr, $store: expr, $lhs: expr, $rhs: expr, $integer: path, $float: path) => {
                    match $ty {
                        AbstractType::Int => $integer($store, $lhs, $rhs),
                        AbstractType::Float => $float($store, $lhs, $rhs),

                        _ => unreachable!(),
                    }
                }
            }

            let ty = typing_context
                .tmp_type(*lhs.data());
            let lhs = Arg::Location(Location::Tmp(tmp_id(*lhs.data())));
            let rhs = Arg::Location(Location::Tmp(tmp_id(*rhs.data())));
            match op {
                BinOp::Add => specific_math_op!(ty, Location::Tmp(store), lhs, rhs, AddI, AddF), 
                BinOp::Sub => specific_math_op!(ty, Location::Tmp(store), lhs, rhs, SubI, SubF), 
                BinOp::Mul => specific_math_op!(ty, Location::Tmp(store), lhs, rhs, MulI, MulF), 
                BinOp::Div => specific_math_op!(ty, Location::Tmp(store), lhs, rhs, DivI, DivF), 
                BinOp::Mod => specific_math_op!(ty, Location::Tmp(store), lhs, rhs, ModI, ModF), 

                BinOp::LogicalAnd => And(Location::Tmp(store), lhs, rhs),
                BinOp::LogicalOr => Or(Location::Tmp(store), lhs, rhs),

                BinOp::GreaterEq => specific_math_op!(ty, Location::Tmp(store), lhs, rhs, GEqI, GEqF),
                BinOp::LesserEq => specific_math_op!(ty, Location::Tmp(store), lhs, rhs, LEqI, LEqF),
                BinOp::Greater => specific_math_op!(ty, Location::Tmp(store), lhs, rhs, GEI, GEF),
                BinOp::Lesser => specific_math_op!(ty, Location::Tmp(store), lhs, rhs, LEI, LEF),

                BinOp::Eq => Eq(Location::Tmp(store), lhs, rhs),
                BinOp::InEq => InEq(Location::Tmp(store), lhs, rhs),
            }
        }

        Value::UniExpr(ref op, ref tmp) => {

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

            let fn_call = FnCall(to_call, args);
            let result_store = TakeReturn(Location::Tmp(store));

            return vec![fn_call, result_store];
        }

        Value::StructInit(ref struct_init) => {
            let field_init = struct_init.raw_field_init();

            let mut map = HashMap::new();
            for (field, typed_tmp_id) in field_init.into_iter() {
                let tmp = typed_tmp_id.data().clone();
                map.insert(field.to_string(), Arg::Location(Location::Tmp(tmp_id(tmp))));
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
            let field_init = struct_init.raw_field_init();

            let mut map = HashMap::new();
            for (field, tmp) in field_init.into_iter() {
                map.insert(field.to_string(), Arg::Location(Location::Tmp(tmp_id(*tmp))));
            }

            StoreStructure(Location::Tmp(store), map)
        }

        Value::TypeInst(ref type_inst) => {
            // NOTE(alex): only allowed to do type instantiations on functions right now
            let func = fn_id(type_inst.get_id().unwrap());
            let location = Location::Namespace(func);

            Store(Location::Tmp(store), Arg::Location(location))
        }
    };

    vec![single]
}

pub fn tmp_id(id: TmpId) -> String {
    format!("_tmp{}", id.raw())
}

pub fn fn_id(id: FnId) -> String {
    format!("_fn{}", id.raw())
}
