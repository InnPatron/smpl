#[macro_use]
use std::collections::HashMap;
use std::str::FromStr;
use std::borrow::BorrowMut;
use std::fmt::Debug;

use ascii::*;

use smpl_type::*;
use ast::*;

pub enum Err {

}

pub struct SemanticChecker {
    pub type_map: HashMap<Path, SmplType>,
    pub binding_map: HashMap<Ident, SmplType>,
}

impl SemanticChecker {
    fn new() -> SemanticChecker {
        let mut type_map = HashMap::new();
        type_map.insert(path!("int"), SmplType::Int);
        type_map.insert(path!("float"), SmplType::Float);
        type_map.insert(path!("String"), SmplType::String);
        type_map.insert(path!("bool"), SmplType::Bool);
        type_map.insert(path!("unit"), SmplType::Unit);

        let mut binding_map = HashMap::new();

        SemanticChecker {
            type_map: type_map,
            binding_map: binding_map,
        }
    }

    fn map_type<T: Into<Path>>(&mut self, name: T, def: SmplType) -> Option<SmplType> {
        self.type_map.insert(name.into(), def)
    }

    fn bind(&mut self, name: Ident, binding_type: SmplType) -> Option<SmplType> {
        self.binding_map.insert(name, binding_type)
    }

    pub fn accept_struct_def(&mut self, struct_def: &Struct) -> Result<(), Err> {
        let def = self.gen_struct_type(struct_def)?;
        match self.map_type(struct_def.name.clone(), SmplType::Struct(def)) {
            Some(_) => unimplemented!("TODO: Handle type override"),
            None => Ok(())
        }
    }

    fn gen_struct_type(&self, struct_def: &Struct) -> ::std::result::Result<StructType, Err> {
        let name = struct_def.name.clone();
        let body = &struct_def.body;

        let mut struct_fields = HashMap::new();

        let body = match body.0 {
            Some(ref b) => b,
            None => {
                return Ok(StructType {
                    name: name,
                    fields: struct_fields,
                });
            }
        };

        for field in body.iter() {
            // TODO: assumes 1 to 1 matching until modules are maybe added
            let type_name = &field.field_type;
            let field_name = &field.name;
            
            match self.type_map.get(type_name) {
                Some(t) => { 
                    let previous = struct_fields.insert(field_name.clone(), t.clone());
                    if previous.is_some() {
                        unimplemented!("Found field with duplicate names");
                    }
                },
                None => unimplemented!("Could not find field type for {}.{}", type_name, field_name),
            }
        }

        Ok(StructType {
                    name: name,
                    fields: struct_fields,
        })
    }

    pub fn accept_fn_def(&mut self, fn_def: &Function) -> Result<(), Err> {
        let fn_type = self.gen_fn_type(fn_def)?;
        match self.bind(fn_def.name.clone(), SmplType::Function(fn_type)) {
            Some(_) => unimplemented!("TODO: Handle binding override"),
            None => Ok(())
        }
    }

    fn gen_fn_type(&self, fn_def: &Function) -> Result<FunctionType, Err> {

        let return_type = {
            match fn_def.return_type {
                Some(ref path) => {
                    // TODO: assumes 1 to 1 matching until modules are maybe added
                    let type_name = path;
                    match self.type_map.get(type_name) {
                        Some(t) => t.clone(),
                        None => unimplemented!("could not find field type for {}.{}", type_name, fn_def.name),
                    }
                }

                None => SmplType::Unit,
            }
        };

        let arg_types = {
            match fn_def.args {
                Some(ref arg_list) => {
                    let mut arg_types = Vec::new();
                    for arg in arg_list.iter() {
                        let type_name = &arg.arg_type;
                        match self.type_map.get(type_name) {
                            Some(t) => arg_types.push(t.clone()),
                            None => unimplemented!("could not find field type for {}.{}", type_name, fn_def.name),
                        }
                    }

                    arg_types
                },

                None => Vec::new(),
            }
        };

        Ok(FunctionType {
            args: arg_types,
            return_type: Box::new(return_type),
        })      
    }

    fn typify_expr<T: BorrowMut<Expr> + PartialEq + Clone + Debug>(&self, expr: &mut AstNode<T>) -> Result<(), Err> {
        use ast::Expr::*;
        match *expr.data.borrow_mut() {
            Literal(ref mut node_l) => self.typify_literal(node_l)?,

            Ident(ref mut node_id) => {
                match self.binding_map.get(&node_id.data) {
                    Some(smpl_type) => node_id.d_type = Some(smpl_type.clone()),
                    None => unimplemented!("Binding does not exist"),
                }
            },

            Bin(ref mut node_bin) => {
                self.typify_expr(&mut node_bin.data.lhs)?;
                self.typify_expr(&mut node_bin.data.rhs)?;
                if node_bin.data.lhs.d_type == node_bin.data.rhs.d_type {
                    node_bin.d_type = node_bin.data.lhs.d_type.clone();
                } else {
                    unimplemented!("lhs and rhs must be the same type");
                }
            },

            FnCall(ref mut fn_call) => {
                let smpl_type = self.binding_map.get(&fn_call.data.name)
                                                .ok_or(unimplemented!("Did not find fn in binding map"))?;
                if let &SmplType::Function(ref fn_type) = smpl_type {

                    // Verify arg types matches types in fn call
                    match fn_call.data.args {
                        Some(ref mut call_args) => {
                            if call_args.len() != fn_type.args.len() {
                                unimplemented!("Arg lengths do not match.");
                            }

                            for (ref mut call_arg, fn_type_arg) in call_args.iter_mut()
                                                                            .zip(fn_type.args.iter()) {
                                self.typify_expr(call_arg)?;
                                if call_arg.d_type.as_ref() != Some(fn_type_arg) {
                                    unimplemented!("Arg types do not match");
                                }
                            }
                        },

                        None => {
                            if fn_type.args.len() != 0 {
                                unimplemented!("Arg lengths do not match.");
                            }
                        }
                    }

                    fn_call.d_type = Some(*fn_type.return_type.clone());
                } else {
                    unimplemented!("Binding does not map to a function type");
                }
            },

            Uni(ref mut uni_expr) => {
                self.typify_expr(&mut uni_expr.data.expr)?;
                unimplemented!("Handle uni op potential type transformation");
            },
        }

        Ok(())
    }

    fn typify_literal(&self, literal: &mut AstNode<Literal>) -> Result<(), Err> {
        use ast::Literal::*;
        if literal.d_type.is_some() {
            return Ok(());
        }

        match literal.data {
            String(_) => literal.d_type = Some(SmplType::String),
            Bool(_) => literal.d_type = Some(SmplType::Bool),
            Number(ref num) => {
                let int_result = num.parse::<i64>();
                let float_result = num.parse::<f64>();

                match (int_result.is_ok(), float_result.is_ok()) {
                    (true, true) => literal.d_type = Some(SmplType::Int),
                    (true, false) => literal.d_type = Some(SmplType::Int),
                    (false, true) => literal.d_type = Some(SmplType::Float),
                    (false, false) => unimplemented!("Should not have parsed"),
                }
            },
        }

        Ok(())
    }
}
