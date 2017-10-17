#[macro_use]
use std::collections::HashMap;
use std::str::FromStr;

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

    pub fn gen_struct_type(&mut self, struct_def: &Struct) -> ::std::result::Result<(), Err> {
        let name = struct_def.name.clone();
        let body = &struct_def.body;

        let mut struct_fields = HashMap::new();

        let body = match body.0 {
            Some(ref b) => b,
            None => {
                // insert struct type into type map
                let struct_type = StructType {
                    name: name,
                    fields: struct_fields,
                };

                return self.insert_type(struct_type.name.clone(),
                                        SmplType::Struct(struct_type));
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

        // insert struct type into type map
        let struct_type = StructType {
            name: name,
            fields: struct_fields,
        };

        self.insert_type(struct_type.name.clone(), 
                         SmplType::Struct(struct_type))
    }

    pub fn gen_fn_type(&mut self, fn_def: &Function) -> ::std::result::Result<(), Err> {

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

        let fn_type = FunctionType {
            args: arg_types,
            return_type: Box::new(return_type),
        };
 
        match self.binding_map.insert(fn_def.name.clone(), SmplType::Function(fn_type)) {
            Some(_) => unimplemented!("TODO: Handle binding overrides"),
            None => Ok(()),
        }
    }

    fn insert_type<T: Into<Path>>(&mut self, name: T, smpl_type: SmplType) -> Result<(), Err> {
        match self.type_map.insert(name.into(), smpl_type) {
            Some(_) => unimplemented!("TODO: Handle type overrides"),
            None => Ok(())
        }
    }
}
