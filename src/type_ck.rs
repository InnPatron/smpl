use std::collections::HashMap;
use smpl_type::*;
use ast::*;

pub enum Err {

}

pub struct MissingType(Path);

pub type Result = ::std::result::Result<(), Err>;

pub struct TypeMap {
    pub map: HashMap<Ident, SmplType>,
}

impl TypeMap {
    fn std() -> TypeMap {
        let mut map = HashMap::new();
        map.insert(Ident::new("int"), SmplType::Int);
        map.insert(Ident::new("float"), SmplType::Float);
        map.insert(Ident::new("String"), SmplType::String);
        map.insert(Ident::new("bool"), SmplType::Bool);
        map.insert(Ident::new("unit"), SmplType::Unit);
        TypeMap {
            map: map
        }
    }
}

pub fn type_check(program: &mut Program) -> Result {
    let mut type_map = TypeMap::std();

    // Walk the tree and assign types
    for (index, decl_stmt) in program.0.iter_mut().enumerate() {
        match *decl_stmt {
            DeclStmt::Struct(ref struct_def) => {
                let def = gen_struct_type(struct_def, &type_map)?;
                match type_map.map.insert(def.name.clone(), SmplType::Struct(def)) {
                    Some(_) => unimplemented!("How to handle type definiion overrides?"),
                    None => (),
                }
            },
            
            DeclStmt::Function(ref fn_def) => {
                let fn_type = gen_fn_type(fn_def, &type_map)?;
            }
        }
    }
    unimplemented!();
}

fn gen_fn_type(fn_def: &Function, type_map: &TypeMap) -> ::std::result::Result<FunctionType, Err> {

    let return_type = {
        match fn_def.return_type {
            Some(ref path) => {
                // TODO: ignore full paths until modules are maybe added
                let type_name = path.0.last().unwrap();
                match type_map.map.get(type_name) {
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
                    let type_name = arg.arg_type.0.last().unwrap();
                    match type_map.map.get(type_name) {
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

fn gen_struct_type(struct_def: &Struct, type_map: &TypeMap) -> ::std::result::Result<StructType, Err> {
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
        // TODO: ignore full paths until modules are maybe added
        let type_name = field.field_type.0.last().unwrap();
        let field_name = &field.name;
        
        match type_map.map.get(type_name) {
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
