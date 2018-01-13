use std::cell::RefCell;
use semantic_ck::*;
use typed_ast::*;
use control_flow::*;
use smpl_type::*;
use ast::{Ident, Path};

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
    pub fn generate(&self, program: &Program) -> String {
        let mut result = String::new();

        unimplemented!(); 
    }

    fn prelude(&self, universe: &Universe, struct_type: &StructType) {
        let mut output = self.output.borrow_mut();
        output.push_str("use std::cell::RefCell;\n");
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
