use std::collections::HashMap;
use std::cell::Cell;
use std::rc::Rc;

use err::Err;
use ast::{Path, DeclStmt, Struct, Function as AstFunction, Program as AstProgram};

use super::smpl_type::*;
use super::semantic_data::*;
use super::control_flow::CFG;
use super::fn_analyzer::analyze_fn;

pub fn check(program: AstProgram) -> Result<Program, Err> {
    let mut universe = Universe::std();
    let mut global_scope = universe.std_scope().clone();

    let mut main = None;

    for decl_stmt in program.0.into_iter() {
        match decl_stmt {
            DeclStmt::Struct(struct_def) => {
                let struct_t = generate_struct_type(&global_scope, struct_def)?;
                let id = universe.new_type_id();
                
                global_scope.insert_type(struct_t.name.clone().into(), id);
                universe.insert_type(id, SmplType::Struct(struct_t));
            },

            DeclStmt::Function(fn_def) => {
                let name: Path = fn_def.name.clone().into();

                let type_id = universe.new_type_id();

                let fn_type = generate_fn_type(&global_scope, &universe, &fn_def)?;
                let cfg = CFG::generate(&universe, fn_def, &fn_type)?;

                let fn_id = universe.new_fn_id();
                universe.insert_fn(fn_id, type_id, fn_type, cfg);
                global_scope.insert_fn(name.clone(), fn_id);

                let func = universe.get_fn(fn_id);
                analyze_fn(&universe, &global_scope, func.cfg(), fn_id)?;

                if name == path!("main") {
                    if main.is_some() {
                        return Err(Err::MultipleMainFns);
                    } else {
                        main = Some(fn_id);
                    }
                }
            },
        }
    }
    
    Ok(Program::new(universe, global_scope, main))
}

fn generate_fn_type(scope: &ScopedData, universe: &Universe, fn_def: &AstFunction) -> Result<FunctionType, Err> {
    let ret_type = match fn_def.return_type {
        Some(ref path) => scope.type_id(path)?,
        None => universe.unit(),
    };

    let params: Vec<_> = match fn_def.params {
        Some(ref params) => params.iter()
                              .map(|ref fn_param| {
                                  scope.type_id(&fn_param.param_type)
                                       .map(|id| FnParameter::new(fn_param.name.clone(), id))
                              })
                              .collect::<Result<Vec<_>, Err>>()?,

        None => Vec::new(),
    };

    Ok(FunctionType {
        params: params,
        return_type: ret_type,
    })
}

fn generate_struct_type(scope: &ScopedData, struct_def: Struct) -> Result<StructType, Err> {
    let struct_name = struct_def.name;
    let mut fields = HashMap::new();
    if let Some(body) = struct_def.body.0 {
        for field in body.into_iter() {
            let f_name = field.name;
            let f_type_path = field.field_type;
            let field_type = scope.type_id(&f_type_path)?;
            fields.insert(f_name, field_type);
        }
    } 

    let struct_t = StructType {
        name: struct_name,
        fields: fields,
    };

    Ok(struct_t)
}

#[cfg(test)]
mod tests {
    use err::*;
    use super::*;
    use parser::*;

    #[test]
    fn basic_test_semantic_analysis() {
        let program =
"struct Test {
    field_1: i32,
    field_2: f32,
    field_3: String,
    field_4: bool
}

fn main() {
    let truthy: bool = true;
    if true {
        truthy = false;
    } else {
        truthy = true;
    }
}
";
        let program = parse_Program(program).unwrap();
        let program = check(program).unwrap();

        let universe = program.universe();

        let main = universe.get_fn(program.main().unwrap());
        let main_type = universe.get_type(main.type_id());
        if let SmplType::Function(ref fn_type) = *main_type {
            assert_eq!(SmplType::Unit, *universe.get_type(fn_type.return_type));
        } else {
            panic!("main()'s TypeId was not mapped to a SmplType::Function");
        }

    }

    #[test]
    fn call_fn_success() {
        use super::super::control_flow::*;
        use super::super::typed_ast::*;

        let input = 
"fn arg_usage(a1: i32, a2: bool) {
	let b1: i32 = a1;
	let b2: bool = a2;
}

fn main() {
	arg_usage(5, false);
}";
        
        let ast = parse_Program(input).unwrap();
        let program = check(ast).unwrap();

        let main = program.main().unwrap();

        let mut called_fn = None;
        for (id, _) in program.universe().all_fns() {
            if id != main {
                called_fn = Some(id);
                break;
            }
        }

        let main = program.universe().get_fn(main);
        let called_fn = called_fn.unwrap();
        
        let fn_call = {
            let scope_enter = main.cfg().after_start();
            main.cfg().next(scope_enter)
        };
        match *main.cfg().node_weight(fn_call) {
            Node::Expr(ref e) => {
                let mut iter = e.execution_order();
                let tmp = e.get_tmp(*iter.last().unwrap());
                match *tmp.value().data() {
                    Value::FnCall(ref call) => {
                        assert_eq!(call.get_id().unwrap(), called_fn);
                    },

                    ref v => panic!("Expected Value::FnCall. Found {:?}", v),
                }
            }
            
            ref n @ _ => panic!("Expected Node::Expr. Found {:?}", n),
        }
    }

    #[test]
    fn embedded_ifs_analysis() {
        let input =
"fn test() {
    if true {
        if false {

        } else {
            let a: i32 = 100;
        }

        let b: i32 = a;
    }
}";

        let ast = parse_Program(input).unwrap();
        match check(ast) {
            Ok(_) => panic!("Passed analysis. Expected Err::UnknownVar"),
            Err(e) => {
                match e {
                    Err::UnknownVar(ident) => {
                        assert_eq!(ident, ident!("a"));
                    }

                    e @ _ => panic!("Expected Err::UnknownVar. Found {:?}", e),
                }
            }
        }
    }

    #[test]
    fn missing_return() {
        let input_0 =
"fn test() -> i32 {
    
}";

        let input_1 = 
"fn test() -> i32 {
    let a: i32 = 5;
}";

        let input_2 = 
"fn test() -> i32 {
    if true {
        return 0;
    }
}";

        let input_3 =
"fn test() -> i32 {
    if true {


    } else {
        return 0;
    }
}";

        let input_4 =
"fn test() -> i32 {
    if true {
        return 0;
    } else {
    
    }
}";

        let input_5 =
"fn test() -> i32 {
    if true {
        if true {

        } else {
            return 0;
        }
    } else {
        return 0;
    }
}";

        let input_6 =
"fn test() -> i32 {
    if true {
        return 0;
    } else {
        if true {
            return 0;
        } else {
            
        }
    }
}";

        let input = vec![input_0, input_1, input_2, input_3, input_4, input_5, input_6];

        for i in 0..input.len() {
            let ast = parse_Program(input[i]).unwrap();
            match check(ast) {
                Ok(_) => panic!("Passed analysis. Expected Err::ControlFlowErr(ControlFlowErr::MissingReturn. Test {}", i),
                Err(e) => {
                    match e {
                        Err::ControlFlowErr(e) => {
                            match e {
                                ControlFlowErr::MissingReturn => (),

                                e @ _ => panic!("Expected ControlFlowErr::MissingReturn. Test {}. Found {:?}", i, e),
                            }
                        }

                        e @ _ => panic!("Expected Err::ControlFlowErr. Test {}. Found {:?}", i, e),
                    }
                }
            }
        }
    }

    #[test]
    fn all_required_returns() {
        let input_0 =
"fn test() -> i32 {
    return 0;
}";

        let input_1 = 
"fn test() -> i32 {
    let a: i32 = 5;

    return 0;
}";

        let input_2 = 
"fn test() -> i32 {
    if true {
        return 0;
    }

    return 0;
}";

        let input_3 =
"fn test() -> i32 {
    if true {
        return 0;
    } else {
        return 0;
    }
}";

        let input_4 =
"fn test() -> i32 {
    if true {
        return 0;
    } else {
        return 0;
    }
}";

        let input_5 =
"fn test() -> i32 {
    if true {
        if true {
            return 0;
        } else {
            return 0;
        }
    } else {
        return 0;
    }
}";

        let input_6 =
"fn test() -> i32 {
    if true {
        return 0;
    } else {
        if true {
            return 0;
        } else {
            return 0;
        }
    }
}";

        let input = vec![input_0, input_1, input_2, input_3, input_4, input_5, input_6];

        for i in 0..input.len() {
            let ast = parse_Program(input[i]).unwrap();
            check(ast).expect(&format!("Test  {} failed.", i));
        }
    }
}
