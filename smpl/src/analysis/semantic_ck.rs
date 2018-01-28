use std::collections::HashMap;
use std::cell::Cell;
use std::rc::Rc;

use err::Err;
use ast::{Ident, Path, DeclStmt, Struct, Function as AstFunction, Module as AstModule};

use super::smpl_type::*;
use super::semantic_data::*;
use super::semantic_data::Module;
use super::control_flow::CFG;
use super::fn_analyzer::analyze_fn;

pub fn check_program(program: Vec<AstModule>) -> Result<Program, Err> {
    let mut universe = Universe::std();

    let program = program.into_iter()
        .map(|ast_module| ModuleCkData::new(&universe, ast_module))
        .collect::<Result<Vec<_>, _>>()?;

    let mut queue = program;

    loop {
        let start_count = queue.len();
        let mut queue_iter = queue.into_iter();
        queue = Vec::new();

        for mut module in queue_iter.next() {
            match check_module(&mut universe, module)? {
                ModuleCkSignal::Success => (),
                ModuleCkSignal::Defer(data) => queue.push(data),
            }       
        }

        let end_count = queue.len();
        if end_count == 0 {
            // No more use declarations to resolve.
            break;
        } else if end_count == start_count {
            let mut unresolved = Vec::new();
            for mod_uses in queue.into_iter().map(|module| module.module_uses) {
                let mod_uses = mod_uses.into_iter().map(|u| u.0);
                unresolved.extend(mod_uses);
            }

            return Err(Err::UnresolvedUses(unresolved));
        } else if end_count > start_count {
            unreachable!();
        }
    }

    let mut main = None;
    for module in universe.all_modules().into_iter() {
        if let Ok(id) = module.module_scope().get_fn(&path!("main")) {
            if main.is_none() {
                main = Some(id)
            } else {
                return Err(Err::MultipleMainFns);
            }
        }
    }

    Ok(Program::new(universe, main))
}

fn check_module(universe: &mut Universe, mut module: ModuleCkData) -> Result<ModuleCkSignal, Err> {
    let module_name = module.name.clone();

    let mut missing_modules = Vec::new();
    for use_decl in module.module_uses.into_iter() {
        match universe.module_id(&use_decl.0) {
            Some(id) => {
                module.module_scope.map_module(use_decl.0.clone(), id);
                let imported_name = use_decl.0.clone();
                let imported_module = universe.get_module(id);
                let imported_scope = imported_module.module_scope();

                let all_types = imported_scope.all_types()
                                              .into_iter()
                                              .map(|(path, id)| {
                                                  let mut path = path.clone();
                                                  path.0.insert(0, imported_name.clone());
                                                  
                                                  (path, id.clone())
                                              })
                                              .collect::<HashMap<_, _>>();
                let all_fns = imported_scope.all_fns()
                                            .into_iter()
                                            .map(|(path, id)| {
                                                let mut path = path.clone();
                                                path.0.insert(0, imported_name.clone());
                                                  
                                                (path, id.clone())
                                            })
                                            .collect::<HashMap<_,_>>();

                // Bring imported types into scope
                for (path, imported) in all_types.into_iter() {
                    if module.module_scope.insert_type(path.clone(), imported).is_some() {
                        panic!("Should not have overrwritten {}. Paths should be unique by prefixing with the originating module.", path);
                    }
                }

                // Bring imported functions into scope
                for(path, imported) in all_fns.into_iter() {
                    module.module_scope.insert_fn(path, imported);
                }
            }
            None => missing_modules.push(use_decl),
        }
    }

    if missing_modules.len() > 0 {
        module.module_uses = missing_modules;
        // Unknown module. Differ checking till later
        return Ok(ModuleCkSignal::Defer(module));
    }

    let mut unresolved = module.module_structs;
    loop {
        let start_count = unresolved.len();
        let mut struct_iter = unresolved.into_iter();

        unresolved = Vec::new();
        for struct_decl in struct_iter.next() {
            let struct_t = generate_struct_type(&module.module_scope, &struct_decl)?;

            let id = universe.new_type_id();
            module.module_scope.insert_type(struct_t.name.clone().into(), id);
            universe.insert_type(id, SmplType::Struct(struct_t));
        }

        let end_count = unresolved.len();
        if end_count == 0 {
            // No more struct declarations to resolve.
            break;
        } else if end_count == start_count {
            // No struct declarations were resolved. Return error.
            return Err(Err::UnresolvedStructs(unresolved.into_iter().map(|s| s.name).collect()));
        } else if end_count > start_count {
            unreachable!();
        }
    }

    let mut unresolved = module.module_fns;
    loop {
        let start_count = unresolved.len();
        let mut module_fn_iter = unresolved.into_iter();

        unresolved = Vec::new();
        for fn_decl in module_fn_iter {
            let name: Path = fn_decl.name.clone().into();

            let type_id = universe.new_type_id();

            let fn_type = generate_fn_type(&module.module_scope, &universe, &fn_decl)?;

            let cfg = CFG::generate(&universe, fn_decl, &fn_type)?;

            let fn_id = universe.new_fn_id();
            universe.insert_fn(fn_id, type_id, fn_type, cfg);
            module.module_scope.insert_fn(name.clone(), fn_id);

            let func = universe.get_fn(fn_id);
            analyze_fn(&universe, &module.module_scope, func.cfg(), fn_id)?;


            let end_count = unresolved.len();
            if end_count == 0 {
                // No more function declarations to resolve.
                break;
            } else if end_count == start_count {
                // No function declarations were resolved. Return error.
                return Err(Err::UnresolvedFns(unresolved.into_iter().map(|f| f.name).collect()));
            } else if end_count > start_count {
                unreachable!();
            }
        }
    }

    let module_id = universe.new_module_id();

    let module = Module::new(module.module_scope, module_id);
    universe.map_module(module_id, module_name, module);
    
    Ok(ModuleCkSignal::Success)
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

fn generate_struct_type(scope: &ScopedData, struct_def: &Struct) -> Result<StructType, Err> {
    let mut fields = HashMap::new();
    if let Some(ref body) = struct_def.body.0 {
        for field in body.iter() {
            let f_name = field.name.clone();
            let f_type_path = &field.field_type;
            let field_type = scope.type_id(f_type_path)?;
            fields.insert(f_name, field_type);
        }
    } 

    let struct_t = StructType {
        name: struct_def.name.clone(),
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
"mod basic_test_semantic_analysis;

struct Test {
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
        let program = parse_module(program).unwrap();
        let program = check_program(vec![program]).unwrap();

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
"mod call_fn_success;

fn arg_usage(a1: i32, a2: bool) {
	let b1: i32 = a1;
	let b2: bool = a2;
}

fn main() {
	arg_usage(5, false);
}";
        
        let program = parse_module(input).unwrap();
        let program = check_program(vec![program]).unwrap();

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
"mod embedded_ifs_analysis;

fn test() {
    if true {
        if false {

        } else {
            let a: i32 = 100;
        }

        let b: i32 = a;
    }
}";

        let program = parse_module(input).unwrap();
        match check_program(vec![program]) {
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
"mod missing_return_0;

fn test() -> i32 {
    
}";

        let input_1 = 
"mod missing_return_1;

fn test() -> i32 {
    let a: i32 = 5;
}";

        let input_2 = 
"mod missing_return_2;

fn test() -> i32 {
    if true {
        return 0;
    }
}";

        let input_3 =
"mod missing_return_3;

fn test() -> i32 {
    if true {


    } else {
        return 0;
    }
}";

        let input_4 =
"mod missing_return_4;

fn test() -> i32 {
    if true {
        return 0;
    } else {
    
    }
}";

        let input_5 =
"mod missing_return_5;
        
fn test() -> i32 {
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

"mod missing_return_6;

fn test() -> i32 {
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
            let program = parse_module(input[i]).unwrap();
            match check_program(vec![program]) {
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
"mod all_required_returns_0;

fn test() -> i32 {
    return 0;
}";

        let input_1 = 
"mod all_required_returns_1;
        
fn test() -> i32 {
    let a: i32 = 5;

    return 0;
}";

        let input_2 = 
"mod all_required_returns_2;

fn test() -> i32 {
    if true {
        return 0;
    }

    return 0;
}";

        let input_3 =
"mod all_required_returns_3;

fn test() -> i32 {
    if true {
        return 0;
    } else {
        return 0;
    }
}";

        let input_4 =
"mod all_required_returns_4;

fn test() -> i32 {
    if true {
        return 0;
    } else {
        return 0;
    }
}";

        let input_5 =
"mod all_required_returns_5;

fn test() -> i32 {
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
"mod all_required_returns_6;

fn test() -> i32 {
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
            let program = parse_module(input[i]).unwrap();
            check_program(vec![program]).expect(&format!("Test  {} failed.", i));
        }
    }
}
