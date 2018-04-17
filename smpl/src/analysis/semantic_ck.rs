use std::collections::HashMap;
use std::cell::Cell;
use std::rc::Rc;

use err::Err;
use ast::{Ident, ModulePath, Path, DeclStmt, Struct, Function as AstFunction, Module as AstModule};

use super::feature_checkers::*;
use super::metadata::*;
use super::smpl_type::*;
use super::semantic_data::*;
use super::semantic_data::Module;
use super::control_flow::CFG;
use super::fn_analyzer::analyze_fn;

use feature::PresentFeatures;

pub fn check_program(modules: Vec<AstModule>) -> Result<Program, Err> {
    let mut metadata = Metadata::new();
    let mut universe = Universe::std();
    let mut features = PresentFeatures::new();

    let mut program = Program::new(universe, metadata, features);

    let modules = modules.into_iter()
        .map(|ast_module| ModuleCkData::new(program.universe(), ast_module))
        .collect::<Result<Vec<_>, _>>()?;

    let mut queue = modules;

    loop {
        let start_count = queue.len();
        let mut queue_iter = queue.into_iter();
        queue = Vec::new();

        for mut module in queue_iter {
            match check_module(&mut program, module)? {
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
            for mod_uses in queue.into_iter().map(|module| module.unresolved_module_uses) {
                let mod_uses = mod_uses.into_iter().map(|u| u.0);
                unresolved.extend(mod_uses);
            }

            return Err(Err::UnresolvedUses(unresolved));
        } else if end_count > start_count {
            unreachable!();
        }
    }

    Metadata::find_main(&mut program)?;

    Ok(program)
}

fn check_module(program: &mut Program, mut module: ModuleCkData) -> Result<ModuleCkSignal, Err> {
    let module_name = module.name.clone();

    let module_id = program.universe().new_module_id();

    let mut missing_modules = Vec::new();
    for use_decl in module.unresolved_module_uses.into_iter() {
        match program.universe().module_id(&use_decl.0) {
            Some(id) => {
                let imported_name = use_decl.0.clone();
                let imported_module = program.universe().get_module(id);
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

                module.dependencies.push(id)
            }
            None => missing_modules.push(use_decl),
        }
    }

    if missing_modules.len() > 0 {
        module.unresolved_module_uses = missing_modules;
        // Unknown module. Differ checking till later
        return Ok(ModuleCkSignal::Defer(module));
    }

    let mut unresolved = module.unresolved_module_structs;
    loop {
        let start_count = unresolved.len();
        let mut struct_iter = unresolved.into_iter();

        unresolved = Vec::new();
        for struct_decl in struct_iter {
            let (struct_t, order) = match generate_struct_type(program, 
                                                               &module.module_scope,
                                                               &struct_decl) {
                Ok(s) => s,
                Err(e) => {
                    match e {
                        Err::UnknownType(_) => {
                            unresolved.push(struct_decl);
                            continue;
                        }
                        e => return Err(e),
                    }
                }
            };

            let id = program.universe().new_type_id();
            module.module_scope.insert_type(struct_t.name.clone().into(), id);
            program.universe_mut().insert_type(id, SmplType::Struct(struct_t));
            module.owned_types.push(id);

            let field_ordering = FieldOrdering::new(id, order);
            program.metadata_mut().insert_field_ordering(id, field_ordering);
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

    let mut unresolved = module.unresolved_module_fns;
    loop {
        let start_count = unresolved.len();
        let mut module_fn_iter = unresolved.into_iter();

        unresolved = Vec::new();
        for fn_decl in module_fn_iter {
            let name: ModulePath = fn_decl.name.clone().into();

            let type_id = program.universe().new_type_id();
            let fn_id = program.universe().new_fn_id();

            let fn_type = generate_fn_type(program, &module.module_scope, fn_id, &fn_decl)?;

            let cfg = CFG::generate(program.universe(), fn_decl.clone(), &fn_type)?;

            program.universe_mut().insert_fn(fn_id, type_id, fn_type, cfg);
            module.module_scope.insert_fn(name.clone(), fn_id);
            module.owned_fns.push(fn_id);

            match analyze_fn(program, &module.module_scope, fn_id, module_id)  {
                Ok(f) => f,
                Err(e) => {
                    match e {
                        Err::UnknownFn(_) => {
                            module.owned_fns.pop();
                            program.universe_mut().unmap_fn(fn_id);
                            unresolved.push(fn_decl);
                            continue;
                        }
                        e => return Err(e),
                    }
                }
            }
        }

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

    let module = Module::new(module.module_scope, 
                             module.owned_types, 
                             module.owned_fns, 
                             module.dependencies,
                             module_id);
    program.universe_mut().map_module(module_id, module_name, module);
    
    Ok(ModuleCkSignal::Success)
}

fn generate_fn_type(program: &mut Program, scope: &ScopedData, fn_id: FnId, fn_def: &AstFunction) -> Result<FunctionType, Err> {
    let (universe, metadata, features) = program.analysis_context();
    let ret_type = match fn_def.return_type {
        Some(ref path) => {
            let type_id = scope.type_id(universe, path.into())?;
            fn_sig_type_scanner(universe, features, type_id);
            type_id
        }
        None => universe.unit(),
    };

    let params = match fn_def.params {
        Some(ref params) => {
            let mut typed_params = Vec::new();
            let mut param_metadata = Vec::new();
            for p in params.iter() {
                let type_id = scope.type_id(universe, (&p.param_type).into())?;
                typed_params.push(type_id);
                param_metadata.push(FunctionParameter::new(p.name.clone(), universe.new_var_id()));

                fn_sig_type_scanner(universe, features, type_id);
            }

            metadata.insert_function_param_ids(fn_id, param_metadata);

            typed_params
        }
        None => {
            metadata.insert_function_param_ids(fn_id, Vec::with_capacity(0));
            Vec::with_capacity(0)
        }
    };

    Ok(FunctionType {
        params: params,
        return_type: ret_type,
    })
}

fn generate_struct_type(program: &mut Program, scope: &ScopedData, struct_def: &Struct) -> Result<(StructType, Vec<FieldId>), Err> {
    let (universe, metadata, features) = program.analysis_context();


    let mut fields = HashMap::new();
    let mut field_map = HashMap::new();
    let mut order = Vec::new();
    if let Some(ref body) = struct_def.body.0 {
        for field in body.iter() {
            let f_id = universe.new_field_id();
            let f_name = field.name.clone();
            let f_type_path = &field.field_type;
            let field_type = scope.type_id(universe, f_type_path.into())?;
            fields.insert(f_id, field_type);
            field_map.insert(f_name, f_id);
            order.push(f_id);

            field_type_scanner(universe, features, field_type);
        }
    } 

    let struct_t = StructType {
        name: struct_def.name.clone(),
        fields: fields,
        field_map: field_map,
    };

    Ok((struct_t, order))
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

        let (main, _) = program.metadata().main().unwrap();
        let main = universe.get_fn(main);
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

        let (main, _) = program.metadata().main().unwrap();

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

    #[test]
    fn fn_out_of_order() {
        let input =
"mod fn_out_of_order;

fn A() {
    B();
}

fn B() {

}";

        let program = parse_module(input).unwrap();
        check_program(vec![program]).unwrap();
    }

    #[test]
    fn struct_out_of_order() {
        let input =
"mod struct_out_of_order;

struct A {
    field: B,
}

struct B{
    field: i32,
}";

        let program = parse_module(input).unwrap();
        check_program(vec![program]).unwrap();
    }

    #[test]
    fn mods_out_of_order() {
        let mod1 =
"mod mod1;

use mod2;

struct A {
    field: mod2::B,
}

fn test() {
    mod2::test();
}";

        let mod2 =
"mod mod2;

struct B {
    field: i32,
}

fn test() {
    
}
";

        let mod1 = parse_module(mod1).unwrap();
        let mod2 = parse_module(mod2).unwrap();
        check_program(vec![mod1, mod2]).unwrap();
    }

    #[test]
    fn correct_array_initialization() {
        let mod1 =
"mod mod1;


fn test() {
    let a: [i32; 100] = [ 10; 100 ];
    let b: [i32; 3] = [ 1, 2, 3 ];
}

";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn heterogenous_array_initialization() {
        let mod1 =
"mod mod1;

fn test() {
    let a: [i32; 2] = [100, false];
}
";

        let mod1 = parse_module(mod1).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected TypeErr::HeterogenousArray. Passed checks."),
            Err(e) => {
                match e {
                    Err::TypeErr(e) => {
                        match e {
                            TypeErr::HeterogenousArray{..} => (),
                            e @ _ => panic!("Expected TypeErr::HeterogenousArray. Found {:?}", e),
                        }
                    }

                    e @ _ => panic!("Expected TypeErr::HeterogenousArray. Found {:?}", e),
                }
            }
        }
    }

    #[test]
    fn mismatch_array_assignment() {
        let mod1 =
"mod mod1;

fn test() {
    let a: [i32; 3] = [100, 100];
}
";

        let mod1 = parse_module(mod1).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected TypeErr::LhsRhsInEq. Passed checks."),
            Err(e) => {
                match e {
                    Err::TypeErr(e) => {
                        match e {
                            TypeErr::LhsRhsInEq(..) => (),
                            e @ _ => panic!("Expected TypeErr::LhsRhsInEq. Found {:?}", e),
                        }
                    }

                    e @ _ => panic!("Expected TypeErr::LhsRhsInEq. Found {:?}", e),
                }
            }
        }
    }

    #[test]
    fn array_indexing() {
        let mod1 =
"
mod mod1;

fn test() {
    let a: [i32; 4] = [0, 1, 2, 3];

    let i1: i32 = a[0];
    let i2: i32 = a[1];
    let i3: i32 = a[2];
    let i4: i32 = a[3];
}
";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn assign_array_index() {
        let mod1= 
"
mod mod1;

struct T {
    t: [i32; 4]
}


fn test() {
    let a: T = init T {
        t: [1, 2, 3, 4]
    };

    a.t[3] = 10;
}";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn function_value() {
        let mod1 =
"
mod mod1;

fn bar(a: i32) -> i32 {
    return a + 5;
}

fn apply(f: Fn(i32) -> i32, in: i32) -> i32 {
    return f(in);
}

fn foo() {
    apply(bar, 10);
}
";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn mod_function_value() {
        let mod2 =
"
mod mod2;

fn foo() -> i32 {
    return 5;
}
";
        let mod1 =
"
mod mod1;

fn main() {
    let a: Fn(i32) -> i32 = mod2::foo;
}
";

        let mod1 = parse_module(mod1).unwrap();
        let mod2 = parse_module(mod2).unwrap();
        check_program(vec![mod1, mod2]).unwrap();
    }
}
