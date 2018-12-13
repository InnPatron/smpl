use crate::err::Err;

use crate::feature::*;
use crate::ast::Module as AstModule;

use super::metadata::*;
use super::semantic_data::*;
use super::mod_resolver;

pub fn check_program(modules: Vec<AstModule>) -> Result<Program, Err> {
    let metadata = Metadata::new();
    let universe = Universe::std();
    let features = PresentFeatures::new();

    let mut program = Program::new(universe, metadata, features);

    mod_resolver::check_modules(&mut program, modules)?;

    Metadata::find_main(&mut program)?;

    Ok(program)
}

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod tests {
    use crate::err::*;
    use super::*;
    use crate::parser::*;
    use crate::analysis::smpl_type::*;
    use crate::ast::Ident;

    #[test]
    fn basic_test_semantic_analysis() {
        let program =
"mod basic_test_semantic_analysis;

struct Test {
    field_1: int,
    field_2: float,
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
        use super::super::typed_ast::*;
        use crate::analysis::*;

        let input = 
"mod call_fn_success;

fn arg_usage(a1: int, a2: bool) {
	let b1: int = a1;
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
            Node::Expr(ref edata) => {
                let e = &edata.expr;
                let mut iter = e.execution_order();
                let tmp = e.get_tmp(*iter.last().unwrap());
                match *tmp.value().data() {
                    Value::FnCall(ref call) => {
                        assert_eq!(call.get_id().unwrap(), BindingId::Fn(called_fn));
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
            let a: int = 100;
        }

        let b: int = a;
    }
}";

        let program = parse_module(input).unwrap();
        match check_program(vec![program]) {
            Ok(_) => panic!("Passed analysis. Expected Err::UnknownBinding"),
            Err(e) => {
                match e {
                    Err::UnknownBinding(ident) => {
                        assert_eq!(ident, ident!("a"));
                    }

                    e @ _ => panic!("Expected Err::UnknownBinding. Found {:?}", e),
                }
            }
        }
    }

    #[test]
    fn missing_return() {
        let input_0 =
"mod missing_return_0;

fn test() -> int {
    
}";

        let input_1 = 
"mod missing_return_1;

fn test() -> int {
    let a: int = 5;
}";

        let input_2 = 
"mod missing_return_2;

fn test() -> int {
    if true {
        return 0;
    }
}";

        let input_3 =
"mod missing_return_3;

fn test() -> int {
    if true {


    } else {
        return 0;
    }
}";

        let input_4 =
"mod missing_return_4;

fn test() -> int {
    if true {
        return 0;
    } else {
    
    }
}";

        let input_5 =
"mod missing_return_5;
        
fn test() -> int {
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

fn test() -> int {
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

fn test() -> int {
    return 0;
}";

        let input_1 = 
"mod all_required_returns_1;
        
fn test() -> int {
    let a: int = 5;

    return 0;
}";

        let input_2 = 
"mod all_required_returns_2;

fn test() -> int {
    if true {
        return 0;
    }

    return 0;
}";

        let input_3 =
"mod all_required_returns_3;

fn test() -> int {
    if true {
        return 0;
    } else {
        return 0;
    }
}";

        let input_4 =
"mod all_required_returns_4;

fn test() -> int {
    if true {
        return 0;
    } else {
        return 0;
    }
}";

        let input_5 =
"mod all_required_returns_5;

fn test() -> int {
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

fn test() -> int {
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
    field: int,
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
    field: int,
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
    let a: [int; 100] = [ 10; 100 ];
    let b: [int; 3] = [ 1, 2, 3 ];
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
    let a: [int; 2] = [100, false];
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
    let a: [int; 3] = [100, 100];
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
    let a: [int; 4] = [0, 1, 2, 3];

    let i1: int = a[0];
    let i2: int = a[1];
    let i3: int = a[2];
    let i4: int = a[3];
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
    t: [int; 4]
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

fn bar(a: int) -> int {
    return a + 5;
}

fn apply(f: fn(int) -> int, in: int) -> int {
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

fn foo() -> int {
    return 5;
}
";
        let mod1 =
"
mod mod1;

use mod2;

fn b() {
    let i: int = mod2::foo();
}

fn main() {
    let a: fn() -> int = mod2::foo;
}
";

        let mod1 = parse_module(mod1).unwrap();
        let mod2 = parse_module(mod2).unwrap();
        check_program(vec![mod1, mod2]).unwrap();
    }

    #[test]
    fn function_field() {
        let mod1 =
"
mod mod1;

struct T {
    f: fn(int),
}

fn b(a: int) {

}

fn main() {
    let t: T = init T {f: b};
    let f: fn(int) = t.f;
    f(5);
}
";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn builtin_function() {
        let mod1 =
"
mod mod1;

struct T {
    i: int
}

builtin fn test_function(t: T) -> bool;

fn main() {
    let t: T = init T {i: 1337};
    test_function(t);
}";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn unchecked_params_builtin_function() {
        let mod1 =
"
mod mod1;

struct T {
    i: int
}

builtin fn test_function(UNCHECKED) -> bool;

fn main() {
    let t: T = init T {i: 1337};
    test_function(1, 2, 3);
}";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn deny_unchecked_params_builtin_function_local() {
        let mod1 =
"
mod mod1;

builtin fn test_function(UNCHECKED) -> bool;

fn main() {
    let t = test_function;
}";

        let mod1 = parse_module(mod1).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Found Ok. Expected Err::UncheckedFunctionBinding"),
            Err(e) => {
                match e {
                    Err::UncheckedFunctionBinding(..) => (),
                    _ => panic!("Expected Err::UncheckedFunctionBinding. Found {:?}", e),
                }
            }
        }
    }

    #[test]
    fn deny_unchecked_params_builtin_function_struct() {
        let mod1 =
"
mod mod1;

struct T {
    i: fn() -> bool,
}

builtin fn test_function(UNCHECKED) -> bool;

fn main() {
    let t = init T { i: test_function };
}";

        let mod1 = parse_module(mod1).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Found Ok. Expected Err::UncheckedFunctionBinding"),
            Err(e) => {
                match e {
                    Err::UncheckedFunctionBinding(..) => (),
                    _ => panic!("Expected Err::UncheckedFunctionBinding. Found {:?}", e),
                }
            }
        }
    }

    #[test]
    fn optional_local_type_annotation() {
        let mod1 =
"
mod mod1;

struct T {
    i: int
}

fn test_function(t: T) -> int {
    return t.i;
}

fn main() {
    let t = init T {i: 1337};
    test_function(t);
}";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn recursive_check() {
        let mod1 =
"
mod mod1;

fn recurse(i: int) -> int {
    if (i == 0) {
        return 0;
    } else {
        return recurse(i - 1);
    }
}
";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn mutually_recursive_check() {
        let mod1 =
"
mod mod1;

fn recurse_a(i: int) -> int {
    if (i == 0) {
        return 5;
    } else {
        return recurse_b(i - 1);
    }
}

fn recurse_b(i: int) -> int {
    if (i == 0) {
        return -5;
    } else {
        return recurse_a(i - 1);
    }
}
";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn cyclic_type() {
        let mod1 =
"
mod mod1;

struct TypeA {
    f1: TypeB
}

struct TypeB {
    f1: TypeA
}
";

        let mod1 = parse_module(mod1).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!(),
            Err(e) => match e {
                Err::TypeErr(e) => {
                    match e {
                        TypeErr::CyclicType(_) => (),
                        _ => panic!(),
                    }
                }

                _ => panic!(),
            }
        }
    }

    #[test]
    fn cyclic_type_ck_empty_types() {
        let mod1 =
"
mod mod1;

struct Foo {
    f1: Data,
    f2: Data,
    f3: Data,
}

struct Data { }
";
        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    #[should_panic]
    fn anonymous_fn_invalid() {
        let mod1 =
"mod mod1;

fn test() {
    let func = fn (foo: int) -> int {
        return true;
    };
}";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }
    
    #[test]
    fn anonymous_fn_call() {
        let mod1 =
"
mod mod1;

fn test() -> int {
    let func = fn (foo: int) -> int {
        return foo + 5;
    };

    return func(10);
}";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn anonymous_fn_arg() {
        let mod1 =
"
mod mod1;

fn test2(func: fn(int) -> int) -> int {
    return func(10);
}

fn test() -> int {
    let func = fn (foo: int) -> int {
        return foo + 5;
    };

    return test2(func);
}";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn fn_piping() {
        let mod1 =
"
mod mod1;

fn inc(i: int) -> int {
    return i + 1;
}

fn test() -> int {
    return inc(0) |> inc() |> inc() |> inc();
}";

        let mod1 = parse_module(mod1).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn annotate_struct() {
        let input =
"mod mod1;
#[test, foo = \"bar\"]
struct Foo { }";

        let mod1 = parse_module(input).unwrap();
        let program = check_program(vec![mod1]).unwrap();
        let module_id = program.universe().module_id(&Ident("mod1".to_string())).unwrap();
        let module = program.universe().get_module(module_id);
        let struct_type = module.owned_types().iter().next().unwrap().clone();
        let annotations = program.metadata().get_struct_annotations(struct_type).unwrap();
        assert!(annotations.get("test") == Some(&None));
        assert!(annotations.get("foo") == Some(&Some("bar".to_string())));
    }

    #[test]
    fn annotate_fn() {
        let input =
"mod mod1;
#[test, foo = \"bar\"]

fn foo() { }";

        let mod1 = parse_module(input).unwrap();
        let program = check_program(vec![mod1]).unwrap();
        let module_id = program.universe().module_id(&Ident("mod1".to_string())).unwrap();
        let module = program.universe().get_module(module_id);
        let function = module.owned_fns().iter().next().unwrap().clone();
        let annotations = program.metadata().get_fn_annotations(function).unwrap();
        assert!(annotations.get("test") == Some(&None));
        assert!(annotations.get("foo") == Some(&Some("bar".to_string())));
    }

    #[test]
    fn opaque_struct() {
        let input =
"mod mod1;
#[opaque]
struct Foo { bla: int }

fn test() {
    init Foo {
        bla: 5
    };
}";
        
        let mod1 = parse_module(input).unwrap();
        let err = check_program(vec![mod1]);

        match err {
            Ok(_) => panic!("Expected err"),
            Err(err) => {
                match err {
                    Err::TypeErr(t) => {
                        match t {
                            TypeErr::InitOpaqueType {..} => (),
                            _ => panic!("{:?}", t),
                        }
                    },

                    _ => panic!("Expected type err"),
                }
            },
        }
    }

    #[test]
    fn builtin_bind() {
        let mod1 = 
"mod mod1;

builtin fn add(a: int, b: int) -> int;

fn bar() -> int {
    let f = add;
    return f(3, 5);
}";

        let mod1 = parse_module(mod1).unwrap();
        let _err = check_program(vec![mod1]).unwrap();
    }
}
