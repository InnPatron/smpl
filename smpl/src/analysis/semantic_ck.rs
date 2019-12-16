use crate::feature::*;
use crate::module::ParsedModule;

use super::error::AnalysisError;
use super::metadata::*;
use super::mod_resolver;
use super::semantic_data::*;

pub fn check_program(
    modules: Vec<ParsedModule>,
) -> Result<Program, AnalysisError> { 

    let mut program = mod_resolver::check_modules(modules)?;

    Metadata::find_main(&mut program)?;

    Ok(program)
}

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod tests {
    use super::super::error::*;
    use super::*;
    use crate::parser::*;
    use crate::ast::Ident;
    use crate::module::UnparsedModule;

    macro_rules! error_variant {
        ($value: expr, $e_pat: pat) => {{
            let value = $value;
            if let Ok(_) = value {
                panic!("Expected {}. Found Ok", stringify!($e_pat));
            } else if let Err($e_pat) = value {
                ()
            } else if let Err(AnalysisError::Errors(errors)) = value {
                let mut found_error = false;

                errors
                    .iter()
                    .for_each(|err| {
                        if let $e_pat = err {
                            found_error = true;
                        }
                    });

                if !found_error {
                    panic!("Expected {}. Found {:?}", stringify!($e_pat), errors);
                }

                ()
            } else if let Err(e) = value {
                panic!("Expected {}. Found {:?}", stringify!($e_pat), e);
            }
        }}
    }

    macro_rules! wrap_input {
        ($input: expr) => {{ 
            UnparsedModule::anonymous($input)
        }}
    }

    macro_rules! include_test {
        ($file_name: expr) => {{
            include_str!(concat!("../../../semantic-tests/", $file_name))
        }}
    }

    macro_rules! test_pass_analysis {
        ($name: ident) => {
            #[test]
            fn $name() {
                let mod1 = include_test!(concat!(stringify!($name), ".smpl"));

                let mod1 = parse_module(wrap_input!(mod1))
                    .expect("Module did not parse correctly");

                let _program = check_program(vec![mod1]).unwrap();
            }
        }
    }

    test_pass_analysis!(tic_tac_toe);
    test_pass_analysis!(basic_test_semantic_analysis);

    test_pass_analysis!(fn_out_of_order);
    test_pass_analysis!(struct_out_of_order);

    test_pass_analysis!(array_indexing);
    test_pass_analysis!(assign_array_index);

    test_pass_analysis!(function_value);
    test_pass_analysis!(function_field);

    test_pass_analysis!(builtin_function);
    test_pass_analysis!(unchecked_params_builtin_function);

    test_pass_analysis!(optional_local_type_annotation);

    test_pass_analysis!(recursive_check);
    test_pass_analysis!(mutually_recursive_check);

    test_pass_analysis!(anonymous_fn_call);
    test_pass_analysis!(anonymous_fn_arg);
    test_pass_analysis!(fn_piping);

    test_pass_analysis!(opaque_type_param);
    test_pass_analysis!(opaque_type_assignment);
    test_pass_analysis!(opaque_type_field);

    test_pass_analysis!(builtin_bind);

    test_pass_analysis!(generic_struct_decl);
    test_pass_analysis!(generic_struct_init);
    test_pass_analysis!(generic_function);
    test_pass_analysis!(generic_fn_binding);
    test_pass_analysis!(generic_builtin_fn_binding);

    test_pass_analysis!(instantiate_fn_binding);
    test_pass_analysis!(instantiate_builtin_fn_binding);

    test_pass_analysis!(generic_fn_param);

    test_pass_analysis!(width_constraint_call);
    test_pass_analysis!(width_constraint_nested);
    test_pass_analysis!(generic_width_constraint);
    test_pass_analysis!(generic_transitive_width_constraint);
    test_pass_analysis!(width_constraint_multi_base);
    test_pass_analysis!(generic_struct_init_type_arg);
    test_pass_analysis!(generic_struct_init_width_constraint);

    test_pass_analysis!(anonymous_struct_init);

    test_pass_analysis!(valid_fn_subtyping);
    test_pass_analysis!(bind_fn_type_app); 

    test_pass_analysis!(array_path_assignment); 

    #[test]
    fn call_fn_success() {
        use super::super::typed_ast::*;
        use crate::analysis::*;

        let input = include_test!("call_fn_success.smpl");
        
        let program = parse_module(wrap_input!(input)).unwrap();
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
        let _called_fn = called_fn.unwrap();
        
        let cfg = if let Function::SMPL(main) = main {
            main.cfg()
        } else {
            panic!("Expected a SMPL function. Found {:?}", main);
        };
        let cfg = cfg.borrow();
        let fn_call = {
            let scope_enter = cfg.after_start();
            cfg.next(scope_enter)
        };
        match cfg.node_weight(fn_call) {
            Node::Block(ref block) => {
                assert!(block.graph().len() == 1);
                let mut iter = block.graph().iter();
                let next = iter.next().unwrap();
                match next {
                    BlockNode::Expr(ref edata) => {
                        let e = &edata.expr;
                        let iter = e.execution_order();
                        let tmp = e.get_tmp(iter.last().unwrap());
                        match *tmp.value().data() {
                            Value::FnCall(ref call) => {
                                let fn_value = call.fn_value();
                                let tmp = e.get_tmp(fn_value);
                                if let Value::Binding(ref _binding) = tmp.value().data() {
                                    ()
                                } else {
                                    panic!("Function call not on binding");
                                }
                            },

                            ref v => panic!("Expected Value::FnCall. Found {:?}", v),
                        }
                    },
                    ref n @ _ => panic!("Expected BlockNode::Expr. Found {:?}", n),
                }
            }
            
            ref n @ _ => panic!("Expected Node::Block. Found {:?}", n),
        }
    }

    #[test]
    fn embedded_ifs_analysis() {
        let input = include_test!("embedded_ifs_analysis.smpl");

        let program = parse_module(wrap_input!(input)).unwrap();
        match check_program(vec![program]) {
            Ok(_) => panic!("Passed analysis. Expected AnalysisError::UnknownBinding"),
            Err(e) => {
                match e {
                    AnalysisError::UnknownBinding(ident, ..) => {
                        assert_eq!(ident, ident!("a"));
                    }

                    e @ _ => panic!("Expected AnalysisError::UnknownBinding. Found {:?}", e),
                }
            }
        }
    }

    #[test]
    fn missing_return() {
        let input_0 = include_test!("missing_return_0.smpl");
        let input_1 = include_test!("missing_return_1.smpl");
        let input_2 = include_test!("missing_return_2.smpl");
        let input_3 = include_test!("missing_return_3.smpl");
        let input_4 = include_test!("missing_return_4.smpl");
        let input_5 = include_test!("missing_return_5.smpl");
        let input_6 = include_test!("missing_return_6.smpl");

        let input = vec![input_0, input_1, input_2, input_3, input_4, input_5, input_6];

        for i in 0..input.len() {
            let program = parse_module(wrap_input!(input[i])).unwrap();
            match check_program(vec![program]) {
                Ok(_) => panic!("Passed analysis. Expected AnalysisError::ControlFlowError(ControlFlowError::MissingReturn. Test {}", i),
                Err(e) => {
                    match e {
                        AnalysisError::ControlFlowError(e) => {
                            match e {
                                ControlFlowError::MissingReturn(..) => (),

                                e @ _ => panic!("Expected ControlFlowError::MissingReturn. Test {}. Found {:?}", i, e),
                            }
                        }

                        e @ _ => panic!("Expected AnalysisError::ControlFlowError. Test {}. Found {:?}", i, e),
                    }
                }
            }
        }
    }

    #[test]
    fn all_required_returns() {
        let input_0 = include_test!("all_required_returns_0.smpl");
        let input_1 = include_test!("all_required_returns_1.smpl");
        let input_2 = include_test!("all_required_returns_2.smpl");
        let input_3 = include_test!("all_required_returns_3.smpl");
        let input_4 = include_test!("all_required_returns_4.smpl");
        let input_5 = include_test!("all_required_returns_5.smpl");
        let input_6 = include_test!("all_required_returns_6.smpl");

        let input = vec![input_0, input_1, input_2, input_3, input_4, input_5, input_6];

        for i in 0..input.len() {
            let program = parse_module(wrap_input!(input[i])).unwrap();
            check_program(vec![program]).expect(&format!("Test  {} failed.", i));
        }
    } 

    #[test]
    fn mods_out_of_order() {
        let mod1 = include_test!("mods_out_of_order_1.smpl");
        let mod2 = include_test!("mods_out_of_order_2.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        let mod2 = parse_module(wrap_input!(mod2)).unwrap();
        check_program(vec![mod1, mod2]).unwrap();
    }

    #[test]
    fn correct_array_initialization() {
        let mod1 = include_test!("correct_array_initialization.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn heterogenous_array_initialization() {
        let mod1 = include_test!("heterogenous_array_initialization.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected TypeError::HeterogenousArray. Passed checks."),
            Err(e) => {
                match e {
                    AnalysisError::TypeError(e) => {
                        match e {
                            TypeError::HeterogenousArray{..} => (),
                            e @ _ => panic!("Expected TypeError::HeterogenousArray. Found {:?}", e),
                        }
                    }

                    e @ _ => panic!("Expected TypeError::HeterogenousArray. Found {:?}", e),
                }
            }
        }
    }

    #[test]
    fn mismatch_array_assignment() {
        let mod1 = include_test!("mismatch_array_assignment.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected TypeError::LhsRhsInEq. Passed checks."),
            Err(e) => {
                match e {
                    AnalysisError::TypeError(_e) => {
                        ()
                    }

                    e @ _ => panic!("Expected TypeError::LhsRhsInEq. Found {:?}", e),
                }
            }
        }
    }

    #[test]
    fn mod_function_value() {
        let mod1 = include_test!("mod_function_value_1.smpl");
        let mod2 = include_test!("mod_function_value_2.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        let mod2 = parse_module(wrap_input!(mod2)).unwrap();
        check_program(vec![mod1, mod2]).unwrap();
    } 

/*
    #[test]
    fn deny_unchecked_params_builtin_function_local() {
        let mod1 =
"
mod mod1;

builtin fn test_function(UNCHECKED) -> bool;

fn main() {
    let t = test_function;
}";

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Found Ok. Expected AnalysisError::UncheckedFunctionBinding"),
            Err(e) => {
                match e {
                    AnalysisError::UncheckedFunctionBinding(..) => (),
                    _ => panic!("Expected AnalysisError::UncheckedFunctionBinding. Found {:?}", e),
                }
            }
        }
    }
*/

    #[test]
    fn deny_unchecked_params_builtin_function_struct() {
        let mod1 = include_test!("deny_unchecked_params_builtin_function_struct.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Found Ok. Expected AnalysisError::UncheckedFunctionBinding"),
            Err(e) => {
                match e {
                    AnalysisError::TypeError(..) => (),
                    _ => panic!("Expected AnalysisError::TypeError. Found {:?}", e),
                }
            }
        }
    } 

    #[test]
    fn anonymous_fn_invalid() {
        let mod1 = include_test!("anonymous_fn_invalid.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        let result = check_program(vec![mod1]);
        if let Err(AnalysisError::TypeError(_t)) = result {
            ();
        } else {
            match result {
                Ok(_) => panic!("Expected a type error. Found Ok"),
                Err(e) => panic!("Expected a type error. Found {:?}", e),
            }
        }
    } 
    
    #[test]
    fn annotate_struct() {
        let input = include_test!("annotate_struct.smpl");

        let mod1 = parse_module(wrap_input!(input)).unwrap();
        let program = check_program(vec![mod1]).unwrap();
        let module_id = program.universe().module_id(&Ident("mod1".to_string())).unwrap();
        let module = program.universe().get_module(module_id);
        let struct_type = module.owned_types().next().unwrap().clone();
        let annotations = program.metadata().get_struct_annotations(struct_type).unwrap();
        assert!(annotations.get("test") == Some(&None));
        assert!(annotations.get("foo") == Some(&Some("bar".to_string())));
    }

    #[test]
    fn annotate_fn() {
        let input = include_test!("annotate_fn.smpl");

        let mod1 = parse_module(wrap_input!(input)).unwrap();
        let program = check_program(vec![mod1]).unwrap();
        let module_id = program.universe().module_id(&Ident("mod1".to_string())).unwrap();
        let module = program.universe().get_module(module_id);
        let function = module.owned_fns().next().unwrap().clone();
        let annotations = program.metadata().get_fn_annotations(function).unwrap();
        assert!(annotations.get("test") == Some(&None));
        assert!(annotations.get("foo") == Some(&Some("bar".to_string())));
    }

    #[test]
    fn opaque_struct() {
        let input = include_test!("opaque_struct.smpl");
        
        let mod1 = parse_module(wrap_input!(input)).unwrap();
        let err = check_program(vec![mod1]);

        match err {
            Ok(_) => panic!("Expected err"),
            Err(err) => {
                match err {
                    AnalysisError::TypeError(t) => {
                        match t {
                            TypeError::InitOpaqueType {..} => (),
                            _ => panic!("{:?}", t),
                        }
                    },

                    _ => panic!("Expected type err. Found {:?}", err),
                }
            },
        }
    } 

    #[test]
    fn opaque_type_invariance() {
        let mod1 = include_test!("opaque_type_invariance.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        error_variant!(check_program(vec![mod1]), AnalysisError::TypeError(_));
    }

    #[test]
    fn opaque_type_field_invariance() {
        let mod1 = include_test!("opaque_type_field_invariance.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        error_variant!(check_program(vec![mod1]), AnalysisError::TypeError(_));
    } 

    #[test]
    fn generic_struct_init_type_arg_error() {
        let mod1 = include_test!("generic_struct_init_type_arg_error.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err());
    } 
        
    #[test]
    fn generic_fn_binding_invalid_type() {
        let mod1 = include_test!("generic_fn_binding_invalid_type.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err());
    }
    
    #[test]
    fn generic_width_constraint_invalid_bind() {
        let mod1 = include_test!("generic_width_constraint_invalid_bind.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err())
    }

    #[test]
    fn generic_width_constraint_invalid_return() {
        let mod1 = include_test!("generic_width_constraint_invalid_return.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err())
    }

    #[test]
    fn generic_width_constraint_invalid_field_bind() {
        let mod1 = include_test!("generic_width_constraint_invalid_field_bind.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err())
    }

    #[test]
    fn generic_invalid_transitive_width_constraint() {
        let mod1 = include_test!("generic_invalid_transitive_width_constraint.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err())
    }

    #[test]
    fn width_constraint_conflicting() {
        let mod1 = include_test!("width_constraint_conflicting.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err());
    }

    #[test]
    fn generic_unknown_type_parameter() {
        let mod1 = include_test!("generic_unknown_type_parameter.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err());
    } 

    #[test]
    fn anonymous_struct_init_invalid_type() {
        let mod1 = include_test!("anonymous_struct_init_invalid_type.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err());
    }

    #[test]
    fn invalid_fn_subtyping_wider() {
        let mod1 = include_test!("invalid_fn_subtyping_wider.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err());
    }

    #[test]
    fn invalid_fn_subtyping_nominal() {
        let mod1 = include_test!("invalid_fn_subtyping_nominal.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        assert!(check_program(vec![mod1]).is_err());
    }

    #[test]
    fn bind_fn_type_app_mod_access() {
        let mod1 = include_test!("bind_fn_type_app_mod_access_1.smpl");
        let mod2 = include_test!("bind_fn_type_app_mod_access_2.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        let mod2 = parse_module(wrap_input!(mod2)).unwrap();
        check_program(vec![mod1, mod2]).unwrap();
    }

    #[test]
    fn bind_fn_type_app_mod_access_stmt() {
        let mod1 = include_test!("bind_fn_type_app_mod_access_stmt_1.smpl");
        let mod2 = include_test!("bind_fn_type_app_mod_access_stmt_2.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        let mod2 = parse_module(wrap_input!(mod2)).unwrap();
        check_program(vec![mod1, mod2]).unwrap();
    }

    #[test]
    fn fn_multi_type_param() {
        let mod1 = include_test!("fn_multi_type_param.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        check_program(vec![mod1]).unwrap();
    }

    #[test]
    fn fn_multi_type_param_err() {
        let mod1 = include_test!("fn_multi_type_param_err.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected a type error. Found OK"),

            Err(e) => {
                if let AnalysisError::TypeError(_) = e {
                    ()
                } else {
                    panic!("Expected a type error. Found {:?}", e);
                }
            }
        }
    }

    #[test]
    fn invalid_type_arg() {
        let mod1 = include_test!("invalid_type_arg.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected a type error. Found OK"),

            Err(e) => {
                if let AnalysisError::TypeError(_) = e {
                    ()
                } else if let AnalysisError::Errors(e) = e {
                    let mut type_error = false;

                    e
                        .iter()
                        .for_each(|err| {
                            if let AnalysisError::TypeError(_) = err {
                                type_error = true;
                            }
                        });

                    if type_error {
                        ()
                    } else {
                        panic!("Expected a type error. Found {:?}", e);
                    }
                } else {
                    panic!("Expected a type error. Found {:?}", e);
                }
            }
        }
    }

    #[test]
    fn top_level_name_collision_struct() {
        let mod1 = include_test!("top_level_name_collision_struct.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected an error. Found OK"),

            Err(e) => {
                if let AnalysisError::TopLevelError(_) = e {
                    ()
                } else {
                    panic!("Expected a top level error. Found {:?}", e);
                }
            }
        }
    }

    #[test]
    fn top_level_name_collision_struct_opaque() {
        let mod1 = include_test!("top_level_name_collision_struct_opaque.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected an error. Found OK"),

            Err(e) => {
                if let AnalysisError::TopLevelError(_) = e {
                    ()
                } else {
                    panic!("Expected a top level error. Found {:?}", e);
                }
            }
        }
    }

    #[test]
    fn top_level_name_collision_opaque() {
        let mod1 = include_test!("top_level_name_collision_opaque.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected an error. Found OK"),

            Err(e) => {
                if let AnalysisError::TopLevelError(_) = e {
                    ()
                } else {
                    panic!("Expected a top level error. Found {:?}", e);
                }
            }
        }
    }

    #[test]
    fn top_level_name_collision_fn() {
        let mod1 = include_test!("top_level_name_collision_fn.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected an error. Found OK"),

            Err(e) => {
                if let AnalysisError::TopLevelError(_) = e {
                    ()
                } else {
                    panic!("Expected a top level error. Found {:?}", e);
                }
            }
        }
    }

    #[test]
    fn top_level_name_collision_fn_builtin() {
        let mod1 = include_test!("top_level_name_collision_fn_builtin.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected an error. Found OK"),

            Err(e) => {
                if let AnalysisError::TopLevelError(_) = e {
                    ()
                } else {
                    panic!("Expected a top level error. Found {:?}", e);
                }
            }
        }
    }

    #[test]
    fn top_level_name_collision_builtin_fn() {
        let mod1 = include_test!("top_level_name_collision_builtin_fn.smpl");

        let mod1 = parse_module(wrap_input!(mod1)).unwrap();
        match check_program(vec![mod1]) {
            Ok(_) => panic!("Expected an error. Found OK"),

            Err(e) => {
                if let AnalysisError::TopLevelError(_) = e {
                    ()
                } else {
                    panic!("Expected a top level error. Found {:?}", e);
                }
            }
        }
    }
}
