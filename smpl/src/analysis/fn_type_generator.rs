use err::*;
use feature::*;
use ast::{AnonymousFn as AstAnonymousFn, BuiltinFnParams, BuiltinFunction as AstBuiltinFunction,
          Function as AstFunction};

use super::*;
use super::smpl_type::*;
use super::metadata::*;
use super::feature_checkers::*;

// TODO: Merge with generate_fn_type()
pub fn generate_anonymous_fn_type(
    program: &mut Program,
    scope: &ScopedData,
    fn_id: FnId,
    fn_def: &AstAnonymousFn,
) -> Result<FunctionType, Err> {
    let (universe, metadata, features) = program.analysis_context();
    let ret_type = match fn_def.return_type {
        Some(ref path) => {
            let data = path.data();
            let type_id = scope.type_id(universe, data.into())?;
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
                let param = p.data();
                let type_path = param.param_type.data();
                let type_id = scope.type_id(universe, type_path.into())?;
                typed_params.push(type_id);
                param_metadata.push(FunctionParameter::new(
                    param.name.data().clone(),
                    universe.new_var_id(),
                ));

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
        params: ParamType::Checked(params),
        return_type: ret_type,
    })
}

pub fn generate_fn_type(
    program: &mut Program,
    scope: &ScopedData,
    fn_id: FnId,
    fn_def: &AstFunction,
) -> Result<FunctionType, Err> {
    let (universe, metadata, features) = program.analysis_context();
    let ret_type = match fn_def.return_type {
        Some(ref path) => {
            let data = path.data();
            let type_id = scope.type_id(universe, data.into())?;
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
                let param = p.data();
                let type_path = param.param_type.data();
                let type_id = scope.type_id(universe, type_path.into())?;
                typed_params.push(type_id);
                param_metadata.push(FunctionParameter::new(
                    param.name.data().clone(),
                    universe.new_var_id(),
                ));

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
        params: ParamType::Checked(params),
        return_type: ret_type,
    })
}

pub fn generate_builtin_fn_type(
    program: &mut Program,
    scope: &ScopedData,
    fn_id: FnId,
    fn_def: &AstBuiltinFunction,
) -> Result<FunctionType, Err> {
    let (universe, metadata, features) = program.analysis_context();
    let ret_type = match fn_def.return_type {
        Some(ref path) => {
            let data = path.data();
            let type_id = scope.type_id(universe, data.into())?;
            fn_sig_type_scanner(universe, features, type_id);
            type_id
        }
        None => universe.unit(),
    };

    let params = match fn_def.params {
        BuiltinFnParams::Checked(ref params) => match *params {
            Some(ref params) => {
                let mut typed_params = Vec::new();
                let mut param_metadata = Vec::new();
                for p in params.iter() {
                    let param = p.data();
                    let type_path = param.param_type.data();
                    let type_id = scope.type_id(universe, type_path.into())?;
                    typed_params.push(type_id);
                    param_metadata.push(FunctionParameter::new(
                        param.name.data().clone(),
                        universe.new_var_id(),
                    ));

                    fn_sig_type_scanner(universe, features, type_id);
                }

                metadata.insert_function_param_ids(fn_id, param_metadata);

                ParamType::Checked(typed_params)
            }
            None => {
                metadata.insert_function_param_ids(fn_id, Vec::with_capacity(0));
                ParamType::Checked(Vec::with_capacity(0))
            }
        },

        BuiltinFnParams::Unchecked => {
            metadata.insert_unchecked_builtin_params(fn_id);
            features.add_feature(UNCHECKED_BUILTIN_FN_PARAMS);

            ParamType::Unchecked
        }
    };

    Ok(FunctionType {
        params: params,
        return_type: ret_type,
    })
}
