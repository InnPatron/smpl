use super::builtins::*;
use super::vm::VM;

use ast::Module;
use err::Err;
use parser::parse_module;

pub fn include(mut modules: Vec<Module>) -> Vec<Module> {
    let log = parse_module(LOG_DECLARATION).unwrap();
    let convert = parse_module(CONVERT_DECLARATION).unwrap();
    let math = parse_module(MATH_DECLARATION).unwrap();
    let err = parse_module(ERR_DECLARATION).unwrap();

    let mut to_add = vec![log, convert, math, err];

    modules.append(&mut to_add);
    
    modules
}

pub fn load(vm: &mut VM) {
    // Log
    {
        use super::builtins::log::*;
        vm.insert_builtin(MOD_LOG, LOG_PRINT, Box::new(Print));
        vm.insert_builtin(MOD_LOG, LOG_PRINTLN, Box::new(Println));
    }

    // Convert
    {
        use super::builtins::convert::*;
        vm.insert_builtin(MOD_CONVERT, CONVERT_INT_TO_FLOAT, Box::new(IntToFloat));
        vm.insert_builtin(MOD_CONVERT, CONVERT_FLOAT_TO_INT, Box::new(FloatToInt));

        vm.insert_builtin(MOD_CONVERT, CONVERT_IS_FLOAT, Box::new(IsFloat));
        vm.insert_builtin(MOD_CONVERT, CONVERT_IS_INT, Box::new(IsInt));

        vm.insert_builtin(MOD_CONVERT, CONVERT_STRING_TO_FLOAT, Box::new(StringToFloat));
        vm.insert_builtin(MOD_CONVERT, CONVERT_STRING_TO_INT, Box::new(StringToInt));
    }

    // Math
    {
        use super::builtins::math::*;
        vm.insert_builtin(MOD_MATH, MATH_SIN, Box::new(Sin));
        vm.insert_builtin(MOD_MATH, MATH_COS, Box::new(Cos));
        vm.insert_builtin(MOD_MATH, MATH_TAN, Box::new(Tan));

        vm.insert_builtin(MOD_MATH, MATH_ASIN, Box::new(Asin));
        vm.insert_builtin(MOD_MATH, MATH_ACOS, Box::new(Acos));
        vm.insert_builtin(MOD_MATH, MATH_ATAN, Box::new(Atan));
        vm.insert_builtin(MOD_MATH, MATH_ATAN2, Box::new(Atan2));

        vm.insert_builtin(MOD_MATH, MATH_TO_RADIANS, Box::new(ToRadians));
        vm.insert_builtin(MOD_MATH, MATH_TO_DEGREES, Box::new(ToDegrees));

        vm.insert_builtin(MOD_MATH, MATH_FPOWF, Box::new(FPowF));
        vm.insert_builtin(MOD_MATH, MATH_FPOWI, Box::new(FPowI));
        vm.insert_builtin(MOD_MATH, MATH_IPOW, Box::new(IPow));

        vm.insert_builtin(MOD_MATH, MATH_FLOOR, Box::new(Floor));
        vm.insert_builtin(MOD_MATH, MATH_CEIL, Box::new(Ceil));
        vm.insert_builtin(MOD_MATH, MATH_ROUND, Box::new(Round));
    }
}
