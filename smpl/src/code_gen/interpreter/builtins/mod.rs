pub mod log;
pub mod convert;

pub const MOD_LOG: &'static str = "log";
pub const LOG_PRINT: &'static str = "print";
pub const LOG_PRINTLN: &'static str = "println";

pub const LOG_DECLARATION: &'static str =
"
mod log;

builtin fn print(UNCHECKED);
builtin fn println(UNCHECKED);
";


pub const MOD_CONVERT: &'static str = "convert";

pub const CONVERT_INT_TO_FLOAT: &'static str = "int_to_float";
pub const CONVERT_FLOAT_TO_INT: &'static str = "float_to_int";

pub const CONVERT_IS_FLOAT: &'static str = "is_float";
pub const CONVERT_IS_INT: &'static str = "is_int";


pub const CONVERT_STRING_TO_FLOAT: &'static str = "string_to_float";
pub const CONVERT_STRING_TO_INT: &'static str = "string_to_int";

pub const CONVERT_DECLARATION: &'static str =
"
mod convert;

builtin fn int_to_float(i: i32) -> f32;
builtin fn float_to_int(f: f32) -> i32;

builtin fn is_float(s: String) -> bool;
builtin fn is_int(s: String) -> bool;

builtin fn string_to_float(s: String) -> f32;
builtin fn string_to_int(s: String) -> i32;
";
