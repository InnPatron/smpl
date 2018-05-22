pub mod log;
pub mod convert;
pub mod math;
pub mod err;

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


pub const MOD_MATH: &'static str = "math";

pub const MATH_SIN: &'static str = "sin";
pub const MATH_COS: &'static str = "cos";
pub const MATH_TAN: &'static str = "tan";

pub const MATH_ASIN: &'static str = "asin";
pub const MATH_ACOS: &'static str = "acos";
pub const MATH_ATAN: &'static str = "atan";
pub const MATH_ATAN2: &'static str = "atan2";

pub const MATH_TO_RADIANS: &'static str = "to_radians";
pub const MATH_TO_DEGREES: &'static str = "to_degrees";

pub const MATH_FPOWF: &'static str = "fpowf";
pub const MATH_FPOWI: &'static str = "fpowi";
pub const MATH_IPOW: &'static str = "ipow";

pub const MATH_FLOOR: &'static str = "floor";
pub const MATH_CEIL: &'static str = "ceil";
pub const MATH_ROUND: &'static str = "round";


pub const MATH_DECLARATION: &'static str =
"
mod math;

builtin fn sin(r: f32) -> f32;
builtin fn cos(r: f32) -> f32;
builtin fn tan(r: f32) -> f32;

builtin fn asin(r: f32) -> f32;
builtin fn acos(r: f32) -> f32;
builtin fn atan(r: f32) -> f32;
builtin fn atan2(r: f32, other: f32) -> f32;

builtin fn to_radians(degrees: f32) -> f32;
builtin fn to_degrees(radians: f32) -> f32;

builtin fn fpowf(base: f32, power: f32) -> f32;
builtin fn fpowi(base: f32, power: i32) -> f32;
builtin fn ipow(base: i32, power: i32) -> i32;

builtin fn floor(f: f32) -> f32;
builtin fn ceil(f: f32) -> f32;
builtin fn round(f: f32) -> f32;
";


pub const MOD_ERR: &'static str = "err";

pub const ERR_PANIC: &'static str = "panic";

pub const ERR_DECLARATION: &'static str =
"
mod err;

builtin fn panic(msg: String);
";
