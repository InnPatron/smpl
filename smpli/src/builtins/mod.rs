pub mod convert;
pub mod err;
pub mod log;
pub mod math;
pub mod str;
pub mod vec;
pub mod option;


use crate::vm_i::{ BuiltinResult, BuiltinFn, ArgType};

/// Provided by u/dtolnay
pub fn erase<F, Fut>(_f: F) -> BuiltinFn
    where
        F: Fn(ArgType)-> Fut + Copy,
        Fut: std::future::Future<Output=BuiltinResult> + 'static,

{
    assert_eq!(std::mem::size_of::<F>(), 0);
    |args| Box::pin(unsafe {
        // F is zero-zied
        // Type is enough to identify which function to call
        std::mem::zeroed::<F>()
    }(args))
}
