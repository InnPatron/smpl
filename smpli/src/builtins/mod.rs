#[macro_export]
macro_rules! async_box {
    ($name: ident) => {
        paste::item! {
            fn [<boxed_ $name>](args: crate::vm_i::ArgType) -> crate::vm_i::NativeReturn {
                Box::pin($name(args))
            }
        }
    }
}


pub mod convert;
pub mod err;
pub mod log;
pub mod math;
pub mod str;
pub mod vec;
pub mod option;
