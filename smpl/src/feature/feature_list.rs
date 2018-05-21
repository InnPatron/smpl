macro_rules! feature {
    ($i: ident, $e:expr) => {
        pub const $i: &'static str = $e; 
    };

    ($i: ident) => { pub const $i: &'static str = stringify!($i); };
}

feature!(STATIC_ARRAY);

feature!(FUNCTION_VALUE);

feature!(MOD_ACCESS);

feature!(BUILTIN_FN);
