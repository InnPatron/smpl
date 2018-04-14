macro_rules! feature {
    ($i: ident, $e:expr) => {
        pub const $i: &'static str = $e; 
    };

    ($i: ident) => { pub const $i: &'static str = stringify!($e); };
}

feature!(STATIC_ARRAY);
