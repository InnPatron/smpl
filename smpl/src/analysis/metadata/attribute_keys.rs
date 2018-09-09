macro_rules! key {
    ($i: ident, $e:expr) => {
        pub const $i: &'static str = $e;
    };

    ($i: ident) => { pub const $i: &'static str = stringify!($i); };
}

key!(OPAQUE, "opaque");
