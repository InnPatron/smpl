use std::str::FromStr;
use ast::*;
use ascii::*;
extern crate lalrpop_util as __lalrpop_util;

mod __parse__Ident {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use std::str::FromStr;
    use ast::*;
    use ascii::*;
    extern crate lalrpop_util as __lalrpop_util;
    #[allow(dead_code)]
    pub enum __Symbol<'input> {
        Term_22_2e_22(&'input str),
        Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23(&'input str),
        Nt_28_22_2e_22_20_3cIdent_3e_29(Ident),
        Nt_28_22_2e_22_20_3cIdent_3e_29_2a(::std::vec::Vec<Ident>),
        Nt_28_22_2e_22_20_3cIdent_3e_29_2b(::std::vec::Vec<Ident>),
        NtIdent(Ident),
        NtPath(Path),
        Nt____Ident(Ident),
        Nt____Path(Path),
    }
    const __ACTION: &'static [i32] = &[
        // State 0
        0, 3,
        // State 1
        -9, -9,
        // State 2
        -6, -6,
    ];
    const __EOF_ACTION: &'static [i32] = &[
        0,
        -9,
        -6,
    ];
    const __GOTO: &'static [i32] = &[
        // State 0
        0, 0, 0, 2, 0, 0, 0,
        // State 1
        0, 0, 0, 0, 0, 0, 0,
        // State 2
        0, 0, 0, 0, 0, 0, 0,
    ];
    fn __expected_tokens(__state: usize) -> Vec<::std::string::String> {
        const __TERMINAL: &'static [&'static str] = &[
            r###"".""###,
            r###"r#"[A-Za-z][A-Za-z0-9]*"#"###,
        ];
        __ACTION[(__state * 2)..].iter().zip(__TERMINAL).filter_map(|(&state, terminal)| {
            if state == 0 {
                None
            } else {
                Some(terminal.to_string())
            }
        }).collect()
    }
    pub fn parse_Ident<
        'input,
    >(
        input: &'input str,
    ) -> Result<Ident, __lalrpop_util::ParseError<usize, (usize, &'input str), ()>>
    {
        let mut __tokens = super::__intern_token::__Matcher::new(input);
        let mut __states = vec![0_i32];
        let mut __symbols = vec![];
        let mut __integer;
        let mut __lookahead;
        let mut __last_location = Default::default();
        '__shift: loop {
            __lookahead = match __tokens.next() {
                Some(Ok(v)) => v,
                None => break '__shift,
                Some(Err(e)) => return Err(e),
            };
            __last_location = __lookahead.2.clone();
            __integer = match __lookahead.1 {
                (1, _) if true => 0,
                (0, _) if true => 1,
                _ => {
                    let __state = *__states.last().unwrap() as usize;
                    let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: __expected_tokens(__state),
                    };
                    return Err(__error);
                }
            };
            '__inner: loop {
                let __state = *__states.last().unwrap() as usize;
                let __action = __ACTION[__state * 2 + __integer];
                if __action > 0 {
                    let __symbol = match __integer {
                        0 => match __lookahead.1 {
                            (1, __tok0) => __Symbol::Term_22_2e_22((__tok0)),
                            _ => unreachable!(),
                        },
                        1 => match __lookahead.1 {
                            (0, __tok0) => __Symbol::Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23((__tok0)),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    __states.push(__action - 1);
                    __symbols.push((__lookahead.0, __symbol, __lookahead.2));
                    continue '__shift;
                } else if __action < 0 {
                    if let Some(r) = __reduce(input, __action, Some(&__lookahead.0), &mut __states, &mut __symbols, ::std::marker::PhantomData::<()>) {
                        return r;
                    }
                } else {
                    let __state = *__states.last().unwrap() as usize;
                    let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: __expected_tokens(__state),
                    };
                    return Err(__error)
                }
            }
        }
        loop {
            let __state = *__states.last().unwrap() as usize;
            let __action = __EOF_ACTION[__state];
            if __action < 0 {
                if let Some(r) = __reduce(input, __action, None, &mut __states, &mut __symbols, ::std::marker::PhantomData::<()>) {
                    return r;
                }
            } else {
                let __state = *__states.last().unwrap() as usize;
                let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                    token: None,
                    expected: __expected_tokens(__state),
                };
                return Err(__error);
            }
        }
    }
    pub fn __reduce<
        'input,
    >(
        input: &'input str,
        __action: i32,
        __lookahead_start: Option<&usize>,
        __states: &mut ::std::vec::Vec<i32>,
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: ::std::marker::PhantomData<()>,
    ) -> Option<Result<Ident,__lalrpop_util::ParseError<usize, (usize, &'input str), ()>>>
    {
        let __nonterminal = match -__action {
            1 => {
                // ("." <Ident>) = ".", Ident => ActionFn(6);
                let __sym1 = __pop_NtIdent(__symbols);
                let __sym0 = __pop_Term_22_2e_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action6::<>(input, __sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29(__nt), __end));
                0
            }
            2 => {
                // ("." <Ident>)* =  => ActionFn(4);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action4::<>(input, &__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2a(__nt), __end));
                1
            }
            3 => {
                // ("." <Ident>)* = ("." <Ident>)+ => ActionFn(5);
                let __sym0 = __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action5::<>(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2a(__nt), __end));
                1
            }
            4 => {
                // ("." <Ident>)+ = ".", Ident => ActionFn(9);
                let __sym1 = __pop_NtIdent(__symbols);
                let __sym0 = __pop_Term_22_2e_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action9::<>(input, __sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__nt), __end));
                2
            }
            5 => {
                // ("." <Ident>)+ = ("." <Ident>)+, ".", Ident => ActionFn(10);
                let __sym2 = __pop_NtIdent(__symbols);
                let __sym1 = __pop_Term_22_2e_22(__symbols);
                let __sym0 = __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action10::<>(input, __sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__nt), __end));
                2
            }
            6 => {
                // Ident = r#"[A-Za-z][A-Za-z0-9]*"# => ActionFn(2);
                let __sym0 = __pop_Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2::<>(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtIdent(__nt), __end));
                3
            }
            7 => {
                // Path = Ident => ActionFn(11);
                let __sym0 = __pop_NtIdent(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action11::<>(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPath(__nt), __end));
                4
            }
            8 => {
                // Path = Ident, ("." <Ident>)+ => ActionFn(12);
                let __sym1 = __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__symbols);
                let __sym0 = __pop_NtIdent(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action12::<>(input, __sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtPath(__nt), __end));
                4
            }
            9 => {
                // __Ident = Ident => ActionFn(0);
                let __sym0 = __pop_NtIdent(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action0::<>(input, __sym0);
                return Some(Ok(__nt));
            }
            10 => {
                // __Path = Path => ActionFn(1);
                let __sym0 = __pop_NtPath(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action1::<>(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt____Path(__nt), __end));
                6
            }
            _ => panic!("invalid action code {}", __action)
        };
        let __state = *__states.last().unwrap() as usize;
        let __next_state = __GOTO[__state * 7 + __nonterminal] - 1;
        __states.push(__next_state);
        None
    }
    fn __pop_Term_22_2e_22<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_2e_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_22_2e_22_20_3cIdent_3e_29<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Ident, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2a<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, ::std::vec::Vec<Ident>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2b<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, ::std::vec::Vec<Ident>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtIdent<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Ident, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtIdent(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtPath<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Path, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtPath(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Ident<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Ident, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Ident(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Path<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Path, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Path(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
}
pub use self::__parse__Ident::parse_Ident;

mod __parse__Path {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use std::str::FromStr;
    use ast::*;
    use ascii::*;
    extern crate lalrpop_util as __lalrpop_util;
    #[allow(dead_code)]
    pub enum __Symbol<'input> {
        Term_22_2e_22(&'input str),
        Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23(&'input str),
        Nt_28_22_2e_22_20_3cIdent_3e_29(Ident),
        Nt_28_22_2e_22_20_3cIdent_3e_29_2a(::std::vec::Vec<Ident>),
        Nt_28_22_2e_22_20_3cIdent_3e_29_2b(::std::vec::Vec<Ident>),
        NtIdent(Ident),
        NtPath(Path),
        Nt____Ident(Ident),
        Nt____Path(Path),
    }
    const __ACTION: &'static [i32] = &[
        // State 0
        0, 4,
        // State 1
        6, 0,
        // State 2
        -10, -10,
        // State 3
        -6, -6,
        // State 4
        7, 0,
        // State 5
        0, 4,
        // State 6
        0, 4,
        // State 7
        -4, -4,
        // State 8
        -5, -5,
    ];
    const __EOF_ACTION: &'static [i32] = &[
        0,
        -7,
        -10,
        -6,
        -8,
        0,
        0,
        -4,
        -5,
    ];
    const __GOTO: &'static [i32] = &[
        // State 0
        0, 0, 0, 2, 3, 0, 0,
        // State 1
        0, 0, 5, 0, 0, 0, 0,
        // State 2
        0, 0, 0, 0, 0, 0, 0,
        // State 3
        0, 0, 0, 0, 0, 0, 0,
        // State 4
        0, 0, 0, 0, 0, 0, 0,
        // State 5
        0, 0, 0, 8, 0, 0, 0,
        // State 6
        0, 0, 0, 9, 0, 0, 0,
        // State 7
        0, 0, 0, 0, 0, 0, 0,
        // State 8
        0, 0, 0, 0, 0, 0, 0,
    ];
    fn __expected_tokens(__state: usize) -> Vec<::std::string::String> {
        const __TERMINAL: &'static [&'static str] = &[
            r###"".""###,
            r###"r#"[A-Za-z][A-Za-z0-9]*"#"###,
        ];
        __ACTION[(__state * 2)..].iter().zip(__TERMINAL).filter_map(|(&state, terminal)| {
            if state == 0 {
                None
            } else {
                Some(terminal.to_string())
            }
        }).collect()
    }
    pub fn parse_Path<
        'input,
    >(
        input: &'input str,
    ) -> Result<Path, __lalrpop_util::ParseError<usize, (usize, &'input str), ()>>
    {
        let mut __tokens = super::__intern_token::__Matcher::new(input);
        let mut __states = vec![0_i32];
        let mut __symbols = vec![];
        let mut __integer;
        let mut __lookahead;
        let mut __last_location = Default::default();
        '__shift: loop {
            __lookahead = match __tokens.next() {
                Some(Ok(v)) => v,
                None => break '__shift,
                Some(Err(e)) => return Err(e),
            };
            __last_location = __lookahead.2.clone();
            __integer = match __lookahead.1 {
                (1, _) if true => 0,
                (0, _) if true => 1,
                _ => {
                    let __state = *__states.last().unwrap() as usize;
                    let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: __expected_tokens(__state),
                    };
                    return Err(__error);
                }
            };
            '__inner: loop {
                let __state = *__states.last().unwrap() as usize;
                let __action = __ACTION[__state * 2 + __integer];
                if __action > 0 {
                    let __symbol = match __integer {
                        0 => match __lookahead.1 {
                            (1, __tok0) => __Symbol::Term_22_2e_22((__tok0)),
                            _ => unreachable!(),
                        },
                        1 => match __lookahead.1 {
                            (0, __tok0) => __Symbol::Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23((__tok0)),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    __states.push(__action - 1);
                    __symbols.push((__lookahead.0, __symbol, __lookahead.2));
                    continue '__shift;
                } else if __action < 0 {
                    if let Some(r) = __reduce(input, __action, Some(&__lookahead.0), &mut __states, &mut __symbols, ::std::marker::PhantomData::<()>) {
                        return r;
                    }
                } else {
                    let __state = *__states.last().unwrap() as usize;
                    let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: __expected_tokens(__state),
                    };
                    return Err(__error)
                }
            }
        }
        loop {
            let __state = *__states.last().unwrap() as usize;
            let __action = __EOF_ACTION[__state];
            if __action < 0 {
                if let Some(r) = __reduce(input, __action, None, &mut __states, &mut __symbols, ::std::marker::PhantomData::<()>) {
                    return r;
                }
            } else {
                let __state = *__states.last().unwrap() as usize;
                let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                    token: None,
                    expected: __expected_tokens(__state),
                };
                return Err(__error);
            }
        }
    }
    pub fn __reduce<
        'input,
    >(
        input: &'input str,
        __action: i32,
        __lookahead_start: Option<&usize>,
        __states: &mut ::std::vec::Vec<i32>,
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: ::std::marker::PhantomData<()>,
    ) -> Option<Result<Path,__lalrpop_util::ParseError<usize, (usize, &'input str), ()>>>
    {
        let __nonterminal = match -__action {
            1 => {
                // ("." <Ident>) = ".", Ident => ActionFn(6);
                let __sym1 = __pop_NtIdent(__symbols);
                let __sym0 = __pop_Term_22_2e_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action6::<>(input, __sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29(__nt), __end));
                0
            }
            2 => {
                // ("." <Ident>)* =  => ActionFn(4);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action4::<>(input, &__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2a(__nt), __end));
                1
            }
            3 => {
                // ("." <Ident>)* = ("." <Ident>)+ => ActionFn(5);
                let __sym0 = __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action5::<>(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2a(__nt), __end));
                1
            }
            4 => {
                // ("." <Ident>)+ = ".", Ident => ActionFn(9);
                let __sym1 = __pop_NtIdent(__symbols);
                let __sym0 = __pop_Term_22_2e_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action9::<>(input, __sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__nt), __end));
                2
            }
            5 => {
                // ("." <Ident>)+ = ("." <Ident>)+, ".", Ident => ActionFn(10);
                let __sym2 = __pop_NtIdent(__symbols);
                let __sym1 = __pop_Term_22_2e_22(__symbols);
                let __sym0 = __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action10::<>(input, __sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__nt), __end));
                2
            }
            6 => {
                // Ident = r#"[A-Za-z][A-Za-z0-9]*"# => ActionFn(2);
                let __sym0 = __pop_Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2::<>(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtIdent(__nt), __end));
                3
            }
            7 => {
                // Path = Ident => ActionFn(11);
                let __sym0 = __pop_NtIdent(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action11::<>(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPath(__nt), __end));
                4
            }
            8 => {
                // Path = Ident, ("." <Ident>)+ => ActionFn(12);
                let __sym1 = __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__symbols);
                let __sym0 = __pop_NtIdent(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action12::<>(input, __sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtPath(__nt), __end));
                4
            }
            9 => {
                // __Ident = Ident => ActionFn(0);
                let __sym0 = __pop_NtIdent(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action0::<>(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt____Ident(__nt), __end));
                5
            }
            10 => {
                // __Path = Path => ActionFn(1);
                let __sym0 = __pop_NtPath(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action1::<>(input, __sym0);
                return Some(Ok(__nt));
            }
            _ => panic!("invalid action code {}", __action)
        };
        let __state = *__states.last().unwrap() as usize;
        let __next_state = __GOTO[__state * 7 + __nonterminal] - 1;
        __states.push(__next_state);
        None
    }
    fn __pop_Term_22_2e_22<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_2e_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Termr_23_22_5bA_2dZa_2dz_5d_5bA_2dZa_2dz0_2d9_5d_2a_22_23(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_22_2e_22_20_3cIdent_3e_29<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Ident, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2a<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, ::std::vec::Vec<Ident>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_22_2e_22_20_3cIdent_3e_29_2b<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, ::std::vec::Vec<Ident>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_22_2e_22_20_3cIdent_3e_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtIdent<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Ident, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtIdent(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtPath<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Path, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtPath(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Ident<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Ident, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Ident(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Path<
      'input,
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Path, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Path(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
}
pub use self::__parse__Path::parse_Path;
mod __intern_token {
    #![allow(unused_imports)]
    use std::str::FromStr;
    use ast::*;
    use ascii::*;
    extern crate lalrpop_util as __lalrpop_util;
    extern crate regex as __regex;
    pub struct __Matcher<'input> {
        text: &'input str,
        consumed: usize,
        regex_set: __regex::RegexSet,
        regex_vec: Vec<__regex::Regex>,
    }

    impl<'input> __Matcher<'input> {
        pub fn new(s: &'input str) -> __Matcher<'input> {
            let __strs: &[&str] = &[
                "^(?u:[A-Za-z])(?u:[0-9A-Za-z])*",
                "^(?u:\\.)",
            ];
            let __regex_set = __regex::RegexSet::new(__strs).unwrap();
            let __regex_vec = vec![
                __regex::Regex::new("^(?u:[A-Za-z])(?u:[0-9A-Za-z])*").unwrap(),
                __regex::Regex::new("^(?u:\\.)").unwrap(),
            ];
            __Matcher {
                text: s,
                consumed: 0,
                regex_set: __regex_set,
                regex_vec: __regex_vec,
            }
        }
    }

    impl<'input> Iterator for __Matcher<'input> {
        type Item = Result<(usize, (usize, &'input str), usize), __lalrpop_util::ParseError<usize,(usize, &'input str),()>>;

        fn next(&mut self) -> Option<Self::Item> {
            let __text = self.text.trim_left();
            let __whitespace = self.text.len() - __text.len();
            let __start_offset = self.consumed + __whitespace;
            if __text.is_empty() {
                self.text = __text;
                self.consumed = __start_offset;
                None
            } else {
                let __matches = self.regex_set.matches(__text);
                if !__matches.matched_any() {
                    Some(Err(__lalrpop_util::ParseError::InvalidToken {
                        location: __start_offset,
                    }))
                } else {
                    let mut __longest_match = 0;
                    let mut __index = 0;
                    for __i in 0 .. 2 {
                        if __matches.matched(__i) {
                            let __match = self.regex_vec[__i].find(__text).unwrap();
                            let __len = __match.end();
                            if __len >= __longest_match {
                                __longest_match = __len;
                                __index = __i;
                            }
                        }
                    }
                    let __result = &__text[..__longest_match];
                    let __remaining = &__text[__longest_match..];
                    let __end_offset = __start_offset + __longest_match;
                    self.text = __remaining;
                    self.consumed = __end_offset;
                    Some(Ok((__start_offset, (__index, __result), __end_offset)))
                }
            }
        }
    }
}

#[allow(unused_variables)]
fn __action0<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, Ident, usize),
) -> Ident
{
    (__0)
}

#[allow(unused_variables)]
fn __action1<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, Path, usize),
) -> Path
{
    (__0)
}

#[allow(unused_variables)]
fn __action2<
    'input,
>(
    input: &'input str,
    (_, ident, _): (usize, &'input str, usize),
) -> Ident
{
    { Ident(AsciiString::from_str(ident).unwrap()) }
}

#[allow(unused_variables)]
fn __action3<
    'input,
>(
    input: &'input str,
    (_, first, _): (usize, Ident, usize),
    (_, v, _): (usize, ::std::vec::Vec<Ident>, usize),
) -> Path
{
    {
		let mut p = Vec::new();
		p.push(first);
		p.extend_from_slice(&v);
		Path(p)
	}
}

#[allow(unused_variables)]
fn __action4<
    'input,
>(
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::vec::Vec<Ident>
{
    vec![]
}

#[allow(unused_variables)]
fn __action5<
    'input,
>(
    input: &'input str,
    (_, v, _): (usize, ::std::vec::Vec<Ident>, usize),
) -> ::std::vec::Vec<Ident>
{
    v
}

#[allow(unused_variables)]
fn __action6<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, Ident, usize),
) -> Ident
{
    (__0)
}

#[allow(unused_variables)]
fn __action7<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, Ident, usize),
) -> ::std::vec::Vec<Ident>
{
    vec![__0]
}

#[allow(unused_variables)]
fn __action8<
    'input,
>(
    input: &'input str,
    (_, v, _): (usize, ::std::vec::Vec<Ident>, usize),
    (_, e, _): (usize, Ident, usize),
) -> ::std::vec::Vec<Ident>
{
    { let mut v = v; v.push(e); v }
}

#[allow(unused_variables)]
fn __action9<
    'input,
>(
    input: &'input str,
    __0: (usize, &'input str, usize),
    __1: (usize, Ident, usize),
) -> ::std::vec::Vec<Ident>
{
    let __start0 = __0.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action6(
        input,
        __0,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action7(
        input,
        __temp0,
    )
}

#[allow(unused_variables)]
fn __action10<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<Ident>, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Ident, usize),
) -> ::std::vec::Vec<Ident>
{
    let __start0 = __1.0.clone();
    let __end0 = __2.2.clone();
    let __temp0 = __action6(
        input,
        __1,
        __2,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action8(
        input,
        __0,
        __temp0,
    )
}

#[allow(unused_variables)]
fn __action11<
    'input,
>(
    input: &'input str,
    __0: (usize, Ident, usize),
) -> Path
{
    let __start0 = __0.2.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action4(
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action3(
        input,
        __0,
        __temp0,
    )
}

#[allow(unused_variables)]
fn __action12<
    'input,
>(
    input: &'input str,
    __0: (usize, Ident, usize),
    __1: (usize, ::std::vec::Vec<Ident>, usize),
) -> Path
{
    let __start0 = __1.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action5(
        input,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action3(
        input,
        __0,
        __temp0,
    )
}

pub trait __ToTriple<'input, > {
    type Error;
    fn to_triple(value: Self) -> Result<(usize,(usize, &'input str),usize),Self::Error>;
}

impl<'input, > __ToTriple<'input, > for (usize, (usize, &'input str), usize) {
    type Error = ();
    fn to_triple(value: Self) -> Result<(usize,(usize, &'input str),usize),()> {
        Ok(value)
    }
}
impl<'input, > __ToTriple<'input, > for Result<(usize, (usize, &'input str), usize),()> {
    type Error = ();
    fn to_triple(value: Self) -> Result<(usize,(usize, &'input str),usize),()> {
        value
    }
}
