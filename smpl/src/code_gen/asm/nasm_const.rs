/*
 * Sources:
 *
 *  1) System V Application Binary Interface
 *  AMD64 Architecture Processor Supplement
 *  Draft Version 0.99.6a
 *
 *  Alias: AMD64_ABI
 *
 *  2) Intel® 64 and IA-32 Architectures
 *  Software Developer’s Manual
 *  Combined Volumes:
 *  1, 2A, 2B, 2C, 2D, 3A, 3B, 3C, 3D and 4
 *
 *  Alias: INTEL_INSTR
 *
 *
 *
 *
 *
 *
 *
 */

// Source: INTEL_INSTR 89 (vol. 1)
#[derive(Clone, Copy, PartialEq)]
pub enum SIZES {
    QWORD,
    DWORD,
    WORD,
    BYTE,
}

impl ::std::fmt::Display for SIZES {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            SIZES::QWORD => write!(f, "QWORD"),
            SIZES::DWORD => write!(f, "DWORD"),
            SIZES::WORD => write!(f, "WORD"),
            SIZES::BYTE => write!(f, "BYTE"),
        }
    }
}

impl SIZES {
    fn size(s: SIZES) -> usize {
        match s {
            SIZES::QWORD => QWORD,
            SIZES::DWORD => DWORD,
            SIZES::WORD => WORD,
            SIZES::BYTE => BYTE,
        }
    }
}

pub const QWORD: usize = 8;
pub const DWORD: usize = 4;
pub const WORD: usize = 2;
pub const BYTE: usize = 1;

// Source: AMD64_ABI 12
pub const FLOAT_SIZE: usize = 4;
pub const INT_SIZE: usize = 4;
pub const BOOL_SIZE: usize = 1;

// Source: AMD64_ABI 13
pub const TRUE: i8 = 1;
pub const FALSE: i8 = 0;

pub const BYTE_ALIGNMENT: usize = 8;
pub const POINTER_SIZE: usize = 8;
pub const REGISTER_SIZE: usize = 8;
pub const LOCAL_BLOCK_SIZE: usize = 8;
pub const MAX_BLOCKS: usize = 500;
