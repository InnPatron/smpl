/*
 * Sources:
 *
 *  1) System V Application Binary Interface
 *  AMD64 Architecture Processor Supplement
 *  Draft Version 0.99.6a
 *
 *  Alias: AMD64_ABI
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

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
