use std::collections::HashMap;

use ast::Ident;

use analysis::smpl_type::*;
use analysis::metadata::*;
use analysis::{Universe, TypeId};

const BYTE_ALIGNMENT: usize = 8;

pub struct X86_64Backend {
    type_sizes: HashMap<TypeId, usize>,
    struct_layouts: HashMap<TypeId, StructLayout>,
    byte_alignment: usize,
}

impl X86_64Backend {

    pub fn new(universe: &Universe) -> X86_64Backend {
        let std_sizes = vec![
            // (universe.unit(), 0),
            (universe.int(), 4),
            (universe.float(), 4),
            // (universe.string(), 8),
            (universe.boolean(), 1),
        ];


        X86_64Backend {
            type_sizes: std_sizes.into_iter().collect(),
            struct_layouts: HashMap::new(),
            byte_alignment: BYTE_ALIGNMENT,
        }
    }

    fn size(&self, id: TypeId) -> usize {
        *self.type_sizes.get(&id).unwrap()
    }
}

struct StructLayout {
    fields: HashMap<Ident, (usize, usize)>,     // (offset, size)
    size: usize,
}

impl StructLayout {
    pub fn new(backend: &X86_64Backend, universe: &Universe, 
               struct_t: &StructType, meta: &StructMetadata) -> StructLayout {

        let mut fields = HashMap::new();

        let mut total_size = 0;
        let mut current_offset = 0;
        for field in meta.order() {
            let field_type = *(struct_t.fields.get(field).unwrap());
            let field_size = backend.size(field_type);
            let padding = field_size % backend.byte_alignment;

            let total_field_size = field_size + padding;

            fields.insert(field.clone(), (current_offset, total_field_size));

            current_offset += total_field_size;
            total_size += total_field_size;
        }
        
        StructLayout {
            fields: fields,
            size: total_size,
        }
    }
}

enum Location {
    Stack(usize, usize),
    Register(Register),
}

enum Register {
    // 64-bit   Lower 32 bites  Lower 16 bits   Lower 8 bites
    RAX,        EAX,            AX,             AL,
    RBX,        EBX,            BX,             BL,
    RCX,        ECX,            CX,             CL,
    RDX,        EDX,            DX,             DL,
    RSI,        ESI,            SI,             SIL,
    RDI,        EDI,            DI,             DIL,
    RBP,        EBP,            BP,             BPL,
    RSP,        ESP,            SP,             SPL,
    R8,         R8D,            R8W,            R8B,
    R9,         R9D,            R9W,            R9B,
    R10,        R10D,           R10W,           R10B,
    R11,        R11D,           R11W,           R11B,
    R12,        R12D,           R12W,           R12B,
    R13,        R13D,           R13W,           R13B,
    R14,        R14D,           R14W,           R14B,
    R15,        R15D,           R15W,           R15B,
}

impl ::std::fmt::Display for Register {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use self::Register::*;

        let str = match *self {
            RAX => "rax",
            EAX => "eax",
            AX => "ax",   
            AL => "al",

            RBX => "rbx", 
            EBX => "ebx",   
            BX => "bx",   
            BL => "bl",

            RCX => "rcx", 
            ECX => "ecx",  
            CX => "cx",   
            CL => "cl",

            RDX => "rdx",
            EDX => "edx",   
            DX => "dx",   
            DL => "dl",

            RSI => "rsi", 
            ESI => "esi",   
            SI => "si",   
            SIL => "sil",

            RDI => "rdi",
            EDI => "edi",
            DI => "di",   
            DIL => "dil",

            RBP => "rbp",
            EBP => "ebp",    
            BP => "bp",   
            BPL => "bpl",

            RSP => "rsp",
            ESP => "esp",   
            SP => "sp",   
            SPL => "spl",

            R8 => "r8",
            R8D => "r8d",     
            R8W => "r8w",   
            R8B => "r8b",

            R9 => "r9",
            R9D => "r9d",     
            R9W => "r9w",   
            R9B => "r9b",

            R10 => "r10",
            R10D => "r10d",     
            R10W => "r10w",   
            R10B => "r10b",

            R11 => "r11",
            R11D => "r11d",     
            R11W => "r11w",   
            R11B => "r11b",

            R12 => "r12",
            R12D => "r12d",     
            R12W => "r12w",   
            R12B => "r12b",

            R13 => "r13",
            R13D => "r13d",     
            R13W => "r13w",   
            R13B => "r13b",

            R14 => "r14",
            R14D => "r14d",     
            R14W => "r14w",   
            R14B => "r14b",

            R15 => "r15",
            R15D => "r15d",     
            R15W => "r15w",   
            R15B => "r15b",
        };

        write!(fmt, "{}", str)
    }
}
