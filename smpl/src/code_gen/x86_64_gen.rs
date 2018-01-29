pub struct X86_64Backend {

}

enum Paramater {
    Offset(usize),      // In bytes
    Register(Register),
}

struct LocalVariable {
    offset: usize,
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
