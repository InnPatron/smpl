macro_rules! mov {
    ($destination: expr, $source: expr) => {{
        format!("mov {}, {}", $destination, $source)
    }}
}

macro_rules! location {
    ($location: expr) => {{
        if let DataLocation::Register(r) = $location {
            format!("{}", r)
        } else {
            stack_offset!($location)
        }
    }}
}

macro_rules! stack_offset {
    ($location: expr) => {{
        match $location {
            DataLocation::Local(l) => {
                format!("[rbp - {}]", l)
            }

            DataLocation::Param(p) => {
                format!("[rbp + {}]", p)
            }

            DataLocation::Register(_) => unreachable!(),
        }
    }}
}

macro_rules! bin_op {
    ($op: expr) => {{
        match $op {
            BinOp::Add => "add",
            BinOp::Sub => "sub",
            BinOp::Mul => "mul",
            BinOp::Div => "div",

            _ => unimplemented!(),
        }
    }}
}
