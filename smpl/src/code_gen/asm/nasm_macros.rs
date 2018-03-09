macro_rules! mov {
    ($destination: expr, $source: expr) => {{
        match $destination {
            DataLocation::Local(l) => {
                format!("mov {}, {}", l, $source)
            }

            DataLocation::Param(p) => {
                format!("mov {}, {}", p, $source)
            }

            DataLocation::Register(r) => {
                format!("mov {}, {}", r, $source)
            }
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
