macro_rules! mov {
    ($self: expr, $destination: expr, $source: expr) => {{
        match $destination {
            DataLocation::Local(l) => {
                $self.body.emit_line(&format!(
                        "mov {}, {}", l, $source));
            }

            DataLocation::Param(p) => {
                $self.body.emit_line(&format!(
                        "mov {}, {}", p, $source));
            }

            DataLocation::Register(r) => {
                $self.body.emit_line(&format!(
                        "mov {}, {}", r, $source));
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
