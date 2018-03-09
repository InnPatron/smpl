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
