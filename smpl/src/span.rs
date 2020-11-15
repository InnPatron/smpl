use std::fmt;

pub type Location = usize;

#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: Location,
    end: Location,
}

impl Span {
    pub fn new(start: Location, end: Location) -> Self {
        Span { start, end }
    }

    pub fn dummy() -> Self {
        Span { start: 0, end: 0 }
    }
}

impl<'a> fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]:[{}]", self.start, self.end)
    }
}
