pub type Span = LocationSpan;

#[derive(Copy, Clone, Debug)]
pub struct LocationSpan {
    start: Location,
    end: Location,
}

#[allow(dead_code)]
impl LocationSpan {
    pub fn new(start: Location, end: Location) -> LocationSpan {
        LocationSpan {
            start: start,
            end: end,
        }
    }

    pub fn span_1(start: Location, char_size: usize) -> LocationSpan {
        let mut end = start.clone();
        end.byte_index += char_size;
        end.char_index += 1;
        end.column += 1;

        LocationSpan::new(start, end)
    }

    pub fn dummy() -> LocationSpan {
        LocationSpan::new(Location::new(0, 0, 0, 0), Location::new(0, 0, 0, 0))
    }

    pub fn start(&self) -> Location {
        self.start
    }

    pub fn end(&self) -> Location {
        self.end
    }

    pub fn combine(l: LocationSpan, r: LocationSpan) -> LocationSpan {
        let start = if l.start().byte_index < r.start().byte_index {
            l.start()
        } else {
            r.start()
        };

        let end = if l.end().byte_index > r.end().byte_index {
            l.end()
        } else {
            r.end()
        };

        LocationSpan::new(start, end)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Location {
    pub byte_index: usize,
    pub char_index: usize,
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn new(byte_index: usize, char_index: usize, line: usize, column: usize) -> Location {
        Location {
            byte_index: byte_index,
            char_index: char_index,
            line: line,
            column: column,
        }
    }
}

impl Default for Location {
    fn default() -> Location {
        Location::new(0, 0, 1, 1)
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}
