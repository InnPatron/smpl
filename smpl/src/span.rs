pub type Span = LocationSpan;

#[derive(Clone, Debug)]
pub struct LocationSpan {
    source: String,
    start: Location,
    end: Location,
}

#[allow(dead_code)]
impl LocationSpan {
    pub fn new(source: String, start: Location, end: Location) -> LocationSpan {
        LocationSpan {
            source: source,
            start: start,
            end: end,
        }
    }

    pub fn span_1(source: String, start: Location, char_size: usize) -> LocationSpan {
        let mut end = start.clone();
        end.byte_index += char_size;
        end.char_index += 1;
        end.column += 1;

        LocationSpan::new(source, start, end)
    }

    pub fn dummy() -> LocationSpan {
        let d_loc = Location::new(
            0,
            0,
            0,
            0,
        );

        LocationSpan::new("dummy".to_string(), d_loc.clone(), d_loc)
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

        LocationSpan::new(l.source, start, end)
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
    pub fn new(
        byte_index: usize,
        char_index: usize,
        line: usize,
        column: usize,
    ) -> Location {
        Location {
            byte_index: byte_index,
            char_index: char_index,
            line: line,
            column: column,
        }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}
