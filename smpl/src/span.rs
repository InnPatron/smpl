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

    pub fn start(&self) -> Location {
        self.start
    }

    pub fn end(&self) -> Location {
        self.end
    }

    pub fn combine(l: LocationSpan, r: LocationSpan) -> LocationSpan {
        let start = if l.start().byte_index() < r.start().byte_index() {
            l.start()
        } else {
            r.start()
        };

        let end = if l.end().byte_index() > r.end().byte_index() {
            l.end()
        } else {
            r.end()
        };

        LocationSpan::new(start, end)
    }
}


#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Location {
    byte_index: usize,
    char_index: usize,
    line: usize,
    column: usize,
}

#[allow(dead_code)]
impl Location {
    pub fn byte_index(&self) -> usize {
        self.byte_index
    }

    pub fn char_index(&self) -> usize {
        self.char_index
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }
}

impl Location {
    fn new(byte_index: usize, char_index: usize, line: usize, column: usize) -> Location {
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
