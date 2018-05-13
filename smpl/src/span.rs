#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    start: usize,   // Inclusive
    end: usize,     // Exclusive
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span {
            start: start,
            end: end
        }
    }

    pub fn flatten(spans: &[Span]) -> Span {
        let mut start = None;
        let mut end = None;

        for span in spans {
            match start {
                Some(startPos) => {
                    if span.start() < startPos {
                        start = Some(span.start);
                    }
                }

                None => start = Some(span.end()),
            }

            match end {
                Some(endPos) => {
                    if span.end() < endPos {
                        end = Some(span.end());
                    }
                }

                None => end = Some(span.end()),
            }
        }

        Span::new(start.unwrap(), end.unwrap())
    }

    pub fn combine(s1: Span, s2: Span) -> Span {
        let start = if s1.start() < s2.start() {
            s1.start()
        } else {
            s2.start()
        };

        let end = if s1.end() > s2.end() {
            s1.end()
        } else {
            s2.end()
        };

        Span {
            start: start,
            end: end
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }
}
