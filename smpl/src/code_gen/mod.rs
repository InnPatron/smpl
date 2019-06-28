struct StringEmitter {
    output: String,
    shift: u32,
}

#[allow(dead_code)]
impl StringEmitter {
    pub fn new() -> StringEmitter {
        StringEmitter {
            output: String::new(),
            shift: 0,
        }
    }

    pub fn consume(self) -> String {
        self.output
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub fn shift(&self) -> u32 {
        self.shift
    }

    pub fn set_shift(&mut self, shift: u32) {
        self.shift = shift;
    }

    pub fn shift_left(&mut self) {
        if self.shift() > 0 {
            let shift = self.shift() - 1;
            self.set_shift(shift);
        }
    }

    pub fn shift_right(&mut self) {
        if self.shift() < u32::max_value() {
            let shift = self.shift() + 1;
            self.set_shift(shift);
        }
    }

    pub fn line_pad(&mut self) {
        self.output.push('\n');
    }

    pub fn emit(&mut self, str: &str) {
        self.output.push_str(str);
    }

    pub fn emit_fmt(&mut self, str: &str) {
        self.emit_shift();
        self.output.push_str(str);
    }

    pub fn emit_line(&mut self, line: &str) {
        self.emit_shift();
        self.output.push_str(line);
        self.output.push('\n');
    }

    fn emit_shift(&mut self) {
        for _ in 0..self.shift() {
            self.output.push('\t');
        }
    }
}
