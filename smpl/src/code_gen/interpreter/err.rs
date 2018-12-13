use failure::Fail;

#[derive(Fail, Debug)]
pub enum InternalError {
    #[fail(display = "Invalid number of arguments. Found {}. {}", _0, _1)]
    InvalidArgCount(usize, ExpectedArgCount),
    #[fail(display = "Unexpected argument type at {}. Found {}. Expected {}", index, found, expected)]
    InvalidArgType {
        index: usize,
        found: String,
        expected: String,
    },
}

#[derive(Fail, Debug)]
pub enum ExpectedArgCount {
    #[fail(display = "Expected {}..={}", _0, _1)]
    Range(usize, usize),
    #[fail(display = "Expected >= {}", _0)]
    Min(usize),
    #[fail(display = "Expected <= {}", _0)]
    Max(usize),
    #[fail(display = "Expected == {}", _0)]
    Exact(usize),
}
