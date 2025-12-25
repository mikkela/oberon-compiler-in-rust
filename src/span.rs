#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn next(&self) -> Self {
        Self { start: self.end, end: self.end + 1 }
    }
}

#[derive(Clone, Debug)]
pub struct SourceFile {
    pub name: String,
    pub text: String,
}

impl SourceFile {
    pub fn new(name: impl Into<String>, text: impl Into<String>) -> Self {
        Self { name: name.into(), text: text.into() }
    }

    pub fn slice(&self, sp: Span) -> &str {
        &self.text[sp.start..sp.end]
    }
}