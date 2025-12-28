#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn increment(&mut self) {
        self.end+=1;
    }

    pub fn decrement(&mut self) {
        self.start-=1;
    }

    pub fn increment_end(&mut self) {}
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