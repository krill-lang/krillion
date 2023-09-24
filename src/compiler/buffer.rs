pub struct Buffer<A> {
    pub buf: Vec<A>,
    pub idx: usize,
}

impl<A> Buffer<A> {
    pub fn empty() -> Self {
        Self {
            buf: Vec::new(),
            idx: 0,
        }
    }

    pub fn from_vec(v: Vec<A>) -> Self {
        Self {
            buf: v,
            idx: 0,
        }
    }

    pub fn next(&mut self) -> Option<&A> {
        self.idx += 1;
        self.buf.get(self.idx - 1)
    }
    pub fn peek(&self) -> Option<&A> {
        self.buf.get(self.idx)
    }

    pub fn current(&self) -> Option<&A> {
        self.buf.get(self.idx-1)
    }

    pub fn prev(&self) -> Option<&A> {
        self.buf.get(self.idx-2)
    }

    pub fn rewind(&mut self) {
        self.idx -= 1;
    }

    pub fn push(&mut self, i: A) {
        self.buf.push(i);
    }
}

use super::*;
pub fn to_atoken_buf<'a, A: Logos<'a>>(lex: &'a mut Lexer<'a, A>) -> Result<Buffer<(A, Span)>, Vec<ACompileError>> {
    let mut buf = Buffer::empty();
    let mut err: Vec<ACompileError> = Vec::new();

    while let Some(t) = lex.next() {
        if t.is_err() {
            err.push((Box::new(LexerError()), lex.span()));
        } else {
            buf.push((t.unwrap(), lex.span()));
        }
    }

    if err.len() != 0 {
        Err(err)
    } else {
        Ok(buf)
    }
}
