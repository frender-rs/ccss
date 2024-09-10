pub use self::whitespace::Whitespace;

use crate::input::Filtered;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WhitespaceToken<'a>(&'a str);

impl<'a> WhitespaceToken<'a> {
    pub const ONE_SPACE: Self = WhitespaceToken(" ");

    pub const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
        let mut new_stream = stream.copy();
        let original = stream;
        while let Some((_, s)) = Whitespace::consume(new_stream.copy()) {
            new_stream = s
        }

        let v = konst::option::map!(original.str_before_non_empty(&new_stream), Self);

        (v, new_stream)
    }

    #[cfg(test)]
    pub(crate) const fn new(s: &'a str) -> Self {
        let (this, stream) = Self::consume(Filtered::new(s));

        if let Some(this) = this {
            stream.assert_empty();
            this
        } else {
            panic!("expect whitespace")
        }
    }
}

mod whitespace {
    use crate::input::{code_points::LF, Filtered};

    /// https://drafts.csswg.org/css-syntax-3/#whitespace
    pub struct Whitespace(char);

    impl Whitespace {
        const fn try_new(c: char) -> Option<Self> {
            // A newline, U+0009 CHARACTER TABULATION, or U+0020 SPACE
            match c {
                LF | '\u{0009}' | '\u{0020}' => Some(Self(c)),
                _ => None,
            }
        }

        pub const fn consume(stream: Filtered) -> Option<(Whitespace, Filtered)> {
            if let Some((fc, stream)) = stream.next() {
                if let Some(this) = Self::try_new(fc.to_char()) {
                    return Some((this, stream));
                }
            }

            None
        }
    }
}
