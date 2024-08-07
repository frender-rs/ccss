use crate::input::{
    code_points::{is_ident_code_point, is_ident_start_code_point, HYPHEN_MINUS, REVERSE_SOLIDUS},
    Filtered, FilteredCharVec,
};

use super::escaped_code_point::EscapedCodePoint;

/// https://drafts.csswg.org/css-syntax-3/#consume-name
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IdentSequence<'a>(&'a str);

impl<'a> IdentSequence<'a> {
    pub const fn to_str(self) -> &'a str {
        self.0
    }

    pub const fn match_url_ascii_case_insensitive(&self) -> bool {
        match self.0.as_bytes() {
            [b'u' | b'U', b'r' | b'R', b'l' | b'L'] => true,
            _ => false,
        }
    }

    /// Returns `Some(..)` if and only if [`IdentSequence::would_start(&stream)`](Self::would_start) returns true.
    pub const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
        if !Self::would_start(&stream) {
            return (None, stream);
        }

        let (ident, stream) = Self::consume_anyway(stream);

        assert!(!ident.to_str().is_empty());

        (Some(ident), stream)
    }

    /// consume anyway, without confirming that [would_start](Self::would_start) returns true.
    ///
    /// returned IdentSequence might be empty
    pub const fn consume_anyway(stream: Filtered<'a>) -> (Self, Filtered<'a>) {
        let original_stream = stream.copy();
        let mut remaining = stream;

        while let Some((fc, new_stream)) = remaining.copy().next() {
            match fc.to_char() {
                u if is_ident_code_point(u) => {
                    remaining = new_stream;
                }
                REVERSE_SOLIDUS => {
                    if let Ok((Some(e), new_stream)) = EscapedCodePoint::consume(new_stream) {
                        let _ = e;
                        remaining = new_stream;
                    } else {
                        // LF | EOF
                        // don't set remaining = new_stream
                        break;
                    }
                }
                _ => {
                    // don't set remaining = new_stream
                    break;
                }
            }
        }

        let s = original_stream.str_before(&remaining);

        (Self(s), remaining)
    }

    /// https://drafts.csswg.org/css-syntax-3/#would-start-an-identifier
    pub const fn would_start(stream: &Filtered<'a>) -> bool {
        Self::chars_would_start(stream.first_n_code_points())
    }

    pub const fn chars_would_start(chars: FilteredCharVec<3>) -> bool {
        match chars.to_chars_padding_zero() {
            [HYPHEN_MINUS, b, c]
                if (b == HYPHEN_MINUS || is_ident_start_code_point(b))
                    || EscapedCodePoint::chars_would_start(chars.crop_and_fit(1)) =>
            {
                true
            }
            [a, _, _] if is_ident_start_code_point(a) => true,
            _ => EscapedCodePoint::chars_would_start(chars.fit_or_keep_first_n()),
        }
    }
}
