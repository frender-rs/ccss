use crate::input::{
    code_points::{LF, REVERSE_SOLIDUS},
    Filtered, FilteredChar, FilteredCharVec,
};

use super::{errors::Eof, hex_digit::HexDigit, hex_digits::HexDigits, whitespace::Whitespace};

/// https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point
pub enum EscapedCodePoint {
    HexDigits(HexDigits<6>),
    Other(FilteredChar),
}

impl EscapedCodePoint {
    /// It assumes that the `U+005C REVERSE SOLIDUS (\)` has already been consumed.
    ///
    /// Returns Ok(None) if and only if next code point is [`LF`]
    ///
    /// Returns Err(Eof) if and only if stream is empty.
    ///
    /// https://drafts.csswg.org/css-syntax-3/#check-if-two-code-points-are-a-valid-escape
    ///
    /// https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point
    pub(crate) const fn consume(stream: Filtered) -> Result<(Option<Self>, Filtered), Eof> {
        let (fc, mut this) = match stream.next() {
            Some(v) => v,
            None => return Err(Eof),
        };

        Ok(match fc.to_char() {
            LF => (None, this),
            u => {
                if let Some(d) = HexDigit::try_new(u) {
                    let mut hex_digits = HexDigits::new().with(d);

                    while hex_digits.can_push() {
                        let d;
                        (d, this) = HexDigit::consume(this);

                        if let Some(d) = d {
                            hex_digits = hex_digits.with(d);
                        } else {
                            break;
                        }
                    }

                    if let Some((ws, new_this)) = Whitespace::consume(this.copy()) {
                        this = new_this;
                        hex_digits = hex_digits.with_whitespace(ws);
                    }

                    (Some(EscapedCodePoint::HexDigits(hex_digits)), this)
                } else {
                    (Some(EscapedCodePoint::Other(fc)), this)
                }
            }
        })
    }

    /// https://drafts.csswg.org/css-syntax-3/#starts-with-a-valid-escape
    pub const fn chars_would_start(chars: FilteredCharVec<2>) -> bool {
        match chars.to_chars_padding_zero() {
            [REVERSE_SOLIDUS, b] => b != LF,
            _ => false,
        }
    }
}
