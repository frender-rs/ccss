use super::hex_digits::HexDigits;
use crate::input::Filtered;

/// https://drafts.csswg.org/css-syntax-3/#hex-digit
#[derive(Clone, Copy)]
pub struct HexDigit(char);
impl HexDigit {
    pub const fn to_char(self) -> char {
        self.0
    }

    pub const fn test(u: char) -> bool {
        #[allow(clippy::match_like_matches_macro)] // for rustfmt
        match u {
            '\u{0030}'..='\u{0039}' | '\u{0041}'..='\u{0046}' | '\u{0061}'..='\u{0066}' => true,
            _ => false,
        }
    }

    pub const fn try_new(u: char) -> Option<Self> {
        if Self::test(u) {
            Some(Self(u))
        } else {
            None
        }
    }

    pub const fn consume(stream: Filtered) -> (Option<Self>, Filtered) {
        let original = stream.copy();
        if let Some((fc, this)) = stream.next() {
            if let Some(d) = Self::try_new(fc.to_char()) {
                return (Some(d), this);
            }
        }

        (None, original)
    }

    pub const fn consume_at_most_n<const N: usize>(
        mut stream: Filtered,
    ) -> (HexDigits<N>, Filtered) {
        let mut res = HexDigits::new();
        while res.can_push() {
            let d;
            (d, stream) = HexDigit::consume(stream);

            if let Some(d) = d {
                res = res.with(d);
            } else {
                break;
            }
        }

        (res, stream)
    }
}
