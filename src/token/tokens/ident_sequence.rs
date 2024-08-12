use crate::{
    collections::array_vec::ArrayVec,
    input::{
        code_points::{
            is_ident_code_point, is_ident_start_code_point, HYPHEN_MINUS, REVERSE_SOLIDUS,
        },
        Filtered, FilteredCharVec,
    },
};

use super::escaped_code_point::EscapedCodePoint;

#[derive(Clone, Copy, Eq)]
struct Repr<'a> {
    full: &'a str,
    has_filtered_chars_or_escaped_code_points: bool,
}

impl<'a> PartialEq for Repr<'a> {
    fn eq(&self, other: &Self) -> bool {
        // equality of has_filtered_chars_or_escaped_code_points is necessary and insufficient for ident's equality
        // has_filtered_chars_or_escaped_code_points is compared first because it is more performant than str::eq
        self.has_filtered_chars_or_escaped_code_points
            == other.has_filtered_chars_or_escaped_code_points
            && self.full == other.full
    }
}

impl<'a> std::fmt::Debug for Repr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.has_filtered_chars_or_escaped_code_points {
            write!(f, "EscapedOrFiltered({:?})", self.full)
        } else {
            self.full.fmt(f)
        }
    }
}

/// Two IdentSequence eq if and only if their original str eq.
/// https://drafts.csswg.org/css-syntax-3/#consume-name
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IdentSequence<'a>(Repr<'a>);

struct Chars<'a> {
    ident_seq: Filtered<'a>,
}

impl<'a> Chars<'a> {
    /// The returned char may be filterer char like `'\0', '\r', FF`.
    const fn into_next(self) -> Option<(char, Self)> {
        if let Some((fc, new_stream)) = self.ident_seq.next() {
            let (c, new_stream) = match fc.to_char() {
                u if is_ident_code_point(u) => (u, new_stream),
                REVERSE_SOLIDUS => {
                    if let Ok((Some(e), new_stream)) = EscapedCodePoint::consume_after_reverse_solidus(new_stream) {
                        (e.to_code_point(), new_stream)
                    } else {
                        // LF | EOF
                        unreachable!()
                    }
                }
                _ => {
                    unreachable!()
                }
            };

            Some((
                c,
                Self {
                    ident_seq: new_stream,
                },
            ))
        } else {
            None
        }
    }

    const fn collect_first_n_chars<const N: usize>(mut self) -> (ArrayVec<char, N>, Self) {
        let mut res = ArrayVec::EMPTY;

        while res.len() < N {
            if let Some((ch, this)) = self.into_next() {
                res = res.with_push(ch);
                self = this;
            } else {
                self = Self {
                    ident_seq: Filtered::EMPTY,
                };
                break;
            }
        }

        (res, self)
    }
}

const fn chars_eq_ignore_ascii_case(this: &[char], other: &[char]) -> bool {
    this.len() == other.len() && {
        let mut i = 0;

        while i < this.len() {
            if !this[i].eq_ignore_ascii_case(&other[i]) {
                return false;
            }
            i += 1;
        }

        true
    }
}

impl<'a> IdentSequence<'a> {
    const fn chars(&self) -> Chars<'a> {
        Chars {
            ident_seq: Filtered::new(self.0.full),
        }
    }

    pub(crate) const fn match_url_ascii_case_insensitive(&self) -> bool {
        const EXPECTED: [char; 3] = ['u', 'r', 'l'];
        let (chars, _) = self
            .chars()
            .collect_first_n_chars::<{ EXPECTED.len() + 1 }>();

        chars_eq_ignore_ascii_case(&EXPECTED, chars.as_slice())
    }

    /// Returns `Some(..)` if and only if [`IdentSequence::would_start(&stream)`](Self::would_start) returns true.
    pub const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
        if !Self::would_start(&stream) {
            return (None, stream);
        }

        let (ident, stream) = Self::consume_anyway(stream);

        assert!(!ident.original_str().is_empty());

        (Some(ident), stream)
    }

    /// consume anyway, without confirming that [would_start](Self::would_start) returns true.
    ///
    /// returned IdentSequence might be empty
    pub(crate) const fn consume_anyway(stream: Filtered<'a>) -> (Self, Filtered<'a>) {
        let original_stream = stream.copy();
        let mut remaining = stream;

        let mut has_filtered_chars_or_escaped_code_points = false;

        while let Some((fc, is_filtered, new_stream)) = remaining.copy().next_and_report() {
            if is_filtered {
                has_filtered_chars_or_escaped_code_points = true;
            }
            match fc.to_char() {
                u if is_ident_code_point(u) => {
                    remaining = new_stream;
                }
                REVERSE_SOLIDUS => {
                    if let Ok((Some(e), new_stream)) = EscapedCodePoint::consume_after_reverse_solidus(new_stream) {
                        has_filtered_chars_or_escaped_code_points = true;
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

        (
            Self(Repr {
                full: s,
                has_filtered_chars_or_escaped_code_points,
            }),
            remaining,
        )
    }

    /// https://drafts.csswg.org/css-syntax-3/#would-start-an-identifier
    pub const fn would_start(stream: &Filtered<'a>) -> bool {
        Self::chars_would_start(stream.first_n_code_points())
    }

    pub const fn chars_would_start(chars: FilteredCharVec<3>) -> bool {
        match chars.to_chars_padding_zero() {
            [HYPHEN_MINUS, b, _]
                if (b == HYPHEN_MINUS || is_ident_start_code_point(b))
                    || EscapedCodePoint::chars_would_start(chars.crop_and_fit(1)) =>
            {
                true
            }
            [a, _, _] if is_ident_start_code_point(a) => true,
            _ => EscapedCodePoint::chars_would_start(chars.fit_or_keep_first_n()),
        }
    }

    /// The returned str may contain [escaped code points](https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point).
    pub(crate) const fn original_str(&self) -> &'a str {
        self.0.full
    }

    pub(crate) const fn matches_chars(&self, chars: &[char]) -> bool {
        let mut this = self.chars();

        let mut i = 0;

        while i < chars.len() {
            this = match this.into_next() {
                Some((ch, this)) if ch == chars[i] => this,
                _ => {
                    return false;
                }
            };

            i += 1;
        }

        this.into_next().is_none()
    }

    pub(crate) const fn matches_important_ascii_case_insensitive(&self) -> bool {
        const EXPECTED: [char; 9] = ['i', 'm', 'p', 'o', 'r', 't', 'a', 'n', 't'];
        let (chars, _) = self
            .chars()
            .collect_first_n_chars::<{ EXPECTED.len() + 1 }>();

        chars_eq_ignore_ascii_case(&EXPECTED, chars.as_slice())
    }
}

#[cfg(feature = "alloc")]
mod alloc {
    use alloc::{borrow::Cow, string::String};

    use super::IdentSequence;

    impl<'a> IdentSequence<'a> {
        pub fn unescape(self) -> Cow<'a, str> {
            if self.0.has_filtered_chars_or_escaped_code_points {
                // TODO: unescaped_str_len can be computed when tokenizing
                let unescaped_str_len = self.0.full.len(); // currently this might be larger
                let mut s = String::with_capacity(unescaped_str_len);

                let mut chars = self.chars();

                while let Some((ch, new_chars)) = chars.into_next() {
                    chars = new_chars;
                    s.push(ch);
                }

                Cow::Owned(s)
            } else {
                Cow::Borrowed(self.0.full)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::input::Filtered;

    use super::IdentSequence;

    const _: () = {
        assert!(IdentSequence::would_start(&Filtered::new(r"\-")));
        assert!(!IdentSequence::would_start(&Filtered::new("\\")));
    };

    #[test]
    #[cfg(feature = "alloc")]
    fn test_escape_zero() {
        use alloc::string::ToString;

        let (v, remaining) = IdentSequence::consume(Filtered::new("\\\0"));
        remaining.assert_empty();
        assert_eq!(
            v.unwrap().unescape(),
            char::REPLACEMENT_CHARACTER.to_string()
        );
    }

    const _: () = {
        let (v, remaining) = IdentSequence::consume(Filtered::new(r"\d"));
        remaining.assert_empty();
        let Some(v) = v else { panic!() };

        assert!(v.matches_chars(&['\r']));
    };

    #[test]
    #[cfg(feature = "alloc")]
    fn test_escape_ff() {
        use alloc::string::ToString;

        let expected = crate::input::code_points::FF.to_string();
        const INPUT: &str = r"\c";
        let (v, remaining) = IdentSequence::consume(Filtered::new(INPUT));

        remaining.assert_empty();

        let v = v.unwrap();

        assert_eq!(v.original_str(), INPUT);
        assert_eq!(v.unescape(), expected);
    }
}
