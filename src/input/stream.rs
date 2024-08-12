//! Note that [`char`] is a subset of code point.

use konst::{string::Chars, try_opt};

use super::code_points::{self, LF};

/// https://drafts.csswg.org/css-syntax-3/#css-filter-code-points
pub struct Filtered<'a>(pub Chars<'a>);

impl<'a> std::fmt::Debug for Filtered<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Filtered").field(&self.0.as_str()).finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FilteredChar(char);

impl FilteredChar {
    /// Panics if `s` contains not exactly one [`FilteredChar`].
    pub const fn from_str(s: &str) -> Self {
        match Filtered::new(s).next() {
            Some((c, stream)) => {
                assert!(stream.next().is_none(), "unexpected remaining code points");
                c
            }
            None => panic!("expect one code point"),
        }
    }

    pub const fn is_digit(&self) -> bool {
        self.0.is_ascii_digit()
    }

    /// https://drafts.csswg.org/css-syntax-3/#whitespace
    pub(crate) const fn is_whitespace(&self) -> bool {
        // this also checks code points that should be already filtered out
        self.0.is_ascii_whitespace()
    }

    /// https://drafts.csswg.org/css-syntax-3/#non-printable-code-point
    pub(crate) const fn is_non_printable(&self) -> bool {
        matches!(self.0, '\u{0000}'..='\u{0008}' | '\u{000B}' | '\u{000E}'..='\u{001F}' | '\u{007F}')
    }

    pub(crate) const REPLACEMENT_CHARACTER: Self = Self(char::REPLACEMENT_CHARACTER);

    pub(crate) const fn from_char(c: char) -> Self {
        assert!(!matches!(c, code_points::CR | code_points::FF | '\0'));

        Self(c)
    }
}

impl<'a> Filtered<'a> {
    pub const fn new(s: &'a str) -> Self {
        Self(konst::string::chars(s))
    }

    pub const fn consume_digits(self) -> (&'a str, Self) {
        let mut stream = self.copy();
        loop {
            match stream.copy().next() {
                Some((fc, new_stream)) if fc.is_digit() => stream = new_stream,
                _ => return (self.str_before(&stream), stream),
            }
        }
    }

    pub const fn first_n_code_points<const N: usize>(&self) -> FilteredCharVec<N> {
        self.copy().consume_n_code_points().0
    }

    pub const fn consume_n_code_points<const N: usize>(mut self) -> (FilteredCharVec<N>, Self) {
        let mut res = FilteredCharVec::EMPTY;

        while res.len < N {
            if let Some((fc, new_stream)) = self.copy().next() {
                res.array[res.len] = fc.to_char();
                self = new_stream;
                res.len += 1;
            } else {
                break;
            }
        }

        (res, self)
    }

    pub(crate) const fn assert_empty(&self) {
        assert!(self.0.as_str().is_empty(), "unexpected remaining tokens")
    }

    pub(crate) fn assert_empty_or_report(&self) {
        let s = self.0.as_str();
        assert!(s.is_empty(), "unexpected remaining tokens: {s:?}")
    }
}

impl Filtered<'_> {
    pub const EMPTY: Self = Self(konst::string::chars(""));

    pub const fn copy(&self) -> Self {
        Self(self.0.copy())
    }
    pub const fn next(self) -> Option<(FilteredChar, Self)> {
        if let Some((u, this)) = self.0.next() {
            const CR: char = '\u{000D}';
            let u = match u {
                // U+000C FORM FEED (FF)
                '\u{000C}' => LF,
                // U+000D CARRIAGE RETURN (CR)
                CR => {
                    if let Some((CR, this)) = this.copy().next() {
                        if let Some((LF, this)) = this.next() {
                            return Some((FilteredChar(LF), Self(this)));
                        } else {
                            LF
                        }
                    } else {
                        LF
                    }
                }
                // U+0000 NULL
                '\u{0000}' => '\u{FFFD}',
                u => u,
            };
            Some((FilteredChar(u), Self(this)))
        } else {
            return None;
        }
    }

    /// The `bool` is true if there are char(s) filtered.
    pub(crate) const fn next_and_report(self) -> Option<(FilteredChar, bool, Self)> {
        if let Some((u, this)) = self.0.next() {
            const CR: char = '\u{000D}';
            let (u, is_filtered) = match u {
                // U+000C FORM FEED (FF)
                '\u{000C}' => (LF, true),
                // U+000D CARRIAGE RETURN (CR)
                CR => (
                    if let Some((CR, this)) = this.copy().next() {
                        if let Some((LF, this)) = this.next() {
                            return Some((FilteredChar(LF), true, Self(this)));
                        } else {
                            LF
                        }
                    } else {
                        LF
                    },
                    true,
                ),
                // U+0000 NULL
                '\u{0000}' => ('\u{FFFD}', true),
                u => (u, false),
            };
            Some((FilteredChar(u), is_filtered, Self(this)))
        } else {
            return None;
        }
    }

    pub const fn next_two(self) -> Option<([FilteredChar; 2], Self)> {
        let (a, this) = try_opt!(self.next());
        let (b, this) = try_opt!(this.next());
        Some(([a, b], this))
    }
}

impl<'a> Filtered<'a> {
    pub const fn str_before(&self, new_stream: &Self) -> &'a str {
        let this = self.0.as_str();
        let new_stream = new_stream.0.as_str();
        assert!(this.len() >= new_stream.len());

        konst::string::split_at(this, this.len() - new_stream.len()).0
    }

    pub const fn str_before_non_empty(&self, new_stream: &Self) -> Option<&'a str> {
        let s = self.str_before(new_stream);
        if s.is_empty() {
            None
        } else {
            Some(s)
        }
    }
}

impl FilteredChar {
    pub const fn to_char(self) -> char {
        self.0
    }
}

/// Consider this as <code> [Vec]<[FilteredChar]> </code>
#[derive(Debug, Clone, Copy)]
pub struct FilteredCharVec<const N: usize> {
    // The first `len` chars are valid FilteredChar
    // The last `N - len` chars are filled with `'\0'`.
    array: [char; N],
    len: usize,
}

impl<const N: usize> FilteredCharVec<N> {
    const EMPTY: Self = Self {
        array: ['\0'; N],
        len: 0,
    };

    pub const fn new_filled(arr: [FilteredChar; N]) -> Self {
        let mut array = ['\0'; N];

        let mut i = 0;

        while i < N {
            array[i] = arr[i].to_char();
            i += 1;
        }

        Self { array, len: N }
    }

    /// The last `N - self.len` chars are filled with `'\0'`.
    pub const fn to_chars_padding_zero(self) -> [char; N] {
        self.array
    }

    pub const fn as_slice(&self) -> FilteredCharSlice<'_> {
        FilteredCharSlice {
            inner: self.as_char_slice(),
        }
    }

    pub const fn as_char_slice(&self) -> &[char] {
        self.array.split_at(self.len).0
    }

    pub const fn crop_and_fit<const M: usize>(self, remove_first_n: usize) -> FilteredCharVec<M> {
        let mut res = FilteredCharVec::EMPTY;

        if self.len > remove_first_n {
            let new_len = self.len - remove_first_n;
            assert!(new_len <= M);

            let chars = self.array.split_at(remove_first_n).1;

            while res.len < new_len {
                res.array[res.len] = chars[res.len];
                res.len += 1;
            }
        }

        res
    }

    pub const fn truncate_and_fit<const M: usize>(self, keep_first_n: usize) -> FilteredCharVec<M> {
        const fn min(a: usize, b: usize) -> usize {
            if a > b {
                b
            } else {
                a
            }
        }

        let new_len = min(self.len, keep_first_n);

        assert!(new_len <= M);

        let mut res = FilteredCharVec::EMPTY;

        while res.len < new_len {
            res.array[res.len] = self.array[res.len];
            res.len += 1;
        }

        res
    }

    pub const fn fit_or_keep_first_n<const M: usize>(self) -> FilteredCharVec<M> {
        let mut res = FilteredCharVec::EMPTY;
        let this = self.as_char_slice();
        while res.len < M && res.len < this.len() {
            res.array[res.len] = this[res.len];
            res.len += 1;
        }

        res
    }

    pub const fn first(&self) -> Option<FilteredChar> {
        if self.len > 0 {
            Some(FilteredChar(self.array[0]))
        } else {
            None
        }
    }

    pub const fn with_capacity<const M: usize>(self) -> FilteredCharVec<M> {
        assert!(M >= self.len);
        let mut res = FilteredCharVec::EMPTY;

        while res.len < self.len {
            res.array[res.len] = self.array[res.len];
        }

        res
    }

    pub const fn join_and_fit<const M: usize>(
        self,
        chars: FilteredCharSlice<'_>,
    ) -> FilteredCharVec<M> {
        let chars = chars.to_char_slice();

        assert!(self.len + chars.len() <= M);

        let mut res = FilteredCharVec::EMPTY;

        while res.len < self.len {
            res.array[res.len] = chars[res.len];
            res.len += 1;
        }

        let mut i = 0;

        while i < chars.len() {
            res.array[res.len] = chars[i];
            res.len += 1;
            i += 1;
        }

        res
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FilteredCharSlice<'a> {
    // must be valid FilteredChar
    inner: &'a [char],
}

impl<'a> FilteredCharSlice<'a> {
    pub const fn to_char_slice(self) -> &'a [char] {
        self.inner
    }
}

#[cfg(test)]
mod tests {
    use super::Filtered;

    const _: () = {
        let chars = Filtered::new("\\").first_n_code_points::<3>();
        assert!(matches!(chars.as_char_slice(), ['\\']));

        assert!(matches!(chars.crop_and_fit::<2>(1).as_char_slice(), []));

        assert!(matches!(
            chars.fit_or_keep_first_n::<2>().as_char_slice(),
            ['\\']
        ));
    };
}
