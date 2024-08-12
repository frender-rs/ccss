use crate::input::{
    code_points::{LF, REVERSE_SOLIDUS},
    Filtered, FilteredChar,
};

use super::{errors::Eof, escaped_code_point::EscapedCodePoint};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct StringToken<'a> {
    full: &'a str,
    starting_code_point_str: &'a str,
    value_original_str: &'a str,
    has_filtered_chars_or_escaped_code_points: bool,
    ending_code_point_str: &'a str,
}

impl<'a> std::fmt::Debug for StringToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("StringToken").field(&self.full).finish()
    }
}

#[derive(Debug)]
pub enum StringTokenParseError<'a> {
    Eof {
        full: &'a str,
        starting_code_point_str: &'a str,
        expected_ending_code_point: FilteredChar,
    },
    /// Unexpected [`LF`]
    Newline {
        before_new_line: &'a str,
        stream_starting_with_new_line: Filtered<'a>,
    },
}

impl<'a> StringToken<'a> {
    /// Unlike [`Self::consume_after_starting_code_point`], this method assumes that the starting code point hasn't been consumed yet.
    ///
    /// This method will return `Some(..)` if and only if
    /// the first code point of stream is `U+0022 QUOTATION MARK (")` or `U+0027 APOSTROPHE (')`.
    ///
    pub const fn consume(
        stream: Filtered<'a>,
    ) -> Result<(Option<Self>, Filtered<'a>), StringTokenParseError> {
        let original = stream.copy();

        let (fc, stream) = stream.consume_n_code_points::<1>();

        if !matches!(fc.to_chars_padding_zero(), ['"' | '\'']) {
            return Ok((None, original));
        }

        let Some(fc) = fc.first() else { unreachable!() };

        let starting_code_point_str = original.str_before(&stream);
        match Self::consume_after_starting_code_point(stream, fc) {
            Ok(ConsumeAfterStartingCodePoint {
                value_original_str,
                has_filtered_chars_or_escaped_code_points,
                ending_code_point_str,
                remaining: stream,
            }) => Ok((
                Some(Self {
                    full: original.str_before(&stream),
                    value_original_str,
                    has_filtered_chars_or_escaped_code_points,
                    starting_code_point_str,
                    ending_code_point_str,
                }),
                stream,
            )),
            Err(err) => Err(match err {
                ConsumeAfterStartingCodePointError::Eof => StringTokenParseError::Eof {
                    full: original.0.as_str(),
                    starting_code_point_str,
                    expected_ending_code_point: fc,
                },
                ConsumeAfterStartingCodePointError::Newline {
                    stream_starting_with_new_line,
                } => StringTokenParseError::Newline {
                    before_new_line: original.str_before(&stream_starting_with_new_line),
                    stream_starting_with_new_line,
                },
            }),
        }
    }

    /// `Ok(..)` returns `ending_code_point_str` and the new stream after `ending_code_point`.
    ///
    /// https://drafts.csswg.org/css-syntax-3/#consume-string-token
    const fn consume_after_starting_code_point(
        mut stream: Filtered<'a>,
        ending_code_point: FilteredChar,
    ) -> Result<ConsumeAfterStartingCodePoint<'a>, ConsumeAfterStartingCodePointError> {
        let after_starting_code_point = stream.copy();
        let mut has_filtered_chars_or_escaped_code_points = false;
        loop {
            let current_stream = stream.copy();

            if let Some((fc, has_filtered, new_stream)) = stream.next_and_report() {
                if has_filtered {
                    has_filtered_chars_or_escaped_code_points = true;
                }
                match fc.to_char() {
                    LF => {
                        return Err(ConsumeAfterStartingCodePointError::Newline {
                            stream_starting_with_new_line: current_stream,
                        });
                    }
                    REVERSE_SOLIDUS => {
                        match EscapedCodePoint::consume(new_stream.copy()) {
                            Ok((None, new_stream)) => {
                                // LF
                                has_filtered_chars_or_escaped_code_points = true;
                                stream = new_stream;
                            }
                            Ok((Some(_), new_stream)) => {
                                // a valid escape code point
                                has_filtered_chars_or_escaped_code_points = true;
                                stream = new_stream;
                            }
                            Err(Eof) => {
                                // EOF
                                // do nothing and let next iteration process EOF
                                stream = new_stream
                            }
                        }
                    }
                    u if u == ending_code_point.to_char() => {
                        let value_original_str =
                            after_starting_code_point.str_before(&current_stream);
                        let ending_code_point_str = current_stream.str_before(&new_stream);
                        return Ok(ConsumeAfterStartingCodePoint {
                            value_original_str,
                            has_filtered_chars_or_escaped_code_points,
                            ending_code_point_str,
                            remaining: new_stream,
                        });
                    }
                    _ => {
                        // anything else
                        stream = new_stream
                    }
                }
            } else {
                return Err(ConsumeAfterStartingCodePointError::Eof);
            }
        }
    }

    fn chars(&self) -> Chars<'a> {
        Chars {
            string_token_value: Filtered::new(self.value_original_str),
        }
    }
}

pub(crate) enum ConsumeAfterStartingCodePointError<'a> {
    Eof,
    Newline {
        stream_starting_with_new_line: Filtered<'a>,
    },
}

struct ConsumeAfterStartingCodePoint<'a> {
    value_original_str: &'a str,
    has_filtered_chars_or_escaped_code_points: bool,
    ending_code_point_str: &'a str,
    remaining: Filtered<'a>,
}

struct Chars<'a> {
    string_token_value: Filtered<'a>,
}

impl<'a> Chars<'a> {
    const fn into_next(self) -> Option<(char, Self)> {
        let mut stream = self.string_token_value;

        loop {
            stream = if let Some((fc, new_stream)) = stream.next() {
                match fc.to_char() {
                    LF => {
                        unreachable!()
                    }
                    REVERSE_SOLIDUS => {
                        match EscapedCodePoint::consume(new_stream.copy()) {
                            Ok((None, new_stream)) => {
                                // LF
                                // consume it but don't append it to <string-token>'s value
                                new_stream
                            }
                            Ok((Some(e), new_stream)) => {
                                return Some((
                                    e.to_code_point(),
                                    Self {
                                        string_token_value: new_stream,
                                    },
                                ));
                            }
                            Err(Eof) => {
                                // EOF
                                // do nothing and let next iteration process EOF
                                unreachable!()
                            }
                        }
                    }
                    ch => {
                        // anything else
                        return Some((
                            ch,
                            Self {
                                string_token_value: new_stream,
                            },
                        ));
                    }
                }
            } else {
                return None;
            }
        }
    }
}

#[cfg(feature = "alloc")]
mod alloc {
    use alloc::{borrow::Cow, string::String};

    use super::StringToken;

    impl<'a> StringToken<'a> {
        pub fn value_unescape(&self) -> Cow<'a, str> {
            if self.has_filtered_chars_or_escaped_code_points {
                let mut chars = self.chars();

                let mut res = String::with_capacity(self.value_original_str.len());

                loop {
                    if let Some((ch, new_chars)) = chars.into_next() {
                        chars = new_chars;
                        res.push(ch);
                    } else {
                        break;
                    }
                }

                Cow::Owned(res)
            } else {
                Cow::Borrowed(self.value_original_str)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "alloc")]
    #[test]
    fn unescape() {
        use crate::input::Filtered;

        use super::StringToken;

        let (s, remaining) = StringToken::consume(Filtered::new("'\\\n'")).unwrap();
        remaining.assert_empty();
        let s = s.unwrap();

        assert_eq!(s.full, "'\\\n'");
        assert_eq!(s.starting_code_point_str, "'");
        assert_eq!(s.value_original_str, "\\\n");
        assert_eq!(s.has_filtered_chars_or_escaped_code_points, true);
        assert_eq!(s.ending_code_point_str, "'");

        assert_eq!(s.value_unescape(), "");
    }
}
