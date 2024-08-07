use crate::input::{
    code_points::{LF, REVERSE_SOLIDUS},
    Filtered, FilteredChar,
};

use super::{errors::Eof, escaped_code_point::EscapedCodePoint};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringToken<'a> {
    full: &'a str,
    starting_code_point_str: &'a str,
    ending_code_point_str: &'a str,
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
            Ok((ending_code_point_str, stream)) => Ok((
                Some(Self {
                    full: original.str_before(&stream),
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
    ) -> Result<(&'a str, Filtered<'a>), ConsumeAfterStartingCodePointError> {
        loop {
            let current_stream = stream.copy();

            if let Some((fc, new_stream)) = stream.next() {
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
                                stream = new_stream;
                            }
                            Ok((Some(_), new_stream)) => {
                                // a valid escape code point
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
                        let ending_code_point_str = current_stream.str_before(&new_stream);
                        return Ok((ending_code_point_str, new_stream));
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
}

pub(crate) enum ConsumeAfterStartingCodePointError<'a> {
    Eof,
    Newline {
        stream_starting_with_new_line: Filtered<'a>,
    },
}
