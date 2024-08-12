use super::errors::Eof;

use crate::input::{
    code_points::{LEFT_PARENTHESIS, REVERSE_SOLIDUS, RIGHT_PARENTHESIS},
    Filtered,
};

use super::escaped_code_point::EscapedCodePoint;
use super::whitespace::WhitespaceToken;
use super::{ident::IdentToken, ident_sequence::IdentSequence};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UrlToken<'a> {
    full: &'a str,
    url: IdentSequence<'a>,
    value: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionToken<'a> {
    full: &'a str,
    value: IdentSequence<'a>,
}

impl<'a> FunctionToken<'a> {
    pub(crate) const fn value(&self) -> IdentSequence<'a> {
        self.value
    }
}

/// https://drafts.csswg.org/css-syntax-3/#consume-ident-like-token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentLikeToken<'a> {
    Ident(IdentToken<'a>),
    Function(FunctionToken<'a>),
    Url(UrlToken<'a>),
}

impl<'a> IdentLikeToken<'a> {
    pub const fn consume(
        stream: Filtered<'a>,
    ) -> Result<(Option<Self>, Filtered<'a>), UrlParseError> {
        let original = stream.copy();
        let (string, stream) = IdentSequence::consume(stream);
        if let Some(string) = string {
            match Self::consume_after_ident_sequence(string, stream, original) {
                Ok((v, stream)) => Ok((Some(v), stream)),
                Err(err) => Err(err),
            }
        } else {
            Ok((None, stream))
        }
    }

    const fn consume_after_ident_sequence(
        string: IdentSequence<'a>,
        stream: Filtered<'a>,
        original: Filtered<'a>,
    ) -> Result<(Self, Filtered<'a>), UrlParseError<'a>> {
        match stream.copy().next() {
            Some((fc, new_stream)) if fc.to_char() == LEFT_PARENTHESIS => {
                if string.match_url_ascii_case_insensitive() {
                    let (_, new_stream) = WhitespaceToken::consume(new_stream);
                    if matches!(
                        new_stream
                            .first_n_code_points::<1>()
                            .to_chars_padding_zero(),
                        ['"' | '\'']
                    ) {
                        return Ok((
                            Self::Function(FunctionToken {
                                full: original.str_before(&new_stream),
                                value: string,
                            }),
                            new_stream,
                        ));
                    } else {
                        // consume a url token
                        match UrlToken::consume_after_url_left_paren(new_stream) {
                            Ok((value, stream)) => {
                                return Ok((
                                    Self::Url(UrlToken {
                                        full: original.str_before(&stream),
                                        url: string,
                                        value,
                                    }),
                                    stream,
                                ));
                            }
                            Err(err) => return Err(err),
                        }
                    }
                } else {
                    return Ok((
                        Self::Function(FunctionToken {
                            full: original.str_before(&new_stream),
                            value: string,
                        }),
                        new_stream,
                    ));
                }
            }
            _ => Ok((Self::Ident(IdentToken::from_ident_sequence(string)), stream)),
        }
    }
}

#[derive(Debug)]
pub enum UrlParseError<'a> {
    BadUrl { remaining: Filtered<'a> },
    Eof,
}

impl<'a> UrlToken<'a> {
    /// This algorithm assumes that the initial "url(" has already been consumed.
    /// This algorithm also assumes that it’s being called to consume an "unquoted" value, like url(foo).
    ///
    /// https://drafts.csswg.org/css-syntax-3/#consume-url-token
    const fn consume_after_url_left_paren(
        stream: Filtered<'a>,
    ) -> Result<(&'a str, Filtered<'a>), UrlParseError<'a>> {
        let (_, mut stream) = WhitespaceToken::consume(stream);
        let after_whitespace = stream.copy();

        loop {
            let old = stream.copy();
            match stream.next() {
                Some((fc, new_stream)) => match fc.to_char() {
                    RIGHT_PARENTHESIS => {
                        let value = after_whitespace.str_before(&old);
                        return Ok((value, new_stream));
                    }
                    _ if fc.is_whitespace() => {
                        let (_, stream) = WhitespaceToken::consume(new_stream);
                        let before_right_paren = stream.copy();
                        match stream.next() {
                            Some((fc, stream)) if fc.to_char() == RIGHT_PARENTHESIS => {
                                let value = after_whitespace.str_before(&before_right_paren);
                                return Ok((value, stream));
                            }
                            Some(_) => {
                                return Err(UrlParseError::BadUrl {
                                    remaining: before_right_paren,
                                })
                            }
                            None => return Err(UrlParseError::Eof),
                        }
                    }
                    u if matches!(u, '"' | '\'' | '(') || fc.is_non_printable() => {
                        return Err(UrlParseError::BadUrl {
                            remaining: new_stream,
                        });
                    }
                    REVERSE_SOLIDUS => match EscapedCodePoint::consume(new_stream) {
                        Ok((Some(_), new_stream)) => {
                            stream = new_stream;
                        }
                        Ok((None, _)) => {
                            // a REVERSE_SOLIDUS followed by LF
                            return Err(UrlParseError::BadUrl { remaining: old });
                        }
                        Err(Eof) => {
                            // a REVERSE_SOLIDUS followed by EOF
                            return Err(UrlParseError::BadUrl { remaining: old });
                        }
                    },
                    _ => stream = new_stream,
                },
                None => return Err(UrlParseError::Eof),
            }
        }
    }
}
