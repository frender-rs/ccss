use crate::collections::HasConstDummyValue;
use crate::input::{code_points::is_ident_code_point, Filtered, FilteredChar, FilteredCharVec};

use super::escaped_code_point::EscapedCodePoint;
use super::ident_like::{FunctionToken, IdentLikeToken, UrlParseError};

use super::ident_sequence::IdentSequence;
use super::simple_token::{
    LeftCurlyBracket, LeftParenthesis, LeftSquareBracket, RightCurlyBracket, RightParenthesis,
    RightSquareBracket,
};
use super::{
    numeric_token::NumericToken,
    simple_token::SimpleToken,
    string::{StringToken, StringTokenParseError},
    whitespace::WhitespaceToken,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CdoToken<'a> {
    full: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CdcToken<'a> {
    full: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AtKeywordToken<'a> {
    full: &'a str,
    ident: IdentSequence<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DelimToken<'a> {
    full: &'a str,
    value: FilteredChar,
}

impl<'a> DelimToken<'a> {
    pub const BANG: Self = Self {
        full: "!",
        value: FilteredChar::from_str("!"),
    };

    pub const fn value(&self) -> FilteredChar {
        self.value
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HashToken<'a> {
    full: &'a str,
    kind: HashTokenKind,
    value: IdentSequence<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HashTokenKind {
    Empty,
    /// type flag is "id"
    Id,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    Whitespace(WhitespaceToken<'a>),
    StringToken(StringToken<'a>),
    Simple(SimpleToken<'a>),
    Numeric(NumericToken<'a>),
    IdentLike(IdentLikeToken<'a>),
    Cdc(CdcToken<'a>),
    Cdo(CdoToken<'a>),
    AtKeyword(AtKeywordToken<'a>),
    Delim(DelimToken<'a>),
    Hash(HashToken<'a>),
}

impl<'a> HasConstDummyValue for Token<'a> {
    const DUMMY_VALUE: Self = Token::Whitespace(WhitespaceToken::ONE_SPACE);
}

/// The input stream starts with a `U+005C REVERSE SOLIDUS (\)`
/// but it doesn't start with a valid escape.
#[derive(Debug)]
pub struct UnexpectedReverseSolidusError<'a> {
    /// the stream that starts with a `U+005C REVERSE SOLIDUS (\)`
    #[allow(unused)]
    stream: Filtered<'a>,
}

#[derive(Debug)]
pub enum TokenParseError<'a> {
    StringToken(StringTokenParseError<'a>),
    Url(UrlParseError<'a>),
    UnexpectedReverseSolidus(UnexpectedReverseSolidusError<'a>),
}

impl<'a> TokenParseError<'a> {
    pub(crate) const DUMMY: Self = Self::Url(UrlParseError::Eof);
}

pub enum TokenParseOutput<'a> {
    TokenAndRemaining(Token<'a>, Filtered<'a>),
    // An valid EOF. Not an unexpected EOF.
    Eof,
}

pub type TokenParseResult<'a, T> = Result<T, TokenParseError<'a>>;

impl<'a> Token<'a> {
    /// https://drafts.csswg.org/css-syntax-3/#consume-token
    // unicode ranges allowed is false
    pub const fn consume(stream: Filtered<'a>) -> TokenParseResult<TokenParseOutput<'a>> {
        // TODO: Consume comments.

        const fn out<'a, Err>(
            token: Token<'a>,
            stream: Filtered<'a>,
        ) -> Result<TokenParseOutput<'a>, Err> {
            Ok(TokenParseOutput::TokenAndRemaining(token, stream))
        }

        let (wst, stream) = WhitespaceToken::consume(stream);
        if let Some(wst) = wst {
            return out(Self::Whitespace(wst), stream);
        }

        let stream = match StringToken::consume(stream) {
            Ok((None, s)) => s,
            Ok((Some(st), stream)) => {
                return out(Self::StringToken(st), stream);
            }
            Err(err) => return Err(TokenParseError::StringToken(err)),
        };

        let (st, stream) = SimpleToken::consume(stream);
        if let Some(st) = st {
            return out(Self::Simple(st), stream);
        }

        let (nt, stream) = NumericToken::consume(stream);
        if let Some(nt) = nt {
            return out(Self::Numeric(nt), stream);
        }

        let before_delim = stream.copy();
        let Some((fc, stream)) = stream.next() else {
            return Ok(TokenParseOutput::Eof);
        };

        struct ShouldProcessDelimToken;

        let after_delim = stream.copy();

        let ShouldProcessDelimToken = match fc.to_char() {
            // U+0023 NUMBER SIGN (#)
            '#' => {
                let two = stream.first_n_code_points::<2>();

                if matches!(two.to_chars_padding_zero(), [a, _] if is_ident_code_point(a))
                    || EscapedCodePoint::chars_would_start(two)
                {
                    // hash token
                    let mut kind = HashTokenKind::Empty;
                    if IdentSequence::would_start(&stream) {
                        kind = HashTokenKind::Id;
                    }

                    let (ident, stream) = IdentSequence::consume_anyway(stream);

                    assert!(!ident.to_str().is_empty());

                    return out(
                        Self::Hash(HashToken {
                            full: before_delim.str_before(&stream),
                            kind,
                            value: ident,
                        }),
                        stream,
                    );
                } else {
                    ShouldProcessDelimToken
                }
            }
            '+' => ShouldProcessDelimToken,
            '-' => {
                let (two, stream) = stream.consume_n_code_points::<2>();
                if matches!(two.to_chars_padding_zero(), ['-', '>']) {
                    return out(
                        Self::Cdc(CdcToken {
                            full: before_delim.str_before(&stream),
                        }),
                        stream,
                    );
                }

                if IdentSequence::chars_would_start(
                    FilteredCharVec::new_filled([fc]).join_and_fit(two.as_slice()),
                ) {
                    match IdentLikeToken::consume(before_delim) {
                        Ok((Some(ident), stream)) => return out(Self::IdentLike(ident), stream),
                        Ok((None, _)) => unreachable!(),
                        Err(err) => return Err(TokenParseError::Url(err)),
                    }
                } else {
                    ShouldProcessDelimToken
                }
            }
            '<' => {
                let (fcs, stream) = stream.consume_n_code_points::<3>();
                match fcs.to_chars_padding_zero() {
                    ['!', '-', '-'] => {
                        return out(
                            Self::Cdo(CdoToken {
                                full: before_delim.str_before(&stream),
                            }),
                            stream,
                        );
                    }
                    _ => ShouldProcessDelimToken,
                }
            }
            '@' => {
                let (ident, stream) = IdentSequence::consume(stream);

                if let Some(ident) = ident {
                    return out(
                        Self::AtKeyword(AtKeywordToken {
                            full: before_delim.str_before(&stream),
                            ident,
                        }),
                        stream,
                    );
                } else {
                    ShouldProcessDelimToken
                }
            }
            '\\' => {
                if EscapedCodePoint::chars_would_start(before_delim.first_n_code_points()) {
                    match IdentLikeToken::consume(before_delim) {
                        Ok((Some(ident), stream)) => return out(Self::IdentLike(ident), stream),
                        Ok((None, _)) => {
                            unreachable!()
                        }
                        Err(err) => return Err(TokenParseError::Url(err)),
                    }
                } else {
                    return Err(TokenParseError::UnexpectedReverseSolidus(
                        UnexpectedReverseSolidusError {
                            stream: before_delim,
                        },
                    ));
                }
            }
            c if is_ident_code_point(c) => match IdentLikeToken::consume(before_delim) {
                Ok((Some(ident), stream)) => return out(Self::IdentLike(ident), stream),
                Ok((None, _)) => {
                    unreachable!()
                }
                Err(err) => return Err(TokenParseError::Url(err)),
            },
            _ => ShouldProcessDelimToken,
        };

        out(
            Self::Delim(DelimToken {
                full: before_delim.str_before(&after_delim),
                value: fc,
            }),
            after_delim,
        )
    }

    pub(crate) const fn as_simple_block_starting_token(
        self,
    ) -> Option<SimpleBlockStartingToken<'a>> {
        match self {
            Self::Simple(SimpleToken::LeftCurlyBracket(a)) => {
                Some(SimpleBlockStartingToken::CurlyBracket(a))
            }
            Self::Simple(SimpleToken::LeftSquareBracket(a)) => {
                Some(SimpleBlockStartingToken::SquareBracket(a))
            }
            Self::Simple(SimpleToken::LeftParenthesis(a)) => {
                Some(SimpleBlockStartingToken::Parenthesis(a))
            }
            _ => None,
        }
    }

    pub(crate) const fn as_function_token(self) -> Option<FunctionToken<'a>> {
        match self {
            Self::IdentLike(IdentLikeToken::Function(t)) => Some(t),
            _ => None,
        }
    }

    pub(crate) const fn as_right_parenthesis(self) -> Option<RightParenthesis<'a>> {
        match self {
            Self::Simple(SimpleToken::RightParenthesis(a)) => Some(a),
            _ => None,
        }
    }

    pub(crate) const fn is_whitespace(&self) -> bool {
        matches!(self, Self::Whitespace(_))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SimpleBlockStartingToken<'a> {
    CurlyBracket(LeftCurlyBracket<'a>),
    SquareBracket(LeftSquareBracket<'a>),
    Parenthesis(LeftParenthesis<'a>),
}

impl<'a> SimpleBlockStartingToken<'a> {
    pub(crate) const fn try_surround_with(
        self,
        token: Token<'a>,
    ) -> Option<SimpleBlockSurroundingTokens<'a>> {
        match (self, token) {
            (
                SimpleBlockStartingToken::CurlyBracket(a),
                Token::Simple(SimpleToken::RightCurlyBracket(b)),
            ) => Some(SimpleBlockSurroundingTokens::CurlyBracket(a, b)),
            (
                SimpleBlockStartingToken::SquareBracket(a),
                Token::Simple(SimpleToken::RightSquareBracket(b)),
            ) => Some(SimpleBlockSurroundingTokens::SquareBracket(a, b)),
            (
                SimpleBlockStartingToken::Parenthesis(a),
                Token::Simple(SimpleToken::RightParenthesis(b)),
            ) => Some(SimpleBlockSurroundingTokens::Parenthesis(a, b)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SimpleBlockEndingToken<'a> {
    CurlyBracket(RightCurlyBracket<'a>),
    SquareBracket(RightSquareBracket<'a>),
    Parenthesis(RightParenthesis<'a>),
}

#[derive(Debug, Clone, Copy)]
pub enum SimpleBlockSurroundingTokens<'a> {
    CurlyBracket(LeftCurlyBracket<'a>, RightCurlyBracket<'a>),
    SquareBracket(LeftSquareBracket<'a>, RightSquareBracket<'a>),
    Parenthesis(LeftParenthesis<'a>, RightParenthesis<'a>),
}

#[derive(Debug, Clone, Copy)]
pub enum SimpleBlockKind {
    CurlyBracket,
    SquareBracket,
    Parenthesis,
}

#[cfg(test)]
mod tests {
    use crate::{
        input::Filtered,
        token::tokens::{IdentLikeToken, IdentToken, TokenParseOutput},
    };

    use super::Token;

    const _: () = {
        match Token::consume(Filtered::new("\\-")) {
            Ok(out) => match out {
                super::TokenParseOutput::TokenAndRemaining(t, remaining) => {
                    remaining.assert_empty();
                    match t {
                        Token::IdentLike(IdentLikeToken::Ident(ident)) => {
                            match ident.to_str().as_bytes() {
                                b"\\-" => {}
                                _ => panic!(),
                            }
                        }
                        _ => {
                            panic!()
                        }
                    }
                }
                super::TokenParseOutput::Eof => panic!(),
            },
            Err(_) => panic!(),
        }
    };

    #[test]
    fn escape_ident() {
        let out = Token::consume(Filtered::new("\\-")).unwrap();

        let TokenParseOutput::TokenAndRemaining(token, remaining) = out else {
            panic!("eof")
        };

        remaining.assert_empty_or_report();

        assert_eq!(
            token,
            Token::IdentLike(IdentLikeToken::Ident(IdentToken::new_const("\\-")))
        );
    }
}
