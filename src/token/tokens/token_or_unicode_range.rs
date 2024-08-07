use crate::input::Filtered;

use super::{
    token::{Token, TokenParseOutput, TokenParseResult},
    unicode_range::UnicodeRangeToken,
};

pub enum TokenOrUnicodeRange<'a> {
    Token(Token<'a>),
    UnicodeRange(UnicodeRangeToken<'a>),
}

pub enum TokenOrUnicodeRangeParseOutput<'a> {
    TokenAndRemaining(TokenOrUnicodeRange<'a>, Filtered<'a>),
    // An valid EOF. Not an unexpected EOF.
    Eof,
}

impl<'a> TokenOrUnicodeRange<'a> {
    /// https://drafts.csswg.org/css-syntax-3/#consume-token
    // unicode ranges allowed is true
    pub const fn consume(
        stream: Filtered<'a>,
    ) -> TokenParseResult<TokenOrUnicodeRangeParseOutput<'a>> {
        let (u, stream) = UnicodeRangeToken::consume(stream);
        if let Some(u) = u {
            Ok(TokenOrUnicodeRangeParseOutput::TokenAndRemaining(
                TokenOrUnicodeRange::UnicodeRange(u),
                stream,
            ))
        } else {
            match Token::consume(stream) {
                Ok(out) => Ok(match out {
                    TokenParseOutput::TokenAndRemaining(token, stream) => {
                        TokenOrUnicodeRangeParseOutput::TokenAndRemaining(
                            TokenOrUnicodeRange::Token(token),
                            stream,
                        )
                    }
                    TokenParseOutput::Eof => TokenOrUnicodeRangeParseOutput::Eof,
                }),
                Err(err) => Err(err),
            }
        }
    }
}
// Consume comments.
