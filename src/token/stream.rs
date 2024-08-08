pub use buffered_token_stream::{BufferedToken, BufferedTokenStream};

use super::tokens::{Token, TokenParseOutput, TokenParseResult};
use crate::input::Filtered;

pub struct TokenStream<'a>(Filtered<'a>);

impl<'a> std::fmt::Debug for TokenStream<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TokenStream")
            .field(&self.0 .0.as_str())
            .finish()
    }
}

/// The next token has been parsed and buffered.
///
/// https://drafts.csswg.org/css-syntax-3/#token-stream-process
pub type TokenStreamProcess<'a> = BufferedTokenStream<'a, 1>;

#[derive(Clone, Copy, Debug)]
pub struct CopyableTokenStream<'a> {
    inner: &'a str,
}

impl<'a> CopyableTokenStream<'a> {
    pub(crate) const EMPTY: Self = TokenStream::EMPTY.to_copyable();

    pub const fn to_token_stream(self) -> TokenStream<'a> {
        TokenStream(Filtered::new(self.inner))
    }

    pub(crate) const fn before(&self, other: CopyableTokenStream<'a>) -> CopyableTokenStream<'a> {
        self.to_token_stream()
            .before(&other.to_token_stream())
            .to_copyable()
    }

    pub(crate) const fn to_str(self) -> &'a str {
        self.inner
    }
}

impl<'a> TokenStream<'a> {
    pub const EMPTY: Self = Self(Filtered::EMPTY);

    pub const fn new(s: &'a str) -> Self {
        Self(Filtered::new(s))
    }

    pub const fn to_copyable(&self) -> CopyableTokenStream<'a> {
        CopyableTokenStream {
            inner: self.0 .0.as_str(),
        }
    }

    pub const fn copy(&self) -> Self {
        Self(self.0.copy())
    }

    pub const fn try_next(self) -> TokenParseResult<'a, (Option<Token<'a>>, Self)> {
        match Token::consume(self.0) {
            Ok(TokenParseOutput::Eof) => Ok((None, Self::EMPTY)),
            Ok(TokenParseOutput::TokenAndRemaining(token, stream)) => {
                Ok((Some(token), Self(stream)))
            }
            Err(err) => Err(err),
        }
    }

    pub const fn try_buffer_n<const N: usize>(
        self,
    ) -> TokenParseResult<'a, BufferedTokenStream<'a, N>> {
        BufferedTokenStream::try_new_fill(self)
    }

    pub const fn try_process(self) -> TokenParseResult<'a, TokenStreamProcess<'a>> {
        self.try_buffer_n::<1>()
    }

    pub(crate) const fn before(&self, other: &TokenStream<'a>) -> TokenStream<'a> {
        TokenStream(Filtered::new(self.0.str_before(&other.0)))
    }

    /// https://drafts.csswg.org/css-syntax-3/#token-stream-discard-whitespace
    pub(crate) const fn try_discard_whitespace(
        self,
    ) -> TokenParseResult<'a, BufferedTokenStream<'a, 1>> {
        let input = match self.try_buffer_n::<1>() {
            Ok(input) => input,
            Err(err) => return Err(err),
        };

        input.try_discard_whitespace()
    }

    pub const fn is_empty(&self) -> bool {
        self.0 .0.as_str().is_empty()
    }
}

mod buffered_token_stream {
    use crate::collections::{array_vec::ArrayVec, HasConstDummyValue};
    use crate::{
        parse::component_value::TokenAndRemaining,
        token::tokens::{Token, TokenParseResult, WhitespaceToken},
    };

    use super::{CopyableTokenStream, TokenStream};

    #[derive(Debug, Clone, Copy)]
    pub struct BufferedToken<'a> {
        pub token: Token<'a>,
        /// token + remaining
        pub token_and_remaining: CopyableTokenStream<'a>,
    }

    impl<'a> HasConstDummyValue for BufferedToken<'a> {
        const DUMMY_VALUE: Self = BufferedToken {
            token: Token::Whitespace(WhitespaceToken::ONE_SPACE),
            token_and_remaining: TokenStream::new(" ").to_copyable(),
        };
    }

    pub struct BufferedTokenStream<'a, const N: usize> {
        // first in first out
        // TODO: optimize with a ring buffer
        buf: ArrayVec<BufferedToken<'a>, N>,
        // len of buf
        // If remaining is not empty, then buf.len() == N, which means buf is filled
        // If buf.len() < N, then remaining is empty
        // This is also true for `N == 0`.
        remaining: TokenStream<'a>,
        // if let Some(first) = buf.first(),
        // assert first.token_and_remaining == buf.map(|buf| buf.token).collect() + remaining
    }

    impl<'a, const N: usize> std::fmt::Debug for BufferedTokenStream<'a, N> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("BufferedTokenStream")
                .field("buf", &self.buffered_tokens().as_slice())
                .field("remaining", &self.remaining.0 .0.as_str())
                .finish()
        }
    }

    impl<'a> BufferedTokenStream<'a, 1> {
        /// For `BufferedTokenStream<'a, 1>`, if next token is available,
        /// it has been parsed and buffered, so it can be unwrapped without parsing.
        ///
        /// Err(input) implies input is empty
        pub const fn try_unwrap_one(self) -> Result<TokenAndRemaining<'a, Token<'a>>, Self> {
            if let Some(BufferedToken {
                token,
                token_and_remaining,
            }) = self.buf.first_copied()
            {
                // the only element in self.buf is returned
                Ok(TokenAndRemaining {
                    token,
                    remaining: self.remaining,
                    full: token_and_remaining.to_token_stream(),
                })
            } else {
                Err(self)
            }
        }
    }

    impl<'a, const N: usize> BufferedTokenStream<'a, N> {
        pub(crate) const EMPTY: Self = Self {
            buf: ArrayVec::EMPTY,
            remaining: TokenStream::EMPTY,
        };

        const fn buffered_tokens(&self) -> ArrayVec<Token<'a>, N> {
            let mut res = ArrayVec::EMPTY;

            let mut i = 0;
            let buf = self.buf.as_slice();

            while i < buf.len() {
                res = res.with_push(buf[i].token);
                i += 1;
            }

            res
        }

        /// all parsed tokens and remaining
        pub const fn tokens_and_remaining(&self) -> TokenStream<'a> {
            match self.buf.first() {
                Some(buf_token) => buf_token.token_and_remaining.to_token_stream(),
                // None includes the condition when N == 0
                None => self.remaining.copy(),
            }
        }

        pub(crate) const fn unparsed_remaining(&self) -> TokenStream<'a> {
            self.remaining.copy()
        }

        // all parsed tokens and remaining
        pub(crate) const fn tokens_and_remaining_to_copyable(&self) -> CopyableTokenStream<'a> {
            match self.buf.first() {
                Some(buf_token) => buf_token.token_and_remaining,
                // None includes the condition when N == 0
                None => self.remaining.to_copyable(),
            }
        }

        pub(crate) const fn new_buffer_filled(
            buf: [BufferedToken<'a>; N],
            remaining: TokenStream<'a>,
        ) -> Self {
            Self {
                buf: ArrayVec::new_filled(buf),
                remaining,
            }
        }

        pub const fn try_new_fill(mut stream: TokenStream<'a>) -> TokenParseResult<'a, Self> {
            let mut len = 0;
            let mut buf = [BufferedToken::DUMMY_VALUE; N];

            while len < N {
                let token_and_remaining = stream.to_copyable();
                match stream.try_next() {
                    Ok((token, new_stream)) => {
                        stream = new_stream;

                        let Some(token) = token else {
                            // EOF
                            break;
                        };

                        buf[len] = BufferedToken {
                            token,
                            token_and_remaining,
                        };
                        len += 1;
                    }
                    Err(err) => return Err(err),
                }
            }

            Ok(Self {
                buf: ArrayVec::new_maybe_filled(buf, len),
                remaining: stream,
            })
        }

        const ASSERT_N_IS_NOT_0: () = {
            assert!(N != 0);
        };

        pub(crate) const fn next_token_copied(&self) -> Option<Token<'a>> {
            () = Self::ASSERT_N_IS_NOT_0;
            match self.buf.first_copied() {
                Some(t) => Some(t.token),
                None => None,
            }
        }

        pub(crate) const fn try_next(mut self) -> TokenParseResult<'a, (Option<Token<'a>>, Self)> {
            if N == 0 {
                // not buffered
                return match self.remaining.try_next() {
                    Ok((token, remaining)) => Ok((
                        token,
                        Self {
                            buf: ArrayVec::EMPTY,
                            remaining,
                        },
                    )),
                    Err(err) => Err(err),
                };
            }

            if self.buf.is_empty() {
                return Ok((None, self));
            }

            let old_buf_len = self.buf.len();

            let buf_token;
            (buf_token, self.buf) = self.buf.with_pop_front();

            let Some(BufferedToken {
                token,
                token_and_remaining: _,
            }) = buf_token
            else {
                unreachable!()
            };

            if old_buf_len < N {
                // token_stream is empty

                Ok((Some(token), self))
            } else {
                // token_stream might have next token

                let new_token_and_remaining = self.remaining.to_copyable();
                match self.remaining.try_next() {
                    Ok((new_token, input)) => {
                        if let Some(new_token) = new_token {
                            self.buf = self.buf.with_push(BufferedToken {
                                token: new_token,
                                token_and_remaining: new_token_and_remaining,
                            });
                        }
                        self.remaining = input;

                        Ok((Some(token), self))
                    }
                    Err(err) => return Err(err),
                }
            }
        }

        pub(crate) const fn try_discard_whitespace(mut self) -> TokenParseResult<'a, Self> {
            () = Self::ASSERT_N_IS_NOT_0;
            while matches!(self.next_token_copied(), Some(Token::Whitespace(_))) {
                match self.try_next() {
                    Ok((_, this)) => self = this,
                    Err(err) => return Err(err),
                }
            }

            Ok(self)
        }
    }

    impl<'a> BufferedTokenStream<'a, 0> {
        pub fn new_unbuffered(input: TokenStream<'a>) -> BufferedTokenStream<'a, 0> {
            BufferedTokenStream {
                buf: ArrayVec::EMPTY,
                remaining: input,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::tokens::{
        DelimToken, IdentLikeToken, IdentToken, Number, NumericToken, SimpleToken, Token,
        WhitespaceToken,
    };

    use super::TokenStream;

    #[test]
    fn declaration_list() {
        let mut input = TokenStream::new("a:b; c:d 42!important;\n");

        let mut tokens = std::iter::from_fn(|| {
            let t;
            (t, input) = std::mem::replace(&mut input, TokenStream::EMPTY)
                .try_next()
                .unwrap();

            t
        });

        const EXPECTED: [Token; 14] = [
            Token::IdentLike(IdentLikeToken::Ident(IdentToken::new_const("a"))),
            Token::Simple(SimpleToken::COLON),
            Token::IdentLike(IdentLikeToken::Ident(IdentToken::new_const("b"))),
            Token::Simple(SimpleToken::SEMICOLON),
            Token::Whitespace(WhitespaceToken::ONE_SPACE),
            Token::IdentLike(IdentLikeToken::Ident(IdentToken::new_const("c"))),
            Token::Simple(SimpleToken::COLON),
            Token::IdentLike(IdentLikeToken::Ident(IdentToken::new_const("d"))),
            Token::Whitespace(WhitespaceToken::ONE_SPACE),
            Token::Numeric(NumericToken::Number(Number::new_integer("42"))),
            Token::Delim(DelimToken::BANG),
            Token::IdentLike(IdentLikeToken::Ident(IdentToken::new_const("important"))),
            Token::Simple(SimpleToken::SEMICOLON),
            Token::Whitespace(WhitespaceToken::new("\n")),
        ];

        let parsed_tokens =
            std::array::from_fn::<_, { EXPECTED.len() }, _>(|_| tokens.next().unwrap());

        assert_eq!(parsed_tokens, EXPECTED);

        assert_eq!(tokens.next(), None);
    }
}
