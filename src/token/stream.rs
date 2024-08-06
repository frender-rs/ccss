pub use buffered_token_stream::{BufferedToken, BufferedTokenStream, BufferedTokenStreamUnwrapOne};

use super::tokens::token::{Token, TokenParseOutput, TokenParseResult};
use crate::input::Filtered;

pub struct TokenStream<'a>(Filtered<'a>);

/// The next token has been parsed and buffered.
///
/// https://drafts.csswg.org/css-syntax-3/#token-stream-process
pub type TokenStreamProcess<'a> = BufferedTokenStream<'a, 1>;

#[derive(Clone, Copy)]
pub struct CopyableTokenStream<'a> {
    inner: &'a str,
}

impl<'a> CopyableTokenStream<'a> {
    pub const fn to_token_stream(self) -> TokenStream<'a> {
        TokenStream(Filtered::new(self.inner))
    }

    pub(crate) const fn before(&self, other: CopyableTokenStream<'a>) -> CopyableTokenStream<'a> {
        self.to_token_stream()
            .before(&other.to_token_stream())
            .to_copyable()
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
    pub(crate) fn try_discard_whitespace(self) -> TokenParseResult<'a, BufferedTokenStream<'a, 1>> {
        let input = match self.try_buffer_n::<1>() {
            Ok(input) => input,
            Err(err) => return Err(err),
        };

        input.try_discard_whitespace()
    }
}

mod buffered_token_stream {

    use crate::token::tokens::{
        token::{Token, TokenParseResult},
        whitespace_token::WhitespaceToken,
    };
    use crate::util::array_vec::{ArrayVec, ConstDummyValueFor};

    use super::{CopyableTokenStream, TokenStream};

    enum DummyForToken {}

    const DUMMY_TOKEN: Token = Token::WhitespaceToken(WhitespaceToken::ONE_SPACE);

    const DUMMY_BUF_TOKEN: BufferedToken = BufferedToken {
        token: DUMMY_TOKEN,
        token_and_remaining: TokenStream::EMPTY.to_copyable(),
    };

    impl<'a> ConstDummyValueFor<BufferedToken<'a>> for DummyForToken {
        const DUMMY_VALUE: BufferedToken<'a> = DUMMY_BUF_TOKEN;
    }

    #[derive(Clone, Copy)]
    pub struct BufferedToken<'a> {
        pub token: Token<'a>,
        /// token + remaining
        pub token_and_remaining: CopyableTokenStream<'a>,
    }

    pub struct BufferedTokenStream<'a, const N: usize> {
        // first in first out
        // TODO: optimize with a ring buffer
        buf: ArrayVec<BufferedToken<'a>, DummyForToken, N>,
        // len of buf
        // If remaining is not empty, then buf.len() == N, which means buf is filled
        // If buf.len() < N, then remaining is empty
        // This is also true for `N == 0`.
        remaining: TokenStream<'a>,
        // if let Some(first) = buf.first(),
        // assert first.token_and_remaining == buf.map(|buf| buf.token).collect() + remaining
    }

    pub struct BufferedTokenStreamUnwrapOne<'a> {
        // buf_token.token_and_remaining = buf_token.token + remaining
        pub buf_token: BufferedToken<'a>,
        pub remaining: TokenStream<'a>,
    }

    impl<'a> BufferedTokenStream<'a, 1> {
        /// Err(input) implies input is empty
        pub const fn try_unwrap_one(self) -> Result<BufferedTokenStreamUnwrapOne<'a>, Self> {
            if let Some(buf_token) = self.buf.first_copied() {
                // the only element in self.buf is returned
                Ok(BufferedTokenStreamUnwrapOne {
                    buf_token,
                    remaining: self.remaining,
                })
            } else {
                Err(self)
            }
        }
    }

    impl<'a, const N: usize> BufferedTokenStream<'a, N> {
        // all parsed tokens and remaining
        pub(crate) const fn tokens_and_remaining(&self) -> TokenStream<'a> {
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
            let mut buf = [DUMMY_BUF_TOKEN; N];

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
            while matches!(self.next_token_copied(), Some(Token::WhitespaceToken(_))) {
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
