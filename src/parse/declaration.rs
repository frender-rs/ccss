use crate::{
    parse::component_value::{ListParseFullError, NextFull, TokenAndRemaining},
    token::{
        stream::{
            BufferedToken, BufferedTokenStream, CopyableTokenStream, TokenStream,
            TokenStreamProcess,
        },
        tokens::{
            ident_like_token::IdentLikeToken,
            ident_token::IdentToken,
            simple_token::{Colon, Semicolon, SimpleToken},
            token::{DelimToken, Token, TokenParseError, TokenParseResult},
            whitespace_token::WhitespaceToken,
        },
    },
    util::array_vec::{ArrayVec, ConstDummyValueFor},
};

use super::component_value::{
    ComponentValue, ComponentValueConsumeList, List, ListParseNotNestedError, NestedConfig,
    NestedFalse, RightCurlyBracketAndRemaining, SemicolonAsStopToken,
};

#[derive(Debug, Clone, Copy)]
pub struct Declaration<'a> {
    full: CopyableTokenStream<'a>,
    name: IdentToken<'a>,
    colon: Colon<'a>,
    value: List<'a>,
    important: Option<Important<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Important<'a> {
    full: CopyableTokenStream<'a>,
    bang: DelimToken<'a>,
    important: IdentToken<'a>,
}

#[derive(Debug)]
pub enum DeclarationParseExpectToken {
    Ident,
    Colon,
}

impl DeclarationParseExpectToken {
    /// Returns `true` if the declaration parse expect token is [`Ident`].
    ///
    /// [`Ident`]: DeclarationParseExpectToken::Ident
    #[must_use]
    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Ident)
    }
}

#[derive(Debug)]
pub struct UnexpectedTokenError<'a> {
    pub expect: DeclarationParseExpectToken,
    pub buffered_input: BufferedTokenStream<'a, 1>,
}

pub(crate) enum DeclarationParseErrorFull<'a, Nested: NestedConfig> {
    UnexpectedToken(UnexpectedTokenError<'a>),
    ComponentValueList(ListParseFullError<'a, Nested>),
}

#[derive(Debug)]
pub enum DeclarationParseError<'a> {
    UnexpectedToken(UnexpectedTokenError<'a>),
    ComponentValueList(ListParseNotNestedError<'a>),
}

pub(crate) enum ParseEndReasonFull<'a, Nested: NestedConfig> {
    NextIsRightCurlyBracket(
        RightCurlyBracketAndRemaining<'a>,
        Nested::RightCurlyBracketIsOk,
    ),
    NextIsStopToken(TokenAndRemaining<'a, Semicolon<'a>>),
    Eof,
}

impl<'a> ParseEndReasonFull<'a, NestedFalse> {
    const fn into_nested_false(self) -> ParseEndReason<'a> {
        match self {
            ParseEndReasonFull::NextIsRightCurlyBracket(_, never) => match never {},
            ParseEndReasonFull::NextIsStopToken(v) => ParseEndReason::NextIsStopToken(v),
            ParseEndReasonFull::Eof => ParseEndReason::Eof,
        }
    }
}

pub enum ParseEndReason<'a> {
    NextIsStopToken(TokenAndRemaining<'a, Semicolon<'a>>),
    Eof,
}

impl<'a, Nested: NestedConfig> DeclarationParseErrorFull<'a, Nested> {
    const fn Token(err: TokenParseError<'a>) -> Self {
        Self::ComponentValueList(ListParseFullError::ComponentValue(
            super::component_value::ComponentValueParseError::Token(err),
        ))
    }
}

impl<'a> DeclarationParseErrorFull<'a, NestedFalse> {
    const fn into_nested_false(self) -> DeclarationParseError<'a> {
        match self {
            DeclarationParseErrorFull::UnexpectedToken(err) => {
                DeclarationParseError::UnexpectedToken(err)
            }
            DeclarationParseErrorFull::ComponentValueList(err) => {
                DeclarationParseError::ComponentValueList(err.into_not_nested_error())
            }
        }
    }
}

impl<'a> Declaration<'a> {
    /// nested is false
    pub const fn try_consume_next(
        input: TokenStreamProcess<'a>,
    ) -> Result<(Self, ParseEndReason<'a>), DeclarationParseError<'a>> {
        match Self::try_consume_next_with_nested_config::<NestedFalse>(input) {
            Ok((this, reason)) => Ok((this, reason.into_nested_false())),
            Err(err) => Err(err.into_nested_false()),
        }
    }

    pub(crate) const fn try_consume_next_with_nested_config<Nested: NestedConfig>(
        input: TokenStreamProcess<'a>,
    ) -> Result<(Self, ParseEndReasonFull<'a, Nested>), DeclarationParseErrorFull<'a, Nested>> {
        match input.try_unwrap_one() {
            Ok(TokenAndRemaining {
                token: Token::IdentLike(IdentLikeToken::Ident(name)),
                remaining,
                full,
            }) => Self::try_consume_after_name(TokenAndRemaining {
                token: name,
                remaining,
                full,
            }),
            Ok(TokenAndRemaining {
                token,
                full: token_and_remaining,
                remaining,
            }) => {
                // unexpected token
                return Err(DeclarationParseErrorFull::UnexpectedToken(
                    UnexpectedTokenError {
                        expect: DeclarationParseExpectToken::Ident,
                        buffered_input: BufferedTokenStream::new_buffer_filled(
                            [BufferedToken {
                                token,
                                token_and_remaining: token_and_remaining.to_copyable(),
                            }],
                            remaining,
                        ),
                    },
                ));
            }
            Err(this) => {
                // EOF
                return Err(DeclarationParseErrorFull::UnexpectedToken(
                    UnexpectedTokenError {
                        expect: DeclarationParseExpectToken::Ident,
                        buffered_input: this,
                    },
                ));
            }
        }
    }

    pub(crate) const fn try_consume_after_name<Nested: NestedConfig>(
        parsed_name: TokenAndRemaining<'a, IdentToken<'a>>,
    ) -> Result<(Self, ParseEndReasonFull<'a, Nested>), DeclarationParseErrorFull<'a, Nested>> {
        let TokenAndRemaining {
            token: name,
            remaining: input,
            full: before_name,
        } = parsed_name;

        let input = match input.try_discard_whitespace() {
            Ok(v) => v,
            Err(err) => return Err(DeclarationParseErrorFull::Token(err)),
        };

        let colon;
        let input = match input.try_next() {
            Ok((Some(Token::Simple(SimpleToken::Colon(t))), remaining)) => {
                colon = t;
                remaining
            }
            Ok((_, this)) => {
                // this includes EOF
                return Err(DeclarationParseErrorFull::UnexpectedToken(
                    UnexpectedTokenError {
                        expect: DeclarationParseExpectToken::Colon,
                        buffered_input: this,
                    },
                ));
            }
            Err(err) => return Err(DeclarationParseErrorFull::Token(err)),
        };
        let after_colon = input.tokens_and_remaining_to_copyable();

        let input = match input.try_discard_whitespace() {
            Ok(v) => v,
            Err(err) => return Err(DeclarationParseErrorFull::Token(err)),
        };

        enum Dummy {}

        impl<'a> ConstDummyValueFor<ValueAndRemaining<'a>> for Dummy {
            const DUMMY_VALUE: ValueAndRemaining<'a> = ValueAndRemaining {
                cv: ComponentValue::PreservedTokens(Token::Whitespace(WhitespaceToken::ONE_SPACE)),
                remaining: TokenStream::EMPTY.to_copyable(),
                full: TokenStream::EMPTY.to_copyable(),
            };
        }

        #[derive(Clone, Copy)]
        struct ValueAndRemaining<'a> {
            cv: ComponentValue<'a>,
            remaining: CopyableTokenStream<'a>,
            // full == cv + remaining
            full: CopyableTokenStream<'a>,
        }

        let mut input = ComponentValueConsumeList::new_with_process(input);

        enum ValueList<'a> {
            Empty,
            NotEmpty {
                first: ValueAndRemaining<'a>,
                last_3: ArrayVec<ValueAndRemaining<'a>, Dummy, 3>,
                real_len: usize,
            },
        }

        enum ValueListPop2<'a> {
            Empty,
            One(ValueAndRemaining<'a>),
            More {
                first: ValueAndRemaining<'a>,
                last: ValueAndRemaining<'a>,
                real_len: usize,
            },
        }
        impl<'a> ValueListPop2<'a> {
            const fn span(&self) -> List<'a> {
                match self {
                    ValueListPop2::Empty => List::EMPTY,
                    ValueListPop2::One(v) => List {
                        full: v.full.before(v.remaining),
                        len: 1,
                    },
                    ValueListPop2::More {
                        first,
                        last,
                        real_len,
                    } => List {
                        full: first.full.before(last.remaining),
                        len: *real_len,
                    },
                }
            }
        }

        impl<'a> ValueList<'a> {
            const fn with_push(self, v: ValueAndRemaining<'a>) -> Self {
                match self {
                    ValueList::Empty => Self::NotEmpty {
                        first: v,
                        last_3: ArrayVec::EMPTY,
                        real_len: 1,
                    },
                    ValueList::NotEmpty {
                        first,
                        last_3,
                        real_len,
                    } => Self::NotEmpty {
                        first,
                        last_3: last_3.with_force_push(v).1,
                        real_len: real_len + 1,
                    },
                }
            }

            const fn pop_last_2_important(
                self,
            ) -> Result<(Important<'a>, ValueListPop2<'a>), Self> {
                match self {
                    ValueList::Empty => Err(self),
                    ValueList::NotEmpty {
                        first,
                        ref last_3,
                        real_len,
                    } => match last_3.as_slice() {
                        [a, b, c] => {
                            debug_assert!(real_len >= 4);
                            if let Some((bang, important)) = is_bang_important(b.cv, c.cv) {
                                Ok((
                                    Important {
                                        full: b.full.before(c.remaining),
                                        bang,
                                        important,
                                    },
                                    ValueListPop2::More {
                                        first,
                                        last: *a,
                                        real_len: real_len - 2,
                                    },
                                ))
                            } else {
                                Err(self)
                            }
                        }
                        [b, c] => {
                            debug_assert!(real_len == 3);

                            if let Some((bang, important)) = is_bang_important(b.cv, c.cv) {
                                Ok((
                                    Important {
                                        full: b.full.before(c.remaining),
                                        bang,
                                        important,
                                    },
                                    ValueListPop2::One(first),
                                ))
                            } else {
                                Err(self)
                            }
                        }
                        [c] => {
                            debug_assert!(real_len == 2);
                            let b = first;
                            if let Some((bang, important)) = is_bang_important(b.cv, c.cv) {
                                Ok((
                                    Important {
                                        full: b.full.before(c.remaining),
                                        bang,
                                        important,
                                    },
                                    ValueListPop2::Empty,
                                ))
                            } else {
                                Err(self)
                            }
                        }
                        [] => {
                            debug_assert!(real_len == 1);

                            Err(self)
                        }
                        _ => unreachable!(),
                    },
                }
            }

            const fn last(&self) -> Option<ValueAndRemaining<'a>> {
                match self {
                    ValueList::Empty => None,
                    ValueList::NotEmpty {
                        first,
                        last_3,
                        real_len: _,
                    } => Some(match last_3.as_slice().last() {
                        Some(v) => *v,
                        None => *first,
                    }),
                }
            }

            const fn span(&self) -> List<'a> {
                match self {
                    ValueList::Empty => List::EMPTY,
                    ValueList::NotEmpty {
                        first,
                        last_3,
                        real_len,
                    } => match last_3.as_slice().last() {
                        Some(last) => List {
                            full: first.full.before(last.remaining),
                            len: *real_len,
                        },
                        None => List {
                            full: first.full.before(first.remaining),
                            len: *real_len,
                        },
                    },
                }
            }
        }

        // non whitespace values
        let mut values = ValueList::Empty;

        loop {
            let before_next = input.input.tokens_and_remaining();
            match input.try_next_full::<SemicolonAsStopToken, Nested>() {
                Ok(res) => {
                    let reason = match res {
                        NextFull::YieldValue(cv, this) => {
                            if !cv.is_whitespace() {
                                values = values.with_push(ValueAndRemaining {
                                    cv,
                                    remaining: this.input.tokens_and_remaining_to_copyable(),
                                    full: before_next.to_copyable(),
                                });
                            }
                            input = this;
                            continue;
                        }
                        NextFull::NextIsRightCurlyBracket(a, b) => {
                            ParseEndReasonFull::NextIsRightCurlyBracket(a, b)
                        }
                        NextFull::NextIsStopToken(TokenAndRemaining {
                            token,
                            remaining,
                            full,
                        }) => {
                            let token = token.into_semicolon();
                            ParseEndReasonFull::NextIsStopToken(TokenAndRemaining {
                                token,
                                remaining,
                                full,
                            })
                        }
                        NextFull::Eof => ParseEndReasonFull::Eof,
                    };

                    let after_last = match values.last() {
                        Some(v) => v.remaining,
                        None => after_colon,
                    };

                    let (value, important) = match values.pop_last_2_important() {
                        Ok((important, values)) => (values.span(), Some(important)),
                        Err(values) => (values.span(), None),
                    };

                    return Ok((
                        Self {
                            full: before_name
                                .before(&after_last.to_token_stream())
                                .to_copyable(),
                            name,
                            colon,
                            value,
                            important,
                        },
                        reason,
                    ));
                }
                Err(err) => match err {
                    ListParseFullError::ComponentValue(_) => todo!(),
                    ListParseFullError::UnexpectedRightCurlyBracket(_, _) => todo!(),
                },
            }
        }
    }
}

const fn is_bang_important<'a>(
    a: ComponentValue<'a>,
    b: ComponentValue<'a>,
) -> Option<(DelimToken<'a>, IdentToken<'a>)> {
    match (a, b) {
        (
            ComponentValue::PreservedTokens(Token::Delim(t)),
            ComponentValue::PreservedTokens(Token::IdentLike(IdentLikeToken::Ident(ident))),
        ) if t.value().to_char() == '!'
            && str_matches_important_ascii_case_insensitive(ident.to_str()) =>
        {
            Some((t, ident))
        }
        _ => None,
    }
}

const fn str_matches_important_ascii_case_insensitive(s: &str) -> bool {
    matches!(
        s.as_bytes(),
        [
            b'i' | b'I',
            b'm' | b'M',
            b'p' | b'P',
            b'o' | b'O',
            b'r' | b'R',
            b't' | b'T',
            b'a' | b'A',
            b'n' | b'N',
            b't' | b'T',
        ]
    )
}

/// [Parse a list of declarations](https://www.w3.org/TR/css-syntax-3/#parse-list-of-declarations)
/// excluding at-rules.
///
/// `nested` is false
pub struct DeclarationParseList<'a> {
    inner: TokenParseResult<'a, TokenStreamProcess<'a>>,
}

impl<'a> DeclarationParseList<'a> {
    pub const fn try_next(self) -> Result<Option<(Declaration<'a>, Self)>, ()> {
        match self.inner {
            Ok(mut input) => {
                loop {
                    match input.try_unwrap_one() {
                        Ok(TokenAndRemaining {
                            token,
                            remaining,
                            full,
                        }) => match token {
                            Token::Whitespace(_) | Token::Simple(SimpleToken::Semicolon(_)) => {
                                // Do nothing.
                                match remaining.try_process() {
                                    Ok(remaining) => input = remaining,
                                    Err(err) => return Err(err),
                                };
                            }
                            Token::IdentLike(IdentLikeToken::Ident(name)) => {
                                match Declaration::try_consume_after_name::<NestedFalse>(
                                    TokenAndRemaining {
                                        token: name,
                                        remaining,
                                        full,
                                    },
                                ) {
                                    Ok((d, reason)) => {
                                        return Ok(Some((
                                            d,
                                            match reason {
                                                ParseEndReasonFull::NextIsRightCurlyBracket(
                                                    _,
                                                    never,
                                                ) => match never {},
                                                ParseEndReasonFull::NextIsStopToken(
                                                    TokenAndRemaining {
                                                        token,
                                                        remaining,
                                                        full,
                                                    },
                                                ) => {
                                                    // discard the semicolon
                                                    Self {
                                                        inner: remaining.try_process(),
                                                    }
                                                }
                                                ParseEndReasonFull::Eof => Self {
                                                    inner: Ok(TokenStreamProcess::EMPTY),
                                                },
                                            },
                                        )));
                                    }
                                    Err(_) => todo!(),
                                }
                            }
                            _ => {
                                return Err();
                            }
                        },
                        Err(_) => {
                            // EOF
                            return Ok(None);
                        }
                    }
                }
            }
            Err(err) => todo!(),
        }
    }
}
