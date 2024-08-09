use std::iter::FusedIterator;

use crate::{
    collections::{
        array_vec::ArrayVec,
        count::Count,
        declaration_value_list::{builder::BuildOutput, KnownDeclarationValueList},
        parsed_value_list::IsKnownParsedValueList,
    },
    parse::component_value::{
        ComponentValueParseList, ListParseFullError, NextFull, TokenAndRemaining,
    },
    token::{
        stream::{
            BufferedToken, BufferedTokenStream, CopyableTokenStream, TokenStream,
            TokenStreamProcess,
        },
        tokens::{
            Colon, DelimToken, IdentLikeToken, IdentToken, Semicolon, SimpleToken, Token,
            TokenParseError, TokenParseResult,
        },
    },
};

use super::component_value::{
    ComponentValue, ComponentValueParseError, ComponentValueParseOrTokenError,
    ListParseNotNestedError, NestedConfig, NestedFalse, RightCurlyBracketAndRemaining,
    SemicolonAsStopToken,
};

#[derive(Debug, Clone, Copy)]
pub struct Declaration<
    'a,
    L: IsKnownParsedValueList<ComponentValue<'a>, CAP> = Count,
    const CAP: usize = 0,
> {
    full: CopyableTokenStream<'a>,
    name: IdentToken<'a>,
    colon: Colon<'a>,
    value_and_important: CopyableTokenStream<'a>,
    value: KnownDeclarationValueList<'a, L, CAP>,
    important: Option<Important<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Important<'a> {
    pub(crate) full: CopyableTokenStream<'a>,
    pub(crate) bang: DelimToken<'a>,
    pub(crate) important: IdentToken<'a>,
}

impl<'a> Important<'a> {
    pub(crate) const fn is_bang_important(
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
}

#[derive(Debug)]
pub enum ExpectedToken {
    /// Expect a declaration name to start a declaration
    Ident,
    /// Expect a colon after a declaration name
    Colon,
}

impl ExpectedToken {
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
    pub expected: ExpectedToken,
    /// This includes unexpected EOF
    pub unexpected_input: BufferedTokenStream<'a, 1>,
}

pub(crate) enum DeclarationParseErrorFull<'a, Nested: NestedConfig> {
    UnexpectedToken(UnexpectedTokenError<'a>),
    ComponentValueList(ListParseFullError<'a, Nested>),
}

pub(crate) enum ConsumeAfterNameErrorFull<'a, Nested: NestedConfig> {
    ExpectColon {
        unexpected_input: BufferedTokenStream<'a, 1>,
    },
    ComponentValueList(ListParseFullError<'a, Nested>),
}

#[derive(Debug)]
pub(crate) enum DeclarationOrComponentValueError<'a> {
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
            ComponentValueParseOrTokenError::Token(err),
        ))
    }
}

impl<'a, Nested: NestedConfig> ConsumeAfterNameErrorFull<'a, Nested> {
    const fn Token(err: TokenParseError<'a>) -> Self {
        Self::ComponentValueList(ListParseFullError::ComponentValue(
            ComponentValueParseOrTokenError::Token(err),
        ))
    }

    const fn into_parse_error_full(self) -> DeclarationParseErrorFull<'a, Nested> {
        match self {
            ConsumeAfterNameErrorFull::ExpectColon { unexpected_input } => {
                DeclarationParseErrorFull::UnexpectedToken(UnexpectedTokenError {
                    expected: ExpectedToken::Colon,
                    unexpected_input,
                })
            }
            ConsumeAfterNameErrorFull::ComponentValueList(err) => {
                DeclarationParseErrorFull::ComponentValueList(err)
            }
        }
    }
}

impl<'a> ConsumeAfterNameErrorFull<'a, NestedFalse> {
    const fn into_parse_list_error_with_nested_false(self) -> DeclarationParseListError<'a> {
        match self {
            ConsumeAfterNameErrorFull::ExpectColon { unexpected_input } => {
                DeclarationParseListError::Declaration(DeclarationParseError::UnexpectedToken(
                    UnexpectedTokenError {
                        expected: ExpectedToken::Colon,
                        unexpected_input,
                    },
                ))
            }
            ConsumeAfterNameErrorFull::ComponentValueList(err) => match err {
                ListParseFullError::ComponentValue(err) => match err {
                    ComponentValueParseOrTokenError::Eof => {
                        DeclarationParseListError::ComponentValue(
                            ComponentValueParseError::UnexpectedEof,
                        )
                    }
                    ComponentValueParseOrTokenError::Token(err) => {
                        DeclarationParseListError::Token(err)
                    }
                },
                ListParseFullError::UnexpectedRightCurlyBracket(tar, ()) => {
                    DeclarationParseListError::ComponentValue(
                        ComponentValueParseError::UnexpectedRightCurlyBracket(tar),
                    )
                }
            },
        }
    }
}

impl<'a> DeclarationParseErrorFull<'a, NestedFalse> {
    const fn into_nested_false(self) -> DeclarationOrComponentValueError<'a> {
        match self {
            DeclarationParseErrorFull::UnexpectedToken(err) => {
                DeclarationOrComponentValueError::UnexpectedToken(err)
            }
            DeclarationParseErrorFull::ComponentValueList(err) => {
                DeclarationOrComponentValueError::ComponentValueList(err.into_not_nested_error())
            }
        }
    }
}

impl<'a> Declaration<'a> {
    pub const fn parse_list(input: TokenStream<'a>) -> DeclarationParseList<'a> {
        DeclarationParseList {
            inner: input.try_process(),
        }
    }

    pub const fn parse_list_from_str(input: &'a str) -> DeclarationParseList<'a> {
        Self::parse_list(TokenStream::new(input))
    }
}

impl<'a, L: IsKnownParsedValueList<ComponentValue<'a>, CAP>, const CAP: usize>
    Declaration<'a, L, CAP>
{
    /// nested is false
    pub(crate) const fn try_consume_next(
        input: TokenStreamProcess<'a>,
    ) -> Result<(Self, ParseEndReason<'a>), DeclarationOrComponentValueError<'a>> {
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
            }) => match Self::try_consume_after_name(TokenAndRemaining {
                token: name,
                remaining,
                full,
            }) {
                Ok(v) => Ok(v),
                Err(err) => Err(err.into_parse_error_full()),
            },
            Ok(TokenAndRemaining {
                token,
                full: token_and_remaining,
                remaining,
            }) => {
                // unexpected token
                return Err(DeclarationParseErrorFull::UnexpectedToken(
                    UnexpectedTokenError {
                        expected: ExpectedToken::Ident,
                        unexpected_input: BufferedTokenStream::new_buffer_filled(
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
                        expected: ExpectedToken::Ident,
                        unexpected_input: this,
                    },
                ));
            }
        }
    }

    pub(crate) const fn try_consume_after_name<Nested: NestedConfig>(
        parsed_name: TokenAndRemaining<'a, IdentToken<'a>>,
    ) -> Result<(Self, ParseEndReasonFull<'a, Nested>), ConsumeAfterNameErrorFull<'a, Nested>> {
        let TokenAndRemaining {
            token: name,
            remaining: input,
            full: before_name,
        } = parsed_name;

        let input = match input.try_discard_whitespace() {
            Ok(v) => v,
            Err(err) => return Err(ConsumeAfterNameErrorFull::Token(err)),
        };

        let colon;
        let input = match input.try_next() {
            Ok((Some(Token::Simple(SimpleToken::Colon(t))), remaining)) => {
                colon = t;
                remaining
            }
            Ok((_, unexpected_input)) => {
                // this includes EOF
                return Err(ConsumeAfterNameErrorFull::ExpectColon { unexpected_input });
            }
            Err(err) => return Err(ConsumeAfterNameErrorFull::Token(err)),
        };
        let after_colon = input.tokens_and_remaining_to_copyable();

        let input = match input.try_discard_whitespace() {
            Ok(v) => v,
            Err(err) => return Err(ConsumeAfterNameErrorFull::Token(err)),
        };

        let mut input = input;

        let mut values_builder = KnownDeclarationValueList::<L, CAP>::start_builder();

        loop {
            let before_next = input.tokens_and_remaining();
            match ComponentValueParseList::try_consume_next_full::<SemicolonAsStopToken, Nested>(
                input,
            ) {
                Ok(res) => {
                    let reason = match res {
                        NextFull::YieldValue(cv, this) => {
                            let new_input = match this.inner {
                                Ok(input) => input,
                                Err(err) => return Err(ConsumeAfterNameErrorFull::Token(err)),
                            };

                            values_builder = values_builder.with_push(TokenAndRemaining {
                                token: cv,
                                remaining: new_input.tokens_and_remaining(),
                                full: before_next,
                            });

                            input = new_input;
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
                        NextFull::Eof(_) => ParseEndReasonFull::Eof,
                    };

                    let BuildOutput {
                        value,
                        important,
                        value_and_important,
                        after_value_and_important,
                    } = values_builder.build(after_colon);

                    return Ok((
                        Self {
                            full: before_name.to_copyable().before(after_value_and_important),
                            name,
                            colon,
                            value_and_important,
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

    pub fn full_as_str(&self) -> &'a str {
        self.full.to_str()
    }

    pub fn name(&self) -> IdentToken<'a> {
        self.name
    }

    pub fn name_as_str(&self) -> &'a str {
        self.name.to_str()
    }

    pub fn value_as_str(&self) -> &'a str {
        self.value.full_as_str()
    }

    pub fn is_important(&self) -> bool {
        self.important.is_some()
    }

    pub fn to_tuple_str(self) -> (&'a str, &'a str, bool) {
        (self.name_as_str(), self.value_as_str(), self.is_important())
    }

    pub const fn value_and_important_as_str(&self) -> &'a str {
        self.value_and_important.to_str()
    }
}

impl<'a, const CAP: usize> Declaration<'a, ArrayVec<ComponentValue<'a>, CAP>, CAP> {
    pub const fn value_as_slice(&self) -> &[ComponentValue<'a>] {
        self.value
            .as_known_parsed_value_list()
            .parsed()
            .as_collection()
            .as_slice()
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
    pub const fn try_into_next<
        L: IsKnownParsedValueList<ComponentValue<'a>, CAP>,
        const CAP: usize,
    >(
        self,
    ) -> Result<(Option<Declaration<'a, L, CAP>>, Self), DeclarationParseListError<'a>> {
        match self.inner {
            Ok(mut input) => {
                loop {
                    match input.try_unwrap_one() {
                        Ok(tar) => match tar.token {
                            Token::Whitespace(_) | Token::Simple(SimpleToken::Semicolon(_)) => {
                                // Do nothing.
                                match tar.remaining.try_process() {
                                    Ok(remaining) => input = remaining,
                                    Err(err) => return Err(DeclarationParseListError::Token(err)),
                                };
                            }
                            Token::IdentLike(IdentLikeToken::Ident(name)) => {
                                match Declaration::try_consume_after_name::<NestedFalse>(
                                    tar.with_token_const(name),
                                ) {
                                    Ok((d, reason)) => {
                                        return Ok((
                                            Some(d),
                                            match reason {
                                                ParseEndReasonFull::NextIsRightCurlyBracket(
                                                    _,
                                                    never,
                                                ) => match never {},
                                                ParseEndReasonFull::NextIsStopToken(tar) => {
                                                    // discard the semicolon
                                                    Self {
                                                        inner: tar.remaining.try_process(),
                                                    }
                                                }
                                                ParseEndReasonFull::Eof => Self {
                                                    inner: Ok(TokenStreamProcess::EMPTY),
                                                },
                                            },
                                        ));
                                    }
                                    Err(err) => {
                                        return Err(err.into_parse_list_error_with_nested_false())
                                    }
                                }
                            }
                            _ => {
                                return Err(DeclarationParseListError::Declaration(
                                    DeclarationParseError::UnexpectedToken(UnexpectedTokenError {
                                        expected: ExpectedToken::Ident,
                                        unexpected_input: tar.into_input(),
                                    }),
                                ));
                            }
                        },
                        Err(empty) => {
                            // EOF
                            return Ok((None, Self { inner: Ok(empty) }));
                        }
                    }
                }
            }
            Err(err) => Err(DeclarationParseListError::Token(err)),
        }
    }

    /// After the first `Err`:
    /// - further calling [`Iterator::next`] would emit `Ok(None)`.
    /// - further calling [`DeclarationParseList::try_next`] would emit `Ok((None, EMPTY))`.
    ///
    /// A const version of this method is [`Self::try_into_next`].
    pub fn try_next<L: IsKnownParsedValueList<ComponentValue<'a>, CAP>, const CAP: usize>(
        &mut self,
    ) -> Result<Option<Declaration<'a, L, CAP>>, DeclarationParseListError<'a>> {
        const DUMMY: DeclarationParseList = DeclarationParseList {
            inner: Err(TokenParseError::DUMMY),
        };

        let this = std::mem::replace(self, DUMMY);

        match this.try_into_next() {
            Ok((v, this)) => {
                *self = this;
                Ok(v)
            }
            Err(err) => {
                *self = Self {
                    inner: Ok(TokenStreamProcess::EMPTY),
                };
                Err(err)
            }
        }
    }

    pub const fn into_iter<L: IsKnownParsedValueList<ComponentValue<'a>, CAP>, const CAP: usize>(
        self,
    ) -> DeclarationParseListIntoIter<'a, L, CAP> {
        DeclarationParseListIntoIter {
            inner: self,
            _phantom: std::marker::PhantomData,
        }
    }
}

#[derive(Debug)]
pub enum DeclarationParseListError<'a> {
    Token(TokenParseError<'a>),
    /// Error while parsing [`ComponentValue`].
    ComponentValue(ComponentValueParseError<'a>),
    /// Error while parsing [`Declaration`]
    Declaration(DeclarationParseError<'a>),
}

#[derive(Debug)]
pub enum DeclarationParseError<'a> {
    UnexpectedToken(UnexpectedTokenError<'a>),
}

pub struct DeclarationParseListIntoIter<
    'a,
    L: IsKnownParsedValueList<ComponentValue<'a>, CAP>,
    const CAP: usize,
> {
    inner: DeclarationParseList<'a>,
    _phantom: std::marker::PhantomData<L>,
}

impl<'a, L: IsKnownParsedValueList<ComponentValue<'a>, CAP>, const CAP: usize>
    DeclarationParseListIntoIter<'a, L, CAP>
{
    pub const fn try_into_next(
        self,
    ) -> Result<(Option<Declaration<'a, L, CAP>>, Self), DeclarationParseListError<'a>> {
        match self.inner.try_into_next() {
            Ok((v, this)) => Ok((v, this.into_iter())),
            Err(err) => Err(err),
        }
    }
    pub fn try_next(
        &mut self,
    ) -> Result<Option<Declaration<'a, L, CAP>>, DeclarationParseListError<'a>> {
        self.inner.try_next()
    }
}

/// After the first `Some(Err)`:
/// - further calling [`Iterator::next`] would emit `None`.
/// - further calling [`DeclarationParseList::try_next`] would emit `Ok((None, EMPTY))`.
impl<'a, L: IsKnownParsedValueList<ComponentValue<'a>, CAP>, const CAP: usize> Iterator
    for DeclarationParseListIntoIter<'a, L, CAP>
{
    type Item = Result<Declaration<'a, L, CAP>, DeclarationParseListError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.try_next().transpose()
    }
}

impl<'a, L: IsKnownParsedValueList<ComponentValue<'a>, CAP>, const CAP: usize> FusedIterator
    for DeclarationParseListIntoIter<'a, L, CAP>
{
}
