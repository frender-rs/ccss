use std::iter::FusedIterator;

use crate::{
    collections::{
        array_vec::ArrayVec,
        collect_nothing::CollectNothing,
        component_value_list::{
            IsKnownComponentValueList, IsKnownComponentValueListWithConstEmpty,
        },
        declaration_value_list::{builder::BuildOutput, KnownDeclarationValueList},
        known::{IsKnownCollection, IsKnownCollectionWithConstEmpty},
        lead_vec::LeadVec,
        parsed_value_list::KnownParsedValueList,
        HasConstDummyValue,
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
            TokenParseError,
        },
    },
};

use super::component_value::{
    ComponentValue, ComponentValueParseError, ComponentValueParseOrTokenError,
    ListParseNotNestedError, NestedConfig, NestedFalse, RightCurlyBracketAndRemaining,
    SemicolonAsStopToken,
};

pub struct Declaration<'a, L: IsKnownComponentValueList<'a> = CollectNothing> {
    full: CopyableTokenStream<'a>,
    name: IdentToken<'a>,
    colon: Colon<'a>,
    value_and_important: CopyableTokenStream<'a>,
    value: KnownDeclarationValueList<'a, L>,
    important: Option<Important<'a>>,
}

impl<'a, L: IsKnownComponentValueListWithConstEmpty<'a>> HasConstDummyValue for Declaration<'a, L> {
    const DUMMY_VALUE: Self = Self {
        full: TokenStream::new("a:").to_copyable(),
        name: IdentToken::new_const("a"),
        colon: Colon::DEFAULT,
        value_and_important: CopyableTokenStream::EMPTY,
        value: KnownDeclarationValueList::EMPTY,
        important: None,
    };
}

impl<'a, L: IsKnownComponentValueList<'a>> Copy for Declaration<'a, L> {}

impl<'a, L: IsKnownComponentValueList<'a>> Clone for Declaration<'a, L> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, L: IsKnownComponentValueList<'a>> std::fmt::Debug for Declaration<'a, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Declaration")
            .field("full", &self.full)
            .field("name", &self.name)
            .field("colon", &self.colon)
            .field("value_and_important", &self.value_and_important)
            .field("value", &self.value)
            .field("important", &self.important)
            .finish()
    }
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
                && ident
                    .as_ident_sequence()
                    .matches_important_ascii_case_insensitive() =>
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
    #[allow(non_snake_case)]
    const fn Token(err: TokenParseError<'a>) -> Self {
        Self::ComponentValueList(ListParseFullError::ComponentValue(
            ComponentValueParseOrTokenError::Token(err),
        ))
    }
}

impl<'a, Nested: NestedConfig> ConsumeAfterNameErrorFull<'a, Nested> {
    #[allow(non_snake_case)]
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

impl<'a, L: IsKnownComponentValueList<'a>> Declaration<'a, L> {
    pub const fn parse_list(input: TokenStream<'a>) -> DeclarationParseList<'a, L> {
        DeclarationParseList {
            inner: input.try_process_or_copy(),
            _list: std::marker::PhantomData,
        }
    }

    pub const fn parse_list_from_str(input: &'a str) -> DeclarationParseList<'a, L> {
        Self::parse_list(TokenStream::new(input))
    }
}

impl<
        'a,
        L: IsKnownComponentValueListWithConstEmpty<'a>,
        const ARRAY_VEC_CAP: usize,
        const LEAD_VEC_CAP: usize,
    > Declaration<'a, L>
where
    L::Collection: IsKnownCollection<
        ComponentValue<'a>,
        ArrayVecType = ArrayVec<ComponentValue<'a>, ARRAY_VEC_CAP>,
        LeadVecType = LeadVec<ComponentValue<'a>, LEAD_VEC_CAP>,
    >,
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

        let (
            BuildOutput {
                value,
                important,
                value_and_important,
                after_value_and_important,
                // after_value_and_important_and_trailing_whitespace: _,
            },
            reason,
        ) = match Self::try_consume_value_and_important(input) {
            Ok(v) => v,
            Err(err) => {
                return Err(err);
            }
        };

        Ok((
            Self {
                full: before_name.to_copyable().before(after_value_and_important),
                name,
                colon,
                value_and_important,
                value,
                important,
            },
            reason,
        ))
    }

    /// Whitespace will be trimmed
    const fn try_consume_value_and_important<Nested: NestedConfig>(
        input: TokenStreamProcess<'a>,
    ) -> Result<
        (BuildOutput<'a, L>, ParseEndReasonFull<'a, Nested>),
        ConsumeAfterNameErrorFull<'a, Nested>,
    > {
        let input = match input.try_discard_whitespace() {
            Ok(v) => v,
            Err(err) => return Err(ConsumeAfterNameErrorFull::Token(err)),
        };

        let before_value = input.tokens_and_remaining_to_copyable();
        let mut input = input;

        let mut values_builder = KnownDeclarationValueList::<L>::start_builder();

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

                    return Ok((values_builder.build(before_value), reason));
                }
                Err(err) => return Err(ConsumeAfterNameErrorFull::ComponentValueList(err)),
            }
        }
    }

    /// Panics if `s` if not a valid [KnownDeclarationValueList].
    ///
    /// Whitespace will be trimmed.
    pub const fn parse_value_from_str(s: &'a str) -> KnownDeclarationValueList<'a, L> {
        let input = TokenStream::new(s);

        let input = match input.try_process() {
            Ok(v) => v,
            Err(err) => panic!("{}", err.to_message()),
        };

        let (out, reason) = match Self::try_consume_value_and_important::<NestedFalse>(input) {
            Ok(v) => v,
            Err(err) => {
                let msg = match err {
                    ConsumeAfterNameErrorFull::ExpectColon { .. } => {
                        unreachable!()
                    }
                    ConsumeAfterNameErrorFull::ComponentValueList(err) => match err {
                        ListParseFullError::ComponentValue(err) => match err {
                            ComponentValueParseOrTokenError::Eof => {
                                "unexpected eof when parsing component value list"
                            }
                            ComponentValueParseOrTokenError::Token(err) => err.to_message(),
                        },
                        ListParseFullError::UnexpectedRightCurlyBracket(_, ()) => {
                            "unexpected right curly bracket '}' when parsing component value list"
                        }
                    },
                };
                panic!("{}", msg);
            }
        };

        match reason {
            ParseEndReasonFull::NextIsRightCurlyBracket(_, never) => match never {},
            ParseEndReasonFull::NextIsStopToken(_) => panic!("unexpected semicolon ';'"),
            ParseEndReasonFull::Eof => {}
        }

        let BuildOutput {
            value,
            important,
            value_and_important: _,
            after_value_and_important: _,
            // after_value_and_important_and_trailing_whitespace: _,
        } = out;

        if important.is_some() {
            panic!("declaration value can't end with \"!important\"")
        }

        value
    }
}

impl<'a, L: IsKnownComponentValueList<'a>> Declaration<'a, L> {
    pub fn full_as_str(&self) -> &'a str {
        self.full.to_str()
    }

    pub const fn name(&self) -> IdentToken<'a> {
        self.name
    }

    /// The returned str may contain [escaped code points](https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point).
    pub const fn name_as_original_str(&self) -> &'a str {
        self.name.as_ident_sequence().original_str()
    }

    pub const fn value(&self) -> &KnownDeclarationValueList<'a, L> {
        &self.value
    }

    /// The returned str may contain [escaped code points](https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point).
    pub const fn value_as_original_str(&self) -> &'a str {
        self.value.full_as_str()
    }

    pub const fn is_important(&self) -> bool {
        self.important.is_some()
    }

    /// The returned str may contain [escaped code points](https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point).
    pub fn to_tuple_original_str(self) -> (&'a str, &'a str, bool) {
        (
            self.name_as_original_str(),
            self.name_as_original_str(),
            self.is_important(),
        )
    }

    pub const fn value_and_important_as_str(&self) -> &'a str {
        self.value_and_important.to_str()
    }
}

impl<'a, const CAP: usize> Declaration<'a, ArrayVec<ComponentValue<'a>, CAP>> {
    pub const fn value_as_slice(&self) -> &[ComponentValue<'a>] {
        self.value
            .as_known_component_value_list()
            .as_known_parsed_value_list()
            .parsed()
            .as_variant()
            .as_slice()
    }
}

/// [Parse a list of declarations](https://www.w3.org/TR/css-syntax-3/#parse-list-of-declarations)
/// excluding at-rules.
///
/// `nested` is false
pub struct DeclarationParseList<'a, VL: IsKnownComponentValueList<'a>> {
    inner: Result<TokenStreamProcess<'a>, (TokenParseError<'a>, TokenStream<'a>)>,
    _list: std::marker::PhantomData<VL>,
}

impl<
        'a,
        VL: IsKnownComponentValueListWithConstEmpty<'a>,
        const ARRAY_VEC_CAP: usize,
        const LEAD_VEC_CAP: usize,
    > DeclarationParseList<'a, VL>
where
    VL::Collection: IsKnownCollection<
        ComponentValue<'a>,
        ArrayVecType = ArrayVec<ComponentValue<'a>, ARRAY_VEC_CAP>,
        LeadVecType = LeadVec<ComponentValue<'a>, LEAD_VEC_CAP>,
    >,
{
    const fn try_consume_next(
        mut input: TokenStreamProcess<'a>,
    ) -> Result<(Option<Declaration<'a, VL>>, Self), DeclarationParseListError<'a>> {
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
                                        ParseEndReasonFull::NextIsRightCurlyBracket(_, never) => {
                                            match never {}
                                        }
                                        ParseEndReasonFull::NextIsStopToken(tar) => {
                                            // discard the semicolon
                                            Self {
                                                inner: tar.remaining.try_process_or_copy(),
                                                _list: std::marker::PhantomData,
                                            }
                                        }
                                        ParseEndReasonFull::Eof => Self {
                                            inner: Ok(TokenStreamProcess::EMPTY),
                                            _list: std::marker::PhantomData,
                                        },
                                    },
                                ));
                            }
                            Err(err) => return Err(err.into_parse_list_error_with_nested_false()),
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
                    return Ok((
                        None,
                        Self {
                            inner: Ok(empty),
                            _list: std::marker::PhantomData,
                        },
                    ));
                }
            }
        }
    }

    pub const fn try_into_next(
        self,
    ) -> Result<(Option<Declaration<'a, VL>>, Self), DeclarationParseListError<'a>> {
        match self.inner {
            Ok(input) => Self::try_consume_next(input),
            Err((err, _)) => Err(DeclarationParseListError::Token(err)),
        }
    }

    const DUMMY: Self = Self {
        inner: Err((TokenParseError::DUMMY, TokenStream::EMPTY)),
        _list: std::marker::PhantomData,
    };

    /// After the first `Err`:
    /// - further calling [`Iterator::next`] would emit `Ok(None)`.
    /// - further calling [`DeclarationParseList::try_next`] would emit `Ok((None, EMPTY))`.
    ///
    /// A const version of this method is [`Self::try_into_next`].
    pub fn try_next(
        &mut self,
    ) -> Result<Option<Declaration<'a, VL>>, DeclarationParseListError<'a>> {
        let this = std::mem::replace(self, Self::DUMMY);

        match this.try_into_next() {
            Ok((v, this)) => {
                *self = this;
                Ok(v)
            }
            Err(err) => {
                *self = Self {
                    inner: Ok(TokenStreamProcess::EMPTY),
                    _list: std::marker::PhantomData,
                };
                Err(err)
            }
        }
    }

    #[allow(clippy::result_large_err)]
    #[allow(clippy::type_complexity)]
    pub const fn try_collect_into_known<
        DL: IsKnownCollectionWithConstEmpty<
            Declaration<'a, VL>,
            ArrayVecType = ArrayVec<Declaration<'a, VL>, DECLARATION_ARRAY_VEC_CAP>,
            LeadVecType = LeadVec<Declaration<'a, VL>, DECLARATION_LEAD_VEC_CAP>,
        >,
        // TODO: find out a pattern where we can remove these explicit const generics
        const DECLARATION_ARRAY_VEC_CAP: usize,
        const DECLARATION_LEAD_VEC_CAP: usize,
    >(
        mut self,
    ) -> Result<
        KnownParsedValueList<'a, DL, Declaration<'a, VL>>,
        (
            KnownParsedValueList<'a, DL, Declaration<'a, VL>>,
            DeclarationParseListError<'a>,
        ),
    > {
        let mut builder = KnownParsedValueList::start_builder();

        loop {
            let before_next = match &self.inner {
                Ok(input) => input.tokens_and_remaining_to_copyable(),
                Err((_, input)) => input.to_copyable(),
            };

            self = match self.try_into_next() {
                Ok((v, this)) => {
                    if let Some(v) = v {
                        builder = builder.with_push(v, before_next);

                        this
                    } else {
                        let input = match this.inner {
                            Ok(input) if input.tokens_and_remaining().is_empty() => input,
                            _ => {
                                panic!(
                                    "unexpected tokens after parsing declaration list that is not nested"
                                );
                            }
                        };
                        let res = builder.build(input.tokens_and_remaining_to_copyable());
                        return Ok(res);
                    }
                }
                Err(err) => return Err((builder.build(before_next), err)),
            }
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

/// After the first `Some(Err)`:
/// - further calling [`Iterator::next`] would emit `None`.
/// - further calling [`DeclarationParseList::try_next`] would emit `Ok((None, EMPTY))`.
impl<
        'a,
        VL: IsKnownComponentValueListWithConstEmpty<'a>,
        const ARRAY_VEC_CAP: usize,
        const LEAD_VEC_CAP: usize,
    > Iterator for DeclarationParseList<'a, VL>
where
    VL::Collection: IsKnownCollection<
        ComponentValue<'a>,
        ArrayVecType = ArrayVec<ComponentValue<'a>, ARRAY_VEC_CAP>,
        LeadVecType = LeadVec<ComponentValue<'a>, LEAD_VEC_CAP>,
    >,
{
    type Item = Result<Declaration<'a, VL>, DeclarationParseListError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.try_next().transpose()
    }
}

impl<
        'a,
        VL: IsKnownComponentValueListWithConstEmpty<'a>,
        const ARRAY_VEC_CAP: usize,
        const LEAD_VEC_CAP: usize,
    > FusedIterator for DeclarationParseList<'a, VL>
where
    VL::Collection: IsKnownCollection<
        ComponentValue<'a>,
        ArrayVecType = ArrayVec<ComponentValue<'a>, ARRAY_VEC_CAP>,
        LeadVecType = LeadVec<ComponentValue<'a>, LEAD_VEC_CAP>,
    >,
{
}
