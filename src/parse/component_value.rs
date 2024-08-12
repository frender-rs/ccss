use crate::{
    collections::{
        array_vec::ArrayVec,
        component_value_list::{IsKnownComponentValueListWithConstEmpty, KnownComponentValueList},
        known::IsKnownCollection,
        lead_vec::LeadVec,
        HasConstDummyValue,
    },
    token::{
        stream::{
            BufferedToken, BufferedTokenStream, CopyableTokenStream, TokenStream,
            TokenStreamProcess,
        },
        tokens::{
            Comma, FunctionToken, IdentLikeToken, IdentSequence, RightCurlyBracket,
            RightParenthesis, Semicolon, SimpleBlockSurroundingTokens, SimpleToken, Token,
            TokenParseError, TokenParseResult,
        },
    },
};

/// https://drafts.csswg.org/css-syntax-3/#component-value
#[derive(Debug, Clone, Copy)]
pub enum ComponentValue<'a> {
    PreservedTokens(Token<'a>),
    Function(Function<'a>),
    SimpleBlock(SimpleBlock<'a>),
}

impl<'a> HasConstDummyValue for ComponentValue<'a> {
    const DUMMY_VALUE: Self = Self::PreservedTokens(Token::DUMMY_VALUE);
}

/// Current the parsing algorithm doesn't allow EOF
#[derive(Debug, Clone, Copy)]
pub struct Function<'a> {
    full: CopyableTokenStream<'a>,
    function_token: FunctionToken<'a>,
    value: List<'a>,
    right_parenthesis: RightParenthesis<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct List<'a> {
    pub full: CopyableTokenStream<'a>,
    pub len: usize,
}

impl<'a> List<'a> {
    pub const EMPTY: Self = Self {
        full: TokenStream::EMPTY.to_copyable(),
        len: 0,
    };
}

impl<'a> Function<'a> {
    pub const fn name(&self) -> IdentSequence<'a> {
        self.function_token.value()
    }

    pub const fn value_as_original_str(&self) -> &'a str {
        self.value.full.to_str()
    }

    const fn consume(
        input: TokenStreamProcess<'a>,
    ) -> Result<(Self, TokenStream<'a>), ComponentValueParseOrTokenError<'a>> {
        let original = input.tokens_and_remaining();
        let Some((function_token, input)) = (match input.try_unwrap_one() {
            Ok(TokenAndRemaining {
                token,
                full: _,
                remaining,
            }) => match token.as_function_token() {
                Some(token) => Some((token, remaining)),
                None => None,
            },
            Err(_) => None,
        }) else {
            panic!("the next token of input should be a function token")
        };

        let before_value = input.copy();

        let mut value_len = 0;
        let mut input = input;

        loop {
            let before_token = input.copy();
            input = match input.try_next() {
                Ok((token, input)) => match token {
                    Some(token) => {
                        if let Some(right_parenthesis) = token.as_right_parenthesis() {
                            return Ok((
                                Self {
                                    full: original.before(&input).to_copyable(),
                                    function_token,
                                    value: List {
                                        full: before_value.before(&before_token).to_copyable(),
                                        len: value_len,
                                    },
                                    right_parenthesis,
                                },
                                input,
                            ));
                        } else {
                            match ComponentValue::try_consume_one(TokenAndRemaining {
                                token: token,
                                remaining: input,
                                full: before_token,
                            }) {
                                Ok((_, input)) => {
                                    value_len += 1;
                                    input
                                }
                                Err(err) => return Err(err),
                            }
                        }
                    }
                    None => {
                        return Err(ComponentValueParseOrTokenError::Eof);
                    }
                },
                Err(err) => return Err(ComponentValueParseOrTokenError::Token(err)),
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SimpleBlock<'a> {
    full: CopyableTokenStream<'a>,
    value: List<'a>,
    surrounding: SimpleBlockSurroundingTokens<'a>,
}

#[derive(Debug)]
pub enum ComponentValueParseOrTokenError<'a> {
    /// unexpected eof after SimpleBlock's starting_token or Function's function_token
    Eof,
    Token(TokenParseError<'a>),
}

#[derive(Debug)]
pub enum ComponentValueParseError<'a> {
    /// Unexpected eof after SimpleBlock's starting_token or Function's function_token
    UnexpectedEof,
    UnexpectedRightCurlyBracket(TokenAndRemaining<'a, RightCurlyBracket<'a>>),
}

pub type ComponentValueParseResult<'a, T> = Result<T, ComponentValueParseOrTokenError<'a>>;

impl<'a> SimpleBlock<'a> {
    const fn consume(
        input: TokenStreamProcess<'a>,
    ) -> Result<(Self, TokenStream<'a>), ComponentValueParseOrTokenError<'a>> {
        let before_starting = input.tokens_and_remaining();
        let (token, input) = match input.try_next() {
            Ok(v) => v,
            Err(err) => return Err(ComponentValueParseOrTokenError::Token(err)),
        };

        // Discard a token from input.
        let Some(starting_token) = (match token {
            Some(token) => token.as_simple_block_starting_token(),
            None => None,
        }) else {
            panic!("the next token of input should be an simple block starting token")
        };

        let mut value_len = 0;
        let before_value = input.tokens_and_remaining();
        let mut input = input;

        loop {
            input = {
                let before_token = input.tokens_and_remaining();

                let Ok(TokenAndRemaining {
                    token,
                    full: token_and_remaining,
                    remaining: input,
                }) = input.try_unwrap_one()
                else {
                    return Err(ComponentValueParseOrTokenError::Eof);
                };

                if let Some(surrounding) = starting_token.try_surround_with(token) {
                    // ending token
                    return Ok((
                        Self {
                            full: before_starting.before(&before_token).to_copyable(),
                            value: List {
                                full: before_value.before(&before_token).to_copyable(),
                                len: value_len,
                            },
                            surrounding,
                        },
                        input,
                    ));
                } else {
                    match ComponentValue::try_consume_one(TokenAndRemaining {
                        token,
                        remaining: input,
                        full: token_and_remaining,
                    }) {
                        Ok((_, input)) => {
                            value_len += 1;
                            match input.try_process() {
                                Ok(input) => input,
                                Err(err) => {
                                    return Err(ComponentValueParseOrTokenError::Token(err))
                                }
                            }
                        }
                        Err(err) => return Err(err),
                    }
                }
            }
        }
    }

    pub const fn surrounding_tokens(&self) -> SimpleBlockSurroundingTokens<'a> {
        self.surrounding
    }

    pub const fn value_as_original_str(&self) -> &'a str {
        self.value.full.to_str()
    }
}

impl<'a> ComponentValue<'a> {
    /// https://drafts.csswg.org/css-syntax-3/#consume-list-of-components
    const fn try_consume_one(
        tar: TokenAndRemaining<'a, Token<'a>>,
    ) -> Result<(Self, TokenStream<'a>), ComponentValueParseOrTokenError<'a>> {
        match tar.token {
            Token::Simple(
                SimpleToken::LeftCurlyBracket(_)
                | SimpleToken::LeftSquareBracket(_)
                | SimpleToken::LeftParenthesis(_),
            ) => match SimpleBlock::consume(tar.into_input()) {
                Ok((v, input)) => Ok((Self::SimpleBlock(v), input)),
                Err(err) => Err(err),
            },
            Token::IdentLike(IdentLikeToken::Function(_)) => {
                match Function::consume(tar.into_input()) {
                    Ok((v, input)) => Ok((Self::Function(v), input)),
                    Err(err) => Err(err),
                }
            }
            token => Ok((Self::PreservedTokens(token), tar.remaining)),
        }
    }

    pub const fn parse_list(input: TokenStream<'a>) -> ComponentValueParseList {
        ComponentValueParseList {
            inner: input.try_process(),
        }
    }

    pub const fn parse_list_from_str(input: &'a str) -> ComponentValueParseList {
        Self::parse_list(TokenStream::new(input))
    }

    pub(crate) const fn is_whitespace(&self) -> bool {
        match self {
            ComponentValue::PreservedTokens(t) => t.is_whitespace(),
            _ => false,
        }
    }

    pub(crate) const fn as_whitespace(&self) -> Option<&crate::token::tokens::WhitespaceToken<'a>> {
        match self {
            ComponentValue::PreservedTokens(Token::Whitespace(t)) => Some(t),
            _ => None,
        }
    }
}

pub struct ComponentValueParseList<'a> {
    pub(crate) inner: TokenParseResult<'a, TokenStreamProcess<'a>>,
}

pub(crate) enum ListParseFullError<'a, Nested: NestedConfig> {
    ComponentValue(ComponentValueParseOrTokenError<'a>),
    UnexpectedRightCurlyBracket(
        RightCurlyBracketAndRemaining<'a>,
        Nested::RightCurlyBracketIsErr,
    ),
}

impl<'a> ListParseFullError<'a, NestedFalse> {
    pub(crate) const fn into_not_nested_error(self) -> ListParseNotNestedError<'a> {
        match self {
            ListParseFullError::ComponentValue(v) => ListParseNotNestedError::ComponentValue(v),
            ListParseFullError::UnexpectedRightCurlyBracket(v, ()) => {
                ListParseNotNestedError::UnexpectedRightCurlyBracket(v)
            }
        }
    }
}

#[derive(Debug)]
pub enum ListParseNotNestedError<'a> {
    ComponentValue(ComponentValueParseOrTokenError<'a>),
    UnexpectedRightCurlyBracket(RightCurlyBracketAndRemaining<'a>),
}

macro_rules! match_no_stop_token {
    ($e:expr) => {
        match $e {
            StopTokenWithConfig::Comma(_, m) => match m {},
            StopTokenWithConfig::Semicolon(_, m) => match m {},
        }
    };
}

impl<'a> ComponentValueParseList<'a> {
    /// Returns `Ok(None, EMPTY)` is self is empty.
    ///
    /// stop_token is unset.
    /// nested is false.
    ///
    /// https://drafts.csswg.org/css-syntax-3/#consume-list-of-components
    pub const fn try_into_next(
        self,
    ) -> Result<(Option<ComponentValue<'a>>, Self), ListParseNotNestedError<'a>> {
        match self.inner {
            Ok(input) => Self::try_consume_next(input),
            Err(err) => Err(ListParseNotNestedError::ComponentValue(
                ComponentValueParseOrTokenError::Token(err),
            )),
        }
    }

    /// stop_token is unset.
    /// nested is false.
    const fn try_consume_next(
        input: TokenStreamProcess<'a>,
    ) -> Result<(Option<ComponentValue<'a>>, Self), ListParseNotNestedError<'a>> {
        match Self::try_consume_next_full::<NoStopToken, NestedFalse>(input) {
            Ok(next) => Ok(match next {
                NextFull::YieldValue(value, this) => (Some(value), this),
                NextFull::NextIsRightCurlyBracket(_, marker) => match marker {},
                NextFull::NextIsStopToken(t) => match_no_stop_token!(t.token),
                NextFull::Eof(this) => (None, this),
            }),
            Err(err) => Err(err.into_not_nested_error()),
        }
    }

    const DUMMY: Self = Self {
        inner: Err(TokenParseError::DUMMY),
    };

    const EMPTY: Self = Self {
        inner: Ok(BufferedTokenStream::EMPTY),
    };

    /// stop_token is unset.
    /// nested is false.
    pub fn try_next(&mut self) -> Result<Option<ComponentValue<'a>>, ListParseNotNestedError<'a>> {
        let this = std::mem::replace(self, Self::DUMMY);
        match this.try_into_next() {
            Ok((v, this)) => {
                *self = this;
                Ok(v)
            }
            Err(err) => {
                *self = Self::EMPTY;
                Err(err)
            }
        }
    }

    /// stop_token is unset.
    /// nested is false.
    pub const fn try_collect_into_known<
        L: IsKnownComponentValueListWithConstEmpty<'a>,
        // TODO: find out a pattern where we can remove these explicit const generics
        const ARRAY_VEC_CAP: usize,
        const LEAD_VEC_CAP: usize,
    >(
        self,
    ) -> Result<
        KnownComponentValueList<'a, L>,
        (KnownComponentValueList<'a, L>, ListParseNotNestedError<'a>),
    >
    where
        L::Collection: IsKnownCollection<
            ComponentValue<'a>,
            ArrayVecType = ArrayVec<ComponentValue<'a>, ARRAY_VEC_CAP>,
            LeadVecType = LeadVec<ComponentValue<'a>, LEAD_VEC_CAP>,
        >,
    {
        let mut input = match self.inner {
            Ok(v) => v,
            Err(err) => {
                return Err((
                    KnownComponentValueList::EMPTY,
                    ListParseNotNestedError::ComponentValue(
                        ComponentValueParseOrTokenError::Token(err),
                    ),
                ))
            }
        };

        let mut builder = KnownComponentValueList::<L>::start_builder();

        loop {
            let before_next = input.tokens_and_remaining_to_copyable();

            match Self::try_consume_next(input) {
                Ok((value, this)) => {
                    if let Some(value) = value {
                        builder = builder.with_push(value, before_next);

                        match this.inner {
                            Ok(new_input) => input = new_input,
                            Err(err) => {
                                return Err((
                                    builder.build(before_next),
                                    ListParseNotNestedError::ComponentValue(
                                        ComponentValueParseOrTokenError::Token(err),
                                    ),
                                ))
                            }
                        }
                    } else {
                        debug_assert!(
                            matches!(this.inner, Ok(input) if input.tokens_and_remaining().is_empty())
                        );
                        return Ok(builder.build(before_next));
                    }
                }
                Err(err) => return Err((builder.build(before_next), err)),
            }
        }
    }

    /// Will consume till EOF.
    ///
    /// stop_token is unset.
    /// nested is false.
    pub const fn try_count(mut self) -> Result<usize, ListParseNotNestedError<'a>> {
        let mut count = 0;

        loop {
            self = match self.try_into_next() {
                Ok((Some(_), this)) => {
                    count += 1;
                    this
                }
                Ok((None, _)) => {
                    return Ok(count);
                }
                Err(err) => return Err(err),
            }
        }
    }

    /// https://drafts.csswg.org/css-syntax-3/#consume-list-of-components
    pub(crate) const fn try_into_next_full<StopToken: StopTokenConfig, Nested: NestedConfig>(
        self,
    ) -> Result<NextFull<'a, StopToken, Nested, Self>, ListParseFullError<'a, Nested>> {
        match self.inner {
            Ok(input) => Self::try_consume_next_full(input),
            Err(err) => Err(ListParseFullError::ComponentValue(
                ComponentValueParseOrTokenError::Token(err),
            )),
        }
    }

    pub(crate) const fn try_consume_next_full<StopToken: StopTokenConfig, Nested: NestedConfig>(
        input: TokenStreamProcess<'a>,
    ) -> Result<NextFull<'a, StopToken, Nested, Self>, ListParseFullError<'a, Nested>> {
        match input.next_token_copied() {
            Some(token) => match token {
                Token::Simple(SimpleToken::RightCurlyBracket(right_curly_bracket)) => {
                    let info = RightCurlyBracketAndRemaining {
                        remaining: input.unparsed_remaining(),
                        token: right_curly_bracket,
                        full: input.tokens_and_remaining(),
                    };
                    return match Nested::RIGHT_CURLY_BRACKET_RESULT {
                        Ok(is_ok) => Ok(NextFull::NextIsRightCurlyBracket(info, is_ok)),
                        Err(is_error) => Err(ListParseFullError::UnexpectedRightCurlyBracket(
                            info, is_error,
                        )),
                    };
                }
                token => {
                    let stop_token = StopToken::STOP_TOKEN_KIND.check(token);
                    match stop_token {
                        Some(token) => {
                            // stop token
                            return Ok(NextFull::NextIsStopToken(TokenAndRemaining {
                                token,
                                remaining: input.unparsed_remaining(),
                                full: input.tokens_and_remaining(),
                            }));
                        }
                        None => {
                            match ComponentValue::try_consume_one(match input.try_unwrap_one() {
                                Ok(tar) => tar,
                                Err(_) => unreachable!(),
                            }) {
                                Ok((value, input)) => {
                                    return Ok(NextFull::YieldValue(
                                        value,
                                        Self {
                                            inner: input.try_process(),
                                        },
                                    ));
                                }
                                Err(err) => return Err(ListParseFullError::ComponentValue(err)),
                            }
                        }
                    }
                }
            },
            None => {
                // EOF
                return Ok(NextFull::Eof(Self { inner: Ok(input) }));
            }
        }
    }
}

#[derive(Debug)]
pub struct TokenAndRemaining<'a, T> {
    pub token: T,
    /// after token
    pub remaining: TokenStream<'a>,
    /// `full` means its struct, which is [`TokenAndRemaining`],
    /// which means `full == token + remaining`
    pub full: TokenStream<'a>,
}

impl<'a> TokenAndRemaining<'a, Token<'a>> {
    pub(crate) const fn into_input(self) -> TokenStreamProcess<'a> {
        TokenStreamProcess::new_buffer_filled(
            [BufferedToken {
                token: self.token,
                token_and_remaining: self.full.to_copyable(),
            }],
            self.remaining,
        )
    }
}

impl<'a, T> TokenAndRemaining<'a, T> {
    pub(crate) const fn with_token_const<V>(self, token: V) -> TokenAndRemaining<'a, V>
    where
        T: Copy,
    {
        TokenAndRemaining {
            token,
            remaining: self.remaining,
            full: self.full,
        }
    }
}

pub(crate) type RightCurlyBracketAndRemaining<'a> = TokenAndRemaining<'a, RightCurlyBracket<'a>>;

pub(crate) enum NextFull<'a, StopToken: StopTokenConfig, Nested: NestedConfig, NextThis> {
    YieldValue(ComponentValue<'a>, NextThis),
    NextIsRightCurlyBracket(
        RightCurlyBracketAndRemaining<'a>,
        Nested::RightCurlyBracketIsOk,
    ),
    NextIsStopToken(TokenAndRemaining<'a, StopTokenWithConfig<'a, StopToken>>),
    Eof(
        /// an empty stream
        NextThis,
    ),
}

pub(crate) enum StopTokenKind<StopToken: StopTokenConfig> {
    Unset(StopToken::IsUnset),
    Comma(StopToken::IsComma),
    Semicolon(StopToken::IsSemicolon),
}

impl<StopToken: StopTokenConfig> StopTokenKind<StopToken> {
    const fn test(&self, token: &Token<'_>) -> bool {
        match (self, token) {
            (Self::Comma(_), Token::Simple(SimpleToken::Comma(_)))
            | (Self::Semicolon(_), Token::Simple(SimpleToken::Semicolon(_))) => true,
            _ => false,
        }
    }

    const fn check<'a>(self, token: Token<'a>) -> Option<StopTokenWithConfig<'a, StopToken>> {
        match (self, token) {
            (Self::Comma(m), Token::Simple(SimpleToken::Comma(t))) => {
                Some(StopTokenWithConfig::Comma(t, m))
            }
            (Self::Semicolon(m), Token::Simple(SimpleToken::Semicolon(t))) => {
                Some(StopTokenWithConfig::Semicolon(t, m))
            }
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum NoStopToken {}

#[derive(Clone, Copy)]
pub(crate) enum SemicolonAsStopToken {}

pub(crate) trait StopTokenConfig: Sized {
    type IsUnset: Copy;
    type IsComma: Copy;
    type IsSemicolon: Copy;
    const STOP_TOKEN_KIND: StopTokenKind<Self>;
}

impl StopTokenConfig for NoStopToken {
    type IsUnset = ();
    type IsComma = Self;
    type IsSemicolon = Self;
    const STOP_TOKEN_KIND: StopTokenKind<Self> = StopTokenKind::Unset(());
}

impl StopTokenConfig for SemicolonAsStopToken {
    type IsUnset = Self;
    type IsComma = Self;
    type IsSemicolon = ();
    const STOP_TOKEN_KIND: StopTokenKind<Self> = StopTokenKind::Semicolon(());
}

pub(crate) trait NestedConfig {
    type RightCurlyBracketIsOk: Copy;
    type RightCurlyBracketIsErr: Copy;
    const RIGHT_CURLY_BRACKET_RESULT: Result<
        Self::RightCurlyBracketIsOk,
        Self::RightCurlyBracketIsErr,
    >;
}

#[derive(Clone, Copy)]
pub(crate) enum NestedTrue {}

impl NestedConfig for NestedTrue {
    type RightCurlyBracketIsOk = ();
    type RightCurlyBracketIsErr = Self;

    const RIGHT_CURLY_BRACKET_RESULT: Result<
        Self::RightCurlyBracketIsOk,
        Self::RightCurlyBracketIsErr,
    > = Ok(());
}

#[derive(Clone, Copy)]
pub(crate) enum NestedFalse {}

impl NestedConfig for NestedFalse {
    type RightCurlyBracketIsOk = Self;
    type RightCurlyBracketIsErr = ();

    const RIGHT_CURLY_BRACKET_RESULT: Result<
        Self::RightCurlyBracketIsOk,
        Self::RightCurlyBracketIsErr,
    > = Err(());
}

enum StopToken<'a> {
    Comma(Comma<'a>),
    Semicolon(Semicolon<'a>),
}

pub(crate) enum StopTokenWithConfig<'a, StopToken: StopTokenConfig> {
    Comma(Comma<'a>, StopToken::IsComma),
    Semicolon(Semicolon<'a>, StopToken::IsSemicolon),
}

impl<'a> StopTokenWithConfig<'a, SemicolonAsStopToken> {
    pub(crate) const fn into_semicolon(self) -> Semicolon<'a> {
        match self {
            StopTokenWithConfig::Comma(_, is_comma) => match is_comma {},
            StopTokenWithConfig::Semicolon(t, _) => t,
        }
    }
}

impl<'a> Iterator for ComponentValueParseList<'a> {
    type Item = Result<ComponentValue<'a>, ListParseNotNestedError<'a>>;

    /// stop_token is unset.
    /// nested is false.
    fn next(&mut self) -> Option<Self::Item> {
        self.try_next().transpose()
    }
}

// https://test.csswg.org/suites/css21_dev/20110323/html4/chapter-4.html
#[cfg(test)]
mod tests {
    use crate::{
        collections::array_vec::ArrayVec,
        parse::component_value::ComponentValueParseList,
        token::{stream::TokenStream, tokens::Token},
    };

    use super::{ComponentValue, Function};

    const TEST: () = {
        let input = TokenStream::new("");
        assert!(matches!(
            ComponentValue::parse_list(input).try_into_next(),
            Ok((
                None,
                ComponentValueParseList { inner: Ok(this) },
            )) if this.tokens_and_remaining().is_empty()
        ))
    };

    #[test]
    fn test() {
        () = TEST
    }

    #[test]
    fn function() {
        let (res, remaining) =
            Function::consume(TokenStream::new("rgba()").try_process().unwrap()).unwrap();
        assert!(remaining.is_empty());

        assert_eq!(res.value_as_original_str(), "");
        assert_eq!(res.full.to_str(), "rgba()");
    }

    #[test]
    fn functions() {
        let res = ComponentValue::parse_list_from_str("rgba0() rgba()")
            .try_collect_into_known::<ArrayVec<_, 3>, 3, 0>()
            .unwrap();

        let res = res.as_known_parsed_value_list();
        assert_eq!(res.full_as_str(), "rgba0() rgba()");
        //    assert_eq!( res.as_slice(),[]);
        assert!(matches!(
            res.as_slice(),
            [
                ComponentValue::Function(_),
                ComponentValue::PreservedTokens(Token::Whitespace(_)),
                ComponentValue::Function(_),
            ]
        ));
    }
}
