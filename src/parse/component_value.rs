use std::marker::PhantomData;

use crate::token::{
    stream::{
        BufferedToken, BufferedTokenStreamUnwrapOne, CopyableTokenStream, TokenStream,
        TokenStreamProcess,
    },
    tokens::{
        ident_like_token::{FunctionToken, IdentLikeToken},
        simple_token::{RightCurlyBracket, RightParenthesis, SimpleToken},
        token::{SimpleBlockSurroundingTokens, Token, TokenParseError, TokenParseResult},
    },
};

/// https://drafts.csswg.org/css-syntax-3/#component-value
pub enum ComponentValue<'a> {
    PreservedTokens(Token<'a>),
    Function(Function<'a>),
    SimpleBlock(SimpleBlock<'a>),
}

pub struct Function<'a> {
    full: CopyableTokenStream<'a>,
    function_token: FunctionToken<'a>,
    value: List<'a>,
    right_parenthesis: RightParenthesis<'a>,
}

pub struct List<'a> {
    full: CopyableTokenStream<'a>,
    len: usize,
}

impl<'a> Function<'a> {
    const fn consume(
        input: TokenStreamProcess<'a>,
    ) -> Result<(Self, TokenStream<'a>), ComponentValueParseError<'a>> {
        let original = input.tokens_and_remaining();
        let Some((function_token, input)) = (match input.try_unwrap_one() {
            Ok(BufferedTokenStreamUnwrapOne {
                buf_token:
                    BufferedToken {
                        token_and_remaining: _,
                        token,
                    },
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
            let before_token = input.to_copyable();
            input = match input.try_next() {
                Ok((token, input)) => match token {
                    Some(token) => {
                        if let Some(right_parenthesis) = token.as_right_parenthesis() {
                            return Ok((
                                Self {
                                    full: original.before(&input).to_copyable(),
                                    function_token,
                                    value: List {
                                        full: before_value.before(&input).to_copyable(),
                                        len: value_len,
                                    },
                                    right_parenthesis,
                                },
                                input,
                            ));
                        } else {
                            match ComponentValue::try_consume_one(
                                TokenStreamProcess::new_buffer_filled(
                                    [BufferedToken {
                                        token,
                                        token_and_remaining: before_token,
                                    }],
                                    input,
                                ),
                            ) {
                                Ok((_, input)) => {
                                    value_len += 1;
                                    input
                                }
                                Err(err) => return Err(err),
                            }
                        }
                    }
                    None => {
                        return Err(ComponentValueParseError::Eof);
                    }
                },
                Err(err) => return Err(ComponentValueParseError::Token(err)),
            }
        }
    }
}

pub struct SimpleBlock<'a> {
    full: CopyableTokenStream<'a>,
    value: List<'a>,
    surrounding: SimpleBlockSurroundingTokens<'a>,
}

pub enum ComponentValueParseError<'a> {
    /// unexpected eof after SimpleBlock's starting_token or Function's function_token
    Eof,
    Token(TokenParseError<'a>),
}

pub type ComponentValueParseResult<'a, T> = Result<T, ComponentValueParseError<'a>>;

impl<'a> SimpleBlock<'a> {
    const fn consume(
        input: TokenStreamProcess<'a>,
    ) -> Result<(Self, TokenStream<'a>), ComponentValueParseError<'a>> {
        let before_starting = input.tokens_and_remaining();
        let (token, input) = match input.try_next() {
            Ok(v) => v,
            Err(err) => return Err(ComponentValueParseError::Token(err)),
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

                let Ok(BufferedTokenStreamUnwrapOne {
                    buf_token,
                    remaining: input,
                }) = input.try_unwrap_one()
                else {
                    return Err(ComponentValueParseError::Eof);
                };

                let token = buf_token.token;

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
                    match ComponentValue::try_consume_one(TokenStreamProcess::new_buffer_filled(
                        [buf_token],
                        input,
                    )) {
                        Ok((_, input)) => {
                            value_len += 1;
                            match input.try_process() {
                                Ok(input) => input,
                                Err(err) => return Err(ComponentValueParseError::Token(err)),
                            }
                        }
                        Err(err) => return Err(err),
                    }
                }
            }
        }
    }
}

impl<'a> ComponentValue<'a> {
    /// Panics if `input` is empty.
    ///
    /// https://drafts.csswg.org/css-syntax-3/#consume-list-of-components
    const fn try_consume_one(
        input: TokenStreamProcess<'a>,
    ) -> Result<(Self, TokenStream<'a>), ComponentValueParseError<'a>> {
        match input.next_token_copied() {
            Some(Token::Simple(
                SimpleToken::LeftCurlyBracket(_)
                | SimpleToken::LeftSquareBracket(_)
                | SimpleToken::LeftParenthesis(_),
            )) => match SimpleBlock::consume(input) {
                Ok((v, input)) => Ok((Self::SimpleBlock(v), input)),
                Err(err) => Err(err),
            },
            Some(Token::IdentLike(IdentLikeToken::Function(_))) => match Function::consume(input) {
                Ok((v, input)) => Ok((Self::Function(v), input)),
                Err(err) => Err(err),
            },
            Some(token) => Ok((
                Self::PreservedTokens(token),
                match input.try_unwrap_one() {
                    Ok(res) => res.remaining,
                    Err(_) => unreachable!(),
                },
            )),
            None => unreachable!(),
        }
    }

    /// `stop token` is unset.
    /// `nested` is false
    pub const fn consume_list(
        input: TokenStream<'a>,
    ) -> ComponentValueConsumeList<'a, NoStopToken> {
        ComponentValueConsumeList {
            input,
            _options: PhantomData,
        }
    }

    /// `stop token` is semicolon.
    /// `nested` is false
    pub const fn consume_list_with_semicolon_as_stop_token(
        input: TokenStream<'a>,
    ) -> ComponentValueConsumeList<'a, NoStopToken> {
        ComponentValueConsumeList {
            input,
            _options: PhantomData,
        }
    }
}

pub struct ComponentValueConsumeList<'a, StopToken: HasComponentValueConsumeListStopToken> {
    input: TokenStream<'a>,
    _options: PhantomData<StopToken>,
}

pub enum ComponentValueListParseNotNestedError<'a> {
    ComponentValue(ComponentValueParseError<'a>),
    UnexpectedRightCurlyBracket {
        unexpected: RightCurlyBracket<'a>,
        remaining: TokenStream<'a>,
    },
}

impl<'a, StopToken: HasComponentValueConsumeListStopToken>
    ComponentValueConsumeList<'a, StopToken>
{
    /// Returns `Ok(None)` is self is empty.
    /// https://drafts.csswg.org/css-syntax-3/#consume-list-of-components
    pub const fn try_next(
        self,
    ) -> Result<Option<(ComponentValue<'a>, Self)>, ComponentValueListParseNotNestedError<'a>> {
        match self.input.try_process() {
            Ok(input) => match input.next_token_copied() {
                Some(token) => match token {
                    Token::Simple(SimpleToken::RightCurlyBracket(unexpected)) => {
                        return Err(
                            ComponentValueListParseNotNestedError::UnexpectedRightCurlyBracket {
                                unexpected,
                                remaining: input.unparsed_remaining(),
                            },
                        );
                    }
                    _ => match ComponentValue::try_consume_one(input) {
                        Ok((value, input)) => {
                            return Ok(Some((
                                value,
                                Self {
                                    input,
                                    _options: PhantomData,
                                },
                            )))
                        }
                        Err(err) => {
                            return Err(ComponentValueListParseNotNestedError::ComponentValue(err))
                        }
                    },
                },
                None => {
                    // EOF
                    return Ok(None);
                }
            },
            Err(err) => {
                return Err(ComponentValueListParseNotNestedError::ComponentValue(
                    ComponentValueParseError::Token(err),
                ))
            }
        }
    }

    /// Will consume till EOF
    pub const fn try_count(mut self) -> Result<usize, ComponentValueListParseNotNestedError<'a>> {
        let mut count = 0;

        loop {
            self = match self.try_next() {
                Ok(Some((_, this))) => {
                    count += 1;
                    this
                }
                Ok(None) => {
                    return Ok(count);
                }
                Err(err) => return Err(err),
            }
        }
    }

    /// Returns `Ok(None)` if self is empty.
    /// https://drafts.csswg.org/css-syntax-3/#consume-list-of-components
    pub const fn try_next_nested(
        self,
    ) -> Result<ComponentValueNextNested<'a, StopToken>, ComponentValueParseError<'a>> {
        match self.input.try_process() {
            Ok(input) => match input.next_token_copied() {
                Some(token) => match token {
                    Token::Simple(SimpleToken::RightCurlyBracket(right_curly_bracket)) => {
                        return Ok(ComponentValueNextNested::NextIsRightCurlyBracket(
                            NextIsRightCurlyBracket {
                                remaining: input.unparsed_remaining(),
                                right_curly_bracket,
                                right_curly_bracket_and_remaining: input.tokens_and_remaining(),
                            },
                        ));
                    }
                    token if false => {
                        // STOP_TOKEN
                        todo!()
                    }
                    _ => match ComponentValue::try_consume_one(input) {
                        Ok((value, input)) => {
                            return Ok(ComponentValueNextNested::YieldValue(
                                value,
                                Self {
                                    input,
                                    _options: PhantomData,
                                },
                            ))
                        }
                        Err(err) => return Err(err),
                    },
                },
                None => {
                    // EOF
                    return Ok(ComponentValueNextNested::Eof);
                }
            },
            Err(err) => return Err(ComponentValueParseError::Token(err)),
        }
    }

    /// Will consume till EOF or RightCurlyBracket
    pub const fn try_count_nested(
        mut self,
    ) -> Result<(usize, Option<NextIsRightCurlyBracket<'a>>), ComponentValueParseError<'a>> {
        let mut count = 0;

        loop {
            self = match self.try_next_nested() {
                Ok(ComponentValueNextNested::YieldValue(_, this)) => {
                    count += 1;
                    this
                }
                Ok(ComponentValueNextNested::NextIsRightCurlyBracket(n)) => {
                    return Ok((count, Some(n)));
                }
                Ok(ComponentValueNextNested::Eof) => {
                    return Ok((count, None));
                }
                Err(err) => return Err(err),
            }
        }
    }
}

pub struct NextIsRightCurlyBracket<'a> {
    pub right_curly_bracket: RightCurlyBracket<'a>,
    /// TokenStream after right_curly_bracket
    pub remaining: TokenStream<'a>,
    /// starts with RightCurlyBracket
    pub right_curly_bracket_and_remaining: TokenStream<'a>,
}

pub enum ComponentValueNextNested<'a, StopToken: HasComponentValueConsumeListStopToken> {
    YieldValue(ComponentValue<'a>, ComponentValueConsumeList<'a, StopToken>),
    NextIsRightCurlyBracket(NextIsRightCurlyBracket<'a>),
    Eof,
}

pub struct ComponentValueConsumeListOptions {
    stop_token: Option<ComponentValueConsumeListStopToken>,
    nested: bool,
}

pub enum ComponentValueConsumeListStopToken {
    Comma,
    Semicolon,
}

pub trait HasComponentValueConsumeListError {
    type ComponentValueConsumeListError<'a>;
}

struct Nested<const NESTED: bool>;

impl Nested<true> {
    const fn error_from_component_value_parse_error(
        err: ComponentValueParseError,
    ) -> ComponentValueParseError {
        err
    }
}

impl Nested<false> {
    const fn error_from_component_value_parse_error(
        err: ComponentValueParseError,
    ) -> ComponentValueListParseNotNestedError {
        ComponentValueListParseNotNestedError::ComponentValue(err)
    }
}

impl HasComponentValueConsumeListError for Nested<true> {
    type ComponentValueConsumeListError<'a> = ComponentValueParseError<'a>;
}

impl HasComponentValueConsumeListError for Nested<false> {
    type ComponentValueConsumeListError<'a> = ComponentValueListParseNotNestedError<'a>;
}

pub trait HasComponentValueConsumeListStopToken {
    const COMPONENT_VALUE_CONSUME_LIST_STOP_TOKEN: Option<ComponentValueConsumeListStopToken>;
}

pub enum NoStopToken {}

impl HasComponentValueConsumeListStopToken for NoStopToken {
    const COMPONENT_VALUE_CONSUME_LIST_STOP_TOKEN: Option<ComponentValueConsumeListStopToken> =
        None;
}
