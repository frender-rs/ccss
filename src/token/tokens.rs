pub mod whitespace_token {
    pub use self::whitespace::Whitespace;

    use crate::input::Filtered;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct WhitespaceToken<'a>(&'a str);

    impl<'a> WhitespaceToken<'a> {
        pub const ONE_SPACE: Self = WhitespaceToken(" ");

        pub const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
            let mut new_stream = stream.copy();
            let original = stream;
            while let Some((_, s)) = Whitespace::consume(new_stream.copy()) {
                new_stream = s
            }

            let v = konst::option::map!(original.str_before_non_empty(&new_stream), Self);

            (v, new_stream)
        }

        pub(crate) const fn new(s: &'a str) -> Self {
            let (this, stream) = Self::consume(Filtered::new(s));

            if let Some(this) = this {
                stream.assert_empty();
                this
            } else {
                panic!("expect whitespace")
            }
        }
    }

    mod whitespace {
        use crate::input::{code_points::LF, Filtered};

        /// https://drafts.csswg.org/css-syntax-3/#whitespace
        pub struct Whitespace(char);

        impl Whitespace {
            const fn try_new(c: char) -> Option<Self> {
                // A newline, U+0009 CHARACTER TABULATION, or U+0020 SPACE
                match c {
                    LF | '\u{0009}' | '\u{0020}' => Some(Self(c)),
                    _ => None,
                }
            }

            pub const fn consume(stream: Filtered) -> Option<(Whitespace, Filtered)> {
                if let Some((fc, stream)) = stream.next() {
                    if let Some(this) = Self::try_new(fc.to_char()) {
                        return Some((this, stream));
                    }
                }

                None
            }
        }
    }
}

mod string_token {

    use crate::input::{
        code_points::{LF, REVERSE_SOLIDUS},
        Filtered, FilteredChar,
    };

    use super::{errors::Eof, escaped_code_point::EscapedCodePoint};

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct StringToken<'a> {
        full: &'a str,
        starting_code_point_str: &'a str,
        ending_code_point_str: &'a str,
    }

    #[derive(Debug)]
    pub enum StringTokenParseError<'a> {
        Eof {
            full: &'a str,
            starting_code_point_str: &'a str,
            expected_ending_code_point: FilteredChar,
        },
        /// Unexpected [`LF`]
        Newline {
            before_new_line: &'a str,
            stream_starting_with_new_line: Filtered<'a>,
        },
    }

    impl<'a> StringToken<'a> {
        /// Unlike [`Self::consume_after_starting_code_point`], this method assumes that the starting code point hasn't been consumed yet.
        ///
        /// This method will return `Some(..)` if and only if
        /// the first code point of stream is `U+0022 QUOTATION MARK (")` or `U+0027 APOSTROPHE (')`.
        ///
        pub const fn consume(
            stream: Filtered<'a>,
        ) -> Result<(Option<Self>, Filtered<'a>), StringTokenParseError> {
            let original = stream.copy();

            let (fc, stream) = stream.consume_n_code_points::<1>();

            if !matches!(fc.to_chars_padding_zero(), ['"' | '\'']) {
                return Ok((None, original));
            }

            let Some(fc) = fc.first() else { unreachable!() };

            let starting_code_point_str = original.str_before(&stream);
            match Self::consume_after_starting_code_point(stream, fc) {
                Ok((ending_code_point_str, stream)) => Ok((
                    Some(Self {
                        full: original.str_before(&stream),
                        starting_code_point_str,
                        ending_code_point_str,
                    }),
                    stream,
                )),
                Err(err) => Err(match err {
                    ConsumeAfterStartingCodePointError::Eof => StringTokenParseError::Eof {
                        full: original.0.as_str(),
                        starting_code_point_str,
                        expected_ending_code_point: fc,
                    },
                    ConsumeAfterStartingCodePointError::Newline {
                        stream_starting_with_new_line,
                    } => StringTokenParseError::Newline {
                        before_new_line: original.str_before(&stream_starting_with_new_line),
                        stream_starting_with_new_line,
                    },
                }),
            }
        }

        /// `Ok(..)` returns `ending_code_point_str` and the new stream after `ending_code_point`.
        ///
        /// https://drafts.csswg.org/css-syntax-3/#consume-string-token
        const fn consume_after_starting_code_point(
            mut stream: Filtered<'a>,
            ending_code_point: FilteredChar,
        ) -> Result<(&'a str, Filtered<'a>), ConsumeAfterStartingCodePointError> {
            loop {
                let current_stream = stream.copy();

                if let Some((fc, new_stream)) = stream.next() {
                    match fc.to_char() {
                        LF => {
                            return Err(ConsumeAfterStartingCodePointError::Newline {
                                stream_starting_with_new_line: current_stream,
                            });
                        }
                        REVERSE_SOLIDUS => {
                            match EscapedCodePoint::consume(new_stream.copy()) {
                                Ok((None, new_stream)) => {
                                    // LF
                                    stream = new_stream;
                                }
                                Ok((Some(_), new_stream)) => {
                                    // a valid escape code point
                                    stream = new_stream;
                                }
                                Err(Eof) => {
                                    // EOF
                                    // do nothing and let next iteration process EOF
                                    stream = new_stream
                                }
                            }
                        }
                        u if u == ending_code_point.to_char() => {
                            let ending_code_point_str = current_stream.str_before(&new_stream);
                            return Ok((ending_code_point_str, new_stream));
                        }
                        _ => {
                            // anything else
                            stream = new_stream
                        }
                    }
                } else {
                    return Err(ConsumeAfterStartingCodePointError::Eof);
                }
            }
        }
    }

    pub enum ConsumeAfterStartingCodePointError<'a> {
        Eof,
        Newline {
            stream_starting_with_new_line: Filtered<'a>,
        },
    }
}

pub mod token {
    use crate::input::{code_points::is_ident_code_point, Filtered, FilteredChar, FilteredCharVec};

    use super::escaped_code_point::EscapedCodePoint;
    use super::ident_like_token::{FunctionToken, IdentLikeToken, UrlParseError};

    use super::ident_sequence::IdentSequence;
    use super::simple_token::{
        LeftCurlyBracket, LeftParenthesis, LeftSquareBracket, RightCurlyBracket, RightParenthesis,
        RightSquareBracket,
    };
    use super::{
        numeric_token::NumericToken,
        simple_token::SimpleToken,
        string_token::{StringToken, StringTokenParseError},
        whitespace_token::WhitespaceToken,
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

    #[derive(Debug)]
    pub enum TokenParseError<'a> {
        StringToken(StringTokenParseError<'a>),
        Url(UrlParseError<'a>),
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
                            Ok((Some(ident), stream)) => {
                                return out(Self::IdentLike(ident), stream)
                            }
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
                    // TODO: this is a parse error. Return a <delim-token> with its value set to the current input code point
                    ShouldProcessDelimToken
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

    #[derive(Clone, Copy)]
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
}

pub mod simple_token {
    use crate::input::Filtered;

    macro_rules! define_simple_tokens {
        (
            $SimpleToken:ident ($(
                $const_name:ident = $name:ident ($e:expr)
            ),* $(,)?)
        ) => {
            $(
                #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                pub struct $name<'a>(&'a str);

                impl $name<'_> {
                    pub const CHAR: char = $e;
                    const CHAR_BYTE: [u8; 1] = {
                        assert!(Self::CHAR.is_ascii());
                        let byte = Self::CHAR as u8;
                        [byte]
                    };
                    const CHAR_STR: &'static str = {
                        match std::str::from_utf8(&Self::CHAR_BYTE) {
                            Ok(s) => s,
                            Err(_) => unreachable!(),
                        }
                    };
                    pub const DEFAULT: Self = Self(Self::CHAR_STR);
                }

                impl<'a> $name<'a> {
                    pub const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
                        match stream.copy().next() {
                            Some((fc, new_stream)) if fc.to_char() == Self::CHAR => {
                                (
                                    Some(Self(stream.str_before(&new_stream))),
                                    new_stream
                                )
                            }
                            _ => (None, stream),
                        }
                    }
                }
            )*

            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum $SimpleToken<'a> {
                $(
                    $name($name<'a>),
                )*
            }

            impl<'a> $SimpleToken<'a> {
                $(
                    pub const $const_name: Self = Self::$name($name::DEFAULT);
                )*
                pub const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
                    match stream.copy().next() {
                        Some((fc, new_stream)) => {
                            match fc.to_char() {
                                $(
                                    $name::CHAR => (
                                        Some(Self::$name($name(stream.str_before(&new_stream)))),
                                        new_stream
                                    ),
                                )*
                                _ => (None, stream),
                            }
                        }
                        _ => (None, stream),
                    }
                }
            }
        };
    }

    define_simple_tokens!(SimpleToken(
        LEFT_PARENTHESIS = LeftParenthesis('\u{0028}'),
        RIGHT_PARENTHESIS = RightParenthesis('\u{0029}'),
        COMMA = Comma('\u{002C}'),
        COLON = Colon('\u{003A}'),
        SEMICOLON = Semicolon('\u{003B}'),
        LEFT_SQUARE_BRACKET = LeftSquareBracket('\u{005B}'),
        RIGHT_SQUARE_BRACKET = RightSquareBracket('\u{005D}'),
        LEFT_CURLY_BRACKET = LeftCurlyBracket('\u{007B}'),
        RIGHT_CURLY_BRACKET = RightCurlyBracket('\u{007D}'),
    ));
}

pub mod numeric_token {

    use crate::input::{code_points::PERCENTAGE_SIGN, Filtered};

    use super::ident_sequence::IdentSequence;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum NumberKind {
        Integer,
        Number,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum NumberSign {
        Positive,
        Negative,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Number<'a> {
        full: &'a str,
        kind: NumberKind,
        sign: Option<NumberSign>,
    }

    impl<'a> Number<'a> {
        /// Returns `Some(..)` if and only if `stream` would
        /// [start a number](https://drafts.csswg.org/css-syntax-3/#starts-with-a-number)
        pub const fn consume(mut stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
            let original = stream.copy();
            let mut kind = NumberKind::Integer;
            let mut sign = None;

            match stream.copy().next() {
                Some((fc, new_stream)) => match fc.to_char() {
                    // U+002B PLUS SIGN (+) or U+002D HYPHEN-MINUS (-)
                    '\u{002B}' => {
                        sign = Some(NumberSign::Positive);
                        stream = new_stream;
                    }
                    '\u{002D}' => {
                        sign = Some(NumberSign::Negative);
                        stream = new_stream;
                    }
                    _ => {}
                },
                _ => {}
            }

            let (number, mut stream) = stream.consume_digits();

            let stream_before_next_two = stream.copy();
            match stream.next_two() {
                // U+002E FULL STOP (.) followed by a digit
                Some(([a, b], new_stream)) if a.to_char() == '.' && b.is_digit() => {
                    (_, stream) = new_stream.consume_digits();
                    kind = NumberKind::Number;
                }
                Some(([a, b], new_stream)) if matches!(a.to_char(), 'E' | 'e') => {
                    if number.is_empty() {
                        return (None, original);
                    }

                    let (c, new_stream) = if let Some((fc, new_stream)) = new_stream.copy().next() {
                        (Some(fc), new_stream)
                    } else {
                        (None, new_stream)
                    };

                    if b.is_digit()
                        || (matches!(b.to_char(), '+' | '-')
                            && matches!(c, Some(c) if c.is_digit()))
                    {
                        (_, stream) = new_stream.consume_digits();
                        kind = NumberKind::Number;
                    } else {
                        stream = stream_before_next_two;
                    }
                }
                _ => {
                    if number.is_empty() {
                        return (None, original);
                    }
                    stream = stream_before_next_two
                }
            }

            (
                Some(Self {
                    full: original.str_before(&stream),
                    kind,
                    sign,
                }),
                stream,
            )
        }

        pub(crate) const fn new(full: &'a str) -> Self {
            let (this, remaining) = Self::consume(Filtered::new(full));
            match this {
                Some(this) => {
                    remaining.assert_empty();
                    this
                }
                None => panic!("not a Number"),
            }
        }

        pub(crate) const fn new_integer(full: &'a str) -> Self {
            let this = Self::new(full);
            assert!(matches!(this.kind, NumberKind::Integer), "not an integer");
            this
        }

        // https://drafts.csswg.org/css-syntax-3/#starts-with-a-number
        // fn would_start(stream: Filtered<'_>) {
        //     1
        // }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct DimensionToken<'a> {
        number: Number<'a>,
        unit: IdentSequence<'a>,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct PercentageToken<'a> {
        // TODO: spec doesn't keep its type (number.kind)
        pub number: Number<'a>,
        pub percentage: &'a str,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum NumericToken<'a> {
        Number(Number<'a>),
        Percentage(PercentageToken<'a>),
        Dimension(DimensionToken<'a>),
    }

    impl<'a> NumericToken<'a> {
        /// Returns `Some(..)` if and only if [`Number::consume`] Returns `Some(..)`.
        pub const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
            let (number, stream) = Number::consume(stream);

            let Some(number) = number else {
                return (None, stream);
            };

            let (this, stream) = Self::consume_after_number(number, stream);
            (Some(this), stream)
        }

        const fn consume_after_number(
            number: Number<'a>,
            stream: Filtered<'a>,
        ) -> (Self, Filtered<'a>) {
            let (unit, stream) = IdentSequence::consume(stream.copy());
            if let Some(unit) = unit {
                return (Self::Dimension(DimensionToken { number, unit }), stream);
            }

            let original = stream.copy();

            match stream.next() {
                Some((fc, new_stream)) if fc.to_char() == PERCENTAGE_SIGN => (
                    Self::Percentage(PercentageToken {
                        percentage: original.str_before(&new_stream),
                        number,
                    }),
                    new_stream,
                ),
                _ => (Self::Number(number), original),
            }
        }
    }
}

pub mod ident_like_token {
    use super::errors::Eof;

    use crate::input::{
        code_points::{LEFT_PARENTHESIS, REVERSE_SOLIDUS, RIGHT_PARENTHESIS},
        Filtered,
    };

    use super::escaped_code_point::EscapedCodePoint;
    use super::whitespace_token::WhitespaceToken;
    use super::{ident_sequence::IdentSequence, ident_token::IdentToken};

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
}

pub mod ident_token {

    use super::ident_sequence::IdentSequence;
    use crate::input::{code_points::LEFT_PARENTHESIS, Filtered};

    /// https://drafts.csswg.org/css-syntax-3/#consume-ident-like-token
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct IdentToken<'a>(IdentSequence<'a>);

    impl<'a> IdentToken<'a> {
        pub const fn to_str(self) -> &'a str {
            self.0.to_str()
        }

        pub const fn new_const(s: &'a str) -> Self {
            let (v, stream) = Self::consume(Filtered::new(s));

            if let Some(v) = v {
                let remaining = stream.0.as_str();
                konst::const_panic::concat_assert!(
                    remaining.is_empty(),
                    "invalid tokens after ident token: ",
                    remaining
                );
                v
            } else {
                konst::const_panic::concat_panic!("invalid ident token: ", s);
            }
        }

        const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
            let original = stream.copy();
            let (ident_sequence, stream) = IdentSequence::consume(stream);

            let Some(ident_sequence) = ident_sequence else {
                return (None, stream);
            };

            if matches!(
                stream.first_n_code_points::<1>().as_char_slice(),
                [LEFT_PARENTHESIS]
            ) {
                // not an <ident-token>
                return (None, original);
            }

            (Some(IdentToken(ident_sequence)), stream)
        }

        pub(crate) const fn from_ident_sequence(v: IdentSequence<'a>) -> Self {
            Self(v)
        }
    }
}

pub mod hex_digit {
    use super::hex_digits::HexDigits;
    use crate::input::Filtered;

    /// https://drafts.csswg.org/css-syntax-3/#hex-digit
    pub struct HexDigit(char);
    impl HexDigit {
        pub const fn to_char(self) -> char {
            self.0
        }

        pub const fn test(u: char) -> bool {
            match u {
                '\u{0030}'..='\u{0039}' | '\u{0041}'..='\u{0046}' | '\u{0061}'..='\u{0066}' => true,
                _ => false,
            }
        }

        pub const fn try_new(u: char) -> Option<Self> {
            if Self::test(u) {
                Some(Self(u))
            } else {
                None
            }
        }

        pub const fn consume(stream: Filtered) -> (Option<Self>, Filtered) {
            let original = stream.copy();
            if let Some((fc, this)) = stream.next() {
                if let Some(d) = Self::try_new(fc.to_char()) {
                    return (Some(d), this);
                }
            }

            (None, original)
        }

        pub const fn consume_at_most_n<const N: usize>(
            mut stream: Filtered,
        ) -> (HexDigits<N>, Filtered) {
            let mut res = HexDigits::new();
            while res.can_push() {
                let d;
                (d, stream) = HexDigit::consume(stream);

                if let Some(d) = d {
                    res = res.with(d);
                } else {
                    break;
                }
            }

            (res, stream)
        }
    }
}

pub mod hex_digits {
    use super::hex_digit::HexDigit;
    use super::whitespace_token::Whitespace;

    pub struct HexDigits<const N: usize> {
        hex_digits: [char; N], // must be HexDigit
        len: usize,
        white_space: Option<Whitespace>,
    }

    impl<const N: usize> HexDigits<N> {
        pub const fn new() -> Self {
            Self {
                hex_digits: ['\0'; N],
                len: 0,
                white_space: None,
            }
        }

        pub const fn with(mut self, d: HexDigit) -> Self {
            assert!(self.can_push());

            self.hex_digits[self.len] = d.to_char();
            self.len += 1;

            self
        }

        pub const fn with_whitespace(mut self, ws: Whitespace) -> Self {
            self.white_space = Some(ws);
            self
        }

        pub const fn can_push(&self) -> bool {
            self.len < N
        }

        pub(crate) const fn len(&self) -> usize {
            self.len
        }
    }
}

mod ident_sequence {

    use crate::input::{
        code_points::{
            is_ident_code_point, is_ident_start_code_point, HYPHEN_MINUS, REVERSE_SOLIDUS,
        },
        Filtered, FilteredCharVec,
    };

    use super::escaped_code_point::EscapedCodePoint;

    /// https://drafts.csswg.org/css-syntax-3/#consume-name
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct IdentSequence<'a>(&'a str);

    impl<'a> IdentSequence<'a> {
        pub const fn to_str(self) -> &'a str {
            self.0
        }

        pub const fn match_url_ascii_case_insensitive(&self) -> bool {
            match self.0.as_bytes() {
                [b'u' | b'U', b'r' | b'R', b'l' | b'L'] => true,
                _ => false,
            }
        }

        /// Returns `Some(..)` if and only if [`IdentSequence::would_start(&stream)`](Self::would_start) returns true.
        pub const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
            if !Self::would_start(&stream) {
                return (None, stream);
            }

            let (ident, stream) = Self::consume_anyway(stream);

            assert!(!ident.to_str().is_empty());

            (Some(ident), stream)
        }

        /// consume anyway, without confirming that [would_start](Self::would_start) returns true.
        ///
        /// returned IdentSequence might be empty
        pub const fn consume_anyway(stream: Filtered<'a>) -> (Self, Filtered<'a>) {
            let original_stream = stream.copy();
            let mut remaining = stream;

            while let Some((fc, new_stream)) = remaining.copy().next() {
                match fc.to_char() {
                    u if is_ident_code_point(u) => {
                        remaining = new_stream;
                    }
                    REVERSE_SOLIDUS => {
                        if let Ok((Some(e), new_stream)) = EscapedCodePoint::consume(new_stream) {
                            let _ = e;
                            remaining = new_stream;
                        } else {
                            // LF | EOF
                            // don't set remaining = new_stream
                            break;
                        }
                    }
                    _ => {
                        // don't set remaining = new_stream
                        break;
                    }
                }
            }

            let s = original_stream.str_before(&remaining);

            (Self(s), remaining)
        }

        /// https://drafts.csswg.org/css-syntax-3/#would-start-an-identifier
        pub const fn would_start(stream: &Filtered<'a>) -> bool {
            Self::chars_would_start(stream.first_n_code_points())
        }

        pub const fn chars_would_start(chars: FilteredCharVec<3>) -> bool {
            match chars.to_chars_padding_zero() {
                [HYPHEN_MINUS, b, c]
                    if (b == HYPHEN_MINUS || is_ident_start_code_point(b))
                        || EscapedCodePoint::chars_would_start(chars.crop_and_fit(1)) =>
                {
                    true
                }
                [a, _, _] if is_ident_start_code_point(a) => true,
                _ => EscapedCodePoint::chars_would_start(chars.fit_or_keep_first_n()),
            }
        }
    }
}

mod escaped_code_point {

    use crate::input::{
        code_points::{LF, REVERSE_SOLIDUS},
        Filtered, FilteredChar, FilteredCharVec,
    };

    use super::{
        errors::Eof, hex_digit::HexDigit, hex_digits::HexDigits, whitespace_token::Whitespace,
    };

    /// https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point
    pub enum EscapedCodePoint {
        HexDigits(HexDigits<6>),
        Other(FilteredChar),
    }

    impl EscapedCodePoint {
        /// It assumes that the `U+005C REVERSE SOLIDUS (\)` has already been consumed.
        ///
        /// Returns Ok(None) if and only if next code point is [`LF`]
        ///
        /// Returns Err(Eof) if and only if stream is empty.
        ///
        /// https://drafts.csswg.org/css-syntax-3/#check-if-two-code-points-are-a-valid-escape
        ///
        /// https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point
        pub const fn consume(stream: Filtered) -> Result<(Option<Self>, Filtered), Eof> {
            let (fc, mut this) = match stream.next() {
                Some(v) => v,
                None => return Err(Eof),
            };

            Ok(match fc.to_char() {
                LF => (None, this),
                u => {
                    if let Some(d) = HexDigit::try_new(u) {
                        let mut hex_digits = HexDigits::new().with(d);

                        while hex_digits.can_push() {
                            let d;
                            (d, this) = HexDigit::consume(this);

                            if let Some(d) = d {
                                hex_digits = hex_digits.with(d);
                            } else {
                                break;
                            }
                        }

                        if let Some((ws, new_this)) = Whitespace::consume(this.copy()) {
                            this = new_this;
                            hex_digits = hex_digits.with_whitespace(ws);
                        }

                        (Some(EscapedCodePoint::HexDigits(hex_digits)), this)
                    } else {
                        (Some(EscapedCodePoint::Other(fc)), this)
                    }
                }
            })
        }

        /// https://drafts.csswg.org/css-syntax-3/#starts-with-a-valid-escape
        pub const fn chars_would_start(chars: FilteredCharVec<2>) -> bool {
            match chars.to_chars_padding_zero() {
                [REVERSE_SOLIDUS, b] => b != LF,
                _ => false,
            }
        }
    }
}

mod unicode_range {
    use crate::input::{Filtered, FilteredCharVec};

    use super::hex_digit::HexDigit;

    #[derive(Debug, Clone, Copy)]
    pub struct UnicodeRangeToken<'a> {
        full: &'a str,
        value: UnicodeRangeTokenValue<'a>,
    }

    #[derive(Debug, Clone, Copy)]
    pub enum UnicodeRangeTokenValue<'a> {
        WithQuestionMark,
        WithRange { start: &'a str, end: &'a str },
        WithOne,
    }

    impl<'a> UnicodeRangeToken<'a> {
        pub const fn chars_starts(chars: FilteredCharVec<3>) -> bool {
            match chars.to_chars_padding_zero() {
                ['U' | 'u', '+', c] if c == '?' || HexDigit::test(c) => true,
                _ => false,
            }
        }

        /// https://drafts.csswg.org/css-syntax-3/#consume-unicode-range-token
        pub const fn consume(stream: Filtered<'a>) -> (Option<Self>, Filtered<'a>) {
            if !Self::would_start(&stream) {
                return (None, stream);
            }

            let original = stream.copy();

            let stream = stream.consume_n_code_points::<2>().1;

            let (values, mut stream) = HexDigit::consume_at_most_n::<6>(stream);
            let total = values.len();
            let mut has_question_mark = false;

            if total < 6 {
                while total <= 6 {
                    let before_next = stream.copy();
                    match stream.next() {
                        Some((fc, new_stream)) if fc.to_char() == '?' => {
                            has_question_mark = true;
                            stream = new_stream;
                        }
                        _ => {
                            stream = before_next;
                            break;
                        }
                    }
                }
            }

            let first_segment = original.str_before(&stream);

            if has_question_mark {
                return (
                    Some(UnicodeRangeToken {
                        full: first_segment,
                        value: UnicodeRangeTokenValue::WithQuestionMark,
                    }),
                    stream,
                );
            }

            let start_of_range = first_segment;

            let before_consume_2 = stream.copy();
            let (a, stream) = stream.consume_n_code_points::<1>();
            let after_consume_1 = stream.copy();
            let (b, mut stream) = stream.consume_n_code_points::<1>();
            if matches!(
                [a.to_chars_padding_zero()[0], b.to_chars_padding_zero()[0]],
                ['-', b] if HexDigit::test(b)
            ) {
                (_, stream) = HexDigit::consume_at_most_n::<5>(stream);

                let end_of_range = after_consume_1.str_before(&stream);
                return (
                    Some(Self {
                        full: original.str_before(&stream),
                        value: UnicodeRangeTokenValue::WithRange {
                            start: start_of_range,
                            end: end_of_range,
                        },
                    }),
                    stream,
                );
            } else {
                return (
                    Some(Self {
                        full: start_of_range,
                        value: UnicodeRangeTokenValue::WithOne,
                    }),
                    before_consume_2,
                );
            }
        }

        pub(crate) const fn would_start(stream: &Filtered<'a>) -> bool {
            Self::chars_starts(stream.first_n_code_points())
        }
    }
}

mod token_or_unicode_range {
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
}

mod errors;
