use std::marker::PhantomData;

use serde::{de::Visitor, Deserialize};

mod literal {
    use std::marker::PhantomData;

    use serde::de;

    pub trait HasConstLiteral {
        const LITERAL: &'static str;
    }

    pub struct Literal<C: HasConstLiteral> {
        _c: PhantomData<C>,
    }

    impl<C: HasConstLiteral> Eq for Literal<C> {}

    impl<C: HasConstLiteral> PartialEq for Literal<C> {
        fn eq(&self, other: &Self) -> bool {
            self._c == other._c
        }
    }

    impl<C: HasConstLiteral> std::fmt::Debug for Literal<C> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("Literal::<")?;
            f.write_str(std::any::type_name::<Self>())?;
            f.write_str(">")
        }
    }

    impl<C: HasConstLiteral> Literal<C> {
        pub const fn new() -> Self {
            Self { _c: PhantomData }
        }
    }

    impl<'de, C: HasConstLiteral> de::Visitor<'de> for Literal<C> {
        type Value = Self;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str(C::LITERAL)
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if C::LITERAL == v {
                Ok(self)
            } else {
                Err(E::invalid_value(de::Unexpected::Str(v), &self))
            }
        }
    }

    impl<'a, C: HasConstLiteral> serde::Deserialize<'a> for Literal<C> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'a>,
        {
            deserializer.deserialize_str(Self::new())
        }
    }
}

macro_rules! define_literals {
    ($(
        $vis:vis type $Type:ident= $HasConstLiteral:ident<$($lit:literal)? $($e:block)?>;
    ),* $(,)?) => {
        $(
            $vis enum $HasConstLiteral {}
            impl crate::util::literal::HasConstLiteral for $HasConstLiteral {
                const LITERAL: &'static str = $($lit)? $($e)?;
            }
            $vis type $Type = crate::util::literal::Literal<$HasConstLiteral>;
        )*
    };
}

pub mod error {
    use serde::Deserialize;

    use super::literal::Literal;

    define_literals!(
        pub type ErrorLiteral = HasErrorLiteral<"error">;
    );

    #[test]
    fn parse_literal() {
        fn parse_str(s: &str) -> Result<ErrorLiteral, serde_json::Error> {
            serde_json::from_str(s)
        }

        assert!(parse_str("").is_err());
        assert!(parse_str("null").is_err());
        assert!(parse_str("1").is_err());
        assert!(parse_str(r##""a""##).is_err());
        assert!(parse_str(r##""ErrorLiteral""##).is_err());
        let _ = parse_str(r##""error""##).unwrap();
    }

    #[derive(Deserialize)]
    struct ErrorRaw<S>(ErrorLiteral, S);

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(from = "ErrorRaw<S>")]
    pub struct Error<S> {
        pub reason: S,
    }

    impl<S> From<ErrorRaw<S>> for Error<S> {
        fn from(ErrorRaw(Literal { .. }, reason): ErrorRaw<S>) -> Self {
            Self { reason }
        }
    }

    #[derive(Deserialize)]
    #[serde(untagged)]
    enum UntaggedResult<T, E> {
        Ok(T),
        Err(E),
    }

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(from = "UntaggedResult<T, Error<S>>")]
    pub struct MaybeError<T, S> {
        pub result: Result<T, Error<S>>,
    }

    impl<S, T> From<UntaggedResult<T, Error<S>>> for MaybeError<T, S> {
        fn from(value: UntaggedResult<T, Error<S>>) -> Self {
            Self {
                result: match value {
                    UntaggedResult::Ok(v) => Ok(v),
                    UntaggedResult::Err(v) => Err(v),
                },
            }
        }
    }

    #[test]
    // This is too verbose for new type variant:
    // instead of Variant(S), we have to write Variant((S,))
    fn test_another_design() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Error<S> {
            reason: S,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        #[serde(tag = "0")]
        #[serde(rename_all = "kebab-case")]
        enum ErrorOrIdent<S> {
            Ident((S,)),
            Error(Error<S>),
        }

        assert_eq!(
            serde_json::from_str::<ErrorOrIdent<&str>>(r##"["ident","ss"]"##).unwrap(),
            ErrorOrIdent::Ident(("ss",)),
        );

        assert_eq!(
            serde_json::from_str::<ErrorOrIdent<&str>>(r##"["error","ee"]"##).unwrap(),
            ErrorOrIdent::Error(Error { reason: "ee" }),
        );
    }
}

mod serde_seq_flatten {
    use std::marker::PhantomData;

    use serde::{
        de::{value::SeqAccessDeserializer, Visitor},
        Deserialize,
    };

    struct VisitSeqFlatten<A, B>(PhantomData<(A, B)>);

    impl<'de, T1: Deserialize<'de>, T2: Deserialize<'de>> Visitor<'de> for VisitSeqFlatten<T1, T2> {
        type Value = (T1, T2);

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a sequence")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'de>,
        {
            let a = T1::deserialize(SeqAccessDeserializer::new(&mut seq))?;
            let b = T2::deserialize(SeqAccessDeserializer::new(&mut seq))?;
            Ok((a, b))
        }
    }

    #[derive(Debug)]
    pub struct SeqFlatten<T1, T2>(pub T1, pub T2);

    impl<'de, T1: Deserialize<'de>, T2: Deserialize<'de>> Deserialize<'de> for SeqFlatten<T1, T2> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            let (a, b) = deserializer.deserialize_seq(VisitSeqFlatten::<T1, T2>(PhantomData))?;

            Ok(Self(a, b))
        }
    }
}

pub mod component_value {

    use std::borrow::Cow;

    use serde::Deserialize;

    use ccss::parse::component_value::ComponentValue as Parsed;

    use crate::util::serde_seq_flatten::SeqFlatten;

    use super::error::Error;

    define_literals!(
        type NumberLiteral = HasNumberLiteral<"number">;
    );

    #[derive(Deserialize)]
    #[serde(from = "SeqFlatten<(L, S), R>")]
    struct NumericRaw<
        L,
        S,
        // rest
        R,
    > {
        literal: L,
        representation: S,
        rest: R,
    }

    impl<L, S, R> From<SeqFlatten<(L, S), R>> for NumericRaw<L, S, R> {
        fn from(SeqFlatten((literal, representation), rest): SeqFlatten<(L, S), R>) -> Self {
            Self {
                literal,
                representation,
                rest,
            }
        }
    }

    #[derive(Deserialize)]
    struct NumberValueRaw(Value, NumberKind);

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(try_from = "NumberValueRaw")]
    pub enum NumberValue {
        Integer(i64),
        Number(f64),
    }

    impl TryFrom<NumberValueRaw> for NumberValue {
        type Error = &'static str;

        fn try_from(NumberValueRaw(value, kind): NumberValueRaw) -> Result<Self, Self::Error> {
            match (value, kind) {
                (Value::Integer(v), NumberKind::Integer) => Ok(NumberValue::Integer(v)),
                (Value::Integer(v), NumberKind::Number) => Ok(NumberValue::Number(v as f64)),
                (Value::Number(v), NumberKind::Number) => Ok(NumberValue::Number(v)),
                _ => Err("unexpected number"),
            }
        }
    }

    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Value {
        Integer(i64),
        Number(f64),
    }

    #[derive(Deserialize)]
    #[serde(rename_all = "kebab-case")]
    enum NumberKind {
        Integer,
        Number,
    }

    // impl From<Seq> for NumberValue {}

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(from = "NumericRaw<NumberLiteral, S, NumberValue>")]
    pub struct Number<S> {
        representation: S,
        value: NumberValue,
    }

    impl<S> Number<S> {
        fn map_str<SS>(self, mut f: impl FnMut(S) -> SS) -> Number<SS> {
            let Self {
                representation,
                value,
            } = self;
            Number {
                representation: f(representation),
                value,
            }
        }
    }

    impl<'a> Number<&'a str> {
        fn from_parsed(t: ccss::token::tokens::Number<'a>) -> Number<&'a str> {
            use ccss::token::tokens::NumberKind;

            let representation = t.full_as_str();

            let value = match t.kind() {
                NumberKind::Integer => NumberValue::Integer(representation.parse().unwrap()),
                NumberKind::Number => NumberValue::Number(representation.parse().unwrap()),
            };

            Self {
                representation,
                value,
            }
        }
    }

    impl<S> From<NumericRaw<NumberLiteral, S, NumberValue>> for Number<S> {
        fn from(
            NumericRaw {
                literal: _,
                representation,
                rest: value,
            }: NumericRaw<NumberLiteral, S, NumberValue>,
        ) -> Self {
            Self {
                representation,
                value,
            }
        }
    }

    #[test]
    fn parse_number() {
        assert_eq!(
            serde_json::from_str::<Number<&str>>(r#"["number", "42", 42, "integer"]"#).unwrap(),
            Number {
                representation: "42",
                value: NumberValue::Integer(42)
            }
        );
        assert_eq!(
            serde_json::from_str::<Number<&str>>(r#"["number", "42", 42, "number"]"#).unwrap(),
            Number {
                representation: "42",
                value: NumberValue::Number(42.)
            }
        );
    }

    define_literals!(
        type PercentageLiteral = HasPercentageLiteral<"percentage">;
    );

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(from = "NumericRaw<PercentageLiteral, S, NumberValue>")]
    pub struct Percentage<S> {
        representation: S,
        value: NumberValue,
    }
    impl<S> Percentage<S> {
        fn map_str<SS>(self, mut f: impl FnMut(S) -> SS) -> Percentage<SS> {
            Percentage {
                representation: f(self.representation),
                value: self.value,
            }
        }
    }

    impl<'a> Percentage<&'a str> {
        fn from_parsed(parsed: ccss::token::tokens::PercentageToken<'a>) -> Self {
            let Number {
                representation,
                value,
            } = Number::from_parsed(parsed.number);
            Self {
                representation,
                value,
            }
        }
    }

    impl<S> From<NumericRaw<PercentageLiteral, S, NumberValue>> for Percentage<S> {
        fn from(
            NumericRaw {
                literal: _,
                representation,
                rest: value,
            }: NumericRaw<PercentageLiteral, S, NumberValue>,
        ) -> Self {
            Self {
                representation,
                value,
            }
        }
    }

    define_literals!(
        type DimensionLiteral = HasDimensionLiteral<"dimension">;
    );

    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(from = "NumericRaw<DimensionLiteral, S, SeqFlatten<NumberValue, (S,)>>")]
    pub struct Dimension<S> {
        pub number: Number<S>,
        pub unit: S,
    }
    impl<S> Dimension<S> {
        fn map_str<SS>(self, mut f: impl FnMut(S) -> SS) -> Dimension<SS> {
            Dimension {
                unit: f(self.unit),
                number: self.number.map_str(f),
            }
        }
    }

    impl<S> From<NumericRaw<DimensionLiteral, S, SeqFlatten<NumberValue, (S,)>>> for Dimension<S> {
        fn from(
            NumericRaw {
                literal: _,
                representation,
                rest: SeqFlatten(value, (unit,)),
            }: NumericRaw<DimensionLiteral, S, SeqFlatten<NumberValue, (S,)>>,
        ) -> Self {
            Self {
                number: Number {
                    representation,
                    value,
                },
                unit,
            }
        }
    }

    #[test]
    fn test_dimension() {
        let v = serde_json::from_str::<Dimension<&str>>(
            r##"["dimension", "-0", 0, "integer", "red"]"##,
        )
        .unwrap();

        assert_eq!(
            v,
            Dimension {
                number: Number {
                    representation: "-0",
                    value: NumberValue::Integer(0)
                },
                unit: "red"
            }
        );
    }

    define_literals!(
        type WhitespaceLiteral = HasWhitespaceLiteral<" ">;
    );

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(from = "WhitespaceLiteral")]
    pub struct Whitespace;

    impl From<WhitespaceLiteral> for Whitespace {
        fn from(_: WhitespaceLiteral) -> Self {
            Self
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct PreservedTokens<S>(pub S);

    impl<'de, S: Deserialize<'de>> Deserialize<'de> for PreservedTokens<S> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            struct VisitOnlyStr<T>(std::marker::PhantomData<T>);

            impl<'de, T: Deserialize<'de>> serde::de::Visitor<'de> for VisitOnlyStr<T> {
                type Value = T;

                fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    formatter.write_str("a string")
                }

                fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    T::deserialize(serde::de::value::StrDeserializer::new(v))
                }

                fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    T::deserialize(serde::de::value::StringDeserializer::new(v))
                }
            }

            deserializer
                // TODO: what if S can be optimized with deserialize_string?
                .deserialize_str(VisitOnlyStr::<S>(std::marker::PhantomData))
                .map(Self)
        }
    }

    define_literals!(
        type FunctionLiteral = HasFunctionLiteral<"function">;
    );

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(from = "SeqFlatten<(FunctionLiteral, S), Vec<ComponentValue<S>>>")]
    pub struct Function<S> {
        pub name: S,
        pub arguments: Vec<ComponentValue<S>>,
    }
    impl<S> Function<S> {
        fn map_str_by_mut<SS>(self, f: &mut impl FnMut(S) -> SS) -> Function<SS> {
            Function {
                name: f(self.name),
                arguments: self
                    .arguments
                    .into_iter()
                    .map(|v| v.map_str_by_mut(f))
                    .collect(),
            }
        }
    }

    impl<S> From<SeqFlatten<(FunctionLiteral, S), Vec<ComponentValue<S>>>> for Function<S> {
        fn from(
            SeqFlatten((_, name), arguments): SeqFlatten<
                (FunctionLiteral, S),
                Vec<ComponentValue<S>>,
            >,
        ) -> Self {
            Self { name, arguments }
        }
    }

    define_literals!(
        type AtKeywordLiteral = HasAtKeywordLiteral<"at-keyword">;
    );

    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(from = "(AtKeywordLiteral, S)")]
    pub struct AtKeyword<S> {
        pub value: S,
    }
    impl<S> AtKeyword<S> {
        fn map_str<SS>(self, mut f: impl FnMut(S) -> SS) -> AtKeyword<SS> {
            AtKeyword {
                value: f(self.value),
            }
        }
    }

    impl<S> From<(AtKeywordLiteral, S)> for AtKeyword<S> {
        fn from((_, value): (AtKeywordLiteral, S)) -> Self {
            Self { value }
        }
    }

    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(rename_all = "kebab-case")]
    pub enum HashKind {
        Id,
        Unrestricted,
    }

    define_literals!(
        type HashLiteral = HasHashLiteral<"hash">;
    );

    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(from = "(HashLiteral, S, HashKind)")]
    pub struct Hash<S> {
        value: S,
        kind: HashKind,
    }
    impl<S> Hash<S> {
        fn map_str<SS>(self, mut f: impl FnMut(S) -> SS) -> Hash<SS> {
            Hash {
                value: f(self.value),
                kind: self.kind,
            }
        }
    }

    impl<S> From<(HashLiteral, S, HashKind)> for Hash<S> {
        fn from((_, value, kind): (HashLiteral, S, HashKind)) -> Self {
            Self { value, kind }
        }
    }

    define_literals!(
        type UnicodeRangeLiteral = HasUnicodeRangeLiteral<"unicode-range">;
    );

    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(from = "(UnicodeRangeLiteral, u32, u32)")]
    pub struct UnicodeRange {
        start: u32,
        end: u32,
    }

    impl From<(UnicodeRangeLiteral, u32, u32)> for UnicodeRange {
        fn from((_, start, end): (UnicodeRangeLiteral, u32, u32)) -> Self {
            Self { start, end }
        }
    }

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(untagged)]
    pub enum ComponentValue<S> {
        Whitespace(Whitespace),
        Delim(char),
        Number(Number<S>),
        Percentage(Percentage<S>),
        Dimension(Dimension<S>),
        Block(Block<S>),
        Function(Function<S>),
        AtKeyword(AtKeyword<S>),
        UnicodeRange(UnicodeRange),
        Hash(Hash<S>),
        Typed(S, S),
        PreservedTokens(PreservedTokens<S>),
    }

    impl<S> ComponentValue<S>
    where
        S: AsRef<str>,
    {
        fn normalize(
            self,
        ) -> std::iter::Chain<std::option::IntoIter<Self>, std::option::IntoIter<Self>> {
            let this = match self {
                Self::PreservedTokens(PreservedTokens(ref v)) => match v.as_ref() {
                    "^=" => {
                        return Some(Self::Delim('^'))
                            .into_iter()
                            .chain(Some(Self::Delim('=')))
                    }
                    _ => self,
                },
                Self::Block(this) => Self::Block(Block {
                    kind: this.kind,
                    content: Self::normalize_list(this.content),
                }),
                Self::Function(this) => Self::Function(Function {
                    name: this.name,
                    arguments: Self::normalize_list(this.arguments),
                }),
                _ => self,
            };

            Some(this).into_iter().chain(None)
        }

        pub(crate) fn into_iter_normalize(
            this: impl IntoIterator<Item = Self>,
        ) -> impl Iterator<Item = Self> {
            this.into_iter().flat_map(Self::normalize)
        }

        pub(crate) fn normalize_list(this: impl IntoIterator<Item = Self>) -> Vec<Self> {
            Self::into_iter_normalize(this).collect()
        }

        const DUMMY: Self = Self::Whitespace(Whitespace);

        fn confirm_all_ok<'a>(
            values: impl IntoIterator<Item = &'a mut Self>,
        ) -> Result<(), Error<S>>
        where
            S: 'a,
        {
            for v in values {
                *v = std::mem::replace(v, Self::DUMMY).confirm_ok()?
            }

            Ok(())
        }

        pub(crate) fn confirm_ok(mut self) -> Result<Self, Error<S>> {
            Ok(match self {
                ComponentValue::Typed(t, reason) if t.as_ref() == "error" => {
                    return Err(Error { reason })
                }
                ComponentValue::Block(ref mut b) => {
                    Self::confirm_all_ok(&mut b.content)?;
                    self
                }
                ComponentValue::Function(ref mut f) => {
                    Self::confirm_all_ok(&mut f.arguments)?;
                    self
                }
                _ => self,
            })
        }
    }

    impl<S> ComponentValue<S> {
        pub(crate) fn map_str<SS>(self, mut f: impl FnMut(S) -> SS) -> ComponentValue<SS> {
            self.map_str_by_mut(&mut f)
        }

        pub(crate) fn map_str_by_mut<SS>(self, f: &mut impl FnMut(S) -> SS) -> ComponentValue<SS> {
            match self {
                ComponentValue::Whitespace(v) => ComponentValue::Whitespace(v),
                ComponentValue::Delim(v) => ComponentValue::Delim(v),
                ComponentValue::Number(v) => ComponentValue::Number(v.map_str(f)),
                ComponentValue::Dimension(v) => ComponentValue::Dimension(v.map_str(f)),
                ComponentValue::Block(v) => ComponentValue::Block(v.map_str_by_mut(f)),
                ComponentValue::Typed(a, b) => ComponentValue::Typed(f(a), f(b)),
                ComponentValue::PreservedTokens(PreservedTokens(s)) => {
                    ComponentValue::PreservedTokens(PreservedTokens(f(s)))
                }
                ComponentValue::Function(v) => ComponentValue::Function(v.map_str_by_mut(f)),
                ComponentValue::AtKeyword(v) => ComponentValue::AtKeyword(v.map_str(f)),
                ComponentValue::Hash(v) => ComponentValue::Hash(v.map_str(f)),
                ComponentValue::Percentage(v) => ComponentValue::Percentage(v.map_str(f)),
                ComponentValue::UnicodeRange(v) => ComponentValue::UnicodeRange(v),
            }
        }

        /// Returns `true` if the component value is [`Whitespace`].
        ///
        /// [`Whitespace`]: ComponentValue::Whitespace
        #[must_use]
        pub(crate) const fn is_whitespace(&self) -> bool {
            matches!(self, Self::Whitespace(..))
        }

        pub(crate) fn has_error(&self) -> bool
        where
            S: AsRef<str>,
        {
            match self {
                ComponentValue::Block(b) => &b.content,
                ComponentValue::Function(f) => &f.arguments,
                ComponentValue::Typed(t, _) => return t.as_ref() == "error",
                _ => return false,
            }
            .iter()
            .any(|v| v.has_error())
        }
    }

    impl<'a> ComponentValue<Cow<'a, str>> {
        fn parse_list_from_str(input: &'a str) -> Vec<ComponentValue<Cow<'a, str>>> {
            Parsed::parse_list_from_str(input)
                .map(|v| Self::from_parsed(v.unwrap()))
                .collect()
        }

        pub(crate) fn from_parsed(v: Parsed<'a>) -> Self {
            use ccss::token::tokens::{
                IdentLikeToken, NumericToken, SimpleBlockSurroundingTokens, SimpleToken, Token,
            };
            match v {
                Parsed::PreservedTokens(t) => match t {
                    Token::Whitespace(_) => Self::Whitespace(Whitespace),
                    Token::StringToken(t) => Self::Typed("string".into(), t.value_unescape()),
                    Token::Simple(t) => Self::Delim(match t {
                        SimpleToken::LeftParenthesis(_) => todo!(),
                        SimpleToken::RightParenthesis(_) => ')',
                        SimpleToken::Comma(_) => ',',
                        SimpleToken::Colon(_) => ':',
                        SimpleToken::Semicolon(_) => ';',
                        SimpleToken::LeftSquareBracket(_) => todo!(),
                        SimpleToken::RightSquareBracket(_) => todo!(),
                        SimpleToken::LeftCurlyBracket(_) => todo!(),
                        SimpleToken::RightCurlyBracket(_) => todo!(),
                    }),
                    Token::Numeric(t) => match t {
                        NumericToken::Number(t) => {
                            Self::Number(Number::from_parsed(t).map_str(Cow::Borrowed))
                        }
                        NumericToken::Percentage(t) => {
                            Self::Percentage(Percentage::from_parsed(t).map_str(Cow::Borrowed))
                        }
                        NumericToken::Dimension(t) => Self::Dimension(Dimension {
                            number: Number::from_parsed(t.number).map_str(Cow::Borrowed),
                            unit: t.unit.unescape(),
                        }),
                    },
                    Token::IdentLike(t) => match t {
                        IdentLikeToken::Ident(t) => Self::Typed("ident".into(), t.unescape()),
                        IdentLikeToken::Function(_) => todo!(),
                        IdentLikeToken::Url(_) => todo!(),
                    },
                    Token::Cdc(t) => Self::PreservedTokens(PreservedTokens("-->".into())),
                    Token::Cdo(_) => todo!(),
                    Token::AtKeyword(t) => Self::AtKeyword(AtKeyword {
                        value: t.value().unescape(),
                    }),
                    Token::Delim(t) => Self::Delim(t.value().to_char()),
                    Token::Hash(t) => Self::Hash(Hash {
                        value: t.value().unescape(),
                        kind: match t.kind() {
                            ccss::token::tokens::HashTokenKind::Empty => HashKind::Unrestricted,
                            ccss::token::tokens::HashTokenKind::Id => HashKind::Id,
                        },
                    }),
                },
                Parsed::Function(t) => Self::Function(Function {
                    name: t.name().unescape(),
                    arguments: Self::parse_list_from_str(t.value_as_original_str()),
                }),
                Parsed::SimpleBlock(t) => Self::Block(Block {
                    kind: match t.surrounding_tokens() {
                        SimpleBlockSurroundingTokens::CurlyBracket(_, _) => BlockKind::CurlyBracket,
                        SimpleBlockSurroundingTokens::SquareBracket(_, _) => {
                            BlockKind::SquareBracket
                        }
                        SimpleBlockSurroundingTokens::Parenthesis(_, _) => BlockKind::Parenthesis,
                    },
                    content: Self::parse_list_from_str(t.value_as_original_str()),
                }),
            }
        }
    }

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(from = "SeqFlatten<(BlockKind,), Vec<ComponentValue<S>>>")]
    pub struct Block<S> {
        kind: BlockKind,
        content: Vec<ComponentValue<S>>,
    }

    impl<S> Block<S> {
        fn map_str_by_mut<SS>(self, f: &mut impl FnMut(S) -> SS) -> Block<SS> {
            let Self { kind, content } = self;
            Block {
                kind,
                content: content.into_iter().map(|v| v.map_str_by_mut(f)).collect(),
            }
        }
    }

    impl<S> From<SeqFlatten<(BlockKind,), Vec<ComponentValue<S>>>> for Block<S> {
        fn from(
            SeqFlatten((kind,), content): SeqFlatten<(BlockKind,), Vec<ComponentValue<S>>>,
        ) -> Self {
            Self { kind, content }
        }
    }

    #[test]
    fn test_block() {
        assert_eq!(
            serde_json::from_str::<Block<&str>>(r#"["{}"]"#).unwrap(),
            Block {
                kind: BlockKind::CurlyBracket,
                content: vec![]
            },
        );
    }

    #[derive(Deserialize, Debug, PartialEq)]
    pub enum BlockKind {
        #[serde(rename = "{}")]
        CurlyBracket,
        #[serde(rename = "[]")]
        SquareBracket,
        #[serde(rename = "()")]
        Parenthesis,
    }

    #[test]
    fn test() {
        assert_eq!(
            serde_json::from_str::<ComponentValue<&str>>(r##"["ident","id"]"##).unwrap(),
            ComponentValue::Typed("ident", "id")
        );
        assert_eq!(
            serde_json::from_str::<Vec<ComponentValue<&str>>>(r##"[["ident","id"]]"##).unwrap(),
            vec![ComponentValue::Typed("ident", "id")]
        );
    }
}

pub mod declaration {
    use serde::Deserialize;

    use super::component_value::ComponentValue;

    define_literals!(
        type DeclarationLiteral = HasDeclarationLiteral<"declaration">;
    );

    #[derive(Deserialize)]
    struct DeclarationRaw<S>(DeclarationLiteral, S, Vec<ComponentValue<S>>, bool);

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(from = "DeclarationRaw<S>")]
    pub struct Declaration<S> {
        pub name: S,
        pub value: Vec<ComponentValue<S>>,
        pub important: bool,
    }

    pub type Parsed<'a, const CAP: usize> = ccss::parse::declaration::Declaration<
        'a,
        ccss::collections::array_vec::ArrayVec<
            ccss::parse::component_value::ComponentValue<'a>,
            CAP,
        >,
    >;

    impl<S> Declaration<S> {
        pub(crate) fn from_parsed<'a, const CAP: usize>(parsed: Parsed<'a, CAP>) -> Self
        where
            S: From<&'a str>,
            S: From<std::borrow::Cow<'a, str>>,
        {
            Self {
                name: parsed.name().unescape().into(),
                value: {
                    parsed
                        .value_as_slice()
                        .iter()
                        .map(|v| ComponentValue::from_parsed(*v).map_str(S::from))
                        .collect()
                },
                important: parsed.is_important(),
            }
        }
    }

    impl<S> From<DeclarationRaw<S>> for Declaration<S> {
        fn from(
            DeclarationRaw(DeclarationLiteral, name, value, important): DeclarationRaw<S>,
        ) -> Self {
            Self {
                name,
                value,
                important,
            }
        }
    }

    pub struct ExpectedDeclarationList<S>(pub Vec<Declaration<S>>);
}

pub mod at_rule {
    use serde::Deserialize;

    use super::component_value::ComponentValue;

    define_literals!(
        type AtRuleLiteral = HasAtRuleLiteral<"at-rule">;
    );

    #[derive(Deserialize)]
    struct AtRuleRaw<S>(
        AtRuleLiteral,
        S,
        Vec<ComponentValue<S>>,
        Option<Vec<ComponentValue<S>>>,
    );

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(from = "AtRuleRaw<S>")]
    pub struct AtRule<S> {
        name: S,
        prelude: Vec<ComponentValue<S>>,
        block_content: Option<Vec<ComponentValue<S>>>,
    }

    impl<S> From<AtRuleRaw<S>> for AtRule<S> {
        fn from(AtRuleRaw(_, name, prelude, block_content): AtRuleRaw<S>) -> Self {
            Self {
                name,
                prelude,
                block_content,
            }
        }
    }
}

pub mod declaration_rule_list {
    use serde::Deserialize;

    use crate::util::{
        component_value::ComponentValue,
        error::{Error, MaybeError},
    };

    use super::{at_rule::AtRule, declaration::Declaration};

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(untagged)]
    pub enum DeclarationRule<S> {
        Declaration(Declaration<S>),
        AtRule(AtRule<S>),
    }

    impl<S> DeclarationRule<S> {
        /// Returns `true` if the declaration rule is [`AtRule`].
        ///
        /// [`AtRule`]: DeclarationRule::AtRule
        #[must_use]
        pub fn is_at_rule(&self) -> bool {
            matches!(self, Self::AtRule(..))
        }
    }

    #[test]
    fn test() {
        fn parse_str(s: &str) -> Result<DeclarationRule<&str>, serde_json::Error> {
            serde_json::from_str(s)
        }

        assert_eq!(
            parse_str(r#"["declaration", "a", [["ident", "b"]], false]"#).unwrap(),
            DeclarationRule::Declaration(Declaration {
                name: "a",
                value: vec![ComponentValue::Typed("ident", "b")],
                important: false
            })
        );
    }

    #[test]
    fn test_error() {
        fn parse_str(
            s: &str,
        ) -> Result<MaybeError<DeclarationRule<&str>, &str>, serde_json::Error> {
            serde_json::from_str(s)
        }

        assert_eq!(
            parse_str(r#"["error", "invalid"]"#)
                .unwrap()
                .result
                .unwrap_err(),
            Error { reason: "invalid" }
        );
    }

    #[test]
    fn test_escaped() {
        fn parse_str(
            s: &str,
        ) -> Result<
            MaybeError<DeclarationRule<std::borrow::Cow<str>>, std::borrow::Cow<str>>,
            serde_json::Error,
        > {
            serde_json::from_str(s)
        }

        assert_eq!(
            parse_str(r#"["error", "invalid\t"]"#)
                .unwrap()
                .result
                .unwrap_err(),
            Error {
                reason: "invalid\t".into()
            }
        );
    }
}

#[derive(Debug, PartialEq)]
pub struct TestSuite<T, E> {
    pub input: T,
    pub expected: E,
}

pub struct TestSuites<T, E>(pub Vec<TestSuite<T, E>>);

impl<T, E> TestSuites<T, E> {
    pub(crate) fn with_append(mut self, mut tests: Self) -> Self {
        self.0.append(&mut tests.0);
        self
    }
}

impl<'de, T: Deserialize<'de>, E: Deserialize<'de>> Deserialize<'de> for TestSuites<T, E> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct FlatVisitor<T, E>(PhantomData<(T, E)>);

        impl<'de, T: Deserialize<'de>, E: Deserialize<'de>> Visitor<'de> for FlatVisitor<T, E> {
            type Value = Vec<TestSuite<T, E>>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a test suite sequence")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut res = Vec::new();

                while let Some(a) = seq.next_element::<T>()? {
                    let Some(b) = seq.next_element::<E>()? else {
                        return Err(<A::Error as serde::de::Error>::invalid_length(
                            res.len() * 2 + 1,
                            &"length of even number",
                        ));
                    };
                    res.push(TestSuite {
                        input: a,
                        expected: b,
                    })
                }

                return Ok(res);
            }
        }

        deserializer
            .deserialize_seq(FlatVisitor(PhantomData))
            .map(TestSuites)
    }
}
