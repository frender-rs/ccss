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

    use serde::Deserialize;

    use ccss::parse::component_value::ComponentValue as Parsed;

    use crate::util::serde_seq_flatten::SeqFlatten;

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

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(untagged)]
    pub enum ComponentValue<S> {
        Whitespace(Whitespace),
        Delim(char),
        Number(Number<S>),
        Block(Block<S>),
        Typed(S, S),
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
                ComponentValue::Block(v) => ComponentValue::Block(v.map_str_by_mut(f)),
                ComponentValue::Typed(a, b) => ComponentValue::Typed(f(a), f(b)),
            }
        }
    }

    impl<'a> ComponentValue<&'a str> {
        pub(crate) fn from_parsed(v: Parsed<'a>) -> Self {
            use ccss::token::tokens::{IdentLikeToken, NumericToken, Token};
            match v {
                Parsed::PreservedTokens(t) => match t {
                    Token::Whitespace(_) => Self::Whitespace(Whitespace),
                    Token::StringToken(_) => todo!(),
                    Token::Simple(_) => todo!(),
                    Token::Numeric(t) => match t {
                        NumericToken::Number(t) => Self::Number(Number::from_parsed(t)),
                        NumericToken::Percentage(_) => todo!(),
                        NumericToken::Dimension(_) => todo!(),
                    },
                    Token::IdentLike(t) => match t {
                        IdentLikeToken::Ident(t) => Self::Typed("ident", t.to_str()),
                        IdentLikeToken::Function(_) => todo!(),
                        IdentLikeToken::Url(_) => todo!(),
                    },
                    Token::Cdc(_) => todo!(),
                    Token::Cdo(_) => todo!(),
                    Token::AtKeyword(_) => todo!(),
                    Token::Delim(t) => Self::Delim(t.value().to_char()),
                    Token::Hash(_) => todo!(),
                },
                Parsed::Function(_) => todo!(),
                Parsed::SimpleBlock(_) => todo!(),
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
