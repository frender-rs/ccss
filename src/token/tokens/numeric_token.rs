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
                    || (matches!(b.to_char(), '+' | '-') && matches!(c, Some(c) if c.is_digit()))
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

    pub const fn full_as_str(&self) -> &'a str {
        self.full
    }

    pub const fn kind(&self) -> NumberKind {
        self.kind
    }

    // https://drafts.csswg.org/css-syntax-3/#starts-with-a-number
    // fn would_start(stream: Filtered<'_>) {
    //     1
    // }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DimensionToken<'a> {
    pub number: Number<'a>,
    pub unit: IdentSequence<'a>,
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
