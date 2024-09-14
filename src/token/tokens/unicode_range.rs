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
        let mut total = values.len();
        let mut has_question_mark = false;

        if total < 6 {
            while total < 6 {
                let before_next = stream.copy();
                match stream.next() {
                    Some((fc, new_stream)) if fc.to_char() == '?' => {
                        total += 1;
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
