use crate::collections::array_vec::ArrayVec;
use crate::input::FilteredChar;

use super::hex_digit::HexDigit;
use super::whitespace::Whitespace;

pub struct HexDigits<const N: usize> {
    hex_digits: ArrayVec<char, N>, // must be HexDigit
    white_space: Option<Whitespace>,
}

impl<const N: usize> HexDigits<N> {
    pub(crate) const fn new() -> Self {
        Self {
            hex_digits: ArrayVec::EMPTY,
            white_space: None,
        }
    }

    pub(crate) const fn with(mut self, d: HexDigit) -> Self {
        assert!(self.white_space.is_none());
        assert!(self.can_push());

        self.hex_digits = self.hex_digits.with_push(d.to_char());

        self
    }

    pub(crate) const fn with_whitespace(mut self, ws: Whitespace) -> Self {
        assert!(self.white_space.is_none());
        self.white_space = Some(ws);
        self
    }

    pub(crate) const fn can_push(&self) -> bool {
        self.hex_digits.len() < N
    }

    pub(crate) const fn len(&self) -> usize {
        self.hex_digits.len()
    }

    pub(crate) const fn to_u32(&self) -> u32 {
        let mut res = 0u32;

        let hex_digits = self.hex_digits.as_slice();
        let mut i = hex_digits.len();

        let mut val = 1;
        loop {
            let digit = hex_digits[i];
            let Some(digit) = digit.to_digit(16) else {
                unreachable!()
            };

            res += digit * val;

            if i > 0 {
                val <<= 4; // val *= 16;
                i -= 1;
            } else {
                break;
            }
        }

        res
    }

    pub(crate) const fn to_code_point(&self) -> FilteredChar {
        let n = self.to_u32();

        match n {
            0 | 0xD800..=0xDBFF | 0xDC00..=0xDFFF => FilteredChar::REPLACEMENT_CHARACTER,
            n if n > 0x10FFFF => FilteredChar::REPLACEMENT_CHARACTER,
            n => FilteredChar::from_char(match char::from_u32(n) {
                Some(c) => c,
                None => unreachable!(), // we have already checked n is valid for char, so just panic
            }),
        }
    }
}
