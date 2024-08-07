use super::hex_digit::HexDigit;
use super::whitespace::Whitespace;

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
