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
