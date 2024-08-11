use super::ident_sequence::IdentSequence;
use crate::input::{code_points::LEFT_PARENTHESIS, Filtered};

/// https://drafts.csswg.org/css-syntax-3/#consume-ident-like-token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IdentToken<'a>(IdentSequence<'a>);

impl<'a> IdentToken<'a> {
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

    pub(crate) const fn as_ident_sequence(&self) -> IdentSequence<'a> {
        self.0
    }
}

#[cfg(feature = "alloc")]
mod alloc {
    use alloc::borrow::Cow;

    use super::IdentToken;

    impl<'a> IdentToken<'a> {
        pub fn unescape(&self) -> Cow<'a, str> {
            self.0.unescape()
        }
    }
}
