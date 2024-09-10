use crate::token::stream::CopyableTokenStream;

use super::{
    array_vec::ArrayVec,
    known::{IsKnownCollection, IsKnownCollectionWithConstEmpty, KnownCollection},
    lead_vec::LeadVec,
};

pub struct KnownParsedValueList<'a, L: IsKnownCollection<T>, T: Copy> {
    /// full contains parsed and unparsed
    full: CopyableTokenStream<'a>,
    parsed: KnownCollection<L, T>,
    unparsed: CopyableTokenStream<'a>,
}

impl<'a, L: IsKnownCollection<T>, T: Copy> std::fmt::Debug for KnownParsedValueList<'a, L, T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("KnownParsedValueList")
            .field("full", &self.full)
            .field("parsed", &self.parsed)
            .field("unparsed", &self.unparsed)
            .finish()
    }
}

impl<'a, L: IsKnownCollection<T>, T: Copy> Clone for KnownParsedValueList<'a, L, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, L: IsKnownCollection<T>, T: Copy> Copy for KnownParsedValueList<'a, L, T> {}

impl<'a, L: IsKnownCollectionWithConstEmpty<T>, T: Copy> KnownParsedValueList<'a, L, T> {
    pub(crate) const EMPTY: Self = Self {
        full: CopyableTokenStream::EMPTY,
        parsed: L::EMPTY_COLLECTION_AS_KNOWN,
        unparsed: CopyableTokenStream::EMPTY,
    };
}

impl<'a, L: IsKnownCollection<T>, T: Copy> KnownParsedValueList<'a, L, T> {
    pub(crate) const fn start_builder() -> KnownParsedValueListBuilder<'a, L, T> {
        KnownParsedValueListBuilder::Empty
    }

    pub const fn full(&self) -> CopyableTokenStream<'a> {
        self.full
    }

    pub const fn full_as_str(&self) -> &'a str {
        self.full.to_str()
    }

    pub(crate) const fn parsed(&self) -> &KnownCollection<L, T> {
        &self.parsed
    }
}

/// Only this variant has all parsed values
impl<'a, T: Copy, const CAP: usize> KnownParsedValueList<'a, ArrayVec<T, CAP>, T> {
    pub const fn as_array_vec(&self) -> &ArrayVec<T, CAP> {
        debug_assert!(self.unparsed.to_str().is_empty());
        self.parsed.as_variant()
    }
    pub const fn as_slice(&self) -> &[T] {
        self.as_array_vec().as_slice()
    }
}

impl<'a, T: Copy> KnownParsedValueList<'a, super::count::Count, T> {
    pub const fn count(&self) -> usize {
        self.parsed.as_variant().len
    }
}

pub(crate) enum KnownParsedValueListBuilder<'a, L: IsKnownCollection<T>, T: Copy> {
    Empty,
    AllParsed {
        parsed_and_remaining: CopyableTokenStream<'a>,
        parsed: KnownCollection<L, T>,
    },
    SomeNotParsed {
        full_and_remaining: CopyableTokenStream<'a>,
        parsed: KnownCollection<L, T>,
        unparsed_and_remaining: CopyableTokenStream<'a>,
    },
}

impl<
        'a,
        L: IsKnownCollectionWithConstEmpty<
            T,
            ArrayVecType = ArrayVec<T, ARRAY_VEC_CAP>,
            LeadVecType = LeadVec<T, LEAD_VEC_CAP>,
        >,
        T: Copy,
        const ARRAY_VEC_CAP: usize,
        const LEAD_VEC_CAP: usize,
    > KnownParsedValueListBuilder<'a, L, T>
{
    pub(crate) const fn build(
        self,
        remaining: CopyableTokenStream<'a>,
    ) -> KnownParsedValueList<'a, L, T>
    where
        T: Copy,
    {
        match self {
            KnownParsedValueListBuilder::Empty => {
                let empty = remaining.before(remaining);
                KnownParsedValueList {
                    full: empty,
                    parsed: L::EMPTY_COLLECTION_AS_KNOWN,
                    unparsed: empty,
                }
            }
            KnownParsedValueListBuilder::AllParsed {
                parsed_and_remaining,
                parsed,
            } => KnownParsedValueList {
                full: parsed_and_remaining.before(remaining),
                parsed,
                unparsed: remaining.before(remaining),
            },
            KnownParsedValueListBuilder::SomeNotParsed {
                full_and_remaining,
                parsed,
                unparsed_and_remaining,
            } => KnownParsedValueList {
                full: full_and_remaining.before(remaining),
                parsed,
                unparsed: unparsed_and_remaining.before(remaining),
            },
        }
    }

    pub(crate) const fn with_push(
        self,
        value: T,
        value_and_remaining: CopyableTokenStream<'a>,
    ) -> Self
    where
        T: Copy,
    {
        match self {
            KnownParsedValueListBuilder::Empty => {
                let parsed = L::EMPTY_COLLECTION_AS_KNOWN;

                let (parsed, fake) = parsed.with_push_maybe_fake(value);

                if matches!(fake, Some(_)) {
                    Self::SomeNotParsed {
                        full_and_remaining: value_and_remaining,
                        parsed,
                        unparsed_and_remaining: value_and_remaining,
                    }
                } else {
                    Self::AllParsed {
                        parsed_and_remaining: value_and_remaining,
                        parsed,
                    }
                }
            }
            KnownParsedValueListBuilder::AllParsed {
                parsed_and_remaining,
                parsed,
            } => {
                let (parsed, fake) = parsed.with_push_maybe_fake(value);
                if matches!(fake, Some(_)) {
                    Self::SomeNotParsed {
                        full_and_remaining: parsed_and_remaining,
                        parsed,
                        unparsed_and_remaining: value_and_remaining,
                    }
                } else {
                    Self::AllParsed {
                        parsed_and_remaining,
                        parsed,
                    }
                }
            }
            this @ KnownParsedValueListBuilder::SomeNotParsed { .. } => this,
        }
    }
}
