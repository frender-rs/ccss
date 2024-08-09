use crate::token::stream::CopyableTokenStream;

use super::{
    array_vec::ArrayVec,
    count::Count,
    known::{IsKnownCollection, KnownCollection},
    lead_vec::LeadVec,
    HasConstDummyValue,
};

pub trait IsKnownParsedValueList<T, const CAP: usize>: IsKnownCollection<T, CAP> + Sized {
    const EMPTY_LIST_AS_KNOWN_COLLECTION: KnownCollection<Self, T, CAP>;
}

impl<T, const CAP: usize> IsKnownParsedValueList<T, CAP> for ArrayVec<T, CAP>
where
    T: HasConstDummyValue,
{
    const EMPTY_LIST_AS_KNOWN_COLLECTION: KnownCollection<Self, T, CAP> =
        KnownCollection::<Self, T, CAP>::from_collection(Self::EMPTY);
}

impl<T, const CAP: usize> IsKnownParsedValueList<T, CAP> for LeadVec<T, CAP>
where
    T: HasConstDummyValue,
{
    const EMPTY_LIST_AS_KNOWN_COLLECTION: KnownCollection<Self, T, CAP> =
        KnownCollection::<Self, T, CAP>::from_collection(Self::EMPTY);
}

impl<T, const CAP: usize> IsKnownParsedValueList<T, CAP> for Count {
    const EMPTY_LIST_AS_KNOWN_COLLECTION: KnownCollection<Self, T, CAP> =
        KnownCollection::<Self, T, CAP>::from_collection(Self::EMPTY);
}

#[derive(Debug, Clone, Copy)]
pub struct KnownParsedValueList<'a, L: IsKnownParsedValueList<T, CAP>, T, const CAP: usize> {
    /// full contains parsed and unparsed
    full: CopyableTokenStream<'a>,
    parsed: KnownCollection<L, T, CAP>,
    unparsed: CopyableTokenStream<'a>,
}

impl<'a, L: IsKnownParsedValueList<T, CAP>, T, const CAP: usize>
    KnownParsedValueList<'a, L, T, CAP>
{
    pub(crate) const EMPTY: Self = Self {
        full: CopyableTokenStream::EMPTY,
        parsed: L::EMPTY_LIST_AS_KNOWN_COLLECTION,
        unparsed: CopyableTokenStream::EMPTY,
    };

    pub(crate) const fn start_builder() -> KnownParsedValueListBuilder<'a, L, T, CAP> {
        KnownParsedValueListBuilder::Empty
    }

    pub const fn full(&self) -> CopyableTokenStream<'a> {
        self.full
    }

    pub const fn full_as_str(&self) -> &'a str {
        self.full.to_str()
    }

    pub(crate) const fn parsed(&self) -> &KnownCollection<L, T, CAP> {
        &self.parsed
    }
}

impl<'a, T, const CAP: usize> KnownParsedValueList<'a, Count, T, CAP> {
    pub(crate) const fn new_count(full: CopyableTokenStream<'a>, len: usize) -> Self {
        KnownParsedValueList {
            full,
            parsed: KnownCollection::<Count, T, CAP>::from_collection(Count { len }),
            unparsed: full,
        }
    }
}

pub(crate) enum KnownParsedValueListBuilder<
    'a,
    L: IsKnownParsedValueList<T, CAP>,
    T,
    const CAP: usize,
> {
    Empty,
    AllParsed {
        parsed_and_remaining: CopyableTokenStream<'a>,
        parsed: KnownCollection<L, T, CAP>,
    },
    SomeNotParsed {
        full_and_remaining: CopyableTokenStream<'a>,
        parsed: KnownCollection<L, T, CAP>,
        unparsed_and_remaining: CopyableTokenStream<'a>,
    },
}

impl<'a, L: IsKnownParsedValueList<T, CAP>, T, const CAP: usize>
    KnownParsedValueListBuilder<'a, L, T, CAP>
{
    pub(crate) const fn build(
        self,
        remaining: CopyableTokenStream<'a>,
    ) -> KnownParsedValueList<'a, L, T, CAP>
    where
        T: Copy,
    {
        match self {
            KnownParsedValueListBuilder::Empty => {
                let empty = remaining.before(remaining);
                KnownParsedValueList {
                    full: empty,
                    parsed: L::EMPTY_LIST_AS_KNOWN_COLLECTION,
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
                let parsed = L::EMPTY_LIST_AS_KNOWN_COLLECTION;

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
