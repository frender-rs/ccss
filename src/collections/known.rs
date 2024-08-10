use crate::const_known::{No, Yes, YesOrNo};

use super::{
    array_vec::ArrayVec, collect_nothing::CollectNothing, count::Count, lead_vec::LeadVec,
    HasConstDummyValue,
};

#[non_exhaustive]
#[derive(Clone, Copy)]
pub enum KnownCollection<V: IsKnownCollection> {
    ArrayVec(V::ArrayVec, V::ArrayVecType),
    LeadVec(V::LeadVec, V::LeadVecType),
    Count(V::Count, Count),
    CollectNothing(V::CollectNothing, CollectNothing),
}

mod sealed {
    pub trait IsKnownCollection {}
    pub trait IsArrayVec {}
    pub trait IsLeadVec {}
}

pub trait IsArrayVec: sealed::IsArrayVec {
    type Item;
}
impl<T, const N: usize> sealed::IsArrayVec for ArrayVec<T, N> {}
impl<T, const N: usize> IsArrayVec for ArrayVec<T, N> {
    type Item = T;
}

pub trait IsLeadVec: sealed::IsLeadVec {
    type Item;
}
impl<T, const N: usize> sealed::IsLeadVec for LeadVec<T, N> {}
impl<T, const N: usize> IsLeadVec for LeadVec<T, N> {
    type Item = T;
}

pub trait IsKnownCollection: sealed::IsKnownCollection + Sized {
    type ArrayVecType: IsArrayVec<Item = <Self::LeadVecType as IsLeadVec>::Item>;
    type ArrayVec: YesOrNo;
    type LeadVecType: IsLeadVec;
    type LeadVec: YesOrNo;
    type Count: YesOrNo;
    type CollectNothing: YesOrNo;
}

pub trait IsKnownCollectionWithConstEmpty: IsKnownCollection {
    const EMPTY_COLLECTION: Self;
    const EMPTY_COLLECTION_AS_KNOWN_COLLECTION: KnownCollection<Self>;
}

impl<T, const CAP: usize> sealed::IsKnownCollection for ArrayVec<T, CAP> {}
impl<T, const CAP: usize> IsKnownCollection for ArrayVec<T, CAP> {
    type ArrayVecType = Self;
    type ArrayVec = Yes;
    type LeadVecType = LeadVec<T, 0>;
    type LeadVec = No;
    type Count = No;
    type CollectNothing = No;
}
impl<T: HasConstDummyValue, const CAP: usize> IsKnownCollectionWithConstEmpty for ArrayVec<T, CAP> {
    const EMPTY_COLLECTION: Self = Self::EMPTY;
    const EMPTY_COLLECTION_AS_KNOWN_COLLECTION: KnownCollection<Self> =
        KnownCollection::<Self>::from_variant(Self::EMPTY);
}

impl<T, const CAP: usize> sealed::IsKnownCollection for LeadVec<T, CAP> {}
impl<T, const CAP: usize> IsKnownCollection for LeadVec<T, CAP> {
    type ArrayVecType = ArrayVec<T, 0>;
    type ArrayVec = No;
    type LeadVecType = Self;
    type LeadVec = Yes;
    type Count = No;
    type CollectNothing = No;
}
impl<T: HasConstDummyValue, const CAP: usize> IsKnownCollectionWithConstEmpty for LeadVec<T, CAP> {
    const EMPTY_COLLECTION: Self = Self::EMPTY;
    const EMPTY_COLLECTION_AS_KNOWN_COLLECTION: KnownCollection<Self> =
        KnownCollection::<Self>::from_variant(Self::EMPTY);
}

impl sealed::IsKnownCollection for Count {}
impl IsKnownCollection for Count {
    type ArrayVecType = ArrayVec<No, 0>;
    type ArrayVec = No;
    type LeadVecType = LeadVec<No, 0>;
    type LeadVec = No;
    type Count = Yes;
    type CollectNothing = No;
}
impl IsKnownCollectionWithConstEmpty for Count {
    const EMPTY_COLLECTION: Self = Self::EMPTY;
    const EMPTY_COLLECTION_AS_KNOWN_COLLECTION: KnownCollection<Self> =
        KnownCollection::<Self>::from_variant(Self::EMPTY);
}

impl sealed::IsKnownCollection for CollectNothing {}
impl IsKnownCollection for CollectNothing {
    type ArrayVecType = ArrayVec<No, 0>;
    type ArrayVec = No;
    type LeadVecType = LeadVec<No, 0>;
    type LeadVec = No;
    type Count = No;
    type CollectNothing = Yes;
}
impl IsKnownCollectionWithConstEmpty for CollectNothing {
    const EMPTY_COLLECTION: Self = Self;
    const EMPTY_COLLECTION_AS_KNOWN_COLLECTION: KnownCollection<Self> =
        KnownCollection::<Self>::from_variant(Self);
}

impl<T, const CAP: usize> KnownCollection<ArrayVec<T, CAP>> {
    pub const fn from_variant(v: ArrayVec<T, CAP>) -> Self {
        Self::ArrayVec(Yes, v)
    }
}
impl<T, const CAP: usize> KnownCollection<LeadVec<T, CAP>> {
    pub const fn from_variant(v: LeadVec<T, CAP>) -> Self {
        Self::LeadVec(Yes, v)
    }
}
impl KnownCollection<Count> {
    pub const fn from_variant(v: Count) -> Self {
        Self::Count(Yes, v)
    }
}
impl KnownCollection<CollectNothing> {
    pub const fn from_variant(v: CollectNothing) -> Self {
        Self::CollectNothing(Yes, v)
    }
}

impl<T, const CAP: usize> KnownCollection<ArrayVec<T, CAP>> {
    pub const fn as_variant(&self) -> &ArrayVec<T, CAP> {
        match self {
            Self::ArrayVec(Yes, this) => this,
            Self::LeadVec(no, _) => match *no {},
            Self::Count(no, _) => match *no {},
            Self::CollectNothing(no, _) => match *no {},
        }
    }
}
impl<T, const CAP: usize> KnownCollection<LeadVec<T, CAP>> {
    pub const fn as_variant(&self) -> &LeadVec<T, CAP> {
        match self {
            Self::ArrayVec(no, _) => match *no {},
            Self::LeadVec(Yes, this) => this,
            Self::Count(no, _) => match *no {},
            Self::CollectNothing(no, _) => match *no {},
        }
    }
}
impl KnownCollection<Count> {
    pub const fn as_variant(&self) -> &Count {
        match self {
            Self::ArrayVec(no, _) => match *no {},
            Self::LeadVec(no, _) => match *no {},
            Self::Count(Yes, this) => this,
            Self::CollectNothing(no, _) => match *no {},
        }
    }
}
impl KnownCollection<CollectNothing> {
    pub const fn as_variant(&self) -> &CollectNothing {
        match self {
            Self::ArrayVec(no, _) => match *no {},
            Self::LeadVec(no, _) => match *no {},
            Self::Count(no, _) => match *no {},
            Self::CollectNothing(Yes, this) => this,
        }
    }
}

impl KnownCollection<CollectNothing> {
    pub const fn into_variant(self) -> CollectNothing {
        match self {
            Self::ArrayVec(no, _) => match no {},
            Self::LeadVec(no, _) => match no {},
            Self::Count(no, _) => match no {},
            Self::CollectNothing(Yes, this) => this,
        }
    }
}

impl<V, Item, const ARRAY_VEC_CAP: usize, const LEAD_VEC_CAP: usize> std::fmt::Debug
    for KnownCollection<V>
where
    Item: std::fmt::Debug,
    V: IsKnownCollection<
        ArrayVecType = ArrayVec<Item, ARRAY_VEC_CAP>,
        LeadVecType = LeadVec<Item, LEAD_VEC_CAP>,
    >,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArrayVec(_, this) => this.fmt(f),
            Self::LeadVec(_, this) => this.fmt(f),
            Self::Count(_, this) => this.fmt(f),
            Self::CollectNothing(_, this) => this.fmt(f),
        }
    }
}

impl<V, T, const ARRAY_VEC_CAP: usize, const LEAD_VEC_CAP: usize> KnownCollection<V>
where
    V: IsKnownCollection<
        ArrayVecType = ArrayVec<T, ARRAY_VEC_CAP>,
        LeadVecType = LeadVec<T, LEAD_VEC_CAP>,
    >,
{
    pub const fn with_push(self, value: T) -> Self
    where
        T: Copy,
    {
        match self {
            KnownCollection::ArrayVec(y, this) => {
                KnownCollection::ArrayVec(y, this.with_push(value))
            }
            KnownCollection::LeadVec(y, this) => KnownCollection::LeadVec(y, this.with_push(value)),
            KnownCollection::Count(y, mut this) => KnownCollection::Count(y, {
                this.len += 1;
                this
            }),
            this @ KnownCollection::CollectNothing(_, _) => this,
        }
    }

    pub const fn with_push_maybe_fake(self, value: T) -> (Self, Option<T>)
    where
        T: Copy,
    {
        match self {
            KnownCollection::ArrayVec(y, this) => {
                (KnownCollection::ArrayVec(y, this.with_push(value)), None)
            }
            KnownCollection::LeadVec(y, this) => {
                let (this, value) = this.with_push_maybe_fake(value);
                (KnownCollection::LeadVec(y, this), value)
            }
            KnownCollection::Count(y, mut this) => (
                KnownCollection::Count(y, {
                    this.len += 1;
                    this
                }),
                Some(value),
            ),
            this @ KnownCollection::CollectNothing(_, _) => (this, Some(value)),
        }
    }
}
