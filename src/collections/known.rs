use crate::const_known::{No, Yes, YesOrNo};

use super::{
    array_vec::ArrayVec, collect_nothing::CollectNothing, count::Count, lead_vec::LeadVec,
    HasConstDummyValue,
};

#[non_exhaustive]
pub enum KnownCollection<V: IsKnownCollection<T>, T> {
    ArrayVec(V::ArrayVec, V::ArrayVecType),
    LeadVec(V::LeadVec, V::LeadVecType),
    Count(V::Count, Count),
    CollectNothing(V::CollectNothing, CollectNothing),
}

impl<
        V: IsKnownCollection<
            T,
            ArrayVecType = ArrayVec<T, ARRAY_VEC_CAP>,
            LeadVecType = LeadVec<T, LEAD_VEC_CAP>,
        >,
        T,
        const ARRAY_VEC_CAP: usize,
        const LEAD_VEC_CAP: usize,
    > std::fmt::Debug for KnownCollection<V, T>
where
    T: std::fmt::Debug,
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

impl<
        V: IsKnownCollection<
            T,
            ArrayVecType = ArrayVec<T, ARRAY_VEC_CAP>,
            LeadVecType = LeadVec<T, LEAD_VEC_CAP>,
        >,
        T,
        const ARRAY_VEC_CAP: usize,
        const LEAD_VEC_CAP: usize,
    > Clone for KnownCollection<V, T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::ArrayVec(arg0, arg1) => Self::ArrayVec(*arg0, arg1.clone()),
            Self::LeadVec(arg0, arg1) => Self::LeadVec(*arg0, arg1.clone()),
            Self::Count(arg0, arg1) => Self::Count(*arg0, arg1.clone()),
            Self::CollectNothing(arg0, arg1) => Self::CollectNothing(*arg0, arg1.clone()),
        }
    }
}

impl<
        V: IsKnownCollection<
            T,
            ArrayVecType = ArrayVec<T, ARRAY_VEC_CAP>,
            LeadVecType = LeadVec<T, LEAD_VEC_CAP>,
        >,
        T,
        const ARRAY_VEC_CAP: usize,
        const LEAD_VEC_CAP: usize,
    > Copy for KnownCollection<V, T>
where
    T: Copy,
{
}

mod sealed {
    pub trait IsKnownCollection<T> {}
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

pub trait IsKnownCollection<T>: Sized + sealed::IsKnownCollection<T> {
    type ArrayVecType: IsArrayVec<Item = T>;
    type ArrayVec: YesOrNo;
    type LeadVecType: IsLeadVec<Item = T>;
    type LeadVec: YesOrNo;
    type Count: YesOrNo;
    type CollectNothing: YesOrNo;
}

pub trait IsKnownCollectionWithConstEmpty<T>: IsKnownCollection<T> + Sized {
    const EMPTY_COLLECTION: Self;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T>;
}

impl<T, const CAP: usize> sealed::IsKnownCollection<T> for ArrayVec<T, CAP> {}
impl<T, const CAP: usize> IsKnownCollection<T> for ArrayVec<T, CAP> {
    type ArrayVecType = Self;
    type ArrayVec = Yes;
    type LeadVecType = LeadVec<T, 0>;
    type LeadVec = No;
    type Count = No;
    type CollectNothing = No;
}
impl<T: HasConstDummyValue, const CAP: usize> IsKnownCollectionWithConstEmpty<T>
    for ArrayVec<T, CAP>
{
    const EMPTY_COLLECTION: Self = Self::EMPTY;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T> =
        KnownCollection::<Self, T>::from_variant(Self::EMPTY);
}

impl<T, const CAP: usize> sealed::IsKnownCollection<T> for LeadVec<T, CAP> {}
impl<T, const CAP: usize> IsKnownCollection<T> for LeadVec<T, CAP> {
    type ArrayVecType = ArrayVec<T, 0>;
    type ArrayVec = No;
    type LeadVecType = Self;
    type LeadVec = Yes;
    type Count = No;
    type CollectNothing = No;
}
impl<T: HasConstDummyValue, const CAP: usize> IsKnownCollectionWithConstEmpty<T>
    for LeadVec<T, CAP>
{
    const EMPTY_COLLECTION: Self = Self::EMPTY;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T> =
        KnownCollection::<Self, T>::from_variant(Self::EMPTY);
}

impl<T> sealed::IsKnownCollection<T> for Count {}
impl<T> IsKnownCollection<T> for Count {
    type ArrayVecType = ArrayVec<T, 0>;
    type ArrayVec = No;
    type LeadVecType = LeadVec<T, 0>;
    type LeadVec = No;
    type Count = Yes;
    type CollectNothing = No;
}
impl<T> IsKnownCollectionWithConstEmpty<T> for Count {
    const EMPTY_COLLECTION: Self = Self::EMPTY;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T> =
        KnownCollection::<Self, T>::from_variant(Self::EMPTY);
}

impl<T> sealed::IsKnownCollection<T> for CollectNothing {}
impl<T> IsKnownCollection<T> for CollectNothing {
    type ArrayVecType = ArrayVec<T, 0>;
    type ArrayVec = No;
    type LeadVecType = LeadVec<T, 0>;
    type LeadVec = No;
    type Count = No;
    type CollectNothing = Yes;
}
impl<T> IsKnownCollectionWithConstEmpty<T> for CollectNothing {
    const EMPTY_COLLECTION: Self = Self;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T> =
        KnownCollection::<Self, T>::from_variant(Self);
}

impl<T, const CAP: usize> KnownCollection<ArrayVec<T, CAP>, T> {
    pub const fn from_variant(v: ArrayVec<T, CAP>) -> Self {
        Self::ArrayVec(Yes, v)
    }
}
impl<T, const CAP: usize> KnownCollection<LeadVec<T, CAP>, T> {
    pub const fn from_variant(v: LeadVec<T, CAP>) -> Self {
        Self::LeadVec(Yes, v)
    }
}
impl<T> KnownCollection<Count, T> {
    pub const fn from_variant(v: Count) -> Self {
        Self::Count(Yes, v)
    }
}
impl<T> KnownCollection<CollectNothing, T> {
    pub const fn from_variant(v: CollectNothing) -> Self {
        Self::CollectNothing(Yes, v)
    }
}

impl<T, const CAP: usize> KnownCollection<ArrayVec<T, CAP>, T> {
    pub const fn as_variant(&self) -> &ArrayVec<T, CAP> {
        match self {
            Self::ArrayVec(Yes, this) => this,
            Self::LeadVec(no, _) => match *no {},
            Self::Count(no, _) => match *no {},
            Self::CollectNothing(no, _) => match *no {},
        }
    }
}
impl<T, const CAP: usize> KnownCollection<LeadVec<T, CAP>, T> {
    pub const fn as_variant(&self) -> &LeadVec<T, CAP> {
        match self {
            Self::ArrayVec(no, _) => match *no {},
            Self::LeadVec(Yes, this) => this,
            Self::Count(no, _) => match *no {},
            Self::CollectNothing(no, _) => match *no {},
        }
    }
}
impl<T> KnownCollection<Count, T> {
    pub const fn as_variant(&self) -> &Count {
        match self {
            Self::ArrayVec(no, _) => match *no {},
            Self::LeadVec(no, _) => match *no {},
            Self::Count(Yes, this) => this,
            Self::CollectNothing(no, _) => match *no {},
        }
    }
}
impl<T> KnownCollection<CollectNothing, T> {
    pub const fn as_variant(&self) -> &CollectNothing {
        match self {
            Self::ArrayVec(no, _) => match *no {},
            Self::LeadVec(no, _) => match *no {},
            Self::Count(no, _) => match *no {},
            Self::CollectNothing(Yes, this) => this,
        }
    }
}

impl<T> KnownCollection<CollectNothing, T> {
    pub const fn into_variant(self) -> CollectNothing {
        match self {
            Self::ArrayVec(no, _) => match no {},
            Self::LeadVec(no, _) => match no {},
            Self::Count(no, _) => match no {},
            Self::CollectNothing(Yes, this) => this,
        }
    }
}

impl<V, T, const ARRAY_VEC_CAP: usize, const LEAD_VEC_CAP: usize> KnownCollection<V, T>
where
    V: IsKnownCollection<
        T,
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
