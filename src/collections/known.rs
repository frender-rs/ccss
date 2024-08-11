use crate::const_known::{No, Yes, YesOrNo};

use super::{
    array_vec::ArrayVec, collect_nothing::CollectNothing, count::Count, lead_vec::LeadVec,
    HasConstDummyValue,
};

/// `T: Copy` is required because push operations require this.
#[non_exhaustive]
pub enum KnownCollection<V: IsKnownCollection<T>, T: Copy> {
    ArrayVec(V::ArrayVec, V::ArrayVecType),
    LeadVec(V::LeadVec, V::LeadVecType),
    Count(V::Count, Count),
    CollectNothing(V::CollectNothing, CollectNothing),
}

impl<V: IsKnownCollection<T>, T: Copy> Copy for KnownCollection<V, T> {}

impl<V: IsKnownCollection<T>, T: Copy> Clone for KnownCollection<V, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<V: IsKnownCollection<T>, T: Copy> std::fmt::Debug for KnownCollection<V, T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArrayVec(_, this) => SelfDebugDerive::self_debug(this).fmt(f),
            Self::LeadVec(_, this) => SelfDebugDerive::self_debug(this).fmt(f),
            Self::Count(_, this) => this.fmt(f),
            Self::CollectNothing(_, this) => this.fmt(f),
        }
    }
}

mod sealed {
    pub trait IsKnownCollection<T> {}
    pub trait IsArrayVec {}
    pub trait IsLeadVec {}
}

pub trait SelfDebugDerive<T: ?Sized> {
    type SelfDebug: std::fmt::Debug
    where
        T: std::fmt::Debug;

    fn self_debug(this: &Self) -> &Self::SelfDebug
    where
        T: std::fmt::Debug;
}

pub trait IsArrayVec: sealed::IsArrayVec + SelfDebugDerive<Self::Item> {
    type Item;
    const CAP: usize;
}
impl<T, const N: usize> sealed::IsArrayVec for ArrayVec<T, N> {}
impl<T, const N: usize> SelfDebugDerive<T> for ArrayVec<T, N> {
    type SelfDebug = Self
    where
        T: std::fmt::Debug;
    fn self_debug(this: &Self) -> &Self::SelfDebug
    where
        T: std::fmt::Debug,
    {
        this
    }
}
impl<T, const N: usize> IsArrayVec for ArrayVec<T, N> {
    type Item = T;
    const CAP: usize = N;
}

pub trait IsLeadVec: sealed::IsLeadVec + SelfDebugDerive<Self::Item> {
    type Item;
}
impl<T, const N: usize> sealed::IsLeadVec for LeadVec<T, N> {}
impl<T, const N: usize> SelfDebugDerive<T> for LeadVec<T, N> {
    type SelfDebug = Self
    where
        T: std::fmt::Debug;
    fn self_debug(this: &Self) -> &Self::SelfDebug
    where
        T: std::fmt::Debug,
    {
        this
    }
}
impl<T, const N: usize> IsLeadVec for LeadVec<T, N> {
    type Item = T;
}

pub trait IsKnownCollection<T: Copy>: Sized + sealed::IsKnownCollection<T> {
    type ArrayVecType: IsArrayVec<Item = T> + Copy;
    type ArrayVec: YesOrNo;
    type LeadVecType: IsLeadVec<Item = T> + Copy;
    type LeadVec: YesOrNo;
    type Count: YesOrNo;
    type CollectNothing: YesOrNo;
}

pub trait IsKnownCollectionWithConstEmpty<T: Copy>: IsKnownCollection<T> + Sized {
    const EMPTY_COLLECTION: Self;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T>;
}

impl<T, const CAP: usize> sealed::IsKnownCollection<T> for ArrayVec<T, CAP> {}
impl<T: Copy, const CAP: usize> IsKnownCollection<T> for ArrayVec<T, CAP> {
    type ArrayVecType = Self;
    type ArrayVec = Yes;
    type LeadVecType = LeadVec<T, 0>;
    type LeadVec = No;
    type Count = No;
    type CollectNothing = No;
}
impl<T: Copy + HasConstDummyValue, const CAP: usize> IsKnownCollectionWithConstEmpty<T>
    for ArrayVec<T, CAP>
{
    const EMPTY_COLLECTION: Self = Self::EMPTY;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T> =
        KnownCollection::<Self, T>::from_variant(Self::EMPTY);
}

impl<T, const CAP: usize> sealed::IsKnownCollection<T> for LeadVec<T, CAP> {}
impl<T: Copy, const CAP: usize> IsKnownCollection<T> for LeadVec<T, CAP> {
    type ArrayVecType = ArrayVec<T, 0>;
    type ArrayVec = No;
    type LeadVecType = Self;
    type LeadVec = Yes;
    type Count = No;
    type CollectNothing = No;
}
impl<T: Copy + HasConstDummyValue, const CAP: usize> IsKnownCollectionWithConstEmpty<T>
    for LeadVec<T, CAP>
{
    const EMPTY_COLLECTION: Self = Self::EMPTY;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T> =
        KnownCollection::<Self, T>::from_variant(Self::EMPTY);
}

impl<T> sealed::IsKnownCollection<T> for Count {}
impl<T: Copy> IsKnownCollection<T> for Count {
    type ArrayVecType = ArrayVec<T, 0>;
    type ArrayVec = No;
    type LeadVecType = LeadVec<T, 0>;
    type LeadVec = No;
    type Count = Yes;
    type CollectNothing = No;
}
impl<T: Copy> IsKnownCollectionWithConstEmpty<T> for Count {
    const EMPTY_COLLECTION: Self = Self::EMPTY;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T> =
        KnownCollection::<Self, T>::from_variant(Self::EMPTY);
}

impl<T> sealed::IsKnownCollection<T> for CollectNothing {}
impl<T: Copy> IsKnownCollection<T> for CollectNothing {
    type ArrayVecType = ArrayVec<T, 0>;
    type ArrayVec = No;
    type LeadVecType = LeadVec<T, 0>;
    type LeadVec = No;
    type Count = No;
    type CollectNothing = Yes;
}
impl<T: Copy> IsKnownCollectionWithConstEmpty<T> for CollectNothing {
    const EMPTY_COLLECTION: Self = Self;
    const EMPTY_COLLECTION_AS_KNOWN: KnownCollection<Self, T> =
        KnownCollection::<Self, T>::from_variant(Self);
}

impl<T: Copy, const CAP: usize> KnownCollection<ArrayVec<T, CAP>, T> {
    pub const fn from_variant(v: ArrayVec<T, CAP>) -> Self {
        Self::ArrayVec(Yes, v)
    }
}
impl<T: Copy, const CAP: usize> KnownCollection<LeadVec<T, CAP>, T> {
    pub const fn from_variant(v: LeadVec<T, CAP>) -> Self {
        Self::LeadVec(Yes, v)
    }
}
impl<T: Copy> KnownCollection<Count, T> {
    pub const fn from_variant(v: Count) -> Self {
        Self::Count(Yes, v)
    }
}
impl<T: Copy> KnownCollection<CollectNothing, T> {
    pub const fn from_variant(v: CollectNothing) -> Self {
        Self::CollectNothing(Yes, v)
    }
}

impl<T: Copy, const CAP: usize> KnownCollection<ArrayVec<T, CAP>, T> {
    pub const fn as_variant(&self) -> &ArrayVec<T, CAP> {
        match self {
            Self::ArrayVec(Yes, this) => this,
            Self::LeadVec(no, _) => match *no {},
            Self::Count(no, _) => match *no {},
            Self::CollectNothing(no, _) => match *no {},
        }
    }
}
impl<T: Copy, const CAP: usize> KnownCollection<LeadVec<T, CAP>, T> {
    pub const fn as_variant(&self) -> &LeadVec<T, CAP> {
        match self {
            Self::ArrayVec(no, _) => match *no {},
            Self::LeadVec(Yes, this) => this,
            Self::Count(no, _) => match *no {},
            Self::CollectNothing(no, _) => match *no {},
        }
    }
}
impl<T: Copy> KnownCollection<Count, T> {
    pub const fn as_variant(&self) -> &Count {
        match self {
            Self::ArrayVec(no, _) => match *no {},
            Self::LeadVec(no, _) => match *no {},
            Self::Count(Yes, this) => this,
            Self::CollectNothing(no, _) => match *no {},
        }
    }
}
impl<T: Copy> KnownCollection<CollectNothing, T> {
    pub const fn as_variant(&self) -> &CollectNothing {
        match self {
            Self::ArrayVec(no, _) => match *no {},
            Self::LeadVec(no, _) => match *no {},
            Self::Count(no, _) => match *no {},
            Self::CollectNothing(Yes, this) => this,
        }
    }
}

impl<T: Copy> KnownCollection<CollectNothing, T> {
    pub const fn into_variant(self) -> CollectNothing {
        match self {
            Self::ArrayVec(no, _) => match no {},
            Self::LeadVec(no, _) => match no {},
            Self::Count(no, _) => match no {},
            Self::CollectNothing(Yes, this) => this,
        }
    }
}

impl<V, T: Copy, const ARRAY_VEC_CAP: usize, const LEAD_VEC_CAP: usize> KnownCollection<V, T>
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
