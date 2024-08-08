use crate::define_known_variants;

use super::{array_vec::ArrayVec, count::Count, lead_vec::LeadVec};

define_known_variants!(
    #[non_exhaustive]
    #[derive(Clone, Copy)]
    pub enum KnownCollection<T, const CAP: usize> {
        /// Panics if pushing values when filled.
        ArrayVec(ArrayVec<T, CAP>),
        /// Only keep the leading values and records the real count.
        LeadVec(LeadVec<T, CAP>),
        /// Just count.
        Count(Count),
    }

    #[sealed(sealed)]
    pub trait IsKnownCollection {}

    #[from_variant]
    pub fn from_collection();

    // #[into_variant]
    // fn into_collection();

    #[as_variant]
    pub fn as_collection();
);

impl<V: IsKnownCollection<T, CAP>, T: std::fmt::Debug, const CAP: usize> std::fmt::Debug
    for KnownCollection<V, T, CAP>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArrayVec(_, this) => this.fmt(f),
            Self::LeadVec(_, this) => this.fmt(f),
            Self::Count(_, this) => this.fmt(f),
        }
    }
}

impl<V: IsKnownCollection<T, CAP>, T, const CAP: usize> KnownCollection<V, T, CAP> {
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
        }
    }
}
