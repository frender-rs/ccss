use super::{array_vec::ArrayVec, lead_vec::LeadVec};

mod sealed {
    pub trait Sealed {}
    pub trait YesOrNoSealed {}
}

pub trait YesOrNo: sealed::YesOrNoSealed {}

struct Yes;

enum No {}

pub trait IsKnownParsedValueList<T, const CAP: usize>: sealed::Sealed {
    type MaybeArrayVec: YesOrNo;
    type MaybeLeadVec: YesOrNo;
}

pub enum KnownParsedValueList<T, const CAP: usize, L: IsKnownParsedValueList<T, CAP>> {
    ArrayVec(L::MaybeArrayVec, ArrayVec<T, CAP>),
    LeadVec(L::MaybeLeadVec, LeadVec<T, CAP>),
    CountOnly(),
}
