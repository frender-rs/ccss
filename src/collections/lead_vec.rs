use super::{array_vec::ArrayVec, HasConstDummyValue};

#[derive(Clone, Copy)]
pub struct LeadVec<T, const CAP: usize> {
    lead: ArrayVec<T, CAP>,
    tail_len: usize,
}

impl<T, const CAP: usize> LeadVec<T, CAP> {
    pub const fn as_slice(&self) -> Option<&[T]> {
        if self.tail_len == 0 {
            Some(self.lead_as_slice())
        } else {
            None
        }
    }

    pub const fn lead_as_slice(&self) -> &[T] {
        self.lead.as_slice()
    }

    pub const fn lead(&self) -> &ArrayVec<T, CAP>
    where
        T: Copy,
    {
        &self.lead
    }

    pub const fn len(&self) -> usize {
        self.lead.len() + self.tail_len
    }

    pub(crate) const fn with_push(mut self, value: T) -> Self
    where
        T: Copy,
    {
        if self.tail_len > 0 {
            self.tail_len += 1;
            return self;
        }

        self.lead = match self.lead.with_try_push(value) {
            Ok(lead) => lead,
            Err((lead, _)) => {
                self.tail_len += 1;
                lead
            }
        };

        self
    }
}

impl<T: HasConstDummyValue, const CAP: usize> LeadVec<T, CAP> {
    pub const EMPTY: Self = Self {
        lead: ArrayVec::EMPTY,
        tail_len: 0,
    };
}
