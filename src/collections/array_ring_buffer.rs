use std::marker::PhantomData;

use super::array_vec::ConstDummyValueFor;

pub struct ArrayRingBuffer<T, D: ConstDummyValueFor<T>, const CAP: usize> {
    buf: [T; CAP],
    read_index: usize,
    write_index: usize,
    _d: PhantomData<D>,
}

impl<T, D: ConstDummyValueFor<T>, const CAP: usize> ArrayRingBuffer<T, D, CAP> {
    pub const EMPTY: Self = Self {
        buf: [D::DUMMY_VALUE; CAP],
        read_index: 0,
        write_index: 0,
        _d: PhantomData,
    };

    pub const fn len(&self) -> usize {
        if self.write_index < self.read_index {
            CAP - (self.read_index - self.write_index)
        } else {
            self.write_index - self.read_index
        }
    }

    pub const fn with_try_push(self, value: T) -> Result<Self, T> {
        if self.write_index < self.read_index {
        } else {
        }
    }
}
