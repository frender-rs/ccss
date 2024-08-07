use std::marker::PhantomData;

pub trait ConstDummyValueFor<T> {
    const DUMMY_VALUE: T;
}

#[derive(Clone, Copy)]
pub struct ArrayVec<T, D: ConstDummyValueFor<T>, const N: usize> {
    array: [T; N],
    _d: PhantomData<D>,
    len: usize,
}

impl<T: std::fmt::Debug, D: ConstDummyValueFor<T>, const N: usize> std::fmt::Debug
    for ArrayVec<T, D, N>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ArrayVec").field(&self.as_slice()).finish()
    }
}

impl<T, D: ConstDummyValueFor<T>, const CAP: usize> ArrayVec<T, D, CAP> {
    pub const EMPTY: Self = Self {
        array: [D::DUMMY_VALUE; CAP],
        len: 0,
        _d: PhantomData,
    };

    /// array[len..] must be padded with DUMMY_VALUE
    pub(crate) const fn new_maybe_filled(array: [T; CAP], len: usize) -> Self {
        assert!(len <= CAP);
        Self {
            len,
            array,
            _d: PhantomData,
        }
    }

    pub const fn new_filled(array: [T; CAP]) -> Self {
        Self {
            len: CAP,
            array,
            _d: PhantomData,
        }
    }

    pub const fn as_slice(&self) -> &[T] {
        self.array.split_at(self.len).0
    }

    pub const fn copy_array_padding_dummy(self) -> [T; CAP]
    where
        T: Copy,
    {
        self.array
    }

    pub const fn with_pop_front(mut self) -> (Option<T>, Self)
    where
        T: Copy,
    {
        if self.len > 0 {
            let first = self.array[0];

            let mut i = 0;
            let mut j = 1;

            while j < self.len {
                self.array[i] = self.array[j];
                i = j;
                j += 1;
            }

            self.len -= 1;
            self.array[i] = D::DUMMY_VALUE;

            (Some(first), self)
        } else {
            (None, self)
        }
    }

    /// Panics if already filled.
    pub const fn with_push(self, value: T) -> Self
    where
        T: Copy,
    {
        match self.with_try_push(value) {
            Ok(this) => this,
            Err(_) => panic!("filled"),
        }
    }

    pub const fn with_try_push(mut self, value: T) -> Result<Self, (Self, T)>
    where
        T: Copy,
    {
        if self.len < CAP {
            self.array[self.len] = value;
            self.len += 1;
            Ok(self)
        } else {
            Err((self, value))
        }
    }

    pub(crate) const fn first(&self) -> Option<&T> {
        if self.len > 0 {
            Some(&self.array[0])
        } else {
            None
        }
    }

    pub(crate) const fn first_copied(&self) -> Option<T>
    where
        T: Copy,
    {
        if self.len > 0 {
            Some(self.array[0])
        } else {
            None
        }
    }

    pub(crate) const fn len(&self) -> usize {
        self.len
    }

    pub(crate) const fn is_empty(&self) -> bool {
        self.len == 0
    }

    const ASSERT_CAP_IS_NON_ZERO: () = assert!(CAP != 0);

    /// If filled, pop_front
    pub(crate) const fn with_force_push(self, value: T) -> (Option<T>, Self)
    where
        T: Copy,
    {
        () = Self::ASSERT_CAP_IS_NON_ZERO;
        if self.len < CAP {
            (None, self.with_push(value))
        } else {
            let (front, this) = self.with_pop_front();
            (front, this.with_push(value))
        }
    }
}
