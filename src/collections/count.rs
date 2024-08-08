pub struct Count {
    pub len: usize,
}

impl Count {
    pub const EMPTY: Self = Count { len: 0 };
}
