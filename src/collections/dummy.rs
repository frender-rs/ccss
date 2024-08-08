/// [`HasConstDummyValue::DUMMY_VALUE`] must be valid.
pub trait HasConstDummyValue {
    const DUMMY_VALUE: Self;
}

impl HasConstDummyValue for char {
    const DUMMY_VALUE: Self = '\0';
}
