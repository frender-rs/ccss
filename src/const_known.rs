// mod macros;
// pub use macros::{__private, auto_fns};

mod sealed {
    pub trait SealedYesOrNo {}
}

pub trait YesOrNo: sealed::SealedYesOrNo + Copy {
    const YES: bool;
}

#[derive(Clone, Copy)]
pub struct Yes;

#[derive(Clone, Copy)]
pub enum No {}

impl sealed::SealedYesOrNo for Yes {}
impl sealed::SealedYesOrNo for No {}

impl YesOrNo for Yes {
    const YES: bool = true;
}
impl YesOrNo for No {
    const YES: bool = false;
}
