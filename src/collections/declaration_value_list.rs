use crate::parse::component_value::ComponentValue;

use super::parsed_value_list::{IsKnownParsedValueList, KnownParsedValueList};

/// A list of [`ComponentValue`] with the following constraints:
/// - None of the values [is whitespace](ComponentValue::is_whitespace).
/// - The list doesn't end with `!` `important`.
#[derive(Debug, Clone, Copy)]
pub struct KnownDeclarationValueList<
    'a,
    L: IsKnownParsedValueList<ComponentValue<'a>, CAP>,
    const CAP: usize,
> {
    inner: KnownParsedValueList<'a, L, ComponentValue<'a>, CAP>,
}

impl<'a, L: IsKnownParsedValueList<ComponentValue<'a>, CAP>, const CAP: usize>
    KnownDeclarationValueList<'a, L, CAP>
{
    pub(crate) const fn start_builder() -> builder::KnownDeclarationValueListBuilder<'a, L, CAP> {
        builder::KnownDeclarationValueListBuilder::new()
    }

    pub const fn full(&self) -> crate::token::stream::CopyableTokenStream<'a> {
        self.inner.full()
    }

    pub const fn full_as_str(&self) -> &'a str {
        self.inner.full_as_str()
    }

    pub(crate) const fn as_known_parsed_value_list(
        &self,
    ) -> &KnownParsedValueList<'a, L, ComponentValue<'a>, CAP> {
        &self.inner
    }
}

pub(crate) mod builder {
    use crate::{
        collections::{
            array_vec::ArrayVec,
            parsed_value_list::{
                IsKnownParsedValueList, KnownParsedValueList, KnownParsedValueListBuilder,
            },
            HasConstDummyValue,
        },
        parse::{
            component_value::{ComponentValue, TokenAndRemaining},
            declaration::Important,
        },
        token::{
            stream::{CopyableTokenStream, TokenStream},
            tokens::Token,
        },
    };

    use super::KnownDeclarationValueList;

    /// A copyable version of [`TokenAndRemaining<ComponentValue>`]
    #[derive(Clone, Copy)]
    struct ValueAndRemaining<'a> {
        cv: ComponentValue<'a>,
        remaining: CopyableTokenStream<'a>,
        // full == cv + remaining
        full: CopyableTokenStream<'a>,
    }

    impl<'a> HasConstDummyValue for ValueAndRemaining<'a> {
        const DUMMY_VALUE: Self = ValueAndRemaining {
            cv: ComponentValue::PreservedTokens(Token::DUMMY_VALUE),
            remaining: CopyableTokenStream::EMPTY,
            full: TokenStream::new(" ").to_copyable(),
        };
    }

    enum ValueList<'a> {
        Empty,
        NotEmpty {
            first: ValueAndRemaining<'a>,
            last_3: ArrayVec<ValueAndRemaining<'a>, 3>,
            real_len: usize,
        },
    }

    enum ValueListPop2<'a> {
        Empty,
        One(ValueAndRemaining<'a>),
        More {
            first: ValueAndRemaining<'a>,
            last: ValueAndRemaining<'a>,
            real_len: usize,
        },
    }

    struct Span<'a> {
        full: CopyableTokenStream<'a>,
        len: usize,
    }

    impl<'a> ValueListPop2<'a> {
        const fn span(&self) -> Span<'a> {
            match self {
                ValueListPop2::Empty => Span {
                    full: CopyableTokenStream::EMPTY,
                    len: 0,
                },
                ValueListPop2::One(v) => Span {
                    full: v.full.before(v.remaining),
                    len: 1,
                },
                ValueListPop2::More {
                    first,
                    last,
                    real_len,
                } => Span {
                    full: first.full.before(last.remaining),
                    len: *real_len,
                },
            }
        }

        const fn last(&self) -> Option<&ValueAndRemaining<'a>> {
            match self {
                ValueListPop2::Empty => None,
                ValueListPop2::One(var) => Some(var),
                ValueListPop2::More {
                    first: _,
                    last,
                    real_len: _,
                } => Some(last),
            }
        }
    }

    impl<'a> ValueList<'a> {
        /// ret.0 means the value that is not one of the last two.
        const fn with_push(
            self,
            v: ValueAndRemaining<'a>,
        ) -> (Option<ValueAndRemaining<'a>>, Self) {
            match self {
                ValueList::Empty => (
                    None,
                    Self::NotEmpty {
                        first: v,
                        last_3: ArrayVec::EMPTY,
                        real_len: 1,
                    },
                ),
                ValueList::NotEmpty {
                    first,
                    last_3,
                    real_len,
                } => {
                    let (popped, last_3) = last_3.with_force_push(v);

                    let returned_val = match last_3.len() {
                        2 => {
                            debug_assert!(popped.is_none());
                            // now we can assure that `first` is not one of the last two.
                            Some(first)
                        }
                        3 => {
                            debug_assert!(popped.is_none());
                            // now we can assure that `last_3[0]` is not one of the last two.
                            Some(match last_3.first() {
                                Some(v) => *v,
                                None => unreachable!(),
                            })
                        }
                        _ => popped,
                    };

                    (
                        returned_val,
                        Self::NotEmpty {
                            first,
                            last_3,
                            real_len: real_len + 1,
                        },
                    )
                }
            }
        }

        const fn pop_last_2_important(self) -> Result<(Important<'a>, ValueListPop2<'a>), Self> {
            match self {
                ValueList::Empty => Err(self),
                ValueList::NotEmpty {
                    first,
                    ref last_3,
                    real_len,
                } => match last_3.as_slice() {
                    [a, b, c] => {
                        debug_assert!(real_len >= 4);
                        if let Some((bang, important)) = Important::is_bang_important(b.cv, c.cv) {
                            Ok((
                                Important {
                                    full: b.full.before(c.remaining),
                                    bang,
                                    important,
                                },
                                ValueListPop2::More {
                                    first,
                                    last: *a,
                                    real_len: real_len - 2,
                                },
                            ))
                        } else {
                            Err(self)
                        }
                    }
                    [b, c] => {
                        debug_assert!(real_len == 3);

                        if let Some((bang, important)) = Important::is_bang_important(b.cv, c.cv) {
                            Ok((
                                Important {
                                    full: b.full.before(c.remaining),
                                    bang,
                                    important,
                                },
                                ValueListPop2::One(first),
                            ))
                        } else {
                            Err(self)
                        }
                    }
                    [c] => {
                        debug_assert!(real_len == 2);
                        let b = first;
                        if let Some((bang, important)) = Important::is_bang_important(b.cv, c.cv) {
                            Ok((
                                Important {
                                    full: b.full.before(c.remaining),
                                    bang,
                                    important,
                                },
                                ValueListPop2::Empty,
                            ))
                        } else {
                            Err(self)
                        }
                    }
                    [] => {
                        debug_assert!(real_len == 1);

                        Err(self)
                    }
                    _ => unreachable!(),
                },
            }
        }

        const fn last(&self) -> Option<ValueAndRemaining<'a>> {
            match self {
                ValueList::Empty => None,
                ValueList::NotEmpty {
                    first,
                    last_3,
                    real_len: _,
                } => Some(match last_3.as_slice().last() {
                    Some(v) => *v,
                    None => *first,
                }),
            }
        }

        const fn span(&self) -> Span<'a> {
            match self {
                ValueList::Empty => Span {
                    full: CopyableTokenStream::EMPTY,
                    len: 0,
                },
                ValueList::NotEmpty {
                    first,
                    last_3,
                    real_len,
                } => match last_3.as_slice().last() {
                    Some(last) => Span {
                        full: first.full.before(last.remaining),
                        len: *real_len,
                    },
                    None => Span {
                        full: first.full.before(first.remaining),
                        len: *real_len,
                    },
                },
            }
        }

        const fn value_including_important(&self) -> Option<CopyableTokenStream<'a>> {
            match self {
                ValueList::Empty => None,
                ValueList::NotEmpty {
                    first,
                    last_3,
                    real_len: _,
                } => Some(match last_3.as_slice().last() {
                    Some(last) => first.full.before(last.remaining),
                    None => first.full.before(first.remaining),
                }),
            }
        }

        const fn last_2_copied(&self) -> ArrayVec<ValueAndRemaining<'a>, 2> {
            match self {
                ValueList::Empty => ArrayVec::EMPTY,
                ValueList::NotEmpty {
                    first,
                    last_3,
                    real_len: _,
                } => match last_3.as_slice() {
                    [_, b, c] => ArrayVec::new_filled([*b, *c]),
                    [b, c] => ArrayVec::new_filled([*b, *c]),
                    [c] => ArrayVec::new_filled([*first, *c]),
                    [] => ArrayVec::EMPTY.with_push(*first),
                    _ => unreachable!(),
                },
            }
        }

        const fn first(&self) -> Option<&ValueAndRemaining<'a>> {
            match self {
                ValueList::Empty => None,
                ValueList::NotEmpty {
                    first,
                    last_3: _,
                    real_len: _,
                } => Some(first),
            }
        }
    }

    pub(crate) struct KnownDeclarationValueListBuilder<
        'a,
        L: IsKnownParsedValueList<ComponentValue<'a>, CAP>,
        const CAP: usize,
    > {
        // the full list is list_builder + value_list.last_3
        list_builder: KnownParsedValueListBuilder<'a, L, ComponentValue<'a>, CAP>,
        value_list: ValueList<'a>,
    }

    impl<'a, L: IsKnownParsedValueList<ComponentValue<'a>, CAP>, const CAP: usize>
        KnownDeclarationValueListBuilder<'a, L, CAP>
    {
        pub(crate) const fn with_push(
            self,
            tar: TokenAndRemaining<'a, ComponentValue<'a>>,
        ) -> Self {
            if tar.token.is_whitespace() {
                return self;
            }

            let TokenAndRemaining {
                token,
                remaining,
                full,
            } = tar;
            let full = full.to_copyable();

            let (value_that_is_not_last_2, value_list) =
                self.value_list.with_push(ValueAndRemaining {
                    cv: token,
                    remaining: remaining.to_copyable(),
                    full,
                });

            let list_builder = if let Some(value_that_is_not_last_2) = value_that_is_not_last_2 {
                self.list_builder
                    .with_push(value_that_is_not_last_2.cv, value_that_is_not_last_2.full)
            } else {
                self.list_builder
            };

            Self {
                list_builder,
                value_list,
            }
        }

        pub(crate) const fn build(
            self,
            after_colon: CopyableTokenStream<'a>,
        ) -> BuildOutput<'a, L, CAP> {
            // after values and !important
            let after_value_and_important = match self.value_list.last() {
                Some(v) => v.remaining,
                None => after_colon,
            };

            let value_and_important = match self.value_list.value_including_important() {
                Some(v) => v,
                None => after_colon.before(after_colon),
            };

            let important;
            let list;
            match self.value_list.pop_last_2_important() {
                Ok((i, values)) => {
                    // values before `!important` has already been pushed into list_builder.
                    important = Some(i);

                    list = {
                        let remaining = match values.last() {
                            Some(last) => last.remaining,
                            None => after_colon,
                        };
                        self.list_builder.build(remaining)
                    };
                }
                Err(values) => {
                    // last 2 values hasn't been pushed into list_builder.
                    important = None;

                    list = {
                        let mut list_builder = self.list_builder;

                        {
                            let last_2 = values.last_2_copied();
                            let last_2 = last_2.as_slice();

                            let mut i = 0;
                            while i < last_2.len() {
                                let var = last_2[i];
                                list_builder = list_builder.with_push(var.cv, var.full);

                                i += 1;
                            }
                        }

                        list_builder.build(match values.last() {
                            Some(last) => last.remaining,
                            None => after_colon,
                        })
                    };
                }
            }

            BuildOutput {
                value: KnownDeclarationValueList { inner: list },
                important,
                value_and_important,
                after_value_and_important,
            }
        }

        pub(super) const fn new() -> Self {
            Self {
                list_builder: KnownParsedValueList::start_builder(),
                value_list: ValueList::Empty,
            }
        }
    }

    pub(crate) struct BuildOutput<
        'a,
        L: IsKnownParsedValueList<ComponentValue<'a>, CAP>,
        const CAP: usize,
    > {
        pub(crate) value: KnownDeclarationValueList<'a, L, CAP>,
        pub(crate) important: Option<Important<'a>>,
        pub(crate) value_and_important: CopyableTokenStream<'a>,
        pub(crate) after_value_and_important: CopyableTokenStream<'a>,
    }
}
