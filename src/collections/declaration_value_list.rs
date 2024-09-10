use super::component_value_list::{
    IsKnownComponentValueList, IsKnownComponentValueListWithConstEmpty, KnownComponentValueList,
};

/// A list of [`ComponentValue`] with the following constraints:
/// - The list doesn't start or end with whitespace.
/// - In the list, no whitespace values are adjacent.
/// - The list doesn't end with `!` `important`.
pub struct KnownDeclarationValueList<'a, L: IsKnownComponentValueList<'a>> {
    inner: KnownComponentValueList<'a, L>,
}

impl<'a, L: IsKnownComponentValueList<'a>> Copy for KnownDeclarationValueList<'a, L> {}
impl<'a, L: IsKnownComponentValueList<'a>> Clone for KnownDeclarationValueList<'a, L> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, L: IsKnownComponentValueList<'a>> std::fmt::Debug for KnownDeclarationValueList<'a, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("KnownDeclarationValueList")
            .field(&self.inner)
            .finish()
    }
}

impl<'a, L: IsKnownComponentValueList<'a>> KnownDeclarationValueList<'a, L> {
    pub const fn full(&self) -> crate::token::stream::CopyableTokenStream<'a> {
        self.inner.full()
    }

    pub const fn full_as_str(&self) -> &'a str {
        self.inner.full_as_str()
    }

    pub(crate) const fn as_known_component_value_list(&self) -> &KnownComponentValueList<'a, L> {
        &self.inner
    }
}

impl<'a, L: IsKnownComponentValueListWithConstEmpty<'a>> KnownDeclarationValueList<'a, L> {
    pub(crate) const EMPTY: Self = {
        Self {
            inner: KnownComponentValueList::EMPTY,
        }
    };
}

impl<'a, L: IsKnownComponentValueList<'a>> KnownDeclarationValueList<'a, L> {
    pub(crate) const fn start_builder() -> builder::KnownDeclarationValueListBuilder<'a, L> {
        builder::KnownDeclarationValueListBuilder::new()
    }
}

pub(crate) mod builder {
    use crate::{
        collections::{
            array_vec::ArrayVec,
            component_value_list::{
                builder::KnownComponentValueListBuilder, IsKnownComponentValueList,
                IsKnownComponentValueListWithConstEmpty, KnownComponentValueList,
            },
            known::IsKnownCollection,
            lead_vec::LeadVec,
            HasConstDummyValue,
        },
        parse::{
            component_value::{ComponentValue, TokenAndRemaining},
            declaration::Important,
        },
        token::{
            stream::{CopyableTokenStream, TokenStream},
            tokens::{Token, WhitespaceToken},
        },
    };

    use super::KnownDeclarationValueList;

    /// A copyable version of [`TokenAndRemaining<ComponentValue>`]
    #[derive(Clone, Copy)]
    struct ValueAndRemaining<'a, V = ComponentValue<'a>> {
        cv: V,
        remaining: CopyableTokenStream<'a>,
        // full == cv + remaining
        full: CopyableTokenStream<'a>,
    }

    impl<'a> ValueAndRemaining<'a> {
        const fn try_into_whitespace(self) -> Option<ValueAndRemaining<'a, WhitespaceToken<'a>>> {
            if let Some(whitespace) = self.cv.as_whitespace() {
                Some(ValueAndRemaining {
                    cv: *whitespace,
                    remaining: self.remaining,
                    full: self.full,
                })
            } else {
                None
            }
        }
    }

    impl<'a> ValueAndRemaining<'a, WhitespaceToken<'a>> {
        const fn into_whitespace_and_remaining(self) -> WhitespaceAndRemaining<'a> {
            WhitespaceAndRemaining {
                whitespace: self.cv,
                whitespace_and_remaining: self.full,
            }
        }
        const fn into_component_value_and_remaining(self) -> ValueAndRemaining<'a> {
            ValueAndRemaining {
                cv: value_of_whitespace(self.cv),
                remaining: self.remaining,
                full: self.full,
            }
        }
    }

    impl<'a> HasConstDummyValue for ValueAndRemaining<'a> {
        const DUMMY_VALUE: Self = ValueAndRemaining {
            cv: ComponentValue::PreservedTokens(Token::DUMMY_VALUE),
            remaining: CopyableTokenStream::EMPTY,
            full: TokenStream::new(" ").to_copyable(),
        };
    }

    #[derive(Clone, Copy)]
    struct WhitespaceAndRemaining<'a> {
        whitespace: WhitespaceToken<'a>,
        whitespace_and_remaining: CopyableTokenStream<'a>,
    }

    #[derive(Clone, Copy)]
    struct WhitespaceAndValueAndRemaining<'a> {
        whitespace: Option<WhitespaceAndRemaining<'a>>,
        value: ValueAndRemaining<'a>,
    }

    impl<'a> WhitespaceAndValueAndRemaining<'a> {
        const fn push_to<
            L: IsKnownComponentValueListWithConstEmpty<'a>,
            const ARRAY_VEC_CAP: usize,
            const LEAD_VEC_CAP: usize,
        >(
            self,
            res: KnownComponentValueListBuilder<'a, L>,
        ) -> KnownComponentValueListBuilder<'a, L>
        where
            L::Collection: IsKnownCollection<
                ComponentValue<'a>,
                ArrayVecType = ArrayVec<ComponentValue<'a>, ARRAY_VEC_CAP>,
                LeadVecType = LeadVec<ComponentValue<'a>, LEAD_VEC_CAP>,
            >,
        {
            let WhitespaceAndValueAndRemaining {
                whitespace,
                value: value_that_is_not_last_2,
            } = self;

            if let Some(w) = whitespace {
                res.with_push(
                    value_of_whitespace(w.whitespace),
                    w.whitespace_and_remaining,
                )
            } else {
                res
            }
            .with_push(value_that_is_not_last_2.cv, value_that_is_not_last_2.full)
        }
    }

    impl<'a> HasConstDummyValue for WhitespaceAndValueAndRemaining<'a> {
        const DUMMY_VALUE: Self = Self {
            whitespace: None,
            value: ValueAndRemaining::DUMMY_VALUE,
        };
    }

    enum ValueList<'a> {
        Empty,
        NotEmpty {
            first: ValueAndRemaining<'a>,
            last_3: ArrayVec<WhitespaceAndValueAndRemaining<'a>, 3>,
            last_whitespace: Option<ValueAndRemaining<'a, WhitespaceToken<'a>>>,
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

    const fn some_to_1<T>(v: &Option<T>) -> usize {
        if v.is_some() {
            1
        } else {
            0
        }
    }

    const fn count_some<T>(vs: &[&Option<T>]) -> usize {
        let mut res = 0;
        let mut i = 0;
        while i < vs.len() {
            if vs[i].is_some() {
                res += 1;
            }
            i += 1;
        }

        res
    }

    impl<'a> ValueList<'a> {
        /// ret.0 means the values that are not one of the last two non-whitespace.
        const fn with_push(
            self,
            v: ValueAndRemaining<'a>,
        ) -> (Option<WhitespaceAndValueAndRemaining<'a>>, Self) {
            match self {
                ValueList::Empty => {
                    assert!(
                        !v.cv.is_whitespace(),
                        "first component value in declaration value list can't be whitespace"
                    );
                    (
                        None,
                        Self::NotEmpty {
                            first: v,
                            last_3: ArrayVec::EMPTY,
                            last_whitespace: None,
                            real_len: 1,
                        },
                    )
                }
                ValueList::NotEmpty {
                    first,
                    last_3,
                    last_whitespace,
                    real_len,
                } => {
                    let v = if let Some(whitespace) = last_whitespace {
                        assert!(
                            !v.cv.is_whitespace(),
                            "unexpected adjacent whitespace tokens"
                        );
                        WhitespaceAndValueAndRemaining {
                            whitespace: Some(whitespace.into_whitespace_and_remaining()),
                            value: v,
                        }
                    } else {
                        if let Some(whitespace) = v.try_into_whitespace() {
                            return (
                                None,
                                Self::NotEmpty {
                                    first,
                                    last_3,
                                    last_whitespace: Some(whitespace),
                                    real_len: real_len + 1,
                                },
                            );
                        } else {
                            WhitespaceAndValueAndRemaining {
                                whitespace: None,
                                value: v,
                            }
                        }
                    };

                    let (popped, last_3) = last_3.with_force_push(v);

                    let returned_val = match last_3.len() {
                        2 => {
                            debug_assert!(popped.is_none());
                            // now we can assure that `first` is not one of the last two.
                            Some(WhitespaceAndValueAndRemaining {
                                whitespace: None,
                                value: first,
                            })
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
                            // we have processed the case where last_whitespace is_some
                            last_whitespace: None,
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
                    last_whitespace,
                    real_len,
                } => match last_3.as_slice() {
                    [WhitespaceAndValueAndRemaining {
                        whitespace: aw,
                        value: a,
                    }, WhitespaceAndValueAndRemaining {
                        whitespace: bw,
                        value: b,
                    }, WhitespaceAndValueAndRemaining {
                        whitespace: cw,
                        value: c,
                    }] => {
                        debug_assert!(
                            real_len >= 4 + count_some(&[aw, bw, cw]) + some_to_1(&last_whitespace)
                        );
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
                    [WhitespaceAndValueAndRemaining {
                        whitespace: bw,
                        value: b,
                    }, WhitespaceAndValueAndRemaining {
                        whitespace: cw,
                        value: c,
                    }] => {
                        debug_assert!(
                            real_len == 3 + count_some(&[bw, cw]) + some_to_1(&last_whitespace)
                        );

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
                    [WhitespaceAndValueAndRemaining {
                        whitespace: cw,
                        value: c,
                    }] => {
                        debug_assert!(real_len == 2 + some_to_1(cw) + some_to_1(&last_whitespace));
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
                        debug_assert!(real_len == 1 + some_to_1(&last_whitespace));

                        Err(self)
                    }
                    _ => unreachable!(),
                },
            }
        }

        const fn last_non_whitespace(&self) -> Option<ValueAndRemaining<'a>> {
            match self {
                ValueList::Empty => None,
                ValueList::NotEmpty {
                    first,
                    last_3,
                    last_whitespace: _,
                    real_len: _,
                } => Some(match last_3.as_slice().last() {
                    Some(v) => v.value,
                    None => *first,
                }),
            }
        }

        const fn last_maybe_whitespace(&self) -> Option<ValueAndRemaining<'a>> {
            match self {
                ValueList::Empty => None,
                ValueList::NotEmpty {
                    first: _,
                    last_3: _,
                    last_whitespace,
                    real_len: _,
                } => {
                    if let Some(last_whitespace) = last_whitespace {
                        Some((*last_whitespace).into_component_value_and_remaining())
                    } else {
                        self.last_non_whitespace()
                    }
                }
            }
        }

        const fn span_without_trailing_whitespace(&self) -> Span<'a> {
            match self {
                ValueList::Empty => Span {
                    full: CopyableTokenStream::EMPTY,
                    len: 0,
                },
                ValueList::NotEmpty {
                    first,
                    last_3,
                    last_whitespace: _,
                    real_len,
                } => match last_3.as_slice().last() {
                    Some(last) => Span {
                        full: first.full.before(last.value.remaining),
                        len: *real_len,
                    },
                    None => Span {
                        full: first.full.before(first.remaining),
                        len: *real_len,
                    },
                },
            }
        }

        const fn value_including_important_without_trailing_whitespace(
            &self,
        ) -> Option<CopyableTokenStream<'a>> {
            match self {
                ValueList::Empty => None,
                ValueList::NotEmpty {
                    first,
                    last_3,
                    last_whitespace: _,
                    real_len: _,
                } => Some(match last_3.as_slice().last() {
                    Some(last) => first.full.before(last.value.remaining),
                    None => first.full.before(first.remaining),
                }),
            }
        }

        /// Last 2 non whitespace that haven't been returned.
        const fn last_2_non_whitespace_copied(
            &self,
        ) -> ArrayVec<WhitespaceAndValueAndRemaining<'a>, 2> {
            match self {
                ValueList::Empty => ArrayVec::EMPTY,
                ValueList::NotEmpty {
                    first,
                    last_3,
                    last_whitespace: _,
                    real_len: _,
                } => {
                    match last_3.as_slice() {
                        // the first has been returned by Self::with_push
                        [_, b, c] => ArrayVec::new_filled([*b, *c]),
                        [b, c] => ArrayVec::new_filled([*b, *c]),
                        [c] => ArrayVec::new_filled([
                            WhitespaceAndValueAndRemaining {
                                whitespace: None,
                                value: *first,
                            },
                            *c,
                        ]),
                        [] => ArrayVec::EMPTY.with_push(WhitespaceAndValueAndRemaining {
                            whitespace: None,
                            value: *first,
                        }),
                        _ => unreachable!(),
                    }
                }
            }
        }

        const fn first(&self) -> Option<&ValueAndRemaining<'a>> {
            match self {
                ValueList::Empty => None,
                ValueList::NotEmpty {
                    first,
                    last_3: _,
                    last_whitespace: _,
                    real_len: _,
                } => Some(first),
            }
        }
    }

    pub(crate) struct KnownDeclarationValueListBuilder<'a, L: IsKnownComponentValueList<'a>> {
        // the full list is list_builder + value_list.last_3
        list_builder: KnownComponentValueListBuilder<'a, L>,
        value_list: ValueList<'a>,
    }

    impl<'a, L: IsKnownComponentValueList<'a>> KnownDeclarationValueListBuilder<'a, L> {
        pub(super) const fn new() -> Self {
            Self {
                list_builder: KnownComponentValueList::start_builder(),
                value_list: ValueList::Empty,
            }
        }
    }

    const fn value_of_whitespace(ws: WhitespaceToken) -> ComponentValue {
        ComponentValue::PreservedTokens(Token::Whitespace(ws))
    }

    impl<
            'a,
            L: IsKnownComponentValueListWithConstEmpty<'a>,
            const ARRAY_VEC_CAP: usize,
            const LEAD_VEC_CAP: usize,
        > KnownDeclarationValueListBuilder<'a, L>
    where
        L::Collection: IsKnownCollection<
            ComponentValue<'a>,
            ArrayVecType = ArrayVec<ComponentValue<'a>, ARRAY_VEC_CAP>,
            LeadVecType = LeadVec<ComponentValue<'a>, LEAD_VEC_CAP>,
        >,
    {
        pub(crate) const fn with_push(
            self,
            tar: TokenAndRemaining<'a, ComponentValue<'a>>,
        ) -> Self {
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

            let list_builder = if let Some(wv) = value_that_is_not_last_2 {
                wv.push_to(self.list_builder)
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
        ) -> BuildOutput<'a, L> {
            // after values and !important
            let after_value_and_important = match self.value_list.last_non_whitespace() {
                Some(v) => v.remaining,
                None => after_colon,
            };

            // after values and !important
            let after_value_and_important_and_trailing_whitespace =
                match self.value_list.last_maybe_whitespace() {
                    Some(v) => v.remaining,
                    None => after_colon,
                };

            let value_and_important = match self
                .value_list
                .value_including_important_without_trailing_whitespace()
            {
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
                            let last_2 = values.last_2_non_whitespace_copied();
                            let last_2 = last_2.as_slice();

                            let mut i = 0;
                            while i < last_2.len() {
                                list_builder = last_2[i].push_to(list_builder);

                                i += 1;
                            }
                        }

                        list_builder.build(match values.last_non_whitespace() {
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
                after_value_and_important_and_trailing_whitespace,
            }
        }
    }

    pub(crate) struct BuildOutput<'a, L: IsKnownComponentValueListWithConstEmpty<'a>> {
        pub(crate) value: KnownDeclarationValueList<'a, L>,
        pub(crate) important: Option<Important<'a>>,
        pub(crate) value_and_important: CopyableTokenStream<'a>,
        pub(crate) after_value_and_important: CopyableTokenStream<'a>,
        pub(crate) after_value_and_important_and_trailing_whitespace: CopyableTokenStream<'a>,
    }
}
