use crate::{
    const_known::{No, Yes, YesOrNo},
    parse::component_value::ComponentValue,
    token::stream::CopyableTokenStream,
};

use super::{
    array_vec::ArrayVec,
    known::{IsKnownCollection, IsKnownCollectionWithConstEmpty, KnownCollection},
    lead_vec::LeadVec,
    parsed_value_list::KnownParsedValueList,
    FilterOutWhitespace,
};

#[non_exhaustive]
pub struct KnownComponentValueList<'a, V: IsKnownComponentValueList<'a>> {
    value_list: KnownParsedValueList<'a, V::Collection, ComponentValue<'a>>,
}

impl<'a, V: IsKnownComponentValueList<'a>> KnownComponentValueList<'a, V> {
    pub(crate) const fn full(&self) -> CopyableTokenStream<'a> {
        self.value_list.full()
    }

    pub(crate) const fn full_as_str(&self) -> &'a str {
        self.value_list.full_as_str()
    }

    pub(crate) const fn as_known_parsed_value_list(
        &self,
    ) -> &KnownParsedValueList<'a, V::Collection, ComponentValue<'a>> {
        &self.value_list
    }
}

impl<'a, V: IsKnownComponentValueListWithConstEmpty<'a>> KnownComponentValueList<'a, V> {
    pub(crate) const EMPTY: Self = Self {
        value_list: KnownParsedValueList::EMPTY,
    };
}

impl<'a, V: IsKnownComponentValueList<'a>> Copy for KnownComponentValueList<'a, V> {}
impl<'a, V: IsKnownComponentValueList<'a>> Clone for KnownComponentValueList<'a, V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, V: IsKnownComponentValueList<'a>> std::fmt::Debug for KnownComponentValueList<'a, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("KnownComponentValueList")
            .field("value_list", &self.value_list)
            .finish()
    }
}

mod sealed {
    pub trait IsKnownComponentValueList<'a> {}
}

pub trait IsKnownComponentValueList<'a>: sealed::IsKnownComponentValueList<'a> {
    type Collection: IsKnownCollection<ComponentValue<'a>>;

    const FILTER_OUT_WHITESPACE: bool;
}

pub trait IsKnownComponentValueListWithConstEmpty<'a>:
    IsKnownComponentValueList<'a, Collection = Self::CollectionWithConstEmpty>
{
    type CollectionWithConstEmpty: IsKnownCollectionWithConstEmpty<ComponentValue<'a>>;
    const EMPTY_AS_KNOWN_COMPONENT_VALUE_LIST: KnownComponentValueList<'a, Self::Collection>;
}

impl<'a, L: IsKnownCollection<ComponentValue<'a>>> sealed::IsKnownComponentValueList<'a> for L {}
impl<'a, L: IsKnownCollection<ComponentValue<'a>>> IsKnownComponentValueList<'a> for L {
    type Collection = L;
    const FILTER_OUT_WHITESPACE: bool = false;
}
impl<'a, L: IsKnownCollectionWithConstEmpty<ComponentValue<'a>>>
    IsKnownComponentValueListWithConstEmpty<'a> for L
{
    type CollectionWithConstEmpty = Self::Collection;
    const EMPTY_AS_KNOWN_COMPONENT_VALUE_LIST: KnownComponentValueList<'a, Self> =
        KnownComponentValueList {
            value_list: KnownParsedValueList::EMPTY,
        };
}

impl<'a, L: IsKnownCollection<ComponentValue<'a>>> sealed::IsKnownComponentValueList<'a>
    for FilterOutWhitespace<L>
{
}
impl<'a, L: IsKnownCollection<ComponentValue<'a>>> IsKnownComponentValueList<'a>
    for FilterOutWhitespace<L>
{
    type Collection = L;
    const FILTER_OUT_WHITESPACE: bool = true;
}
impl<'a, L: IsKnownCollectionWithConstEmpty<ComponentValue<'a>>>
    IsKnownComponentValueListWithConstEmpty<'a> for FilterOutWhitespace<L>
{
    type CollectionWithConstEmpty = Self::Collection;
    const EMPTY_AS_KNOWN_COMPONENT_VALUE_LIST: KnownComponentValueList<'a, Self::Collection> =
        L::EMPTY_AS_KNOWN_COMPONENT_VALUE_LIST;
}

pub(crate) mod builder {
    use crate::{
        collections::{
            array_vec::ArrayVec,
            known::IsKnownCollectionWithConstEmpty,
            lead_vec::LeadVec,
            parsed_value_list::{KnownParsedValueList, KnownParsedValueListBuilder},
        },
        parse::component_value::ComponentValue,
        token::{
            stream::CopyableTokenStream,
            tokens::{Whitespace, WhitespaceToken},
        },
    };

    use super::{
        IsKnownComponentValueList, IsKnownComponentValueListWithConstEmpty, KnownComponentValueList,
    };
    impl<'a, V: IsKnownComponentValueList<'a>> KnownComponentValueList<'a, V> {
        pub(crate) const fn start_builder() -> KnownComponentValueListBuilder<'a, V> {
            KnownComponentValueListBuilder {
                value_list_builder: KnownParsedValueList::start_builder(),
            }
        }
    }

    pub(crate) struct KnownComponentValueListBuilder<'a, V: IsKnownComponentValueList<'a>> {
        value_list_builder: KnownParsedValueListBuilder<'a, V::Collection, ComponentValue<'a>>,
    }

    struct WhitespaceTokenAndRemaining<'a> {
        token: WhitespaceToken<'a>,
        token_and_remaining: CopyableTokenStream<'a>,
    }

    impl<
            'a,
            V: IsKnownComponentValueListWithConstEmpty<
                'a,
                Collection = L,
                CollectionWithConstEmpty = L,
            >,
            L: IsKnownCollectionWithConstEmpty<
                ComponentValue<'a>,
                ArrayVecType = ArrayVec<ComponentValue<'a>, ARRAY_VEC_CAP>,
                LeadVecType = LeadVec<ComponentValue<'a>, LEAD_VEC_CAP>,
            >,
            const ARRAY_VEC_CAP: usize,
            const LEAD_VEC_CAP: usize,
        > KnownComponentValueListBuilder<'a, V>
    {
        pub(crate) const fn with_push(
            self,
            value: ComponentValue<'a>,
            value_and_remaining: CopyableTokenStream<'a>,
        ) -> Self {
            if V::FILTER_OUT_WHITESPACE && value.is_whitespace() {
                return self;
            }

            Self {
                value_list_builder: self
                    .value_list_builder
                    .with_push(value, value_and_remaining),
            }
        }

        pub(crate) const fn build(
            self,
            remaining: CopyableTokenStream<'a>,
        ) -> KnownComponentValueList<'a, V> {
            KnownComponentValueList {
                value_list: self.value_list_builder.build(remaining),
            }
        }
    }
}
