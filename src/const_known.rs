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

impl Yes {
    #[inline(always)]
    pub const fn unwrap_value_or_no<R>(self, v: R) -> R {
        v
    }

    #[inline(always)]
    pub const fn unwrap_value<R>(self, v: R) -> R {
        v
    }
}

impl No {
    #[inline(always)]
    pub const fn unwrap_value_or_no<R>(self, _: R) -> No {
        match self {}
    }
    #[inline(always)]
    pub const fn unwrap_value<R>(self, _: impl Sized) -> R {
        match self {}
    }
}

/// ## `#[non_exhaustive]` and `#[sealed(my_sealed_mod)]`
///
/// ```
/// # use ccss::define_known_variants;
/// define_known_variants!(
///     #[non_exhaustive]
///     pub enum KnownUnsignedInteger {
///         U8(u8),
///         U16(u16),
///         U32(u32),
///         U64(u64),
///     }
///
///     #[sealed(my_sealed_mod)]
///     pub trait IsKnownUnsignedInteger {}
/// );
/// ```
#[macro_export]
macro_rules! define_known_variants {
    (
        $(#$enum_attr:tt)*
        $enum_vis:vis enum $KnownVariant:ident <$($generics_and_rest:tt)*
    ) => {
        $crate::__parse_generics! {
            {
                generics {}
                impl_generics {}
                type_generics {}
                generics_info {}
            }
            {$($generics_and_rest)*}
            {$($generics_and_rest)*}
            {
                macro {$crate::__define_known_variants_after_parse_generics!}
                before {
                    dollar { $ }
                    enum {
                        $(#$enum_attr)*
                        $enum_vis $KnownVariant
                    }
                }
                after {}
            }
        }
    };
    (
        $(#$enum_attr:tt)*
        $enum_vis:vis enum $KnownVariant:ident $($rest:tt)*
    ) => {
        $crate::define_known_variants! {
            $(#$enum_attr)*
            $enum_vis enum $KnownVariant <> $($rest)*
        }
    };
}

#[doc(hidden)]
pub mod __private {
    pub use ::syn_lite::{expand_if_else, parse_generics};
    pub use core::stringify;

    #[macro_export]
    macro_rules! __resolve_finish {
        (
            {
                macro {$($macro_and_bang:tt)*}
                before { $($before:tt)* }
                after  { $($after:tt )* }
            }
            $($t:tt)*
        ) => {
            $($macro_and_bang)* {
                $($before)*
                $($t)*
                $($after)*
            }
        };
    }

    #[macro_export]
    // till an outer `>`
    macro_rules! __consume_till_gt {
        // >
        (
            $consumed:tt
            {> $($_rest:tt)*}
            $rest:tt
            [] // no inner `<` before this `>`
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                before_gt $consumed
                gt_and_rest $rest
            }
        };
        // >>
        (
            {$($consumed:tt)*}
            {>> $($_rest:tt)*}
            {>> $($rest:tt )*}
            [<] // no inner `<` before the second `>`
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                before_gt {$($consumed)* >}
                gt_and_rest { > $($rest)* }
            }
        };
        // <
        (
            {$($consumed:tt)*}
            {<     $($_rest:tt)*}
            {$t:tt $($rest:tt )*}
            [$($got_lt:tt)*]
            $finish:tt
        ) => {
            $crate::__consume_till_gt! {
                {$($consumed)* $t}
                {$($rest)*}
                {$($rest)*}
                [$($got_lt)* $t]
                $finish
            }
        };
        // <<
        (
            {$($consumed:tt)*}
            {<<    $($_rest:tt)*}
            {$t:tt $($rest:tt )*}
            [$($got_lt:tt)*]
            $finish:tt
        ) => {
            $crate::__consume_till_gt! {
                {$($consumed)* $t}
                {$($rest)*}
                {$($rest)*}
                [$($got_lt)* < <] // split `<<` into two `<`
                $finish
            }
        };
        // `>` matched a previous `<`
        (
            {$($consumed:tt)*}
            {>     $($_rest:tt)*}
            {$t:tt $($rest:tt )*}
            [< $($got_lt:tt)*]
            $finish:tt
        ) => {
            $crate::__consume_till_gt! {
                {$($consumed)* $t}
                {$($rest)*}
                {$($rest)*}
                [$($got_lt)*]
                $finish
            }
        };
        // `>>` matches two previous `<`
        (
            {$($consumed:tt)*}
            {>>    $($_rest:tt)*}
            {$t:tt $($rest:tt )*}
            [< < $($got_lt:tt)*]
            $finish:tt
        ) => {
            $crate::__consume_till_gt! {
                {$($consumed)* $t}
                {$($rest)*}
                {$($rest)*}
                [$($got_lt)*]
                $finish
            }
        };
        // anything else
        (
            {$($consumed:tt)*}
            $_rest:tt
            {$t:tt $($rest:tt)*}
            $got_lt:tt
            $finish:tt
        ) => {
            $crate::__consume_till_gt! {
                {$($consumed)* $t}
                {$($rest)*}
                {$($rest)*}
                $got_lt
                $finish
            }
        };
    }

    #[macro_export]
    macro_rules! __parse_bounds_finish_consume_till_gt {
        (
            before_gt {$($consumed:tt)*}
            gt_and_rest {$gt:tt $($rest:tt)*}
            finish $finish:tt
        ) => {
            // continue parse bounds
            $crate::__parse_bounds! {
                {$($consumed)* $gt}
                {$($rest)*}
                {$($rest)*}
                $finish
            }
        };
    }

    // till one of the following tokens:
    // - `,`
    // - `where`
    // - an outer `>`
    // - an outer `=`
    // - {..}
    // - EOF
    #[macro_export]
    macro_rules! __parse_bounds {
        // ,
        (
            $parsed_bounds:tt
            {, $($after:tt)*}
            $rest:tt
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_bounds $parsed_bounds
                rest $rest
            }
        };
        // where
        (
            $parsed_bounds:tt
            {where $($after:tt)*}
            $rest:tt
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_bounds $parsed_bounds
                rest $rest
            }
        };
        // an outer >
        (
            $parsed_bounds:tt
            {> $($after:tt)*}
            $rest:tt
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_bounds $parsed_bounds
                rest $rest
            }
        };
        // an outer =
        (
            $parsed_bounds:tt
            {= $($after:tt)*}
            $rest:tt
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_bounds $parsed_bounds
                rest $rest
            }
        };
        // {..}
        (
            $parsed_bounds:tt
            {{$($_t:tt)*} $($after:tt)*}
            $rest:tt
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_bounds $parsed_bounds
                rest $rest
            }
        };
        // EOF
        (
            $parsed_bounds:tt
            {} // EOF
            $rest:tt
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_bounds $parsed_bounds
                rest $rest
            }
        };
        // `<` , consume till a matched `>`
        (
            {$($parsed_bounds:tt)*}
            {<     $($_rest:tt)*}
            {$t:tt $($rest:tt )*}
            $finish:tt
        ) => {
            $crate::__consume_till_gt! {
                {$($parsed_bounds)* $t}
                {$($rest)*}
                {$($rest)*}
                []
                {
                    macro { $crate::__parse_bounds_finish_consume_till_gt! }
                    before {}
                    after { finish $finish }
                }
            }
        };
        // other cases, just consume
        (
            {$($parsed_bounds:tt)*}
            {$t:tt $($rest:tt)*}
            $t_and_rest:tt
            $finish:tt
        ) => {
            $crate::__parse_bounds! {
                {$($parsed_bounds)* $t}
                {$($rest)*}
                {$($rest)*}
                $finish
            }
        };
    }

    #[cfg(test)]
    mod tests {
        macro_rules! expect_parsed_bounds {
            (
                parsed_bounds {IsKnownParsedValueList<ComponentValue<'a>, CAP>}
                rest {,}
            ) => {};
        }
        __parse_bounds! {
            {}
            {IsKnownParsedValueList<ComponentValue<'a>, CAP>,}
            {IsKnownParsedValueList<ComponentValue<'a>, CAP>,}
            {
                macro {expect_parsed_bounds!}
                before {}
                after {}
            }
        }

        macro_rules! check_gt_3 {
            (>> >) => {};
        }

        check_gt_3! {>>>}

        macro_rules! check_lt_3 {
            (<< <) => {};
        }

        check_lt_3! {<<<}

        macro_rules! expect_consume_till_gt {
            (
                before_gt {AsRef<u8>}
                gt_and_rest {>}
            ) => {};
        }
        __consume_till_gt!(
            {}
            {AsRef<u8>>}
            {AsRef<u8>>}
            []
            {
                macro {expect_consume_till_gt!}
                before {}
                after {}
            }
        );

        macro_rules! expect_consume_till_gt_complex {
            (
                before_gt {AsRef<<u8 as Trait>::Type<u8>>}
                gt_and_rest {>}
            ) => {};
        }

        __consume_till_gt!(
            {}
            {AsRef<<u8 as Trait>::Type<u8>>>}
            {AsRef<<u8 as Trait>::Type<u8>>>}
            []
            {
                macro {expect_consume_till_gt_complex!}
                before {}
                after {}
            }
        );
    }

    #[macro_export]
    macro_rules! __parse_generics_after_parse_bounds_type {
        (
            parsed_generics {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            generic_and_colon { $name:ident $colon:tt }
            parsed_bounds {$($parsed_bounds:tt)*}
            rest
            {=      $_ty:ty $(, $($_rest:tt)*)?}
            {$eq:tt $ty:ty  $(, $($rest:tt )*)?}
            finish $finish:tt
        ) => {
            $crate::__parse_generics! {
                {
                    generics {      $($generics)*      $name $colon $($parsed_bounds)* $eq $ty , }
                    impl_generics { $($impl_generics)* $name $colon $($parsed_bounds)*         , }
                    type_generics { $($type_generics)* $name                                   , }
                    generics_info {
                        $($generics_info)*
                        {
                            name { $name }
                            bounds { $($parsed_bounds)* }
                            default_ty { $ty }
                        }
                    }
                }
                {$($rest)*}
                {$($rest)*}
                $finish:tt
            }
        };
        (
            parsed_generics {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            generic_and_colon { $name:ident $colon:tt }
            parsed_bounds {$($parsed_bounds:tt)*}
            rest $rest:tt $_rest:tt
            finish $finish:tt
        ) => {
            $crate::__parse_generics_match_eof_or_comma! {
                // trailing comma is added later
                parsed_generics {
                    generics { $($generics)* $name $colon $($parsed_bounds)* }
                    impl_generics { $($impl_generics)* $name $colon $($parsed_bounds)* }
                    type_generics { $($type_generics)* $name }
                    generics_info {
                        $($generics_info)*
                        {
                            name { $name }
                            bounds {$($parsed_bounds)*}
                        }
                    }
                }
                rest $rest $rest
                finish $finish
            }
        };
    }

    #[macro_export]
    macro_rules! __parse_generics_after_parse_bounds {
        (
            parsed_generics {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            generic_and_colon { $lt:lifetime $colon:tt }
            parsed_bounds {$($parsed_bounds:tt)*}
            rest $rest:tt
            finish $finish:tt
        ) => {
            $crate::__parse_generics_match_eof_or_comma! {
                // trailing comma is added later
                parsed_generics {
                    generics { $($generics)* $lt $colon $($parsed_bounds)* }
                    impl_generics { $($impl_generics)* $lt $colon $($parsed_bounds)* }
                    type_generics { $($type_generics:tt)* $lt }
                    generics_info {
                        $($generics_info:tt)*
                        {
                            lifetime {$lt}
                            bounds {$($parsed_bounds)*}
                        }
                    }
                }
                rest $rest $rest
                finish $finish
            }
        };
        (
            parsed_generics $parsed_generics:tt
            generic_and_colon { $name:ident $colon:tt }
            parsed_bounds $parsed_bounds:tt
            rest $rest:tt
            finish $finish:tt
        ) => {
            $crate::__parse_generics_after_parse_bounds_type! {
                parsed_generics $parsed_generics
                generic_and_colon { $name $colon }
                parsed_bounds $parsed_bounds
                rest $rest $rest
                finish $finish
            }
        };
    }

    #[macro_export]
    macro_rules! __parse_generics_match_eof_or_comma {
        // add a trailing comma from the rest tokens
        (
            parsed_generics {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info $generics_info:tt
            }
            rest
            {,         $($_rest:tt)*}
            {$comma:tt $($rest:tt )*}
            finish $finish:tt
        ) => {
            $crate::__parse_generics! {
                {
                    generics { $($generics)* $comma }
                    impl_generics { $($impl_generics)* $comma }
                    type_generics { $($type_generics)* $comma }
                    generics_info $generics_info
                }
                {$($rest)*}
                {$($rest)*}
                $finish
            }
        };
        // add a trailing comma
        (
            parsed_generics {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info $generics_info:tt
            }
            rest $_rest:tt $rest:tt
            finish $finish:tt
        ) => {
            $crate::__parse_generics! {
                {
                    generics { $($generics)* , }
                    impl_generics { $($impl_generics)* , }
                    type_generics { $($type_generics)* , }
                    generics_info $generics_info
                }
                $_rest
                $rest
                $finish
            }
        };
    }

    //  parsed_generics {
    //      // original with trailing comma
    //      generics {}
    //      // remove default types, with trailing comma
    //      impl_generics {}
    //      // lifetimes, types, and const names, with trailing comma
    //      type_generics {}
    //      generics_info [
    //          {
    //              lifetime {'a}
    //              bounds {}
    //          }
    //          {
    //              name {T}
    //              bounds {Default}
    //              default_ty {()}
    //          }
    //          {
    //              name {U}
    //              bounds {}
    //          }
    //          {
    //              const {const}
    //              name {N}
    //              bounds {usize}
    //          }
    //          {
    //              const {const}
    //              name {M}
    //              bounds {usize}
    //              default_expr {0}
    //          }
    //      ]
    //  }
    #[macro_export]
    macro_rules! __parse_generics {
        // >
        (
            $parsed_generics:tt
            { >      $($_rest:tt)* }
            { $gt:tt $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_generics $parsed_generics
                gt {$gt}
                rest {$($rest)*}
            }
        };
        // 'a:
        (
            $parsed:tt
            { $_lt:lifetime :         $($_bounds_and_rest:tt)* }
            { $lt:lifetime  $colon:tt $($bounds_and_rest:tt )* }
            $finish:tt
        ) => {
            $crate::__parse_bounds! {
                {}
                {$($bounds_and_rest)*}
                {$($bounds_and_rest)*}
                {
                    macro { $crate::__parse_generics_after_parse_bounds! }
                    before {
                        parsed_generics $parsed
                        generic_and_colon { $lt $colon }
                    }
                    after {
                        finish $finish
                    }
                }
            }
        };
        // 'a,
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { $_lt:lifetime ,         $($_rest:tt)* }
            { $lt:lifetime  $comma:tt $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__parse_generics! {
                {
                    generics { $($generics)* $lt $comma }
                    impl_generics { $($impl_generics)* $lt $comma }
                    type_generics { $($type_generics)* $lt $comma }
                    generics_info {
                        $($generics_info)*
                        {
                            lifetime {$lt}
                            bounds {}
                        }
                    }
                }
                {$($rest)*}
                {$($rest)*}
                $finish
            }
        };
        // 'a>
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { $_lt:lifetime >      $($_rest:tt)* }
            { $lt:lifetime  $gt:tt $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_generics {
                    generics { $($generics)* $lt , }
                    impl_generics { $($impl_generics)* $lt , }
                    type_generics { $($type_generics)* $lt , }
                    generics_info {
                        $($generics_info)*
                        {
                            lifetime {$lt}
                            bounds {}
                        }
                    }
                }
                gt {$gt}
                rest {$($rest)*}
            }
        };
        // T:
        (
            $parsed:tt
            { $_name:ident :         $($_bounds_and_rest:tt)* }
            { $name:ident  $colon:tt $($bounds_and_rest:tt )* }
            $finish:tt
        ) => {
            $crate::__parse_bounds! {
                {}
                {$($bounds_and_rest)*}
                {$($bounds_and_rest)*}
                {
                    macro { $crate::__parse_generics_after_parse_bounds! }
                    before {
                        parsed_generics $parsed
                        generic_and_colon { $name $colon }
                    }
                    after {
                        finish $finish
                    }
                }
            }
        };
        // T,
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { $_name:ident ,         $($_rest:tt)* }
            { $name:ident  $comma:tt $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__parse_generics! {
                {
                    generics { $($generics)* $name $comma }
                    impl_generics { $($impl_generics)* $name $comma }
                    type_generics { $($type_generics)* $name $comma }
                    generics_info {
                        $($generics_info)*
                        {
                            name {$name}
                            bounds {}
                        }
                    }
                }
                {$($rest)*}
                {$($rest)*}
                $finish
            }
        };
        // T = _,
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { $_name:ident =      $_default_ty:ty , $($_rest:tt)* }
            { $name:ident  $eq:tt $default_ty:ty  , $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__parse_generics! {
                {
                    generics { $($generics)* $name $eq $default_ty , }
                    impl_generics { $($impl_generics)* $name , }
                    type_generics { $($type_generics)* $name , }
                    generics_info {
                        $($generics_info)*
                        {
                            name {$name}
                            bounds {}
                            default_ty {$default_ty}
                        }
                    }
                }
                {$($rest)*}
                {$($rest)*}
                $finish
            }
        };
        // T>
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { $_name:ident >      $($_rest:tt)* }
            { $name:ident  $gt:tt $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_generics {
                    generics { $($generics)* $name , }
                    impl_generics { $($impl_generics)* $name , }
                    type_generics { $($type_generics)* $name , }
                    generics_info {
                        $($generics_info)*
                        {
                            name {$name}
                            bounds {}
                        }
                    }
                }
                gt {$gt}
                rest {$($rest)*}
            }
        };
        // T = _>
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { $_name:ident =      $_default_ty:ty > $($_rest:tt)* }
            { $name:ident  $eq:tt $default_ty:ty  > $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_generics {
                    generics { $($generics)* $name $eq $default_ty, }
                    impl_generics { $($impl_generics)* $name , }
                    type_generics { $($type_generics)* $name , }
                    generics_info {
                        $($generics_info)*
                        {
                            name {$name}
                            bounds {}
                            default_ty {$default_ty}
                        }
                    }
                }
                gt {>}
                rest {$($rest)*}
            }
        };
        // `const N: usize,` or `const N: usize = expr,`
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { const        $_name:ident :         $_bounds:ty $(= $_default_expr:expr)? , $($_rest:tt)* }
            { $const:ident $name:ident  $colon:tt $bounds:ty  $(= $default_expr:expr )? , $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__parse_generics! {
                {
                    generics { $($generics)* $const $name $colon $bounds $(= $default_expr)? , }
                    impl_generics { $($impl_generics)* $const $name $colon $bounds , }
                    type_generics { $($type_generics)* $name , }
                    generics_info {
                        $($generics_info)*
                        {
                            const {$const}
                            name {$name}
                            bounds {$bounds}
                            $(default_expr {$default_expr})?
                        }
                    }
                }
                {$($rest)*}
                {$($rest)*}
                $finish
            }
        };
        // `const N: usize>`
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { const        $_name:ident :         $_bounds:ty > $($_rest:tt)* }
            { $const:ident $name:ident  $colon:tt $bounds:ty  > $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_generics {
                    generics { $($generics)* $const $name $colon $bounds , }
                    impl_generics { $($impl_generics)* $const $name $colon $bounds , }
                    type_generics { $($type_generics)* $name , }
                    generics_info {
                        $($generics_info)*
                        {
                            const {$const}
                            name {$name}
                            bounds {$bounds}
                        }
                    }
                }
                gt {>}
                rest {$($rest)*}
            }
        };
        // `const N: usize = path>`
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { const        $_name:ident :         $_bounds:ty = $_default_expr:path > $($_rest:tt)* }
            { $const:ident $name:ident  $colon:tt $bounds:ty  = $default_expr:path  > $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_generics {
                    generics { $($generics)* $const $name $colon $bounds = $default_expr , }
                    impl_generics { $($impl_generics)* $const $name $colon $bounds , }
                    type_generics { $($type_generics)* $name , }
                    generics_info {
                        $($generics_info)*
                        {
                            const {$const}
                            name {$name}
                            bounds {$bounds}
                            default_expr {$default_expr}
                        }
                    }
                }
                gt {>}
                rest {$($rest)*}
            }
        };
        // `const N: usize = {..}>`
        (
            {
                generics { $($generics:tt)* }
                impl_generics { $($impl_generics:tt)* }
                type_generics { $($type_generics:tt)* }
                generics_info { $($generics_info:tt)* }
            }
            { const        $_name:ident :         $_bounds:ty = $_default_expr:block >      $($_rest:tt)* }
            { $const:ident $name:ident  $colon:tt $bounds:ty  = $default_expr:block  $gt:tt $($rest:tt )* }
            $finish:tt
        ) => {
            $crate::__resolve_finish! {
                $finish
                parsed_generics {
                    generics { $($generics)* $const $name $colon $bounds = $default_expr , }
                    impl_generics { $($impl_generics)* $const $name $colon $bounds , }
                    type_generics { $($type_generics)* $name , }
                    generics_info {
                        $($generics_info)*
                        {
                            const {$const}
                            name {$name}
                            bounds {$bounds}
                            default_expr {$default_expr}
                        }
                    }
                }
                gt {$gt}
                rest {$($rest)*}
            }
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_trait_IsKnownVariant {
        (
            sealed_mod {$sealed_mod:ident}
            sealed_bound { $sealed_bound:path }
            trait_vis {$trait_vis:vis}
            trait_path {$($trait_path:tt)*}
            trait_bounds {$($trait_bounds:tt)*}
            $trait_block:tt
        ) => {
            $trait_vis trait $($trait_path)*
            :
            $sealed_bound +
            $($trait_bounds)*
            $trait_block
        };
        (
            sealed_mod {}
            sealed_bound { $sealed_bound:path }
            trait_vis {$trait_vis:vis}
            trait_path {$($trait_path:tt)*}
            trait_bounds {$($trait_bounds:tt)*}
            $trait_block:tt
        ) => {
            $trait_vis trait $($trait_path)*
            :
            $($trait_bounds)*
            $trait_block
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_define_enum {
        (
            enum {
                $(#$enum_attr:tt)*
                $enum_vis:vis $KnownVariant:ident
            }
            variant_bounds { $($variant_bounds:tt)* }
            generics_info {
                $({
                    lifetime {$generic_lifetime:lifetime}
                    bounds {$($generic_lifetime_bounds:tt)*}
                })*
                $({
                    $(const {$generic_const:tt})?
                    name { $generic_name:ident }
                    bounds {$($generic_bounds:tt)*}
                    $(default_ty {$default_ty:ty})?
                    $(default_expr {$($default_expr:tt)+})?
                })*
            }
            variants {
                $(
                    $(#$var_attr:tt)*
                    $VarName:ident ($VarType:ty)
                ),*
                $(,)?
            }
        ) => {
            $(#$enum_attr)*
            $enum_vis enum $KnownVariant<
                $($generic_lifetime: $($generic_lifetime_bounds:tt)* ,)*
                __Variant: $($variant_bounds)*,
                $(
                    $($generic_const)?
                    $generic_name
                    : $($generic_bounds)*
                    $(= $default_ty)?
                    $(= $($default_expr)+)?
                    ,
                )*
            > {
                $(
                    $(#$var_attr)*
                    $VarName(__Variant::$VarName, $VarType),
                )*
            }
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_after_parse_generics {
        (
            dollar { $dollar:tt }
            enum {
                $(#$enum_attr:tt)*
                $enum_vis:vis $KnownVariant:ident
            }
            parsed_generics {
                generics { $($params:tt)* }
                impl_generics $impl_generics:tt
                type_generics { $($type_generics:tt)* }
                generics_info $generics_info:tt
            }
            gt {>}
            rest {
                {
                    $(
                        $(#$var_attr:tt)*
                        $VarName:ident ($VarType:ty)
                    ),*
                    $(,)?
                }

                $(#[sealed ($sealed_mod:ident)])?
                $trait_vis:vis trait $IsKnownVariant:ident
                $(
                    :
                    $($trait_bound_lt:lifetime)?
                    $(+ $trait_bounds_lt:lifetime)*
                    $(
                        $( + $({$plus_ignore:tt })? )?
                        $( ? $([$relax_ignore:tt])? )?
                        $bounds:path
                    )*
                )?
                {}

                $($rest:tt)*
            }
        ) => {
            $crate::__define_known_variants_define_enum! {
                enum {
                    $(#$enum_attr)*
                    $enum_vis $KnownVariant
                }
                variant_bounds { $IsKnownVariant<$($type_generics)*> }
                generics_info $generics_info
                variants {
                    $(
                        $(#$var_attr)*
                        $VarName ($VarType)
                    ),*
                }
            }

            $crate::const_known::__private::expand_if_else! { [$($sealed_mod)?]{
                mod $($sealed_mod)? {
                    #[allow(unused_imports)]
                    use super::*;

                    $trait_vis trait $IsKnownVariant<$($params)*> {}
                }
            }{}}

            $crate::__define_known_variants_trait_IsKnownVariant! {
                sealed_mod {$($sealed_mod)?}
                sealed_bound { $($sealed_mod)? ::$IsKnownVariant<$($type_generics)*> }
                trait_vis { $trait_vis }
                trait_path { $IsKnownVariant<$($params)*> }
                trait_bounds {
                    $(
                        $($trait_bound_lt)?
                        $(+ $trait_bounds_lt)*
                        $(
                            $( + $({$plus_ignore })? )?
                            $( ? $([$relax_ignore])? )?
                            $bounds
                        )*
                    )?
                }
                {
                    $(
                        #[doc = " Indicates whether `Self` may be [`"]
                        #[doc = $crate::const_known::__private::stringify!($VarType)]
                        #[doc = "`]"]
                        type $VarName: $crate::const_known::YesOrNo;
                    )*
                }
            }

            const _: () = {
                macro_rules! __ident_match {
                    $(
                        ($VarName $VarName) => {
                            $crate::const_known::Yes
                        };
                        ($VarName $VarName { $dollar($yes:tt)* } $no:tt) => {
                            $dollar($yes)*
                        };
                    )*
                    ($_0:ident $_1:ident ) => {
                        $crate::const_known::No
                    };
                    ($_0:ident $_1:ident $yes:tt { $dollar($no:tt)* } ) => {
                        $dollar($no)*
                    };
                }

                $crate::__define_known_variants_impl_for_known_variants! {
                    impl_generics $impl_generics
                    trait { $IsKnownVariant<$($type_generics)*> }
                    sealed_trait {
                        sealed_mod { $($sealed_mod)? }
                        sealed_trait_path {
                            $($sealed_mod)? ::$IsKnownVariant< $($type_generics)* >
                        }
                    }
                    [$(
                        { $VarName($VarType) }
                    )*]
                    [$(
                        $VarName
                    )*]
                }

                $crate::__define_known_variants_impl_auto_fns! {
                    {$($rest)*}
                    {
                        variants {$(
                            { $VarName($VarType) }
                        )*}
                        {
                            impl_generics $impl_generics
                            Self { $KnownVariant }
                            type_generics { $($type_generics)* }
                            generics_info $generics_info
                            variant_names {$(
                                $VarName
                            )*}
                        }
                    }
                }
            };
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_impl_for_known_variant {
        (
            impl_generics { $($impl_generics:tt)* }
            trait { $trait_path:path }
            sealed_trait {
                sealed_mod {$($sealed_mod:ident)?}
                sealed_trait_path {
                    $($sealed_trait_path:path)?
                }
            }
            { $VarName:ident ($VarType:ty) }
            [$($variant_name:ident)*]
        ) => {
            $crate::const_known::__private::expand_if_else! { [$($sealed_mod)?]{
                impl<$($impl_generics)*> $($sealed_trait_path)? for $VarType {}
            }{}}

            impl<$($impl_generics)*> $trait_path for $VarType {
                $(
                    type $variant_name = __ident_match![$VarName $variant_name];
                )*
            }
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_impl_for_known_variants {
        (
            impl_generics $impl_generics:tt
            trait $trait_path:tt
            sealed_trait $sealed_trait:tt
            [$($var_name_and_type:tt)*]
            $variant_names:tt
        ) => {
            $(
                $crate::__define_known_variants_impl_for_known_variant! {
                    impl_generics $impl_generics
                    trait $trait_path
                    sealed_trait $sealed_trait
                    $var_name_and_type
                    $variant_names
                }
            )*
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_impl_auto_fn {
        (
            {
                fn_name $fn_name:ident
                $($rest:tt)*
            }
        ) => {
            $crate::__define_known_variants_impl_auto_fn! {
                $fn_name !
                {
                    fn_name $fn_name
                    $($rest)*
                }
            }
        };
        (
            $fn_macro:ident ! $info:tt
        ) => {
            $crate::const_known::auto_fns::$fn_macro ! $info
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_impl_auto_fns {
        (
            {
                $(
                    $(#[$auto_fn_macro:ident])?
                    $fn_vis:vis fn $fn_name:ident $fn_args:tt $fn_body_or_semi:tt
                )*
            }
            $common_info:tt
        ) => {
            $(
                $crate::__define_known_variants_impl_auto_fn! {
                    $($auto_fn_macro !)? {
                        fn_name $fn_name
                        fn_vis {$fn_vis}
                        fn_args $fn_args
                        fn_body_or_semi $fn_body_or_semi
                        $common_info
                    }
                }
            )*
        };
    }
}

#[doc(hidden)]
pub mod auto_fns {
    #[macro_export]
    macro_rules! __define_known_variants_auto_fn_from_variant_one {
        (
            {$VarName:ident ($VarType:ty)}
            fn_vis { $fn_vis:vis }
            fn_name $fn_name:ident
            {
                impl_generics {$($impl_generics:tt)*}
                Self { $SelfName:ident }
                type_generics $type_generics:tt
                generics_info {
                    $({
                        lifetime {$generic_lifetime:lifetime}
                        bounds {$($generic_lifetime_bounds:tt)*}
                    })*
                    $({
                        $(const {$generic_const:tt})?
                        name { $generic_name:ident }
                        bounds {$($generic_bounds:tt)*}
                        $(default_ty {$default_ty:ty})?
                        $(default_expr {$($default_expr:tt)+})?
                    })*
                }
                variant_names {$(
                    $AllVarName:ident
                )*}
            }
        ) => {
            impl<$($impl_generics)*> $SelfName<
                $($generic_lifetime,)*
                $VarType,
                $($generic_name,)*
            > {
                $fn_vis const fn $fn_name(v: $VarType) -> Self {
                    Self::$VarName($crate::const_known::Yes, v)
                }
            }
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_auto_fn_from_variant {
        (
            fn_name $fn_name:ident
            fn_vis $fn_vis:tt
            fn_args ()
            fn_body_or_semi ;
            {
                variants {$($var:tt)*}
                $common_info_for_each_var:tt
            }
        ) => {
            $(
                $crate::__define_known_variants_auto_fn_from_variant_one! {
                    $var
                    fn_vis $fn_vis
                    fn_name $fn_name
                    $common_info_for_each_var
                }
            )*
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_auto_fn_into_variant_one {
        (
            {$VarName:ident ($VarType:ty)}
            fn_vis { $fn_vis:vis }
            fn_name $fn_name:ident
            {
                impl_generics {$($impl_generics:tt)*}
                Self { $SelfName:ident }
                type_generics $type_generics:tt
                generics_info {
                    $({
                        lifetime {$generic_lifetime:lifetime}
                        bounds {$($generic_lifetime_bounds:tt)*}
                    })*
                    $({
                        $(const {$generic_const:tt})?
                        name { $generic_name:ident }
                        bounds {$($generic_bounds:tt)*}
                        $(default_ty {$default_ty:ty})?
                        $(default_expr {$($default_expr:tt)+})?
                    })*
                }
                variant_names {$(
                    $AllVarName:ident
                )*}
            }
        ) => {
            impl<$($impl_generics)*> $SelfName<
                $($generic_lifetime,)*
                $VarType,
                $($generic_name,)*
            > {
                $fn_vis const fn $fn_name(self) -> $VarType {
                    match self {
                        $(
                            Self::$AllVarName(
                                __ident_match![$AllVarName $VarName {$crate::const_known::Yes} {no}],
                                __ident_match![$AllVarName $VarName {this} {_}],
                            ) => __ident_match![$AllVarName $VarName {this} { match no {} }],
                        )*
                    }
                }
            }
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_auto_fn_into_variant {
        (
            fn_name $fn_name:ident
            fn_vis $fn_vis:tt
            fn_args ()
            fn_body_or_semi ;
            {
                variants {$($var:tt)*}
                $common_info_for_each_var:tt
            }
        ) => {
            $(
                $crate::__define_known_variants_auto_fn_into_variant_one! {
                    $var
                    fn_vis $fn_vis
                    fn_name $fn_name
                    $common_info_for_each_var
                }
            )*
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_auto_fn_as_variant_one {
        (
            {$VarName:ident ($VarType:ty)}
            fn_vis { $fn_vis:vis }
            fn_name $fn_name:ident
            {
                impl_generics {$($impl_generics:tt)*}
                Self { $SelfName:ident }
                type_generics $type_generics:tt
                generics_info {
                    $({
                        lifetime {$generic_lifetime:lifetime}
                        bounds {$($generic_lifetime_bounds:tt)*}
                    })*
                    $({
                        $(const {$generic_const:tt})?
                        name { $generic_name:ident }
                        bounds {$($generic_bounds:tt)*}
                        $(default_ty {$default_ty:ty})?
                        $(default_expr {$($default_expr:tt)+})?
                    })*
                }
                variant_names {$(
                    $AllVarName:ident
                )*}
            }
        ) => {
            impl<$($impl_generics)*> $SelfName<
                $($generic_lifetime,)*
                $VarType,
                $($generic_name,)*
            > {
                $fn_vis const fn $fn_name(&self) -> &$VarType {
                    match self {
                        $(
                            Self::$AllVarName(yes_or_no, this) => yes_or_no.unwrap_value(this),
                        )*
                    }
                }
            }
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_auto_fn_as_variant {
        (
            fn_name $fn_name:ident
            fn_vis $fn_vis:tt
            fn_args ()
            fn_body_or_semi ;
            {
                variants {$($var:tt)*}
                $common_info_for_each_var:tt
            }
        ) => {
            $(
                $crate::__define_known_variants_auto_fn_as_variant_one! {
                    $var
                    fn_vis $fn_vis
                    fn_name $fn_name
                    $common_info_for_each_var
                }
            )*
        };
    }

    pub use {
        __define_known_variants_auto_fn_as_variant as as_variant,
        __define_known_variants_auto_fn_from_variant as from_variant,
        __define_known_variants_auto_fn_into_variant as into_variant,
    };
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    mod with_lifetimes {
        define_known_variants!(
            enum KnownAsSlice<'a, T, const N: usize> {
                Slice(&'a [T]),
                Array([T; N]),
            }

            trait IsKnownAsSlice {}

            fn from_variant();
        );

        impl<'a, V: IsKnownAsSlice<'a, T, N>, T, const N: usize> KnownAsSlice<'a, V, T, N> {
            const fn as_slice(&self) -> &[T] {
                match self {
                    KnownAsSlice::Slice(_, this) => this,
                    KnownAsSlice::Array(_, this) => this,
                }
            }
        }

        #[test]
        const fn test() {
            let res = KnownAsSlice::<[u8; 0], _, 0>::from_variant([0; 0]);

            assert!(matches!(res.as_slice(), []));

            let res = KnownAsSlice::<&[_], _, 0>::from_variant(&[1, 2, 3]);

            assert!(matches!(res.as_slice(), [1, 2, 3]));
        }
    }

    define_known_variants!(
        enum KnownAddU8 {
            U8(u8),
            U16(u16),
        }

        trait IsKnownAddU8 {}

        fn from_variant();
        fn into_variant();
        fn as_variant();
    );

    impl<V: IsKnownAddU8> KnownAddU8<V> {
        const fn add_u8(self, other: u8) -> Self {
            match self {
                KnownAddU8::U8(yes_or_no, v) => KnownAddU8::U8(yes_or_no, v + other),
                KnownAddU8::U16(yes_or_no, v) => KnownAddU8::U16(yes_or_no, v + other as u16),
            }
        }
    }

    const TEST_U8: () = {
        let res = KnownAddU8::<u8>::from_variant(1).add_u8(1);
        assert!(*res.as_variant() == 2);
        assert!(res.into_variant() == 2);
    };

    const TEST_SIZE: () = {
        enum NeverVariants {
            _Never(std::convert::Infallible, u8),
        }

        // TODO: this should be 0
        assert!(size_of::<NeverVariants>() > 0);

        // TODO: this should eq
        assert!(size_of::<KnownAddU8<u8>>() > size_of::<u8>());
    };

    #[test]
    const fn test() {
        () = TEST_U8;
        () = TEST_SIZE;

        assert!(size_of::<KnownAddU8<u16>>() > size_of::<u16>());

        let res = KnownAddU8::<u16>::from_variant(2).add_u8(1);
        assert!(*res.as_variant() == 3);
        assert!(res.into_variant() == 3);
    }
}
