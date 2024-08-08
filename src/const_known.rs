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

#[macro_export]
macro_rules! define_known_variants {
    (
        $enum_vis:vis enum $KnownVariant:ident $($generics_and_rest:tt)*
    ) => {
        $crate::const_known::__private::parse_generics! {
            [
                enum { $enum_vis $KnownVariant }
            ]
            {$($generics_and_rest)*}
            => $crate::__define_known_variants_after_parse_generics!
        }
    };
}

#[doc(hidden)]
pub mod __private {
    pub use ::syn_lite::parse_generics;
    pub use core::stringify;

    #[macro_export]
    macro_rules! __define_known_variants_after_parse_generics {
        (
            enum { $enum_vis:vis $KnownVariant:ident }
            generics! {
                params! { $($params:tt)* }
                impl_generics! $impl_generics:tt
                type_generics! { $($type_generics:tt)* }
                params_name! { $($params_name:tt)* }
            }
            rest! {
                {
                    $(
                        $VarName:ident ($VarType:ty)
                    ),*
                    $(,)?
                }

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
            $enum_vis enum $KnownVariant<
                __Variant: $IsKnownVariant<$($params_name)*>,
                $($params)*
            > {
                $(
                    $VarName(__Variant::$VarName, $VarType),
                )*
            }

            $trait_vis trait $IsKnownVariant<$($params)*> {
                $(
                    #[doc = " Indicates whether `Self` may be [`"]
                    #[doc = $crate::const_known::__private::stringify!($VarType)]
                    #[doc = "`]"]
                    type $VarName: $crate::const_known::YesOrNo;
                )*
            }

            const _: () = {
                macro_rules! __ident_match {
                    $(
                        ($VarName $VarName) => {
                            $crate::const_known::Yes
                        };
                    )*
                    ($_0:ident $_1:ident ) => {
                        $crate::const_known::No
                    };
                }

                $crate::__define_known_variants_impl_for_known_variants! {
                    impl_generics $impl_generics
                    trait { $IsKnownVariant<$($params_name)*> }
                    [$(
                        { $VarName($VarType) }
                    )*]
                    [$(
                        $VarName
                    )*]
                }
            };

            $crate::__define_known_variants_impl_auto_fns! {
                {$($rest)*}
                {
                    variants {$(
                        { $VarName($VarType) }
                    )*}
                    {
                        impl_generics $impl_generics
                        Self { $KnownVariant }
                        params_name { $($params_name)* }
                        variant_names {$(
                            $VarName
                        )*}
                    }
                }
            }
        };
    }

    #[macro_export]
    macro_rules! __define_known_variants_impl_for_known_variant {
        (
            impl_generics { $($impl_generics:tt)* }
            trait { $trait_path:path }
            { $VarName:ident ($VarType:ty) }
            [$($variant_name:ident)*]
        ) => {
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
            [$($var_name_and_type:tt)*]
            $variant_names:tt
        ) => {
            $(
                $crate::__define_known_variants_impl_for_known_variant! {
                    impl_generics $impl_generics
                    trait $trait_path
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
                params_name { $($params_name:tt)* }
                variant_names {$(
                    $AllVarName:ident
                )*}
            }
        ) => {
            impl<$($impl_generics)*> $SelfName<$VarType, $($params_name)*> {
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
                params_name { $($params_name:tt)* }
                variant_names {$(
                    $AllVarName:ident
                )*}
            }
        ) => {
            impl<$($impl_generics)*> $SelfName<$VarType, $($params_name)*> {
                $fn_vis const fn $fn_name(self) -> $VarType {
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
                params_name { $($params_name:tt)* }
                variant_names {$(
                    $AllVarName:ident
                )*}
            }
        ) => {
            impl<$($impl_generics)*> $SelfName<$VarType, $($params_name)*> {
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

    #[test]
    const fn test() {
        () = TEST_U8;
        let res = KnownAddU8::<u16>::from_variant(2).add_u8(1);
        assert!(*res.as_variant() == 3);
        assert!(res.into_variant() == 3);
    }
}
