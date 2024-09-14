# Changelog

## 0.1.0 (2024-09-14)


### ⚠ BREAKING CHANGES

* no default features for dependency konst
* changed ArrayVec::with_try_push
* remove const known macros
* fixed and changed many api
* remove IdentSequence::to_str and IdentToken::to_str because they are ambiguous when the string contains escaped code points
* TokenParseError::UnexpectedReverseSolidus
* make EscapedCodePoint::consume private
* changed ComponentValueParseList::try_collect_into_known
* new api for DeclarationParseList
* new api for DeclarationParseList
* new api for declaration value
* refactor and organize exports

### Features

* add ArrayVec::with_extend_from_slice, with_try_extend_from_slice, try_into_filled_array ([bc0fcff](https://github.com/frender-rs/ccss/commit/bc0fcffb45e846b6260d4e249c7712884af98b33))
* changed ArrayVec::with_try_push ([305a02a](https://github.com/frender-rs/ccss/commit/305a02aeca332463172926c161e4f82c3f55be56))
* changed ComponentValueParseList::try_collect_into_known ([001799a](https://github.com/frender-rs/ccss/commit/001799a4f9fb642cc8ab1b348273bfcc38e83cff))
* CollectNothing ([171e9a2](https://github.com/frender-rs/ccss/commit/171e9a22b4300aed9d356f5eb26ab7b5891f28d6))
* ComponentValue::parse_list_from_str ([896ad35](https://github.com/frender-rs/ccss/commit/896ad35f8b8d18d8d129e167cf850661ca36892d))
* const_known ([c14a6ec](https://github.com/frender-rs/ccss/commit/c14a6ec07c35a41e1dc0f1c2d0aad6fade3eda2d))
* Declaration::parse_list ([d131c88](https://github.com/frender-rs/ccss/commit/d131c887228b99fd6b231e48efd4be6aba3cef48))
* Declaration::parse_value_from_str ([2f64193](https://github.com/frender-rs/ccss/commit/2f64193babd5a0e4042842b8e64b820c24c18be0))
* Declaration::value ([07e0f10](https://github.com/frender-rs/ccss/commit/07e0f1092102b82b8c026936afbf3c34718e42b6))
* export collections ([eb3d0ed](https://github.com/frender-rs/ccss/commit/eb3d0edd06447a541c92823fc7a35405166f87d8))
* export DimensionToken fields ([ea5473a](https://github.com/frender-rs/ccss/commit/ea5473a36a3a6189731ceb870ee199db63a544e0))
* export KnownComponentValueList::as_known_parsed_value_list ([26266ec](https://github.com/frender-rs/ccss/commit/26266ecf75457130b232f68566cae37ecf3ad4ad))
* fixed and changed many api ([f59ea1a](https://github.com/frender-rs/ccss/commit/f59ea1a02a69b8a5068f1e9ec8c9ab4a57235023))
* Function::full_as_original_str, value_count, right_parenthesis. SimpleBlock::full_as_original_str ([c20df2e](https://github.com/frender-rs/ccss/commit/c20df2e446ce6b80507d0eba9d821d1099f5edc6))
* IdentToken.original_str ([664a2b4](https://github.com/frender-rs/ccss/commit/664a2b43071c47d92fd7f958dee52af5956d5d28))
* impl HasConstDummyValue for Declaration, DeclarationParseList::try_collect_into_known, KnownParsedValueList::as_array_vec, KnownParsedValueList::as_slice ([939f689](https://github.com/frender-rs/ccss/commit/939f689651e2632d09df9618aaf764758215d105))
* KnownCollection ([6f0a601](https://github.com/frender-rs/ccss/commit/6f0a6019d043a3ac203b6813e63964f8f966b858))
* KnownParsedValueList ([63a0ca5](https://github.com/frender-rs/ccss/commit/63a0ca5a33f6c61e11b34a3e778e37073077539e))
* KnownParsedValueList::count ([f250614](https://github.com/frender-rs/ccss/commit/f2506144e5353824e96a4f69c4bdbd77361535fd))
* LeadVec ([3791e3e](https://github.com/frender-rs/ccss/commit/3791e3e47b90c1ba62aff4b2a7206bfcffde8a23))
* LeadVec::is_empty ([9499e26](https://github.com/frender-rs/ccss/commit/9499e26915c99d3d8864226dafe9a8e4fecc63fa))
* make EscapedCodePoint::consume private ([79ca603](https://github.com/frender-rs/ccss/commit/79ca603c7ced6481dac09aae162d7749a526474d))
* make some fns const: Declaration::name, name_as_original_str, value_as_original_str, is_important ([a2d5074](https://github.com/frender-rs/ccss/commit/a2d50745be03ce30db5ada9d3a2be00de3a145eb))
* new api ([aca144a](https://github.com/frender-rs/ccss/commit/aca144acb125632cc9af173f3f219999c3d4f292))
* new api for declaration value ([6b34aa2](https://github.com/frender-rs/ccss/commit/6b34aa2b454edd202ebe056f8575e77f58cbb4ef))
* new api for DeclarationParseList ([eaf7c92](https://github.com/frender-rs/ccss/commit/eaf7c92348887afc6b953a690fe012c1259e6ebd))
* new api for DeclarationParseList ([070353f](https://github.com/frender-rs/ccss/commit/070353fdf9809be6a975b47fde766b55b5df5a00))
* no default features for dependency konst ([dc81a95](https://github.com/frender-rs/ccss/commit/dc81a9564d7f1ced21fcbbee2e79998ad02460f0))
* refactor and organize exports ([c9a1e36](https://github.com/frender-rs/ccss/commit/c9a1e362c7d733d2434f799135d6b97f7b1e481f))
* remove const known macros ([0b804e2](https://github.com/frender-rs/ccss/commit/0b804e2e9866b845fe7e49301f3c82f1d635b624))
* remove IdentSequence::to_str and IdentToken::to_str because they are ambiguous when the string contains escaped code points ([fbeb245](https://github.com/frender-rs/ccss/commit/fbeb245110e5998304252aed340df291b85370af))
* TokenParseError::UnexpectedReverseSolidus ([0396e7f](https://github.com/frender-rs/ccss/commit/0396e7f262377e460b4e396a80d93959a75b32a3))


### Bug Fixes

* better macro_rules! define_known_variants ([6a4568e](https://github.com/frender-rs/ccss/commit/6a4568edeb024158d84dd4040afe58b77aeaf26a))
* escaped ident is not properly parsed ([0396e7f](https://github.com/frender-rs/ccss/commit/0396e7f262377e460b4e396a80d93959a75b32a3))
* FilteredCharVec::fit_or_keep_first_n ([3c1e8b3](https://github.com/frender-rs/ccss/commit/3c1e8b31684ea5556db7422ebe4be87a8c211573))
* LeadVec::with_push_maybe_fake ([b644c96](https://github.com/frender-rs/ccss/commit/b644c9665dd6359aec4b193b35ac337c90d53aeb))
* macro now works with &lt;< and &gt;> ([402d583](https://github.com/frender-rs/ccss/commit/402d5837a0b28c856e57c8807ad57fd167a73a6e))
* NumericToken should not be empty ([9e70101](https://github.com/frender-rs/ccss/commit/9e70101e962f2724d19c42cc0b9cc11030d56de9))
* process error ([42eb07e](https://github.com/frender-rs/ccss/commit/42eb07e39e916bcd47f16479b63d2718e2503373))
* UnicodeRangeToken::consume ([f098136](https://github.com/frender-rs/ccss/commit/f098136b374d3b2e0d5b9d79f5180a3f36e3f4d0))
