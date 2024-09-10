#![cfg(feature = "alloc")]

use std::{borrow::Cow, collections::HashMap};

use ccss::{
    collections::array_vec::ArrayVec,
    parse::component_value::{ComponentValue as ParsedComponentValue, ListParseNotNestedError},
};
use util::{
    component_value::ComponentValue,
    error::{Error, MaybeError},
    TestSuite, TestSuites,
};

pub mod util;

type Expected<S> = MaybeError<ComponentValue<S>, S>;

const TEST: &str = include_str!("../css-parsing-tests/component_value_list.json");

const ADDITIONAL_TEST: &str = r##"[
"red -->", [
	["ident", "red"], " ", "-->"
],
"red-->", [
	["ident", "red--"], ">"
]
]"##;

const TESTS_AS_ERROR: &[&str] = &[
    // trailing '\'
    "\\30red \\00030 red \\30\r\nred \\0000000red \\1100000red \\red \\r ed \\.red \\ red \\\nred \\376\\37 6\\000376\\0000376\\",
    "@media0 @-Media @--media @-\\-media @0media @-0media @_media @.media @medİa @\\30 media\\",
    "#red0 #-Red #--red #-\\-red #0red #-0red #_Red #.red #rêd #êrd #\\.red\\",
    // Function ended with EOF is not allowed in ccss
    "URL(foo) Url(foo) ûrl(foo) url (foo) url\\ (foo) url(\t 'foo' ",
];

#[test]
fn test_all() {
    let test_suites = serde_json::from_str::<TestSuites<Cow<str>, Vec<Expected<Cow<str>>>>>(TEST)
        .unwrap()
        .with_append(serde_json::from_str(ADDITIONAL_TEST).unwrap());

    let mut tests_as_error =
        HashMap::<&str, bool>::from_iter(TESTS_AS_ERROR.iter().map(|v| (*v, false)));

    for TestSuite { input, expected } in test_suites.0 {
        if input.contains("/*") && input.contains("*/") {
            // TODO: comments are not supported yet
            continue;
        }

        let mut expected = expected
            .into_iter()
            .map(|e| e.result?.confirm_ok())
            .collect::<Result<Vec<_>, _>>();

        let res = parse_one(&input);

        if expected.is_ok() {
            if let Some(tested) = tests_as_error.get_mut(input.as_ref()) {
                assert!(!*tested, "duplicate input: {input:?}");
                *tested = true;
                expected = Err(Error {
                    reason: "force-test-as-error".into(),
                });
            }
        }

        match expected {
            Err(_) => {
                match res {
                    Ok(res) => panic!("unexpected success: {res:?}\nfor input: {input:?}"),
                    Err(_) => {}
                }
                // TODO: test errors
            }
            Ok(mut expected) => {
                {
                    // specialize this test
                    const SPECIAL_IDENT: &str = "\u{0080}\u{0081}";
                    if input.ends_with(SPECIAL_IDENT) {
                        assert_eq!(
                            expected.pop().unwrap(),
                            ComponentValue::Typed("ident".into(), SPECIAL_IDENT.into())
                        );
                        SPECIAL_IDENT
                            .chars()
                            .for_each(|ch| expected.push(ComponentValue::Delim(ch)));
                    }
                }

                if expected
                    .iter()
                    .any(|v| matches!(v, ComponentValue::UnicodeRange(_)))
                {
                    // TODO: unicode ranges are not supported yet
                    continue;
                }

                let d = res.unwrap_or_else(|err| {
                    panic!("unexpected error: {err:?}\nfor input: {input:?}")
                });

                let expected = ComponentValue::normalize_list(expected);

                let d = d
                    .as_slice()
                    .iter()
                    .map(|cv| ComponentValue::from_parsed(*cv))
                    .collect::<Vec<_>>();

                assert_eq!(d, expected, "\ninput: {input:?}");
            }
        }
    }

    let untested_as_error = tests_as_error
        .iter()
        .filter_map(|(v, tested)| (!tested).then_some(v))
        .collect::<Vec<_>>();

    if !untested_as_error.is_empty() {
        panic!("test_as_error not found in test suites: {untested_as_error:#?}")
    }
}

const ARRAY_VEC_CAP: usize = 60;

type ComponentValueList<'a> = ArrayVec<ParsedComponentValue<'a>, ARRAY_VEC_CAP>;

const fn parse_one(input: &str) -> Result<ComponentValueList, ListParseNotNestedError> {
    match ParsedComponentValue::parse_list_from_str(input)
        .try_collect_into_known::<ComponentValueList, ARRAY_VEC_CAP, 0>()
    {
        Ok(res) => Ok(*res.as_known_parsed_value_list().as_array_vec()),
        Err((_, err)) => Err(err),
    }
}
