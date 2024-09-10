#![cfg(feature = "alloc")]

use std::borrow::Cow;

use ccss::parse::declaration::DeclarationParseListError;
use util::{error::MaybeError, TestSuite, TestSuites};

pub mod util;

type Expected<S> = MaybeError<util::declaration::Declaration<S>, S>;

const TEST: &str = include_str!("../css-parsing-tests/one_declaration.json");

#[test]
fn test_all() {
    let test_suites =
        serde_json::from_str::<TestSuites<Cow<str>, Expected<Cow<str>>>>(TEST).unwrap();
    for TestSuite { input, expected } in test_suites.0 {
        if input.contains("/*") {
            // TODO: comments are not supported yet
            continue;
        }

        // skip tests that ends with ";"
        if input.contains(";") {
            eprintln!("Tests with semicolon are skipped: {input:?}");
            continue;
        }

        let res = parse_one(&input);

        match expected.result {
            Ok(mut expected) => {
                // trim whitespace tokens
                while expected.value.last().is_some_and(|v| v.is_whitespace()) {
                    _ = expected.value.pop().unwrap();
                }

                {
                    let start_whitespace_count =
                        expected.value.iter().try_fold(0, |count, item| {
                            if item.is_whitespace() {
                                Ok(count + 1)
                            } else {
                                Err(count)
                            }
                        });

                    let start_whitespace_count =
                        start_whitespace_count.unwrap_or_else(std::convert::identity);

                    expected.value.drain(..start_whitespace_count)
                };

                let Ok(Some(d)) = res else {
                    panic!(
                        "expect declaration: {expected:?}\nUnexpected: {res:?}\ninput: {input:?}"
                    )
                };

                assert_eq!(
                    util::declaration::Declaration::from_parsed(d),
                    expected,
                    "\ninput: {input:?}"
                );
            }
            Err(err) => match &*err.reason {
                "empty" => {
                    assert!(
                        matches!(res, Ok(None)),
                        "Expect empty declaration\nUnexpected: {res:?}\ninput: {input:?}"
                    )
                }
                "invalid" => {
                    assert!(
                        matches!(res, Err(_)),
                        "Expect error declaration\nUnexpected: {res:?}\ninput: {input:?}"
                    )
                }
                reason => {
                    panic!("unknown error reason: {reason}\ninput: {input:?}")
                }
            },
        }
    }
}

type Parsed<'a> = util::declaration::Parsed<'a, 10>;

const fn parse_one(input: &str) -> Result<Option<Parsed>, DeclarationParseListError> {
    let parse_list = Parsed::parse_list_from_str(input);
    let (v, parse_list) = match parse_list.try_into_next() {
        Ok(v) => v,
        Err(err) => return Err(err),
    };

    assert!(matches!(parse_list.try_into_next(), Ok((None, _))));

    Ok(v)
}
