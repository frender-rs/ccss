#![cfg(feature = "alloc")]

use std::borrow::Cow;

use ccss::{
    collections::{array_vec::ArrayVec, parsed_value_list::KnownParsedValueList},
    parse::{
        component_value::ComponentValue,
        declaration::{Declaration, DeclarationParseListError},
    },
};
use util::{declaration_rule_list::DeclarationRule, error::MaybeError, TestSuite, TestSuites};

pub mod util;

type Expected<S> = MaybeError<DeclarationRule<S>, S>;

const TEST: &str = include_str!("../css-parsing-tests/declaration_list.json");

#[test]
fn test_all() {
    let test_suites =
        serde_json::from_str::<TestSuites<Cow<str>, Vec<Expected<Cow<str>>>>>(TEST).unwrap();
    for TestSuite { input, expected } in test_suites.0 {
        if input.contains("/**/") {
            // TODO: comments are not supported yet
            continue;
        }

        if expected
            .iter()
            .any(|v| v.result.as_ref().is_ok_and(|v| v.is_at_rule()))
        {
            // TODO: at-rules are not supported yet
            continue;
        }

        let is_err = expected.iter().any(|e| e.result.is_err());
        let res = parse_one(&input);

        if is_err {
            res.unwrap_err();
            // TODO: test errors
        } else {
            let d = res.unwrap();
            let d = d
                .as_slice()
                .iter()
                .map(|d| util::declaration::Declaration::from_parsed(*d))
                .map(DeclarationRule::Declaration)
                .map(Ok)
                .map(|result| MaybeError { result })
                .collect::<Vec<_>>();

            assert_eq!(d, expected);
        }
    }
}

type ComponentValueList<'a> = ArrayVec<ComponentValue<'a>, 10>;
type DeclarationList<'a> = ArrayVec<Declaration<'a, ComponentValueList<'a>>, 10>;

const fn parse_one(
    input: &str,
) -> Result<
    KnownParsedValueList<DeclarationList, Declaration<ComponentValueList>>,
    DeclarationParseListError,
> {
    match Declaration::parse_list_from_str(input).try_collect_into_known::<DeclarationList, 10, 0>()
    {
        Ok(res) => Ok(res),
        Err((_, err)) => Err(err),
    }
}
