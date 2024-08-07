use std::borrow::Cow;

use ccss::parse::declaration::{Declaration, DeclarationParseListError, ParseEndReason};
use util::{declaration_rule_list::DeclarationRule, error::MaybeError, TestSuite, TestSuites};

mod util;

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
        }
    }
}

fn parse_one(input: &str) -> Result<Vec<Declaration>, DeclarationParseListError> {
    Declaration::parse_list_from_str(input).collect()
}
