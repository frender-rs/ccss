use std::borrow::Cow;

use ccss::{
    parse::{
        component_value::ComponentValue,
        declaration::{Declaration, DeclarationParseListError, ParseEndReason},
    },
    token::stream::TokenStream,
};
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
            let d = d
                .into_iter()
                .map(|d| util::declaration::Declaration {
                    name: d.name_as_str().into(),
                    value: {
                        // TODO: prevent re-parse
                        let s = d.value_as_str();
                        let mut list =
                            ComponentValue::try_consume_list(TokenStream::new(s)).unwrap();

                        let mut res = Vec::new();
                        while let Some((v, new_list)) = list.try_next().unwrap() {
                            list = new_list;
                            res.push(
                                util::component_value::ComponentValue::from_parsed(v)
                                    .map_str(Cow::Borrowed),
                            );
                        }

                        res
                    },
                    important: d.is_important(),
                })
                .map(DeclarationRule::Declaration)
                .map(Ok)
                .map(|result| MaybeError { result })
                .collect::<Vec<_>>();

            assert_eq!(d, expected);
        }
    }
}

fn parse_one(input: &str) -> Result<Vec<Declaration>, DeclarationParseListError> {
    Declaration::parse_list_from_str(input).collect()
}
