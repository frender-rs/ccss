use std::borrow::Cow;

use ccss::{
    collections::array_vec::ArrayVec,
    parse::{
        component_value::ComponentValue,
        declaration::{Declaration, DeclarationParseListError},
    },
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
                        let s = d.value_as_slice();

                        s.iter()
                            .map(|v| {
                                util::component_value::ComponentValue::from_parsed(*v)
                                    .map_str(Cow::Borrowed)
                            })
                            .collect()
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

type List<'a> = ArrayVec<ComponentValue<'a>, 10>;

fn parse_one(input: &str) -> Result<Vec<Declaration<List>>, DeclarationParseListError> {
    Declaration::parse_list_from_str(input)
        .into_iter()
        .collect()
}
