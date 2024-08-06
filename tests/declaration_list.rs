use std::borrow::Cow;

use util::{declaration_rule_list::DeclarationRule, error::MaybeError, TestSuite, TestSuites};

mod util;

type Expected<S> = MaybeError<DeclarationRule<S>, S>;

const TEST: &str = include_str!("../css-parsing-tests/declaration_list.json");

#[test]
fn test_all() {
    let test_suites =
        serde_json::from_str::<TestSuites<Cow<str>, Vec<Expected<Cow<str>>>>>(TEST).unwrap();
    for TestSuite { input, expected } in test_suites.0 {}
}

fn test_one(input: &str) {
    let input = ccss::token::stream::TokenStream::new(input);
    ccss::parse::declaration::Declaration
}
