use std::borrow::Cow;

use ccss::parse::declaration::{Declaration, DeclarationParseError, ParseEndReason};
use util::{declaration_rule_list::DeclarationRule, error::MaybeError, TestSuite, TestSuites};

mod util;

type Expected<S> = MaybeError<DeclarationRule<S>, S>;

const TEST: &str = include_str!("../css-parsing-tests/declaration_list.json");

#[test]
fn one() {
    parse_one("a:b; c:d 42!important;\n").unwrap();
}

#[test]
fn test_all() {
    let test_suites =
        serde_json::from_str::<TestSuites<Cow<str>, Vec<Expected<Cow<str>>>>>(TEST).unwrap();
    for TestSuite { input, expected } in test_suites.0 {
        if input.contains("/**/") {
            // TODO: comments are not supported yet
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

fn parse_one(input: &str) -> Result<Vec<Declaration>, DeclarationParseError> {
    let input = ccss::token::stream::TokenStream::new(input);

    let mut input = input.try_process().unwrap();

    let mut out = vec![];

    loop {
        let res = Declaration::try_consume_next(input);
        let (d, reason) = match res {
            Ok(v) => v,
            Err(err) => match err {
                DeclarationParseError::UnexpectedToken(t)
                    if t.expect.is_ident()
                        && t.buffered_input.tokens_and_remaining().is_empty() =>
                {
                    return Ok(out);
                }
                err => return Err(err),
            },
        };

        match reason {
            ParseEndReason::NextIsStopToken(t) => input = t.remaining.try_buffer_n().unwrap(),
            ParseEndReason::Eof => {
                out.push(d);
                return Ok(out);
            }
        }
    }
}
