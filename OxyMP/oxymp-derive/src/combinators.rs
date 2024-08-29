#![allow(dead_code)]

use std::collections::HashSet;
use std::rc::Rc;

pub type ParseResult<'a, Out> = Result<(&'a str, Out), ParseError<'a>>;

pub type Parser<'a, Out> = Rc<dyn Fn(&'a str) -> ParseResult<'a, Out> + 'a>;

#[derive(Debug, PartialEq)]
pub enum ParseErrorDetails {
    EOI,
    NotSatisfied,
    Unexpected { expected: String, found: String },
    ChoicesFailed,
}

#[derive(Debug, PartialEq)]
pub enum ParserKind {
    Char,
    Satisfies,
    Alpha,
    Digit,
    OneOf,
    NoneOf,
    Tag,
    FirstOf,
    Sequence,
    Terminated,
    Preceeded,
    Delimited,

    External(&'static str),
}

macro_rules! extend_trace {
    ($result:expr, $parser_kind:expr) => {
        $result.map_err(|mut e| {
            e.trace.push($parser_kind);
            e
        })
    };
}
macro_rules! change_not_satisfied {
    ($result:expr, $expected:expr, $input:expr) => {
        $result.map_err(|mut e| {
            if let ParseErrorDetails::NotSatisfied = e.details {
                e.details = ParseErrorDetails::Unexpected {
                    expected: $expected.to_string(),
                    found: $input[..1].to_string(),
                };
            }
            e
        })
    };
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub trace: Vec<ParserKind>,
    pub details: ParseErrorDetails,
    pub input: &'a str,
}

fn truncate(s: &str) -> String {
    let max_length = 10;

    if s.len() > max_length {
        format!("{}...", &s[..max_length])
    } else {
        s.into()
    }
}

pub fn char<'a>() -> Parser<'a, char> {
    Rc::new(|input| {
        let mut chars = input.chars();
        if let Some(first_char) = chars.next() {
            let remainder = &input[first_char.len_utf8()..];
            Ok((remainder, first_char))
        } else {
            Err(ParseError {
                trace: vec![ParserKind::Char],
                details: ParseErrorDetails::EOI,
                input,
            })
        }
    })
}

pub fn satisfies<'a>(predicate: impl Fn(char) -> bool + 'a) -> Parser<'a, char> {
    Rc::new(move |input| {
        extend_trace!(char()(input), ParserKind::Satisfies).map(
            |(remaining, ch)| match predicate(ch) {
                true => Ok((remaining, ch)),
                false => Err(ParseError {
                    trace: vec![ParserKind::Satisfies],
                    details: ParseErrorDetails::NotSatisfied,
                    input,
                }),
            },
        )?
    })
}

pub fn alpha<'a>() -> Parser<'a, char> {
    Rc::new(|input| {
        let p = satisfies(|c| c.is_alphabetic());

        change_not_satisfied!(
            extend_trace!(p(input), ParserKind::Alpha),
            "alphabetic",
            input
        )
    })
}

pub fn digit<'a>(radix: u32) -> Parser<'a, char> {
    Rc::new(move |input| {
        let p = satisfies(move |c| c.is_digit(radix));
        change_not_satisfied!(extend_trace!(p(input), ParserKind::Digit), "digit", input)
    })
}

pub fn one_of<'a>(list: &'a str) -> Parser<'a, char> {
    let set = Rc::new(list.chars().collect::<HashSet<char>>());

    Rc::new(move |input| {
        let set_clone = set.clone();
        let p = satisfies(move |c| set_clone.contains(&c));

        change_not_satisfied!(
            extend_trace!(p(input), ParserKind::OneOf),
            format!("one of: {}", set.iter().collect::<String>()),
            input
        )
    })
}

pub fn none_of<'a>(list: &'a str) -> Parser<'a, char> {
    let set = Rc::new(list.chars().collect::<HashSet<char>>());

    Rc::new(move |input| {
        let set_clone = set.clone();
        let p = satisfies(move |c| !set_clone.contains(&c));

        change_not_satisfied!(
            extend_trace!(p(input), ParserKind::NoneOf),
            format!("none of: {}", set.iter().collect::<String>()),
            input
        )
    })
}

pub fn tag<'a>(tag: &'a str) -> Parser<'a, &'a str> {
    Rc::new(move |input| {
        if input.len() < tag.len() {
            return Err(ParseError {
                trace: vec![ParserKind::Tag],
                details: ParseErrorDetails::EOI,
                input,
            });
        }

        let (to_match, rest) = input.split_at(tag.len());

        if *to_match == *tag {
            Ok((rest, to_match))
        } else {
            Err(ParseError {
                trace: vec![ParserKind::Tag],
                details: ParseErrorDetails::Unexpected {
                    expected: tag.to_string(),
                    found: to_match.to_string(),
                },
                input,
            })
        }
    })
}

pub fn first_of<'a, Out>(parsers: &'a [Parser<'a, Out>]) -> Parser<'a, Out> {
    Rc::new(move |input| {
        for choice in parsers {
            let result = choice(input);

            if result.is_ok() {
                return result;
            }
        }

        Err(ParseError {
            trace: vec![ParserKind::FirstOf],
            details: ParseErrorDetails::ChoicesFailed,
            input,
        })
    })
}

pub fn sequence<'a, Out>(choices: &'a [Parser<'a, Out>]) -> Parser<'a, Vec<Out>> {
    Rc::new(move |input| {
        let mut ret = Vec::new();
        let mut input = input;

        for choice in choices {
            let result = extend_trace!(choice(input), ParserKind::Sequence)?;
            input = result.0;
            ret.push(result.1);
        }

        Ok((input, ret))
    })
}

pub fn terminated<'a, Out1, Out2>(
    first: Parser<'a, Out1>,
    second: Parser<'a, Out2>,
) -> Parser<'a, Out1>
where
    Out1: 'a,
    Out2: 'a,
{
    Rc::new(move |input| {
        let ret = extend_trace!(first(input), ParserKind::Terminated)?;
        extend_trace!(second(input), ParserKind::Terminated)?;

        Ok(ret)
    })
}

pub fn preceeded<'a, Out1, Out2>(
    first: Parser<'a, Out1>,
    second: Parser<'a, Out2>,
) -> Parser<'a, Out2>
where
    Out1: 'a,
    Out2: 'a,
{
    Rc::new(move |input| {
        extend_trace!(first(input), ParserKind::Preceeded)?;
        let ret = extend_trace!(second(input), ParserKind::Preceeded)?;
        Ok(ret)
    })
}

pub fn delimited<'a, Out1, Out2, Out3>(
    left: Parser<'a, Out1>,
    middle: Parser<'a, Out2>,
    right: Parser<'a, Out3>,
) -> Parser<'a, Out2>
where
    Out1: 'a,
    Out2: 'a,
    Out3: 'a,
{
    preceeded(left, terminated(middle, right))
}

#[cfg(test)]
mod tests {
    use core::panic;

    use super::*;

    fn trace_ends_with_kind(trace: &Vec<ParserKind>, kind: ParserKind) {
        let last = trace
            .iter()
            .last()
            .expect("Trace must have at least 1 parser");
        assert_eq!(last, &kind);
    }

    mod char_tests {
        use super::*;

        #[test]
        fn success_single_char() {
            let parser = char();
            let input = "a";
            let value = parser(input).expect("value");

            assert_eq!(value, ("", 'a'));
        }

        #[test]
        fn success_multiple_chars() {
            let parser = char();
            let input = "abc";
            let value = parser(input).expect("value");

            assert_eq!(value, ("bc", 'a'));
        }

        #[test]
        fn utf8_success() {
            let parser = char();
            let input = "🙂bc";
            let value = parser(input).expect("value");

            assert_eq!(value, ("bc", '🙂'));
        }

        #[test]
        fn whitespace() {
            let parser = char();
            let input = " a";
            let value = parser(input).expect("value");

            assert_eq!(value, ("a", ' '));
        }

        #[test]
        fn empty_input() {
            let parser = char();
            let input = "";
            let error = parser(input).expect_err("error");

            trace_ends_with_kind(&error.trace, ParserKind::Char);
            assert_eq!(error.details, ParseErrorDetails::EOI);
            assert_eq!(error.input, input);
        }
    }

    mod satisfies_tests {
        use super::*;

        #[test]
        fn success() {
            let parser = satisfies(|ch| ch.is_ascii_digit());
            let input = "123";
            let value = parser(input).expect("value");

            assert_eq!(value, ("23", '1'));
        }

        #[test]
        fn fail() {
            let parser = satisfies(|ch| ch.is_ascii_digit());
            let input = "abc";
            let error = parser(input).expect_err("error");

            trace_ends_with_kind(&error.trace, ParserKind::Satisfies);
            assert_eq!(error.details, ParseErrorDetails::NotSatisfied);
            assert_eq!(error.input, input);
        }

        #[test]
        fn empty_input() {
            let parser = satisfies(|ch| ch.is_ascii_digit());
            let input = "";
            let error = parser(input).expect_err("error");

            trace_ends_with_kind(&error.trace, ParserKind::Satisfies);
            assert_eq!(error.details, ParseErrorDetails::EOI);
            assert_eq!(error.input, input);
        }

        #[test]
        fn utf8_success() {
            let parser = satisfies(|ch| ch == '🙂');
            let input = "🙂bc";
            let value = parser(input).expect("value");

            assert_eq!(value, ("bc", '🙂'));
        }
    }

    mod alpha_tests {
        use super::*;

        #[test]
        fn success_lowercase() {
            let parser = alpha();
            let input = "abc";
            let value = parser(input).expect("value");

            assert_eq!(value, ("bc", 'a'));
        }

        #[test]
        fn success_uppercase() {
            let parser = alpha();
            let input = "ABC";
            let value = parser(input).expect("value");

            assert_eq!(value, ("BC", 'A'));
        }

        #[test]
        fn success_non_ascii() {
            let parser = alpha();
            let input = "鈴木雅之";
            let value = parser(input).expect("value");

            assert_eq!(value, ("木雅之", '鈴'));
        }

        #[test]
        fn failure_non_alpha() {
            let parser = alpha();
            let input = "123";
            let error = parser(input).expect_err("error");

            trace_ends_with_kind(&error.trace, ParserKind::Alpha);
            assert_eq!(
                error.details,
                ParseErrorDetails::Unexpected {
                    expected: "alphabetic".to_string(),
                    found: "1".to_string(),
                }
            );
            assert_eq!(error.input, input);
        }

        #[test]
        fn empty_input() {
            let parser = alpha();
            let input = "";
            let error = parser(input).expect_err("error");

            trace_ends_with_kind(&error.trace, ParserKind::Alpha);
            assert_eq!(error.details, ParseErrorDetails::EOI);
            assert_eq!(error.input, input);
        }

        #[test]
        fn success_mixed_input() {
            let parser = alpha();
            let input = "a1b2c3";
            let value = parser(input).expect("value");

            assert_eq!(value, ("1b2c3", 'a'));
        }
    }

    mod digit_tests {
        use super::*;

        #[test]
        fn decimal() {
            let parser = digit(10);
            let input = "5abc";
            let value = parser(input).expect("value");

            assert_eq!(value, ("abc", '5'));
        }

        #[test]
        fn invalid_input() {
            let parser = digit(10);
            let input = "x123";
            let error = parser(input).expect_err("error");

            trace_ends_with_kind(&error.trace, ParserKind::Digit);
            assert_eq!(
                error.details,
                ParseErrorDetails::Unexpected {
                    expected: "digit".to_string(),
                    found: "x".to_string(),
                }
            );
            assert_eq!(error.input, input);
        }

        #[test]
        fn empty_input() {
            let parser = digit(10);
            let input = "";
            let error = parser(input).expect_err("error");

            trace_ends_with_kind(&error.trace, ParserKind::Digit);
            assert_eq!(error.details, ParseErrorDetails::EOI);
            assert_eq!(error.input, input);
        }

        #[test]
        fn hex() {
            let parser = digit(16);
            let input = "fF123";
            let value = parser(input).expect("value");

            assert_eq!(value, ("F123", 'f'));
        }

        #[test]
        fn invalid_digit_for_radix() {
            let parser = digit(8);
            let input = "89abc";
            let error = parser(input).expect_err("error");

            trace_ends_with_kind(&error.trace, ParserKind::Digit);
            assert_eq!(
                error.details,
                ParseErrorDetails::Unexpected {
                    expected: "digit".to_string(),
                    found: "8".to_string(),
                }
            );
            assert_eq!(error.input, input);
        }
    }
}
