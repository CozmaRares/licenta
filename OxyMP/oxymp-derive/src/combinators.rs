#![allow(dead_code)]

use std::collections::HashSet;
use std::rc::Rc;

pub type ParseResult<'a, Out> = Result<(&'a str, Out), ParseError<'a>>;

pub type Parser<'a, Out> = Rc<dyn Fn(&'a str) -> ParseResult<'a, Out> + 'a>;

#[derive(Debug)]
pub enum ParseErrorDetails {
    EOI,
    NotSatisfied,
    Unexpected { expected: String, found: String },
    ChoicesFailed,
}

#[derive(Debug)]
pub enum ParserKind {
    Char,
    Satisfies,
    Alpha,
    Digit,
    OneOf,
    NoneOf,
    Tag,
    Choice,
    Sequence,

    External(&'static str),
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
        char()(input)
            .map_err(|mut e| {
                e.trace.push(ParserKind::Satisfies);
                e
            })
            .map(|(remaining, ch)| match predicate(ch) {
                true => Ok((remaining, ch)),
                false => Err(ParseError {
                    trace: vec![ParserKind::Satisfies],
                    details: ParseErrorDetails::NotSatisfied,
                    input,
                }),
            })?
    })
}

pub fn alpha<'a>() -> Parser<'a, char> {
    Rc::new(|input| {
        satisfies(|c| c.is_alphabetic())(input).map_err(|mut e| {
            e.trace.push(ParserKind::Alpha);
            e.details = match e.details {
                ParseErrorDetails::NotSatisfied => ParseErrorDetails::Unexpected {
                    expected: "alphabetic".to_string(),
                    found: truncate(input),
                },
                _ => e.details,
            };
            e
        })
    })
}

pub fn digit<'a>(radix: u32) -> Parser<'a, char> {
    Rc::new(move |input| {
        satisfies(move |c| c.is_digit(radix))(input).map_err(|mut e| {
            e.trace.push(ParserKind::Digit);
            e.details = match e.details {
                ParseErrorDetails::NotSatisfied => ParseErrorDetails::Unexpected {
                    expected: "digit".to_string(),
                    found: truncate(input),
                },
                _ => e.details,
            };
            e
        })
    })
}

pub fn one_of<'a>(list: &'a str) -> Parser<'a, char> {
    let set = Rc::new(list.chars().collect::<HashSet<char>>());

    Rc::new(move |input| {
        let set_clone = set.clone();
        satisfies(move |c| set_clone.contains(&c))(input).map_err(|mut e| {
            e.trace.push(ParserKind::OneOf);
            e.details = match e.details {
                ParseErrorDetails::NotSatisfied => ParseErrorDetails::Unexpected {
                    expected: format!("one of: {}", set.iter().collect::<String>()),
                    found: truncate(input),
                },
                _ => e.details,
            };
            e
        })
    })
}

pub fn none_of<'a>(list: &'a str) -> Parser<'a, char> {
    let set = Rc::new(list.chars().collect::<HashSet<char>>());

    Rc::new(move |input| {
        let set_clone = set.clone();
        satisfies(move |c| !set_clone.contains(&c))(input).map_err(|mut e| {
            e.trace.push(ParserKind::OneOf);
            e.details = match e.details {
                ParseErrorDetails::NotSatisfied => ParseErrorDetails::Unexpected {
                    expected: format!("none of: {}", set.iter().collect::<String>()),
                    found: truncate(input),
                },
                _ => e.details,
            };
            e
        })
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

pub fn choice<'a, Out>(choices: &'a [Parser<'a, Out>]) -> Parser<'a, Out> {
    Rc::new(move |input| {
        for choice in choices {
            let result = choice(input);

            if result.is_ok() {
                return result;
            }
        }

        Err(ParseError {
            trace: vec![ParserKind::Choice],
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
            let result = choice(input).map_err(|mut e| {
                e.trace.push(ParserKind::Sequence);
                e
            })?;
            input = result.0;
            ret.push(result.1);
        }

        return Ok((input, ret));
    })
}
