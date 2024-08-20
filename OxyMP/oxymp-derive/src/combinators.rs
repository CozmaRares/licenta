#![allow(dead_code)]

use std::collections::HashSet;
use std::rc::Rc;

pub type ParseResult<'a, Out> = Result<(&'a str, Out), ParseError>;

pub type Parser<'a, Out> = Rc<dyn Fn(&'a str) -> ParseResult<'a, Out> + 'a>;

#[derive(Debug)]
pub enum ParseErrorDetails {
    EOI,
    NotSatisfied,
    Unexpected { expected: String, found: String },
}

#[derive(Debug)]
pub enum ParserKind {
    Char,
    Satisfy,
    Alpha,
    Digit,
    OneOf,
    NoneOf,
    Tag,
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParserKind,
    pub details: ParseErrorDetails,
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
                kind: ParserKind::Char,
                details: ParseErrorDetails::EOI,
            })
        }
    })
}

pub fn satisfies<'a>(predicate: impl Fn(char) -> bool + 'a) -> Parser<'a, char> {
    Rc::new(move |input| {
        char()(input).map(|(remaining, ch)| match predicate(ch) {
            true => Ok((remaining, ch)),
            false => Err(ParseError {
                kind: ParserKind::Satisfy,
                details: ParseErrorDetails::NotSatisfied,
            }),
        })?
    })
}

pub fn alpha<'a>() -> Parser<'a, char> {
    Rc::new(|input| {
        satisfies(|c| c.is_alphabetic())(input).map_err(|e| ParseError {
            kind: ParserKind::Alpha,
            details: match e.details {
                ParseErrorDetails::NotSatisfied => ParseErrorDetails::Unexpected {
                    expected: "alphabetic".to_string(),
                    found: truncate(input),
                },
                _ => e.details,
            },
        })
    })
}

pub fn digit<'a>(radix: u32) -> Parser<'a, char> {
    Rc::new(move |input| {
        satisfies(move |c| c.is_digit(radix))(input).map_err(|e| ParseError {
            kind: ParserKind::Digit,
            details: match e.details {
                ParseErrorDetails::NotSatisfied => ParseErrorDetails::Unexpected {
                    expected: "digit".to_string(),
                    found: truncate(input),
                },
                _ => e.details,
            },
        })
    })
}

pub fn one_of<'a>(list: &'a str) -> Parser<'a, char> {
    let set = Rc::new(list.chars().collect::<HashSet<char>>());

    Rc::new(move |input| {
        let set_clone = set.clone();
        satisfies(move |c| set_clone.contains(&c))(input).map_err(|e| ParseError {
            kind: ParserKind::OneOf,
            details: match e.details {
                ParseErrorDetails::NotSatisfied => ParseErrorDetails::Unexpected {
                    expected: format!("one of: {}", set.iter().collect::<String>()),
                    found: truncate(input),
                },
                _ => e.details,
            },
        })
    })
}

pub fn none_of<'a>(list: &'a str) -> Parser<'a, char> {
    let set = Rc::new(list.chars().collect::<HashSet<char>>());

    Rc::new(move |input| {
        let set_clone = set.clone();
        satisfies(move |c| !set_clone.contains(&c))(input).map_err(|e| ParseError {
            kind: ParserKind::NoneOf,
            details: match e.details {
                ParseErrorDetails::NotSatisfied => ParseErrorDetails::Unexpected {
                    expected: format!("none of: {}", set.iter().collect::<String>()),
                    found: truncate(input),
                },
                _ => e.details,
            },
        })
    })
}

pub fn tag<'a>(tag: &'a str) -> Parser<'a, &'a str> {
    Rc::new(move |input| {
        if input.len() < tag.len() {
            return Err(ParseError {
                kind: ParserKind::Tag,
                details: ParseErrorDetails::EOI,
            });
        }

        let (to_match, rest) = input.split_at(tag.len());

        if *to_match == *tag {
            Ok((rest, to_match))
        } else {
            Err(ParseError {
                kind: ParserKind::Tag,
                details: ParseErrorDetails::Unexpected {
                    expected: tag.to_string(),
                    found: to_match.to_string(),
                },
            })
        }
    })
}
