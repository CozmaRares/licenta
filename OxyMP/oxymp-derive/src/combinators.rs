#![allow(dead_code)]

use std::collections::HashSet;

pub type ParseResult<'a, Out> = Result<(&'a str, Out), ParseError<'a>>;

#[derive(Debug)]
pub enum ParseErrorDetails<'a> {
    EOI,
    None,
    Unexpected { expected: &'a str, found: &'a str },
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
pub struct ParseError<'a> {
    pub kind: ParserKind,
    pub details: ParseErrorDetails<'a>,
}

fn truncate(s: &str) -> String {
    let max_length = 10;

    if s.len() > max_length {
        format!("{}...", &s[..max_length])
    } else {
        s.into()
    }
}

pub fn char<'a>() -> impl Fn(&'a str) -> ParseResult<'a, char> {
    |input| {
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
    }
}

pub fn satisfies<'a>(
    predicate: impl Fn(char) -> bool,
) -> impl Fn(&'a str) -> ParseResult<'a, char> {
    move |input| {
        char()(input).map(|(remaining, ch)| match predicate(ch) {
            true => Ok((remaining, ch)),
            false => Err(ParseError {
                kind: ParserKind::Satisfy,
                details: ParseErrorDetails::None,
            }),
        })?
    }
}

pub fn alpha<'a>() -> impl Fn(&'a str) -> ParseResult<'a, char> {
    |input| {
        satisfies(|c| c.is_alphabetic())(input).map_err(|e| ParseError {
            kind: ParserKind::Alpha,
            details: e.details,
        })
    }
}

pub fn digit<'a>(radix: u32) -> impl Fn(&'a str) -> ParseResult<'a, char> {
    move |input| {
        satisfies(|c| c.is_digit(radix))(input).map_err(|e| ParseError {
            kind: ParserKind::Digit,
            details: e.details,
        })
    }
}

pub fn one_of<'a>(list: &'a str) -> impl Fn(&'a str) -> ParseResult<'a, char> {
    let list = list.chars().collect::<HashSet<char>>();

    move |input| {
        satisfies(|c| list.contains(&c))(input).map_err(|e| ParseError {
            kind: ParserKind::OneOf,
            details: e.details,
        })
    }
}

pub fn none_of<'a>(list: &'a str) -> impl Fn(&'a str) -> ParseResult<'a, char> {
    let list = list.chars().collect::<HashSet<char>>();

    move |input| {
        satisfies(|c| !list.contains(&c))(input).map_err(|e| ParseError {
            kind: ParserKind::NoneOf,
            details: e.details,
        })
    }
}

pub fn tag<'a>(tag: &'a str) -> impl Fn(&'a str) -> ParseResult<'a, &'a str> {
    move |input| {
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
                    expected: tag,
                    found: to_match,
                },
            })
        }
    }
}
