pub type ParseResult<'a, Out> = Result<(&'a str, Out), ParseError<'a>>;

#[derive(Debug)]
pub enum ParseErrorDetails<'a> {
    EOI,
    Unexpected { expected: &'a str, found: &'a str },
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub context: String,
    pub details: ParseErrorDetails<'a>,
}

type Parser<'a, Out> = dyn Fn(&'a str) -> ParseResult<'a, Out>;

fn truncate(s: &str) -> String {
    let max_length = 10;

    if s.len() > max_length {
        format!("{}...", &s[..max_length])
    } else {
        s.into()
    }
}
