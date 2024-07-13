use oxymp::RecursiveDescent;

fn match_number(matched: &str) -> i64 {
    return matched.parse().unwrap();
}

#[derive(RecursiveDescent)]
#[token("ParanLeft", "(")]
#[token("ParanRight", ")")]
#[token("NewLine", "\n")]
#[token("Plus", "+")]
#[token("Minus", "-")]
#[token("Number", i64, ("/-?[0-9]+/", match_number))]
#[grammar("expr -> Number (('+' | '-') expr)?")]
struct Foo;

fn main() {}
