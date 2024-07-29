use oxymp::RecursiveDescent;

fn match_number(matched: &str) -> i64 {
    matched.parse().unwrap()
}

fn match_ws(matched: &str) -> String {
    matched.to_string()
}

#[derive(RecursiveDescent)]
#[exact_token(name = "ParanLeft", pattern = "(")]
#[exact_token(name = "ParanRight", pattern = ")")]
#[exact_token(name = "NewLine", pattern = "\n")]
#[exact_token(name = "Plus", pattern = "+")]
#[exact_token(name = "Minus", pattern = "-")]
//#[regex_token(
//    name = "Number",
//    regex = r"-?[0-9]+",
//    transformer_fn = "match_number",
//    type = "i64"
//)]
#[ignore_pattern(regex = r"\s+")]
#[grammar = "expr -> Number (('+' | '-') expr)?"]
struct Parser;

fn main() {}
