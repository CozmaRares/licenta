// In Rust, macro-urile se comporta diferit de cele din C.
// Aici, la compilare, am acces la token-urile din cod si
// pot sa le modific si sa adaug altele. 

// defnitia generala pentru in parser
// in principiu, o sa fie mai multe tipuri de parsere precum LR, LL, etc.
#[derive(RecursiveDescent)]

// definitia pentru un token
#[token("ParanLeft", '(')]
#[token("ParanRight", ')')]
#[token("NewLine", '\n')]
#[token("Plus", '+')]
#[token("Minus", '-')]

// pentru un token ce se foloseste de regex
// trebuie o functie care extrage valoarea din string
#[token(
    "Number",
    "<i64>", // tipul intern o sa fie numar intreg pe 64 biti, similar cu yylval.int_val
    (
        "/-?[0-9]+/", // expresia regex trebuie sa existe daca tokenul primeste un tip generic, intre <...>
        |matched| matched.parse::<i64>().unwrap() // functie lambda care extrage numarul din string-ul corespunzator expresiei regex
    )
)]

// definitia pentru sintaxa parser-ului
// o expresie este formata din 2 grupuri:
//   - un numar
//   - optinal, un semn (+ sau -) si o alta expresie
#[grammar("expr -> Number (('+' | '-') expr)?")]

// Z = expr
#[start("expr")]
pub struct Parser;
// pub este pentru public

// parser-ul in sine o sa fie o structura fara stare interna,
// si toate metodele sale o sa fie statice
// dar, user-ul poate extinde parserul cu ce ii mai trebuie 

// ===========================================================
// totul de mai jos este codul care va fi generat  

// pentru type safety, trebuie sa existe 2 definitii pentru token-uri  

// definitiile concrete
pub mod token {
    pub struct Null;
    pub struct NewLine;
    pub struct Plus;
    pub struct Minus;
    pub struct Number(pub i64);
}

// definitia care agregeaza celelalte definitii intr-un singur tip
pub enum Token {
    Null(token::Null),
    NewLine(token::NewLine),
    Plus(token::Plus),
    Minus(token::Minus),
    Number(token::Number),
}

// reprezentarea in cod a:
// ('+' | '-')
// din syntaxa unei expresii 
pub enum ExprChoice1 {
    _1(token::Plus),
    _2(token::Minus),
}

// o expresie si cele 2 grupui din care este alcatuita
pub struct Expr {
    pub _1: token::Number,
    pub _2: Option<(ExprChoice1, Box<Expr>)>,
}

// Option este un tip builtin din Rust care poate contine o valoare (Some x), sau nu (None)

// definitia arborelui abstract de sintaxa
// aici vor fi toate definitiile nodurilor agregate
pub enum AST {
    Expr(Expr),
}

// pentru ca parser-ul nu are stare interna, este nevoie de o structura externa
pub struct ParserState<'a> {
    pub toks: &'a [token::Token],
    pub current_token: usize, 
}

// definitia metodelor
impl<'a> ParserState<'a> {
    pub fn new(tokens: &'a Vec<token::Token>) -> Self { todo!() }
    pub fn current(&self) -> Option<&token::Token> { todo!() }
    pub fn next(&self) -> Option<&token::Token> { todo!() }
    pub fn increment(&self) -> Self { todo!() }
}
// 'self' insemana obiectul pe care este aplata metoda
// 'Self' este tipul, in acest caz ParserState

// o structura care va fi responsabila de reportarea erorilor produse in timpul parsarii
pub struct ParseError {}

// definitia metodelor pentru parser
impl Parser {
    // o metoda este echivalenta cu un apel al macro-ului grammar, precum:
    // #[grammar("expr -> Number (('+' | '-') expr)?")]
    //
    // metoda va returna fie:
    //    - o pereche cu arborele de sintaxa pentru creat si noua stare a parser-ului 
    //    - o eroare

    // numele va fi extras din gramatica
    pub fn expr(state: ParserState) -> Result<(AST, ParserState), ParseError> {
        // verifica daca token-ul curent este numar
        let _1 = match Token::to_number(state.current()) {
            Some(x) => x, // daca este, atunci ii da valoare interna lui '_1'
            None => return Err(ParseError {}),
        };
        let state = state.increment();

        // verifica al doilea grup
        let choice = match state.current() {
            Some(token::Plus(_)) => ExprChoice1::_1(token::Plus),
            Some(token::Minus(_)) => ExprChoice1::_2(token::Minus),
            _ => {
                // daca este orice alt token in afara de + sau -, atunci expresia este doar un numar
                return Ok((
                    AST::Expr(Expr {
                        _1: _1.clone(),
                        _2: None,
                    }),
                    state,
                ))
            }
        };
        let state = state.increment();

        let (expr, state) = Parser::expr(state)?;

        let expr = match AST::to_expr(expr) {
            Some(x) => x,
            None => return Err(ParseError {}),
        };

        return Ok((
            AST::Expr(Expr {
                _1: _1.clone(),
                _2: Some((choice, Box::new(r1))),
            }),
            state,
        ));
    }

    pub fn parse(tokens: &Vec<token::Token>) -> Result<AST, ParseError> {
        let state = ParserState::new(tokens);
        Parser::parse_state(state).map(|(ast, _)| ast)
    }

    // in caz ca user-ul deja are o stare pe care vrea sa o folosesca in continuare
    pub fn parse_state(state: ParserState) -> Result<(AST, ParserState), ParseError> {
        Parser::expr(state)
    }
}

// lexer-ul va fi auto generat
// un lexer este compus din mai multe reguil care au:
//    - matcher: char, string, sau regex
//    - handler: token, functie lambda care creeaza un token dinamic  
pub fn make_lexer() -> Lexer<Token> {
    let rules = vec![
        TokenRule {
            matcher: Matcher::Char('\n'),
            handler: Handler::Token(Token::new_line()),
        },
        TokenRule {
            matcher: Matcher::Char('+'),
            handler: Handler::Token(Token::plus()),
        },
        TokenRule {
            matcher: Matcher::Char('-'),
            handler: Handler::Token(Token::minus()),
        },
        TokenRule {
            matcher: Matcher::regex("-?[0-9]+").unwrap(),
            handler: Handler::Lambda(Box::new(|matched| Token::number(matched.parse().unwrap()))),
        },
    ];

    Lexer::new(rules)
}

