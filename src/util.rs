use combine::char::{char, digit, string};
use combine::error::{ParseError, ParseResult};
use combine::parser::combinator::recognize;
use combine::{
    choice, eof, from_str, look_ahead, many, many1, none_of, not_followed_by, one_of, optional,
    parser, skip_many, skip_many1, token, Parser, Stream,
};

pub(crate) fn symbol<I>(name: &'static str) -> impl Parser<Input = I, Output = &'static str>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex(symbol_(name))
}

fn symbol_<I>(name: &'static str) -> impl Parser<Input = I, Output = &'static str>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string(name)
        .and(look_ahead(
            one_of(" \t\n\\[]{}".chars()).or(eof().map(|_| ' ')),
        ))
        .map(|(x, _)| x)
}

pub(crate) fn float<I>() -> impl Parser<Input = I, Output = f64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex(float_())
}

fn float_<I>() -> impl Parser<Input = I, Output = f64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    from_str(recognize::<String, _>((
        optional(token('-')),
        (token('.').and(skip_many1(digit())).map(|_| '0')).or((
            token('0').skip(not_followed_by(digit())).or((
                one_of("123456789".chars()),
                skip_many(digit()),
            )
                .map(|_| '0')),
            optional((token('.'), skip_many(digit()))),
        )
            .map(|_| '0')),
        optional((
            (one_of("eE".chars()), optional(one_of("+-".chars()))),
            skip_many1(digit()),
        )),
    )))
    .expected("float")
}

pub(crate) fn item<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex(item_())
}

fn item_<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    item_literal()
        .or(item_literal_braces())
        .or(item_head()
            .and(many(parser(item_body)))
            .map(|(x, y): (_, Vec<String>)| {
                let mut ret = String::new();
                ret.push(x);
                for s in y {
                    ret.push_str(&s);
                }
                ret
            }))
}

fn item_literal<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between('\"', '\"', many(none_of("\"".chars())))
}

fn item_literal_braces<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between('{', '}', many(none_of("}".chars())))
}

fn item_head<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    none_of(" -{}[]\r\n\t".chars())
}

fn item_body<I>(input: &mut I) -> ParseResult<String, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(none_of(" -{}[]\r\n\t".chars()))
        .or(combine::between(char('['), char(']'), parser(item_body)).map(|x| format!("[{}]", x)))
        .parse_stream(input)
}

pub(crate) fn braces<I, O>(
    parser: impl Parser<Input = I, Output = O>,
) -> impl Parser<Input = I, Output = O>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between('{', '}', parser)
}

pub(crate) fn brackets<I, O>(
    parser: impl Parser<Input = I, Output = O>,
) -> impl Parser<Input = I, Output = O>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between('[', ']', parser)
}

fn between<I, O>(
    start: char,
    end: char,
    parser: impl Parser<Input = I, Output = O>,
) -> impl Parser<Input = I, Output = O>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    combine::between(lex(char(start)), lex(char(end)), parser)
}

pub(crate) fn lex<I, O>(
    parser: impl Parser<Input = I, Output = O>,
) -> impl Parser<Input = I, Output = O>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser
        .and(skip_many(one_of(" \t".chars()).or(
            char('\\').with(char('\n').or(string("\r\n").map(|_| ' '))),
        )))
        .map(|(x, _)| x)
}

pub(crate) fn braces_strings<I>(input: &mut I) -> ParseResult<Vec<String>, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((braces(parser(braces_strings)), many1(item()))).parse_stream(input)
}
