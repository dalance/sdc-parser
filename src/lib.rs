pub mod object;
pub mod sdc;

use combine::error::ParseError;
use combine::{parser, Parser, Stream};

/// sdc_parser
pub fn sdc_parser<I>() -> impl Parser<Input = I, Output = Vec<sdc::Command>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser(sdc::sdc)
}
