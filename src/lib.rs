pub mod object;
pub mod sdc;

use crate::sdc::{sdc, Sdc};
use combine::error::{ParseError, ParseResult};
use combine::{parser, Parser, Stream};
use std::marker::PhantomData;

/// SdcParser
pub struct SdcParser<I>(PhantomData<fn(I) -> I>);

impl<I> Parser for SdcParser<I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    type Input = I;
    type Output = Sdc;
    type PartialState = ();
    #[inline]
    fn parse_stream(&mut self, input: &mut I) -> ParseResult<Self::Output, Self::Input> {
        let mut parser = parser(sdc);
        parser.parse_stream(input)
    }
}

/// sdc_parser
pub fn sdc_parser<I>() -> SdcParser<I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    SdcParser(PhantomData)
}

#[cfg(test)]
mod test {
    use super::*;
    use combine::stream::state::State;
    use std::fs::{self, File};
    use std::io::Read;

    #[test]
    fn test_sdc_parser_file() {
        for entry in fs::read_dir("./pass").unwrap() {
            let file = entry.unwrap().path();
            let mut f = File::open(&file).unwrap();
            let mut buf = String::new();
            let _ = f.read_to_string(&mut buf);

            let mut parser = sdc_parser();
            let ret = parser.easy_parse(State::new(buf.as_str()));

            assert!(ret.is_ok(), "Parse is failed at {:?}: {:?}", file, ret);
            assert_eq!("", ret.unwrap().1.input, "Input is Remained at {:?}", file);
        }
    }

    #[test]
    fn test_sdc_parser_text() {
        let tgt = "set_operating_conditions -library [get_libs a]\ncreate_clock -period 4.000000";
        let mut parser = sdc_parser();
        let ret = parser.easy_parse(State::new(tgt));
        assert!(ret.is_ok(), "Parse is failed at {:?}", ret);
        assert_eq!("", ret.unwrap().1.input);
    }
}
