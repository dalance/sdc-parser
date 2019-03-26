pub mod object;
pub mod sdc;
pub mod util;

use crate::sdc::{sdc, Sdc};
use combine::error::{ParseError, ParseResult};
use combine::{parser, Parser, Stream};
use std::marker::PhantomData;

/// A type representing sdc parser
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

/// Generate sdc parser
///
/// # Examples
///
/// ```
/// use sdc_parser::{sdc_parser, sdc};
/// use combine::parser::Parser;
///
/// let mut parser = sdc_parser();
/// let (result, rest) = parser.parse("current_instance duv").unwrap();
///
/// let expect = sdc::Sdc {
///     commands: vec![sdc::Command::CurrentInstance(
///         sdc::CurrentInstance {
///             instance: Some(String::from("duv"))
///         }
///     )]
/// };
/// assert_eq!(expect, result);
/// ```
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
    use std::fs::File;
    use std::io::Read;
    use walkdir::WalkDir;

    #[test]
    fn test_sdc_parser_pass() {
        for entry in WalkDir::new("./testcase/pass") {
            if let Ok(entry) = entry {
                if entry.file_type().is_dir() {
                    continue;
                }
                let file = entry.path();
                let mut f = File::open(&file).unwrap();
                let mut buf = String::new();
                let _ = f.read_to_string(&mut buf);

                let file = dbg!(file);

                let mut parser = sdc_parser();
                let ret = parser.easy_parse(State::new(buf.as_str()));

                assert!(ret.is_ok(), "Parse is failed at {:?}: {:?}", file, ret);
                assert_eq!("", ret.unwrap().1.input, "Input is Remained at {:?}", file);
            }
        }
    }

    #[test]
    #[should_panic]
    fn test_sdc_parser_fail() {
        for entry in WalkDir::new("./testcase/fail") {
            if let Ok(entry) = entry {
                if entry.file_type().is_dir() {
                    continue;
                }
                let file = entry.path();
                let mut f = File::open(&file).unwrap();
                let mut buf = String::new();
                let _ = f.read_to_string(&mut buf);

                let file = dbg!(file);

                let mut parser = sdc_parser();
                let ret = parser.easy_parse(State::new(buf.as_str()));

                assert!(ret.is_err(), "Parse is passed at {:?}: {:?}", file, ret);
            }
        }
    }
}
