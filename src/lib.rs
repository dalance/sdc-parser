pub mod object;
pub mod sdc;
pub mod util;

use crate::sdc::{sdc, Sdc};
use combine::error::{ParseError, ParseResult};
use combine::{Parser, Stream};
use std::marker::PhantomData;

/// Parse Sdc
///
/// This function isn't failed.
/// Any line failed to parse is contained as `sdc::Command::Unknown`.
/// Any vendor extension (eg. `derive_clock_uncertainty`) is contained as `Sdc::Command::Unknown` too.
///
/// # Examples
///
/// ```
/// use sdc_parser::{self, sdc};
///
/// let result = sdc_parser::parse("current_instance duv");
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
pub fn parse(s: &str) -> Sdc {
    let mut parser = sdc_parser();
    parser.easy_parse(s).unwrap().0
}

struct SdcParser_<I>(PhantomData<fn(I) -> I>);

impl<I> Parser for SdcParser_<I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<char, I::Range, I::Position>,
    <I::Error as ParseError<char, I::Range, I::Position>>::StreamError:
        From<::combine::easy::Error<char, I::Range>>,
{
    type Input = I;
    type Output = Sdc;
    type PartialState = ();
    #[inline]
    fn parse_stream(&mut self, input: &mut I) -> ParseResult<Self::Output, Self::Input> {
        let mut parser = sdc();
        parser.parse_stream(input)
    }
}

fn sdc_parser<I>() -> SdcParser_<I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    SdcParser_(PhantomData)
}

#[cfg(test)]
mod test {
    use super::*;
    use combine::stream::state::State;
    use std::fs::File;
    use std::io::Read;
    use walkdir::WalkDir;

    #[test]
    fn test_sdc_parser_testcase() {
        for entry in WalkDir::new("./testcase") {
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
}
