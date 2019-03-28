pub mod object;
pub mod sdc;
pub mod util;

use crate::sdc::{sdc, sdc_strict, Sdc};
use combine::error::ParseResult;
use combine::{Parser, Stream};
use failure::{Backtrace, Context, Fail};
use std::fmt::{self, Display};
use std::marker::PhantomData;

// -----------------------------------------------------------------------------

/// Parse SDC string
///
/// This function always successes.
/// Any line failed to parse ( include vendor extension ) is contained as `sdc::Command::Unknown`.
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

/// Parse SDC string strictly
///
/// This function fails if the input is not valid SDC.
///
/// # Examples
///
/// ```
/// use sdc_parser::{self, sdc};
///
/// let result = sdc_parser::parse_strict("current_instance duv").unwrap();
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
pub fn parse_strict(s: &str) -> Result<Sdc, Error> {
    let mut parser = sdc_parser_strict();
    let (ret, rest) = parser.easy_parse(s).map_err::<combine::easy::Errors<
        char,
        &str,
        combine::stream::PointerOffset,
    >, _>(Into::into)?;

    if rest.is_empty() {
        Ok(ret)
    } else {
        Err(Error {
            inner: Context::new(ErrorKind::Interrupt(InterruptError {
                parsed: ret,
                rest: String::from(rest),
            })),
        })
    }
}

// -----------------------------------------------------------------------------

#[derive(Fail, Debug)]
pub enum ErrorKind {
    #[fail(display = "Parse failed {:?}", 0)]
    Parse(ParseError),

    #[fail(display = "Parse interruptted {:?}", 0)]
    Interrupt(InterruptError),
}

#[derive(Debug)]
pub struct ParseError {
    pub messages: Vec<String>,
}

#[derive(Debug)]
pub struct InterruptError {
    pub parsed: Sdc,
    pub rest: String,
}

// -----------------------------------------------------------------------------

#[derive(Debug)]
pub struct Error {
    inner: Context<ErrorKind>,
}

impl Fail for Error {
    fn cause(&self) -> Option<&Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl Error {
    pub fn new(inner: Context<ErrorKind>) -> Error {
        Error { inner }
    }

    pub fn kind(&self) -> &ErrorKind {
        self.inner.get_context()
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Error {
        Error {
            inner: Context::new(kind),
        }
    }
}

impl From<Context<ErrorKind>> for Error {
    fn from(inner: Context<ErrorKind>) -> Error {
        Error { inner }
    }
}

// -----------------------------------------------------------------------------

impl From<combine::easy::Errors<char, &str, combine::stream::PointerOffset>> for Error {
    fn from(error: combine::easy::Errors<char, &str, combine::stream::PointerOffset>) -> Error {
        let mut messages = Vec::new();

        for e in &error.errors {
            messages.push(format!("{:?}", e))
        }

        Error {
            inner: Context::new(ErrorKind::Parse(ParseError { messages })),
        }
    }
}

// -----------------------------------------------------------------------------

struct SdcParser<I>(PhantomData<fn(I) -> I>);

impl<I> Parser for SdcParser<I>
where
    I: Stream<Item = char>,
    I::Error: combine::error::ParseError<char, I::Range, I::Position>,
    <I::Error as combine::error::ParseError<char, I::Range, I::Position>>::StreamError:
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

fn sdc_parser<I>() -> SdcParser<I>
where
    I: Stream<Item = char>,
    I::Error: combine::error::ParseError<I::Item, I::Range, I::Position>,
{
    SdcParser(PhantomData)
}

// -----------------------------------------------------------------------------

struct SdcParserStrict<I>(PhantomData<fn(I) -> I>);

impl<I> Parser for SdcParserStrict<I>
where
    I: Stream<Item = char>,
    I::Error: combine::error::ParseError<char, I::Range, I::Position>,
    <I::Error as combine::error::ParseError<char, I::Range, I::Position>>::StreamError:
        From<::combine::easy::Error<char, I::Range>>,
{
    type Input = I;
    type Output = Sdc;
    type PartialState = ();
    #[inline]
    fn parse_stream(&mut self, input: &mut I) -> ParseResult<Self::Output, Self::Input> {
        let mut parser = sdc_strict();
        parser.parse_stream(input)
    }
}

fn sdc_parser_strict<I>() -> SdcParserStrict<I>
where
    I: Stream<Item = char>,
    I::Error: combine::error::ParseError<I::Item, I::Range, I::Position>,
{
    SdcParserStrict(PhantomData)
}

// -----------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use combine::stream::state::State;
    use std::fs::File;
    use std::io::Read;
    use walkdir::WalkDir;

    #[test]
    fn test_parse() {
        let result = parse("current_instance duv");

        let expect = Sdc {
            commands: vec![sdc::Command::CurrentInstance(sdc::CurrentInstance {
                instance: Some(String::from("duv")),
            })],
        };
        assert_eq!(expect, result);
    }

    #[test]
    fn test_by_testcase() {
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
