use crate::util::*;
use combine::error::{ParseError, ParseResult};
use combine::parser::Parser;
use combine::{attempt, choice, many, many1, parser, Stream};

// -----------------------------------------------------------------------------

/// Object
#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    AllClocks,
    AllInputs(AllInputs),
    AllOutputs(AllOutputs),
    AllRegisters(AllRegisters),
    CurrentDesign,
    GetCells(GetCells),
    GetClocks(GetClocks),
    GetLibCells(GetLibCells),
    GetLibPins(GetLibPins),
    GetLibs(GetLibs),
    GetNets(GetNets),
    GetPins(GetPins),
    GetPorts(GetPorts),
    List(Vec<Object>),
    String(Vec<String>),
    Unknown,
}

impl Default for Object {
    fn default() -> Self {
        Object::Unknown
    }
}

pub(crate) fn object<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let items = (
        attempt(parser(all_clocks)),
        attempt(parser(all_inputs)),
        attempt(parser(all_outputs)),
        attempt(parser(all_registers)),
        attempt(parser(current_design)),
        attempt(parser(get_cells)),
        attempt(parser(get_clocks)),
        attempt(parser(get_lib_cells)),
        attempt(parser(get_lib_pins)),
        attempt(parser(get_libs)),
        attempt(parser(get_nets)),
        attempt(parser(get_pins)),
        attempt(parser(get_ports)),
        attempt(parser(list)),
        attempt(parser(string)),
    );
    choice(items).parse_stream(input)
}

// -----------------------------------------------------------------------------

enum ObjectArg {
    AsyncPins,
    Cells,
    Clock(String),
    ClockPins,
    DataPins,
    EdgeTriggered,
    FallClock(String),
    Hierarchical,
    Hsc(String),
    LevelSensitive,
    MasterSlave,
    NoHierarchy,
    Nocase,
    OfObject(Object),
    OutputPins,
    Patterns(Vec<String>),
    Regexp,
    RiseClock(String),
    SlaveClockPins,
}

// -----------------------------------------------------------------------------

fn all_clocks<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("all_clocks").map(|_| Object::AllClocks);
    brackets(command).parse_stream(input)
}

#[test]
fn test_all_clocks() {
    let mut parser = parser(object);
    let tgt = "[all_clocks]";
    assert_eq!(Object::AllClocks, parser.parse(tgt).unwrap().0);
}

// -----------------------------------------------------------------------------

/// A type containing information of `all_inputs`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct AllInputs {
    pub level_sensitive: bool,
    pub edge_triggered: bool,
    pub clock: Option<String>,
}

fn all_inputs<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("all_inputs");
    let level_sensitive = symbol("-level_sensitive").map(|_| ObjectArg::LevelSensitive);
    let edge_triggered = symbol("-edge_triggered").map(|_| ObjectArg::EdgeTriggered);
    let clock = symbol("-clock").with(item()).map(|x| ObjectArg::Clock(x));
    let args = (
        attempt(level_sensitive),
        attempt(edge_triggered),
        attempt(clock),
    );
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut level_sensitive = false;
            let mut edge_triggered = false;
            let mut clock = None;
            for x in xs {
                match x {
                    ObjectArg::LevelSensitive => level_sensitive = true,
                    ObjectArg::EdgeTriggered => edge_triggered = true,
                    ObjectArg::Clock(x) => clock = Some(x),
                    _ => unreachable!(),
                }
            }
            Object::AllInputs(AllInputs {
                level_sensitive,
                edge_triggered,
                clock,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_all_inputs() {
    let mut parser = parser(object);
    let tgt = "[all_inputs -edge_triggered -clock clk]";
    assert_eq!(
        Object::AllInputs(AllInputs {
            level_sensitive: false,
            edge_triggered: true,
            clock: Some(String::from("clk")),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `all_outputs`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct AllOutputs {
    pub level_sensitive: bool,
    pub edge_triggered: bool,
    pub clock: Option<String>,
}

fn all_outputs<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("all_outputs");
    let level_sensitive = symbol("-level_sensitive").map(|_| ObjectArg::LevelSensitive);
    let edge_triggered = symbol("-edge_triggered").map(|_| ObjectArg::EdgeTriggered);
    let clock = symbol("-clock").with(item()).map(|x| ObjectArg::Clock(x));
    let args = (
        attempt(level_sensitive),
        attempt(edge_triggered),
        attempt(clock),
    );
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut level_sensitive = false;
            let mut edge_triggered = false;
            let mut clock = None;
            for x in xs {
                match x {
                    ObjectArg::LevelSensitive => level_sensitive = true,
                    ObjectArg::EdgeTriggered => edge_triggered = true,
                    ObjectArg::Clock(x) => clock = Some(x),
                    _ => unreachable!(),
                }
            }
            Object::AllOutputs(AllOutputs {
                level_sensitive,
                edge_triggered,
                clock,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_all_outputs() {
    let mut parser = parser(object);
    let tgt = "[all_outputs -level_sensitive]";
    assert_eq!(
        Object::AllOutputs(AllOutputs {
            level_sensitive: true,
            edge_triggered: false,
            clock: None,
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `all_registers`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct AllRegisters {
    pub no_hierarchy: bool,
    pub hsc: Option<String>,
    pub clock: Option<String>,
    pub rise_clock: Option<String>,
    pub fall_clock: Option<String>,
    pub cells: bool,
    pub data_pins: bool,
    pub clock_pins: bool,
    pub slave_clock_pins: bool,
    pub async_pins: bool,
    pub output_pins: bool,
    pub level_sensitive: bool,
    pub edge_triggered: bool,
    pub master_slave: bool,
}

fn all_registers<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("all_registers");
    let no_hierarchy = symbol("-no_hierarchy").map(|_| ObjectArg::NoHierarchy);
    let hsc = symbol("-hsc").with(item()).map(|x| ObjectArg::Hsc(x));
    let clock = symbol("-clock").with(item()).map(|x| ObjectArg::Clock(x));
    let rise_clock = symbol("-rise_clock")
        .with(item())
        .map(|x| ObjectArg::RiseClock(x));
    let fall_clock = symbol("-fall_clock")
        .with(item())
        .map(|x| ObjectArg::FallClock(x));
    let cells = symbol("-cells").map(|_| ObjectArg::Cells);
    let data_pins = symbol("-data_pins").map(|_| ObjectArg::DataPins);
    let clock_pins = symbol("-clock_pins").map(|_| ObjectArg::ClockPins);
    let slave_clock_pins = symbol("-slave_clock_pins").map(|_| ObjectArg::SlaveClockPins);
    let async_pins = symbol("-async_pins").map(|_| ObjectArg::AsyncPins);
    let output_pins = symbol("-output_pins").map(|_| ObjectArg::OutputPins);
    let level_sensitive = symbol("-level_sensitive").map(|_| ObjectArg::LevelSensitive);
    let edge_triggered = symbol("-edge_triggered").map(|_| ObjectArg::EdgeTriggered);
    let master_slave = symbol("-master_slave").map(|_| ObjectArg::MasterSlave);
    let args = (
        attempt(no_hierarchy),
        attempt(hsc),
        attempt(clock),
        attempt(rise_clock),
        attempt(fall_clock),
        attempt(cells),
        attempt(data_pins),
        attempt(clock_pins),
        attempt(slave_clock_pins),
        attempt(async_pins),
        attempt(output_pins),
        attempt(level_sensitive),
        attempt(edge_triggered),
        attempt(master_slave),
    );
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut no_hierarchy = false;
            let mut hsc = None;
            let mut clock = None;
            let mut rise_clock = None;
            let mut fall_clock = None;
            let mut cells = false;
            let mut data_pins = false;
            let mut clock_pins = false;
            let mut slave_clock_pins = false;
            let mut async_pins = false;
            let mut output_pins = false;
            let mut level_sensitive = false;
            let mut edge_triggered = false;
            let mut master_slave = false;
            for x in xs {
                match x {
                    ObjectArg::NoHierarchy => no_hierarchy = true,
                    ObjectArg::Hsc(x) => hsc = Some(x),
                    ObjectArg::Clock(x) => clock = Some(x),
                    ObjectArg::RiseClock(x) => rise_clock = Some(x),
                    ObjectArg::FallClock(x) => fall_clock = Some(x),
                    ObjectArg::Cells => cells = true,
                    ObjectArg::DataPins => data_pins = true,
                    ObjectArg::ClockPins => clock_pins = true,
                    ObjectArg::SlaveClockPins => slave_clock_pins = true,
                    ObjectArg::AsyncPins => async_pins = true,
                    ObjectArg::OutputPins => output_pins = true,
                    ObjectArg::LevelSensitive => level_sensitive = true,
                    ObjectArg::EdgeTriggered => edge_triggered = true,
                    ObjectArg::MasterSlave => master_slave = true,
                    _ => unreachable!(),
                }
            }
            Object::AllRegisters(AllRegisters {
                no_hierarchy,
                hsc,
                clock,
                rise_clock,
                fall_clock,
                cells,
                data_pins,
                clock_pins,
                slave_clock_pins,
                async_pins,
                output_pins,
                level_sensitive,
                edge_triggered,
                master_slave,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_all_registers() {
    let mut parser = parser(object);
    let tgt = "[all_registers -no_hierarchy -hsc a -clock b -rise_clock c -fall_clock d -cells -data_pins -clock_pins -slave_clock_pins -async_pins -output_pins -level_sensitive -edge_triggered -master_slave]";
    assert_eq!(
        Object::AllRegisters(AllRegisters {
            no_hierarchy: true,
            hsc: Some(String::from("a")),
            clock: Some(String::from("b")),
            rise_clock: Some(String::from("c")),
            fall_clock: Some(String::from("d")),
            cells: true,
            data_pins: true,
            clock_pins: true,
            slave_clock_pins: true,
            async_pins: true,
            output_pins: true,
            level_sensitive: true,
            edge_triggered: true,
            master_slave: true,
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

fn current_design<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("current_design").map(|_| Object::CurrentDesign);
    brackets(command).parse_stream(input)
}

#[test]
fn test_current_design() {
    let mut parser = parser(object);
    let tgt = "[current_design]";
    assert_eq!(Object::CurrentDesign, parser.parse(tgt).unwrap().0);
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_cells`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetCells {
    pub hierarchical: bool,
    pub hsc: Option<String>,
    pub regexp: bool,
    pub nocase: bool,
    pub of_objects: Option<Box<Object>>,
    pub patterns: Vec<String>,
}

fn get_cells<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = attempt(symbol("get_cells")).or(symbol("get_cell"));
    let hierarchical = symbol("-hierarchical").map(|_| ObjectArg::Hierarchical);
    let hsc = symbol("-hsc").with(item()).map(|x| ObjectArg::Hsc(x));
    let regexp = symbol("-regexp").map(|_| ObjectArg::Regexp);
    let nocase = symbol("-nocase").map(|_| ObjectArg::Nocase);
    let of_objects = attempt(symbol("-of_objects"))
        .or(symbol("-of_object"))
        .with(parser(object))
        .map(|x| ObjectArg::OfObject(x));
    let patterns = choice((braces(parser(braces_strings)), item().map(|x| vec![x])))
        .map(|x| ObjectArg::Patterns(x));
    let args = (
        attempt(hierarchical),
        attempt(hsc),
        attempt(regexp),
        attempt(nocase),
        attempt(of_objects),
        patterns,
    );
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut hierarchical = false;
            let mut hsc = None;
            let mut regexp = false;
            let mut nocase = false;
            let mut of_objects = None;
            let mut patterns = vec![];
            for x in xs {
                match x {
                    ObjectArg::Hierarchical => hierarchical = true,
                    ObjectArg::Hsc(x) => hsc = Some(x),
                    ObjectArg::Regexp => regexp = true,
                    ObjectArg::Nocase => nocase = true,
                    ObjectArg::OfObject(x) => of_objects = Some(Box::new(x)),
                    ObjectArg::Patterns(x) => patterns = x,
                    _ => unreachable!(),
                }
            }
            Object::GetCells(GetCells {
                hierarchical,
                hsc,
                regexp,
                nocase,
                of_objects,
                patterns,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_get_cells() {
    let mut parser = parser(object);
    let tgt = "[get_cells {c1 c2}]";
    assert_eq!(
        Object::GetCells(GetCells {
            hierarchical: false,
            hsc: None,
            regexp: false,
            nocase: false,
            of_objects: None,
            patterns: vec![String::from("c1"), String::from("c2")]
        }),
        parser.parse(tgt).unwrap().0
    );
    let tgt = "[get_cells -of_objects [get_nets n1]]";
    assert_eq!(
        Object::GetCells(GetCells {
            hierarchical: false,
            hsc: None,
            regexp: false,
            nocase: false,
            of_objects: Some(Box::new(Object::GetNets(GetNets {
                hierarchical: false,
                hsc: None,
                regexp: false,
                nocase: false,
                of_objects: None,
                patterns: vec![String::from("n1")]
            }))),
            patterns: vec![]
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_clocks`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetClocks {
    pub regexp: bool,
    pub nocase: bool,
    pub patterns: Vec<String>,
}

fn get_clocks<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = attempt(symbol("get_clocks")).or(symbol("get_clock"));
    let regexp = symbol("-regexp").map(|_| ObjectArg::Regexp);
    let nocase = symbol("-nocase").map(|_| ObjectArg::Nocase);
    let patterns = choice((braces(parser(braces_strings)), item().map(|x| vec![x])))
        .map(|x| ObjectArg::Patterns(x));
    let args = (attempt(regexp), attempt(nocase), patterns);
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut regexp = false;
            let mut nocase = false;
            let mut patterns = vec![];
            for x in xs {
                match x {
                    ObjectArg::Regexp => regexp = true,
                    ObjectArg::Nocase => nocase = true,
                    ObjectArg::Patterns(x) => patterns = x,
                    _ => unreachable!(),
                }
            }
            Object::GetClocks(GetClocks {
                regexp,
                nocase,
                patterns,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_get_clocks() {
    let mut parser = parser(object);
    let tgt = "[get_clocks clk1]";
    assert_eq!(
        Object::GetClocks(GetClocks {
            regexp: false,
            nocase: false,
            patterns: vec![String::from("clk1")]
        }),
        parser.parse(tgt).unwrap().0
    );

    let tgt = "[get_clocks {clk1 clk2}]";
    assert_eq!(
        Object::GetClocks(GetClocks {
            regexp: false,
            nocase: false,
            patterns: vec![String::from("clk1"), String::from("clk2")]
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_lib_cells`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetLibCells {
    pub hsc: Option<String>,
    pub regexp: bool,
    pub nocase: bool,
    pub patterns: Vec<String>,
}

fn get_lib_cells<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = attempt(symbol("get_lib_cells")).or(symbol("get_lib_cell"));
    let hsc = symbol("-hsc").with(item()).map(|x| ObjectArg::Hsc(x));
    let regexp = symbol("-regexp").map(|_| ObjectArg::Regexp);
    let nocase = symbol("-nocase").map(|_| ObjectArg::Nocase);
    let patterns = choice((braces(parser(braces_strings)), item().map(|x| vec![x])))
        .map(|x| ObjectArg::Patterns(x));
    let args = (attempt(hsc), attempt(regexp), attempt(nocase), patterns);
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut hsc = None;
            let mut regexp = false;
            let mut nocase = false;
            let mut patterns = vec![];
            for x in xs {
                match x {
                    ObjectArg::Hsc(x) => hsc = Some(x),
                    ObjectArg::Regexp => regexp = true,
                    ObjectArg::Nocase => nocase = true,
                    ObjectArg::Patterns(x) => patterns = x,
                    _ => unreachable!(),
                }
            }
            Object::GetLibCells(GetLibCells {
                hsc,
                regexp,
                nocase,
                patterns,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_get_lib_cells() {
    let mut parser = parser(object);
    let tgt = "[get_lib_cells {c1 c2}]";
    assert_eq!(
        Object::GetLibCells(GetLibCells {
            hsc: None,
            regexp: false,
            nocase: false,
            patterns: vec![String::from("c1"), String::from("c2")]
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_lib_pins`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetLibPins {
    pub regexp: bool,
    pub nocase: bool,
    pub patterns: Vec<String>,
}

fn get_lib_pins<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = attempt(symbol("get_lib_pins")).or(symbol("get_lib_pin"));
    let regexp = symbol("-regexp").map(|_| ObjectArg::Regexp);
    let nocase = symbol("-nocase").map(|_| ObjectArg::Nocase);
    let patterns = choice((braces(parser(braces_strings)), item().map(|x| vec![x])))
        .map(|x| ObjectArg::Patterns(x));
    let args = (attempt(regexp), attempt(nocase), patterns);
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut regexp = false;
            let mut nocase = false;
            let mut patterns = vec![];
            for x in xs {
                match x {
                    ObjectArg::Regexp => regexp = true,
                    ObjectArg::Nocase => nocase = true,
                    ObjectArg::Patterns(x) => patterns = x,
                    _ => unreachable!(),
                }
            }
            Object::GetLibPins(GetLibPins {
                regexp,
                nocase,
                patterns,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_get_lib_pins() {
    let mut parser = parser(object);
    let tgt = "[get_lib_pins {c1 c2}]";
    assert_eq!(
        Object::GetLibPins(GetLibPins {
            regexp: false,
            nocase: false,
            patterns: vec![String::from("c1"), String::from("c2")]
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_libs`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetLibs {
    pub regexp: bool,
    pub nocase: bool,
    pub patterns: Vec<String>,
}

fn get_libs<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("get_libs");
    let regexp = symbol("-regexp").map(|_| ObjectArg::Regexp);
    let nocase = symbol("-nocase").map(|_| ObjectArg::Nocase);
    let patterns = choice((braces(parser(braces_strings)), item().map(|x| vec![x])))
        .map(|x| ObjectArg::Patterns(x));
    let args = (attempt(regexp), attempt(nocase), patterns);
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut regexp = false;
            let mut nocase = false;
            let mut patterns = vec![];
            for x in xs {
                match x {
                    ObjectArg::Regexp => regexp = true,
                    ObjectArg::Nocase => nocase = true,
                    ObjectArg::Patterns(x) => patterns = x,
                    _ => unreachable!(),
                }
            }
            Object::GetLibs(GetLibs {
                regexp,
                nocase,
                patterns,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_get_libs() {
    let mut parser = parser(object);
    let tgt = "[get_libs {c1 c2}]";
    assert_eq!(
        Object::GetLibs(GetLibs {
            regexp: false,
            nocase: false,
            patterns: vec![String::from("c1"), String::from("c2")]
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_nets`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetNets {
    pub hierarchical: bool,
    pub hsc: Option<String>,
    pub regexp: bool,
    pub nocase: bool,
    pub of_objects: Option<Box<Object>>,
    pub patterns: Vec<String>,
}

fn get_nets<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = attempt(symbol("get_nets")).or(symbol("get_net"));
    let hierarchical = symbol("-hierarchical").map(|_| ObjectArg::Hierarchical);
    let hsc = symbol("-hsc").with(item()).map(|x| ObjectArg::Hsc(x));
    let regexp = symbol("-regexp").map(|_| ObjectArg::Regexp);
    let nocase = symbol("-nocase").map(|_| ObjectArg::Nocase);
    let of_objects = attempt(symbol("-of_objects"))
        .or(symbol("-of_object"))
        .with(parser(object))
        .map(|x| ObjectArg::OfObject(x));
    let patterns = choice((braces(parser(braces_strings)), item().map(|x| vec![x])))
        .map(|x| ObjectArg::Patterns(x));
    let args = (
        attempt(hierarchical),
        attempt(hsc),
        attempt(regexp),
        attempt(nocase),
        attempt(of_objects),
        patterns,
    );
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut hierarchical = false;
            let mut hsc = None;
            let mut regexp = false;
            let mut nocase = false;
            let mut of_objects = None;
            let mut patterns = vec![];
            for x in xs {
                match x {
                    ObjectArg::Hierarchical => hierarchical = true,
                    ObjectArg::Hsc(x) => hsc = Some(x),
                    ObjectArg::Regexp => regexp = true,
                    ObjectArg::Nocase => nocase = true,
                    ObjectArg::OfObject(x) => of_objects = Some(Box::new(x)),
                    ObjectArg::Patterns(x) => patterns = x,
                    _ => unreachable!(),
                }
            }
            Object::GetNets(GetNets {
                hierarchical,
                hsc,
                regexp,
                nocase,
                of_objects,
                patterns,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_get_nets() {
    let mut parser = parser(object);
    let tgt = "[get_nets {c1 c2}]";
    assert_eq!(
        Object::GetNets(GetNets {
            hierarchical: false,
            hsc: None,
            regexp: false,
            nocase: false,
            of_objects: None,
            patterns: vec![String::from("c1"), String::from("c2")]
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_pins`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetPins {
    pub hierarchical: bool,
    pub hsc: Option<String>,
    pub regexp: bool,
    pub nocase: bool,
    pub of_objects: Option<Box<Object>>,
    pub patterns: Vec<String>,
}

fn get_pins<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = attempt(symbol("get_pins")).or(symbol("get_pin"));
    let hierarchical = symbol("-hierarchical").map(|_| ObjectArg::Hierarchical);
    let hsc = symbol("-hsc").with(item()).map(|x| ObjectArg::Hsc(x));
    let regexp = symbol("-regexp").map(|_| ObjectArg::Regexp);
    let nocase = symbol("-nocase").map(|_| ObjectArg::Nocase);
    let of_objects = attempt(symbol("-of_objects"))
        .or(symbol("-of_object"))
        .with(parser(object))
        .map(|x| ObjectArg::OfObject(x));
    let patterns = choice((braces(parser(braces_strings)), item().map(|x| vec![x])))
        .map(|x| ObjectArg::Patterns(x));
    let args = (
        attempt(hierarchical),
        attempt(hsc),
        attempt(regexp),
        attempt(nocase),
        attempt(of_objects),
        patterns,
    );
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut hierarchical = false;
            let mut hsc = None;
            let mut regexp = false;
            let mut nocase = false;
            let mut of_objects = None;
            let mut patterns = vec![];
            for x in xs {
                match x {
                    ObjectArg::Hierarchical => hierarchical = true,
                    ObjectArg::Hsc(x) => hsc = Some(x),
                    ObjectArg::Regexp => regexp = true,
                    ObjectArg::Nocase => nocase = true,
                    ObjectArg::OfObject(x) => of_objects = Some(Box::new(x)),
                    ObjectArg::Patterns(x) => patterns = x,
                    _ => unreachable!(),
                }
            }
            Object::GetPins(GetPins {
                hierarchical,
                hsc,
                regexp,
                nocase,
                of_objects,
                patterns,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_get_pins() {
    let mut parser = parser(object);
    let tgt = "[get_pins {c1 c2}]";
    assert_eq!(
        Object::GetPins(GetPins {
            hierarchical: false,
            hsc: None,
            regexp: false,
            nocase: false,
            of_objects: None,
            patterns: vec![String::from("c1"), String::from("c2")]
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_ports`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetPorts {
    pub regexp: bool,
    pub nocase: bool,
    pub patterns: Vec<String>,
}

fn get_ports<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = attempt(symbol("get_ports")).or(symbol("get_port"));
    let regexp = symbol("-regexp").map(|_| ObjectArg::Regexp);
    let nocase = symbol("-nocase").map(|_| ObjectArg::Nocase);
    let patterns = choice((braces(parser(braces_strings)), item().map(|x| vec![x])))
        .map(|x| ObjectArg::Patterns(x));
    let args = (attempt(regexp), attempt(nocase), patterns);
    brackets(command.with(many(choice(args))))
        .map(|xs: Vec<_>| {
            let mut regexp = false;
            let mut nocase = false;
            let mut patterns = vec![];
            for x in xs {
                match x {
                    ObjectArg::Regexp => regexp = true,
                    ObjectArg::Nocase => nocase = true,
                    ObjectArg::Patterns(x) => patterns = x,
                    _ => unreachable!(),
                }
            }
            Object::GetPorts(GetPorts {
                regexp,
                nocase,
                patterns,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_get_ports() {
    let mut parser = parser(object);
    let tgt = "[get_ports {p1 p2}]";
    assert_eq!(
        Object::GetPorts(GetPorts {
            regexp: false,
            nocase: false,
            patterns: vec![String::from("p1"), String::from("p2")]
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

fn list<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("list");
    brackets(command.with(many1(parser(object))))
        .map(|x| Object::List(x))
        .parse_stream(input)
}

#[test]
fn test_list() {
    let mut parser = parser(object);
    let tgt = "[list [all_inputs] [all_outputs]]";
    assert_eq!(
        Object::List(vec![
            Object::AllInputs(AllInputs {
                level_sensitive: false,
                edge_triggered: false,
                clock: None
            }),
            Object::AllOutputs(AllOutputs {
                level_sensitive: false,
                edge_triggered: false,
                clock: None
            }),
        ]),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

fn string<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        braces(parser(braces_strings).map(|x| Object::String(x))),
        item().map(|x| Object::String(vec![x])),
    ))
    .parse_stream(input)
}

#[test]
fn test_string() {
    let mut parser = parser(object);
    let tgt = "a";
    assert_eq!(
        Object::String(vec![String::from("a")]),
        parser.parse(tgt).unwrap().0
    );

    let tgt = "{a b c}";
    assert_eq!(
        Object::String(vec![
            String::from("a"),
            String::from("b"),
            String::from("c")
        ]),
        parser.parse(tgt).unwrap().0
    );
}
