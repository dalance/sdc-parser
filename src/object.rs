use crate::util::*;
use combine::error::{ParseError, ParseResult};
use combine::parser::Parser;
use combine::{attempt, choice, many, many1, parser, Stream};
use std::fmt;

// -----------------------------------------------------------------------------

/// Object
#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    AllClocks(AllClocks),
    AllInputs(AllInputs),
    AllOutputs(AllOutputs),
    AllRegisters(AllRegisters),
    CurrentDesign(CurrentDesign),
    GetCells(GetCells),
    GetClocks(GetClocks),
    GetLibCells(GetLibCells),
    GetLibPins(GetLibPins),
    GetLibs(GetLibs),
    GetNets(GetNets),
    GetPins(GetPins),
    GetPorts(GetPorts),
    List(List),
    String(ObjectString),
    Unknown,
}

impl Default for Object {
    fn default() -> Self {
        Object::Unknown
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::AllClocks(x) => write!(f, "{}", x),
            Object::AllInputs(x) => write!(f, "{}", x),
            Object::AllOutputs(x) => write!(f, "{}", x),
            Object::AllRegisters(x) => write!(f, "{}", x),
            Object::CurrentDesign(x) => write!(f, "{}", x),
            Object::GetCells(x) => write!(f, "{}", x),
            Object::GetClocks(x) => write!(f, "{}", x),
            Object::GetLibCells(x) => write!(f, "{}", x),
            Object::GetLibPins(x) => write!(f, "{}", x),
            Object::GetLibs(x) => write!(f, "{}", x),
            Object::GetNets(x) => write!(f, "{}", x),
            Object::GetPins(x) => write!(f, "{}", x),
            Object::GetPorts(x) => write!(f, "{}", x),
            Object::List(x) => write!(f, "{}", x),
            Object::String(x) => write!(f, "{}", x),
            Object::Unknown => write!(f, ""),
        }
    }
}

pub(crate) fn object<I>(input: &mut I) -> ParseResult<Object, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let items = (
        attempt(all_clocks()),
        attempt(all_inputs()),
        attempt(all_outputs()),
        attempt(all_registers()),
        attempt(current_design()),
        attempt(get_cells()),
        attempt(get_clocks()),
        attempt(get_lib_cells()),
        attempt(get_lib_pins()),
        attempt(get_libs()),
        attempt(get_nets()),
        attempt(get_pins()),
        attempt(get_ports()),
        attempt(list()),
        attempt(string()),
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

/// A type containing information of `all_clocks`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct AllClocks;

impl fmt::Display for AllClocks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[all_clocks]")
    }
}

fn all_clocks<I>() -> impl Parser<Input = I, Output = Object>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("all_clocks").map(|_| Object::AllClocks(AllClocks {}));
    brackets(command)
}

#[test]
fn test_all_clocks() {
    let mut parser = parser(object);
    let tgt = "[all_clocks]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(Object::AllClocks(AllClocks {}), ret);
    assert_eq!(tgt, format!("{}", ret));
}

// -----------------------------------------------------------------------------

/// A type containing information of `all_inputs`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct AllInputs {
    pub level_sensitive: bool,
    pub edge_triggered: bool,
    pub clock: Option<String>,
}

impl fmt::Display for AllInputs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.level_sensitive {
            args.push_str(" -level_sensitive");
        }
        if self.edge_triggered {
            args.push_str(" -edge_triggered");
        }
        if let Some(clock) = &self.clock {
            args.push_str(&format!(" -clock {}", clock));
        }
        write!(f, "[all_inputs{}]", args)
    }
}

fn all_inputs<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_all_inputs() {
    let mut parser = parser(object);
    let tgt = "[all_inputs -level_sensitive -edge_triggered -clock a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::AllInputs(AllInputs {
            level_sensitive: true,
            edge_triggered: true,
            clock: Some(String::from("a")),
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
}

// -----------------------------------------------------------------------------

/// A type containing information of `all_outputs`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct AllOutputs {
    pub level_sensitive: bool,
    pub edge_triggered: bool,
    pub clock: Option<String>,
}

impl fmt::Display for AllOutputs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.level_sensitive {
            args.push_str(" -level_sensitive");
        }
        if self.edge_triggered {
            args.push_str(" -edge_triggered");
        }
        if let Some(clock) = &self.clock {
            args.push_str(&format!(" -clock {}", clock));
        }
        write!(f, "[all_outputs{}]", args)
    }
}

fn all_outputs<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_all_outputs() {
    let mut parser = parser(object);
    let tgt = "[all_outputs -level_sensitive -edge_triggered -clock a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::AllOutputs(AllOutputs {
            level_sensitive: true,
            edge_triggered: true,
            clock: Some(String::from("a")),
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
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

impl fmt::Display for AllRegisters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.no_hierarchy {
            args.push_str(" -no_hierarchy");
        }
        if let Some(hsc) = &self.hsc {
            args.push_str(&format!(" -hsc {}", hsc));
        }
        if let Some(clock) = &self.clock {
            args.push_str(&format!(" -clock {}", clock));
        }
        if let Some(rise_clock) = &self.rise_clock {
            args.push_str(&format!(" -rise_clock {}", rise_clock));
        }
        if let Some(fall_clock) = &self.fall_clock {
            args.push_str(&format!(" -fall_clock {}", fall_clock));
        }
        if self.cells {
            args.push_str(" -cells");
        }
        if self.data_pins {
            args.push_str(" -data_pins");
        }
        if self.clock_pins {
            args.push_str(" -clock_pins");
        }
        if self.slave_clock_pins {
            args.push_str(" -slave_clock_pins");
        }
        if self.async_pins {
            args.push_str(" -async_pins");
        }
        if self.output_pins {
            args.push_str(" -output_pins");
        }
        if self.level_sensitive {
            args.push_str(" -level_sensitive");
        }
        if self.edge_triggered {
            args.push_str(" -edge_triggered");
        }
        if self.master_slave {
            args.push_str(" -master_slave");
        }
        write!(f, "[all_registers{}]", args)
    }
}

fn all_registers<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_all_registers() {
    let mut parser = parser(object);
    let tgt = "[all_registers -no_hierarchy -hsc a -clock a -rise_clock a -fall_clock a -cells -data_pins -clock_pins -slave_clock_pins -async_pins -output_pins -level_sensitive -edge_triggered -master_slave]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::AllRegisters(AllRegisters {
            no_hierarchy: true,
            hsc: Some(String::from("a")),
            clock: Some(String::from("a")),
            rise_clock: Some(String::from("a")),
            fall_clock: Some(String::from("a")),
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
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
}

// -----------------------------------------------------------------------------

/// A type containing information of `current_design`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct CurrentDesign;

impl fmt::Display for CurrentDesign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[current_design]")
    }
}

fn current_design<I>() -> impl Parser<Input = I, Output = Object>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("current_design").map(|_| Object::CurrentDesign(CurrentDesign {}));
    brackets(command)
}

#[test]
fn test_current_design() {
    let mut parser = parser(object);
    let tgt = "[current_design]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(Object::CurrentDesign(CurrentDesign {}), ret);
    assert_eq!(tgt, format!("{}", ret));
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

impl fmt::Display for GetCells {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.hierarchical {
            args.push_str(" -hierarchical");
        }
        if let Some(hsc) = &self.hsc {
            args.push_str(&format!(" -hsc {}", hsc));
        }
        if self.regexp {
            args.push_str(" -regexp");
        }
        if self.nocase {
            args.push_str(" -nocase");
        }
        if let Some(of_objects) = &self.of_objects {
            args.push_str(&format!(" -of_objects {}", of_objects));
        }
        if self.patterns.len() == 1 {
            args.push_str(&format!(" {}", self.patterns[0]));
        } else if self.patterns.len() > 1 {
            args.push_str(" {");
            for (i, s) in self.patterns.iter().enumerate() {
                if i == 0 {
                    args.push_str(&format!("{}", s));
                } else {
                    args.push_str(&format!(" {}", s));
                }
            }
            args.push_str("}");
        }
        write!(f, "[get_cells{}]", args)
    }
}

fn get_cells<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_get_cells() {
    let mut parser = parser(object);
    let tgt = "[get_cells -hierarchical -hsc a -regexp -nocase -of_objects a a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetCells(GetCells {
            hierarchical: true,
            hsc: Some(String::from("a")),
            regexp: true,
            nocase: true,
            of_objects: Some(Box::new(Object::String(ObjectString {
                strings: vec![String::from("a")]
            }))),
            patterns: vec![String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "[get_cells -hierarchical -hsc a -regexp -nocase -of_objects a {a a}]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetCells(GetCells {
            hierarchical: true,
            hsc: Some(String::from("a")),
            regexp: true,
            nocase: true,
            of_objects: Some(Box::new(Object::String(ObjectString {
                strings: vec![String::from("a")]
            }))),
            patterns: vec![String::from("a"), String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_clocks`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetClocks {
    pub regexp: bool,
    pub nocase: bool,
    pub patterns: Vec<String>,
}

impl fmt::Display for GetClocks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.regexp {
            args.push_str(" -regexp");
        }
        if self.nocase {
            args.push_str(" -nocase");
        }
        if self.patterns.len() == 1 {
            args.push_str(&format!(" {}", self.patterns[0]));
        } else if self.patterns.len() > 1 {
            args.push_str(" {");
            for (i, s) in self.patterns.iter().enumerate() {
                if i == 0 {
                    args.push_str(&format!("{}", s));
                } else {
                    args.push_str(&format!(" {}", s));
                }
            }
            args.push_str("}");
        }
        write!(f, "[get_clocks{}]", args)
    }
}

fn get_clocks<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_get_clocks() {
    let mut parser = parser(object);
    let tgt = "[get_clocks -regexp -nocase a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetClocks(GetClocks {
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "[get_clocks -regexp -nocase {a a}]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetClocks(GetClocks {
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a"), String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
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

impl fmt::Display for GetLibCells {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if let Some(hsc) = &self.hsc {
            args.push_str(&format!(" -hsc {}", hsc));
        }
        if self.regexp {
            args.push_str(" -regexp");
        }
        if self.nocase {
            args.push_str(" -nocase");
        }
        if self.patterns.len() == 1 {
            args.push_str(&format!(" {}", self.patterns[0]));
        } else if self.patterns.len() > 1 {
            args.push_str(" {");
            for (i, s) in self.patterns.iter().enumerate() {
                if i == 0 {
                    args.push_str(&format!("{}", s));
                } else {
                    args.push_str(&format!(" {}", s));
                }
            }
            args.push_str("}");
        }
        write!(f, "[get_lib_cells{}]", args)
    }
}

fn get_lib_cells<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_get_lib_cells() {
    let mut parser = parser(object);
    let tgt = "[get_lib_cells -hsc a -regexp -nocase a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetLibCells(GetLibCells {
            hsc: Some(String::from("a")),
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "[get_lib_cells -hsc a -regexp -nocase {a a}]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetLibCells(GetLibCells {
            hsc: Some(String::from("a")),
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a"), String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_lib_pins`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetLibPins {
    pub regexp: bool,
    pub nocase: bool,
    pub patterns: Vec<String>,
}

impl fmt::Display for GetLibPins {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.regexp {
            args.push_str(" -regexp");
        }
        if self.nocase {
            args.push_str(" -nocase");
        }
        if self.patterns.len() == 1 {
            args.push_str(&format!(" {}", self.patterns[0]));
        } else if self.patterns.len() > 1 {
            args.push_str(" {");
            for (i, s) in self.patterns.iter().enumerate() {
                if i == 0 {
                    args.push_str(&format!("{}", s));
                } else {
                    args.push_str(&format!(" {}", s));
                }
            }
            args.push_str("}");
        }
        write!(f, "[get_lib_pins{}]", args)
    }
}

fn get_lib_pins<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_get_lib_pins() {
    let mut parser = parser(object);
    let tgt = "[get_lib_pins -regexp -nocase a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetLibPins(GetLibPins {
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "[get_lib_pins -regexp -nocase {a a}]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetLibPins(GetLibPins {
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a"), String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_libs`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetLibs {
    pub regexp: bool,
    pub nocase: bool,
    pub patterns: Vec<String>,
}

impl fmt::Display for GetLibs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.regexp {
            args.push_str(" -regexp");
        }
        if self.nocase {
            args.push_str(" -nocase");
        }
        if self.patterns.len() == 1 {
            args.push_str(&format!(" {}", self.patterns[0]));
        } else if self.patterns.len() > 1 {
            args.push_str(" {");
            for (i, s) in self.patterns.iter().enumerate() {
                if i == 0 {
                    args.push_str(&format!("{}", s));
                } else {
                    args.push_str(&format!(" {}", s));
                }
            }
            args.push_str("}");
        }
        write!(f, "[get_libs{}]", args)
    }
}

fn get_libs<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_get_libs() {
    let mut parser = parser(object);
    let tgt = "[get_libs -regexp -nocase a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetLibs(GetLibs {
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "[get_libs -regexp -nocase {a a}]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetLibs(GetLibs {
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a"), String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
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

impl fmt::Display for GetNets {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.hierarchical {
            args.push_str(" -hierarchical");
        }
        if let Some(hsc) = &self.hsc {
            args.push_str(&format!(" -hsc {}", hsc));
        }
        if self.regexp {
            args.push_str(" -regexp");
        }
        if self.nocase {
            args.push_str(" -nocase");
        }
        if let Some(of_objects) = &self.of_objects {
            args.push_str(&format!(" -of_objects {}", of_objects));
        }
        if self.patterns.len() == 1 {
            args.push_str(&format!(" {}", self.patterns[0]));
        } else if self.patterns.len() > 1 {
            args.push_str(" {");
            for (i, s) in self.patterns.iter().enumerate() {
                if i == 0 {
                    args.push_str(&format!("{}", s));
                } else {
                    args.push_str(&format!(" {}", s));
                }
            }
            args.push_str("}");
        }
        write!(f, "[get_nets{}]", args)
    }
}

fn get_nets<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_get_nets() {
    let mut parser = parser(object);
    let tgt = "[get_nets -hierarchical -hsc a -regexp -nocase -of_objects a a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetNets(GetNets {
            hierarchical: true,
            hsc: Some(String::from("a")),
            regexp: true,
            nocase: true,
            of_objects: Some(Box::new(Object::String(ObjectString {
                strings: vec![String::from("a")]
            }))),
            patterns: vec![String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "[get_nets -hierarchical -hsc a -regexp -nocase -of_objects a {a a}]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetNets(GetNets {
            hierarchical: true,
            hsc: Some(String::from("a")),
            regexp: true,
            nocase: true,
            of_objects: Some(Box::new(Object::String(ObjectString {
                strings: vec![String::from("a")]
            }))),
            patterns: vec![String::from("a"), String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
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

impl fmt::Display for GetPins {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.hierarchical {
            args.push_str(" -hierarchical");
        }
        if let Some(hsc) = &self.hsc {
            args.push_str(&format!(" -hsc {}", hsc));
        }
        if self.regexp {
            args.push_str(" -regexp");
        }
        if self.nocase {
            args.push_str(" -nocase");
        }
        if let Some(of_objects) = &self.of_objects {
            args.push_str(&format!(" -of_objects {}", of_objects));
        }
        if self.patterns.len() == 1 {
            args.push_str(&format!(" {}", self.patterns[0]));
        } else if self.patterns.len() > 1 {
            args.push_str(" {");
            for (i, s) in self.patterns.iter().enumerate() {
                if i == 0 {
                    args.push_str(&format!("{}", s));
                } else {
                    args.push_str(&format!(" {}", s));
                }
            }
            args.push_str("}");
        }
        write!(f, "[get_pins{}]", args)
    }
}

fn get_pins<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_get_pins() {
    let mut parser = parser(object);
    let tgt = "[get_pins -hierarchical -hsc a -regexp -nocase -of_objects a a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetPins(GetPins {
            hierarchical: true,
            hsc: Some(String::from("a")),
            regexp: true,
            nocase: true,
            of_objects: Some(Box::new(Object::String(ObjectString {
                strings: vec![String::from("a")]
            }))),
            patterns: vec![String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "[get_pins -hierarchical -hsc a -regexp -nocase -of_objects a {a a}]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetPins(GetPins {
            hierarchical: true,
            hsc: Some(String::from("a")),
            regexp: true,
            nocase: true,
            of_objects: Some(Box::new(Object::String(ObjectString {
                strings: vec![String::from("a")]
            }))),
            patterns: vec![String::from("a"), String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
}

// -----------------------------------------------------------------------------

/// A type containing information of `get_ports`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GetPorts {
    pub regexp: bool,
    pub nocase: bool,
    pub patterns: Vec<String>,
}

impl fmt::Display for GetPorts {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        if self.regexp {
            args.push_str(" -regexp");
        }
        if self.nocase {
            args.push_str(" -nocase");
        }
        if self.patterns.len() == 1 {
            args.push_str(&format!(" {}", self.patterns[0]));
        } else if self.patterns.len() > 1 {
            args.push_str(" {");
            for (i, s) in self.patterns.iter().enumerate() {
                if i == 0 {
                    args.push_str(&format!("{}", s));
                } else {
                    args.push_str(&format!(" {}", s));
                }
            }
            args.push_str("}");
        }
        write!(f, "[get_ports{}]", args)
    }
}

fn get_ports<I>() -> impl Parser<Input = I, Output = Object>
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
    brackets(command.with(many(choice(args)))).map(|xs: Vec<_>| {
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
}

#[test]
fn test_get_ports() {
    let mut parser = parser(object);
    let tgt = "[get_ports -regexp -nocase a]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetPorts(GetPorts {
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "[get_ports -regexp -nocase {a a}]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::GetPorts(GetPorts {
            regexp: true,
            nocase: true,
            patterns: vec![String::from("a"), String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
}

// -----------------------------------------------------------------------------

/// A type containing information of `list`
#[derive(Clone, Debug, Default, PartialEq)]
pub struct List {
    pub objects: Vec<Object>,
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = String::from("");
        for s in &self.objects {
            args.push_str(&format!(" {}", s));
        }
        write!(f, "[list{}]", args)
    }
}

fn list<I>() -> impl Parser<Input = I, Output = Object>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("list");
    brackets(command.with(many1(parser(object)))).map(|x| Object::List(List { objects: x }))
}

#[test]
fn test_list() {
    let mut parser = parser(object);
    let tgt = "[list [all_inputs] [all_outputs]]";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::List(List {
            objects: vec![
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
            ]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
}

// -----------------------------------------------------------------------------

/// A string type
#[derive(Clone, Debug, Default, PartialEq)]
pub struct ObjectString {
    pub strings: Vec<String>,
}

impl fmt::Display for ObjectString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut ret = String::from("");
        if self.strings.len() == 0 {
        } else if self.strings.len() == 1 {
            if self.strings[0].chars().any(|x| x.is_whitespace()) {
                ret.push_str(&format!("\"{}\"", self.strings[0]))
            } else {
                ret.push_str(&self.strings[0])
            }
        } else {
            ret.push_str("{");
            for (i, s) in self.strings.iter().enumerate() {
                if i != 0 {
                    ret.push_str(" ");
                }
                if s.chars().any(|x| x.is_whitespace()) {
                    ret.push_str(&format!("\"{}\"", s));
                } else {
                    ret.push_str(&format!("{}", s));
                }
            }
            ret.push_str("}");
        }
        write!(f, "{}", ret)
    }
}

fn string<I>() -> impl Parser<Input = I, Output = Object>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        braces(parser(braces_strings).map(|x| Object::String(ObjectString { strings: x }))),
        item().map(|x| Object::String(ObjectString { strings: vec![x] })),
    ))
}

#[test]
fn test_string() {
    let mut parser = parser(object);
    let tgt = "a";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::String(ObjectString {
            strings: vec![String::from("a")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "{a b c}";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::String(ObjectString {
            strings: vec![String::from("a"), String::from("b"), String::from("c")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));

    let tgt = "\"a b c\"";
    let ret = parser.parse(tgt).unwrap().0;
    assert_eq!(
        Object::String(ObjectString {
            strings: vec![String::from("a b c")]
        }),
        ret
    );
    assert_eq!(tgt, format!("{}", ret));
}
