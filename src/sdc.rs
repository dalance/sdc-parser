use crate::object::*;
use combine::char::{alpha_num, letter, string};
use combine::error::{ParseError, ParseResult};
use combine::parser::Parser;
use combine::{attempt, choice, many, many1, one_of, optional, parser, satisfy, Stream};
use combine_language::{Identifier, LanguageDef, LanguageEnv};

// -----------------------------------------------------------------------------

/// Command
#[derive(Debug, PartialEq)]
pub enum Command {
    Comment(String),
    CreateClock(CreateClock),
    CreateGeneratedClock(CreateGeneratedClock),
    CreateVoltageArea,
    CurrentInstance(String),
    GroupPath(GroupPath),
    SetCaseAnalysis,
    SetClockGatingCheck,
    SetClockGroups,
    SetClockLatency,
    SetClockTransition,
    SetClockUncertainty,
    SetDataCheck,
    SetDisableTiming,
    SetDrive,
    SetDrivingCell,
    SetFalsePath,
    SetFanoutLoad,
    SetIdealLatency,
    SetIdealNetwork,
    SetIdealTransition,
    SetInputDelay,
    SetInputTransition,
    SetLevelShifterStrategy,
    SetLevelShifterThreshold,
    SetLoad,
    SetLogicDc,
    SetLogicOne,
    SetLogicZero,
    SetMaxArea,
    SetMaxCapacitance,
    SetMaxDelay,
    SetMaxDynamicPower,
    SetMaxFanout,
    SetMaxLeakagePower,
    SetMaxTimeBorrow,
    SetMaxTransition,
    SetMinCapacitance,
    SetMinDelay,
    SetMinPulseWidth,
    SetMulticyclePath,
    SetOperatingConditions(SetOperatingConditions),
    SetOutputDelay,
    SetPortFanoutNumber,
    SetPropagatedClock,
    SetResistance,
    SetSense,
    SetTimingDerate,
    SetUnits(Vec<Unit>),
    SetVersion(f64),
    SetWireLoadMinBlockSize,
    SetWireLoadMode,
    SetWireLoadModel,
    SetWireLoadSelectionGroup,
}

// -----------------------------------------------------------------------------

enum CommandArg {
    Add,
    AddDelay,
    AllowPaths,
    AnalysisType(String),
    Asynchronous,
    CaseValue(String),
    CellCheck,
    CellDelay,
    ClockObj(Object),
    Clock,
    ClockFall,
    ClockLeaf,
    ClockPath,
    Clocks(Object),
    Combinational,
    Comment(String),
    Coordinate(Object),
    Data,
    DataPath,
    Default,
    DivideBy(f64),
    DontScale,
    DutyCycle(f64),
    Dynamic,
    Early,
    EdgeShift(Vec<f64>),
    Edges(Vec<f64>),
    End,
    Fall,
    FallFrom(Object),
    FallThrough(Object),
    FallTo(Object),
    From(Object),
    FromPin(Object),
    Group(Object),
    GuardBandX(f64),
    GuardBandY(f64),
    High,
    Hold(f64),
    IgnoreClockLatency,
    Increment,
    InputTransitionFall(f64),
    InputTransitionRise(f64),
    Invert,
    Late,
    LevelSensitive,
    LibCell(Object),
    Library(Object),
    LogicallyExclusive,
    Low,
    MasterClock(Object),
    Max,
    MaxCond(String),
    MaxLibrary(Object),
    Min,
    MinCond(String),
    MinLibrary(Object),
    MultiplyBy(f64),
    Name(String),
    Negative,
    NetDelay,
    NetworkLatencyIncluded,
    NoDesignRule,
    NoPropagate,
    NonUnate,
    ObjectList(Object),
    Object(Object),
    Percent(f64),
    Period(f64),
    PhyiscallyExclusive,
    Pin(Object),
    PinLoad,
    Positive,
    Pulse(String),
    ReferencePin(Object),
    Rise,
    RiseFrom(Object),
    RiseThrough(Object),
    RiseTo(Object),
    Rule(String),
    Setup(f64),
    Source,
    SourceLatencyIncluded,
    SourceObj(Object),
    Start,
    Static,
    StopPropagation,
    String(String),
    SubtractPinLoad,
    Through(Object),
    To(Object),
    Type(String),
    Value(f64),
    Voltage(f64),
    Waveform(Vec<f64>),
    Weight(f64),
    WireLoad,
}

// -----------------------------------------------------------------------------

/// Unit
#[derive(Debug, PartialEq)]
pub enum Unit {
    Capacitance(String),
    Resistance(String),
    Time(String),
    Voltage(String),
    Current(String),
    Power(String),
}

// -----------------------------------------------------------------------------

pub(crate) fn sdc<I>(input: &mut I) -> ParseResult<Vec<Command>, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(parser(command)).parse_stream(input)
}

fn command<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let items = (
        attempt(parser(current_instance)),
        attempt(parser(set_version)),
        attempt(parser(set_units)),
        attempt(parser(set_operating_conditions)),
        attempt(parser(create_clock)),
        attempt(parser(create_generated_clock)),
        attempt(parser(group_path)),
    );
    choice(items).parse_stream(input)
}

pub(crate) fn sdc_env<'a, I>() -> LanguageEnv<'a, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
    I: 'a,
{
    LanguageEnv::new(LanguageDef {
        ident: Identifier {
            start: letter(),
            rest: choice((alpha_num(), one_of("_/@^#.|:$*?".chars()))),
            reserved: vec![],
        },
        op: Identifier {
            start: satisfy(|_| false),
            rest: satisfy(|_| false),
            reserved: vec![],
        },
        comment_start: satisfy(|_| false).map(|_| ()),
        comment_end: satisfy(|_| false).map(|_| ()),
        comment_line: string("#").map(|_| ()),
    })
}

// -----------------------------------------------------------------------------

fn current_instance<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let env = sdc_env();
    let command = env.symbol("current_instance");
    let ident = env.identifier().map(|x| Command::CurrentInstance(x));
    command.with(ident).parse_stream(input)
}

#[test]
fn test_current_instance() {
    let mut parser = parser(current_instance);
    let tgt = "current_instance dut";
    assert_eq!(
        Command::CurrentInstance(String::from("dut")),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

fn set_version<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let env = sdc_env();
    let command = env.symbol("set").with(env.symbol("version"));
    let version = env.float().map(|x| Command::SetVersion(x));
    command.with(version).parse_stream(input)
}

#[test]
fn test_set_version() {
    let mut parser = parser(set_version);
    let tgt = "set version 2.1";
    assert_eq!(Command::SetVersion(2.1), parser.parse(tgt).unwrap().0);
}

// -----------------------------------------------------------------------------

fn unit_value<I>(input: &mut I) -> ParseResult<String, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let env = sdc_env();
    let mut unit_value = optional(env.float())
        .and(env.identifier())
        .map(|(x, y)| match x {
            Some(x) => format!("{}{}", x, y),
            None => format!("{}", y),
        });
    unit_value.parse_stream(input)
}

fn set_units<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let env = sdc_env();
    let command = env.symbol("set_units");
    let cap = env
        .symbol("-capacitance")
        .with(parser(unit_value))
        .map(|x| Unit::Capacitance(x));
    let res = env
        .symbol("-resistance")
        .with(parser(unit_value))
        .map(|x| Unit::Resistance(x));
    let tim = env
        .symbol("-time")
        .with(parser(unit_value))
        .map(|x| Unit::Time(x));
    let vol = env
        .symbol("-voltage")
        .with(parser(unit_value))
        .map(|x| Unit::Voltage(x));
    let cur = env
        .symbol("-current")
        .with(parser(unit_value))
        .map(|x| Unit::Current(x));
    let pow = env
        .symbol("-power")
        .with(parser(unit_value))
        .map(|x| Unit::Power(x));
    let opts = (
        attempt(cap),
        attempt(res),
        attempt(tim),
        attempt(vol),
        attempt(cur),
        attempt(pow),
    );
    command
        .with(many1(choice(opts)))
        .map(|x| Command::SetUnits(x))
        .parse_stream(input)
}

#[test]
fn test_set_units() {
    let mut parser = parser(set_units);
    let tgt = "set_units -resistance 10MOhm -capacitance 1.2pF -time ns";
    assert_eq!(
        Command::SetUnits(vec![
            Unit::Resistance(String::from("10MOhm")),
            Unit::Capacitance(String::from("1.2pF")),
            Unit::Time(String::from("ns")),
        ]),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `create_clock`
#[derive(Debug, Default, PartialEq)]
pub struct CreateClock {
    pub period: f64,
    pub name: Option<String>,
    pub waveform: Vec<f64>,
    pub add: bool,
    pub comment: Option<String>,
    pub source_objects: Option<Object>,
}

fn create_clock<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let env = sdc_env();
    let command = env.symbol("create_clock");
    let period = env
        .symbol("-period")
        .with(env.float())
        .map(|x| CommandArg::Period(x));
    let name = env
        .symbol("-name")
        .with(env.identifier())
        .map(|x| CommandArg::Name(x));
    let waveform = env
        .symbol("-waveform")
        .with(env.braces(many1(env.float())))
        .map(|x| CommandArg::Waveform(x));
    let add = env.symbol("-add").map(|_| CommandArg::Add);
    let comment = env
        .symbol("-name")
        .with(env.string_literal())
        .map(|x| CommandArg::Comment(x));
    let source_objects = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(period),
        attempt(name),
        attempt(waveform),
        attempt(add),
        attempt(comment),
        attempt(source_objects),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<CommandArg>| {
            let mut period = None;
            let mut name = None;
            let mut waveform = vec![];
            let mut add = false;
            let mut comment = None;
            let mut source_objects = None;
            for x in xs {
                match x {
                    CommandArg::Period(x) => period = Some(x),
                    CommandArg::Name(x) => name = Some(x),
                    CommandArg::Waveform(x) => waveform = x,
                    CommandArg::Add => add = true,
                    CommandArg::Comment(x) => comment = Some(x),
                    CommandArg::Object(x) => source_objects = Some(x),
                    _ => unreachable!(),
                }
            }
            let period = period.unwrap();
            Command::CreateClock(CreateClock {
                period,
                name,
                waveform,
                add,
                comment,
                source_objects,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_create_clock() {
    let mut parser = parser(command);
    let tgt = "create_clock -period 10";
    assert_eq!(
        Command::CreateClock(CreateClock {
            period: 10.0,
            ..Default::default()
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `create_generated_clock`
#[derive(Debug, Default, PartialEq)]
pub struct CreateGeneratedClock {
    pub name: Option<String>,
    pub source: Object,
    pub edges: Vec<f64>,
    pub divide_by: Option<f64>,
    pub multiply_by: Option<f64>,
    pub duty_cycle: Option<f64>,
    pub invert: bool,
    pub edge_shift: Vec<f64>,
    pub add: bool,
    pub master_clock: Option<Object>,
    pub combinational: bool,
    pub comment: Option<String>,
    pub source_objects: Object,
}

fn create_generated_clock<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let env = sdc_env();
    let command = env.symbol("create_generated_clock");
    let name = env
        .symbol("-name")
        .with(env.identifier())
        .map(|x| CommandArg::Name(x));
    let source = env
        .symbol("-source")
        .with(parser(object))
        .map(|x| CommandArg::SourceObj(x));
    let edges = env
        .symbol("-edges")
        .with(env.braces(many1(env.float())))
        .map(|x| CommandArg::Edges(x));
    let divide_by = env
        .symbol("-divide_by")
        .with(env.float())
        .map(|x| CommandArg::DivideBy(x));
    let multiply_by = env
        .symbol("-multiply_by")
        .with(env.float())
        .map(|x| CommandArg::MultiplyBy(x));
    let duty_cycle = env
        .symbol("-duty_cycle")
        .with(env.float())
        .map(|x| CommandArg::DutyCycle(x));
    let invert = env.symbol("-invert").map(|_| CommandArg::Invert);
    let edge_shift = env
        .symbol("-edge_shift")
        .with(env.braces(many1(env.float())))
        .map(|x| CommandArg::EdgeShift(x));
    let add = env.symbol("-add").map(|_| CommandArg::Add);
    let master_clock = env
        .symbol("-master_clock")
        .with(parser(object))
        .map(|x| CommandArg::MasterClock(x));
    let combinational = env
        .symbol("-combinational")
        .map(|_| CommandArg::Combinational);
    let comment = env
        .symbol("-name")
        .with(env.string_literal())
        .map(|x| CommandArg::Comment(x));
    let source_objects = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(name),
        attempt(source),
        attempt(edges),
        attempt(divide_by),
        attempt(multiply_by),
        attempt(duty_cycle),
        attempt(invert),
        attempt(edge_shift),
        attempt(add),
        attempt(master_clock),
        attempt(combinational),
        attempt(comment),
        attempt(source_objects),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<CommandArg>| {
            let mut name = None;
            let mut source = None;
            let mut edges = vec![];
            let mut divide_by = None;
            let mut multiply_by = None;
            let mut duty_cycle = None;
            let mut invert = false;
            let mut edge_shift = vec![];
            let mut add = false;
            let mut master_clock = None;
            let mut combinational = false;
            let mut comment = None;
            let mut source_objects = None;
            for x in xs {
                match x {
                    CommandArg::Name(x) => name = Some(x),
                    CommandArg::SourceObj(x) => source = Some(x),
                    CommandArg::Edges(x) => edges = x,
                    CommandArg::DivideBy(x) => divide_by = Some(x),
                    CommandArg::MultiplyBy(x) => multiply_by = Some(x),
                    CommandArg::DutyCycle(x) => duty_cycle = Some(x),
                    CommandArg::Invert => invert = true,
                    CommandArg::EdgeShift(x) => edge_shift = x,
                    CommandArg::Add => add = true,
                    CommandArg::MasterClock(x) => master_clock = Some(x),
                    CommandArg::Combinational => combinational = true,
                    CommandArg::Comment(x) => comment = Some(x),
                    CommandArg::Object(x) => source_objects = Some(x),
                    _ => unreachable!(),
                }
            }
            let source = source.unwrap();
            let source_objects = source_objects.unwrap();
            Command::CreateGeneratedClock(CreateGeneratedClock {
                name,
                source,
                edges,
                divide_by,
                multiply_by,
                duty_cycle,
                invert,
                edge_shift,
                add,
                master_clock,
                combinational,
                comment,
                source_objects,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_create_generated_clock() {
    let mut parser = parser(command);
    let tgt = "create_generated_clock -divide_by 3 -source [get_pins a] [get_pins b]";
    assert_eq!(
        Command::CreateGeneratedClock(CreateGeneratedClock {
            source: Object::GetPins(GetPins {
                patterns: vec![String::from("a")],
                ..Default::default()
            }),
            divide_by: Some(3.0),
            source_objects: Object::GetPins(GetPins {
                patterns: vec![String::from("b")],
                ..Default::default()
            }),
            ..Default::default()
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `group_path`
#[derive(Debug, Default, PartialEq)]
pub struct GroupPath {
    pub name: Option<String>,
    pub default: bool,
    pub weight: Option<f64>,
    pub from: Option<Object>,
    pub rise_from: Option<Object>,
    pub fall_from: Option<Object>,
    pub to: Option<Object>,
    pub rise_to: Option<Object>,
    pub fall_to: Option<Object>,
    pub through: Option<Object>,
    pub rise_through: Option<Object>,
    pub fall_through: Option<Object>,
    pub comment: Option<String>,
}

fn group_path<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let env = sdc_env();
    let command = env.symbol("group_path");
    let name = env
        .symbol("-name")
        .with(env.identifier())
        .map(|x| CommandArg::Name(x));
    let default = env.symbol("-default").map(|_| CommandArg::Default);
    let weight = env
        .symbol("-weight")
        .with(env.float())
        .map(|x| CommandArg::Weight(x));
    let from = env
        .symbol("-from")
        .with(parser(object))
        .map(|x| CommandArg::From(x));
    let rise_from = env
        .symbol("-rise_from")
        .with(parser(object))
        .map(|x| CommandArg::RiseFrom(x));
    let fall_from = env
        .symbol("-fall_from")
        .with(parser(object))
        .map(|x| CommandArg::FallFrom(x));
    let to = env
        .symbol("-to")
        .with(parser(object))
        .map(|x| CommandArg::To(x));
    let rise_to = env
        .symbol("-rise_to")
        .with(parser(object))
        .map(|x| CommandArg::RiseTo(x));
    let fall_to = env
        .symbol("-fall_to")
        .with(parser(object))
        .map(|x| CommandArg::FallTo(x));
    let through = env
        .symbol("-through")
        .with(parser(object))
        .map(|x| CommandArg::Through(x));
    let rise_through = env
        .symbol("-rise_through")
        .with(parser(object))
        .map(|x| CommandArg::RiseThrough(x));
    let fall_through = env
        .symbol("-fall_through")
        .with(parser(object))
        .map(|x| CommandArg::FallThrough(x));
    let comment = env
        .symbol("-name")
        .with(env.string_literal())
        .map(|x| CommandArg::Comment(x));
    let args = (
        attempt(name),
        attempt(default),
        attempt(weight),
        attempt(from),
        attempt(rise_from),
        attempt(fall_from),
        attempt(to),
        attempt(rise_to),
        attempt(fall_to),
        attempt(through),
        attempt(rise_through),
        attempt(fall_through),
        attempt(comment),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<CommandArg>| {
            let mut name = None;
            let mut default = false;
            let mut weight = None;
            let mut from = None;
            let mut rise_from = None;
            let mut fall_from = None;
            let mut to = None;
            let mut rise_to = None;
            let mut fall_to = None;
            let mut through = None;
            let mut rise_through = None;
            let mut fall_through = None;
            let mut comment = None;
            for x in xs {
                match x {
                    CommandArg::Name(x) => name = Some(x),
                    CommandArg::Default => default = true,
                    CommandArg::Weight(x) => weight = Some(x),
                    CommandArg::From(x) => from = Some(x),
                    CommandArg::RiseFrom(x) => rise_from = Some(x),
                    CommandArg::FallFrom(x) => fall_from = Some(x),
                    CommandArg::To(x) => to = Some(x),
                    CommandArg::RiseTo(x) => rise_to = Some(x),
                    CommandArg::FallTo(x) => fall_to = Some(x),
                    CommandArg::Through(x) => through = Some(x),
                    CommandArg::RiseThrough(x) => rise_through = Some(x),
                    CommandArg::FallThrough(x) => fall_through = Some(x),
                    CommandArg::Comment(x) => comment = Some(x),
                    _ => unreachable!(),
                }
            }
            Command::GroupPath(GroupPath {
                name,
                default,
                weight,
                from,
                rise_from,
                fall_from,
                to,
                rise_to,
                fall_to,
                through,
                rise_through,
                fall_through,
                comment,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_group_path() {
    let mut parser = parser(command);
    let tgt = "group_path -from [get_pins a]";
    assert_eq!(
        Command::GroupPath(GroupPath {
            from: Some(Object::GetPins(GetPins {
                patterns: vec![String::from("a")],
                ..Default::default()
            })),
            ..Default::default()
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_operating_conditions`
#[derive(Debug, PartialEq)]
pub struct SetOperatingConditions {
    pub library: Option<Object>,
}

fn set_operating_conditions<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let env = sdc_env();
    let command = env.symbol("set_operating_conditions");
    let library = env.symbol("-library").and(parser(object));
    let opts = (attempt(library),);
    command
        .with(many(choice(opts)))
        .map(|xs: Vec<(&'static str, Object)>| {
            let mut library = None;
            for (s, x) in xs {
                match &*s {
                    "-library" => library = Some(x),
                    _ => unreachable!(),
                }
            }
            Command::SetOperatingConditions(SetOperatingConditions { library })
        })
        .parse_stream(input)
}

#[test]
fn test_set_operating_conditions() {
    let mut parser = parser(set_operating_conditions);
    let tgt =
        "set_operating_conditions -library [get_libs {cb13fs120_tsmc_max.db:cb13fs120_tsmc_max}]";
    assert_eq!(
        Command::SetOperatingConditions(SetOperatingConditions {
            library: Some(Object::GetLibs(GetLibs {
                regexp: false,
                nocase: false,
                patterns: vec![String::from("cb13fs120_tsmc_max.db:cb13fs120_tsmc_max")]
            }))
        }),
        parser.parse(tgt).unwrap().0
    );
}
