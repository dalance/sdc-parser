use crate::object::*;
use crate::util::*;
use combine::char::{char, space, string};
use combine::error::{ParseError, ParseResult};
use combine::parser::Parser;
use combine::{attempt, choice, look_ahead, many, many1, none_of, optional, parser, token, Stream};

// -----------------------------------------------------------------------------

/// A type representing sdc
#[derive(Debug, Default, PartialEq)]
pub struct Sdc {
    pub commands: Vec<Command>,
}

// -----------------------------------------------------------------------------

/// Enumeration on sdc command
#[derive(Debug, PartialEq)]
pub enum Command {
    LineBreak,
    Comment(String),
    CreateClock(CreateClock),
    CreateGeneratedClock(CreateGeneratedClock),
    CreateVoltageArea(CreateVoltageArea),
    CurrentInstance(CurrentInstance),
    GroupPath(GroupPath),
    Set(Set),
    SetCaseAnalysis(SetCaseAnalysis),
    SetClockGatingCheck(SetClockGatingCheck),
    SetClockGroups(SetClockGroups),
    SetClockLatency(SetClockLatency),
    SetClockTransition(SetClockTransition),
    SetClockUncertainty(SetClockUncertainty),
    SetDataCheck(SetDataCheck),
    SetDisableTiming(SetDisableTiming),
    SetDrive(SetDrive),
    SetDrivingCell(SetDrivingCell),
    SetFalsePath(SetFalsePath),
    SetFanoutLoad(SetFanoutLoad),
    SetIdealLatency(SetIdealLatency),
    SetIdealNetwork(SetIdealNetwork),
    SetIdealTransition(SetIdealTransition),
    SetInputDelay(SetInputDelay),
    SetInputTransition(SetInputTransition),
    SetLevelShifterStrategy(SetLevelShifterStrategy),
    SetLevelShifterThreshold(SetLevelShifterThreshold),
    SetLoad(SetLoad),
    SetLogicDc(SetLogicDc),
    SetLogicOne(SetLogicOne),
    SetLogicZero(SetLogicZero),
    SetMaxArea(SetMaxArea),
    SetMaxCapacitance(SetMaxCapacitance),
    SetMaxDelay(SetMaxDelay),
    SetMaxDynamicPower(SetMaxDynamicPower),
    SetMaxFanout(SetMaxFanout),
    SetMaxLeakagePower(SetMaxLeakagePower),
    SetMaxTimeBorrow(SetMaxTimeBorrow),
    SetMaxTransition(SetMaxTransition),
    SetMinCapacitance(SetMinCapacitance),
    SetMinDelay(SetMinDelay),
    SetMinPorosity(SetMinPorosity),
    SetMinPulseWidth(SetMinPulseWidth),
    SetMulticyclePath(SetMulticyclePath),
    SetOperatingConditions(SetOperatingConditions),
    SetOutputDelay(SetOutputDelay),
    SetPortFanoutNumber(SetPortFanoutNumber),
    SetPropagatedClock(SetPropagatedClock),
    SetResistance(SetResistance),
    SetSense(SetSense),
    SetTimingDerate(SetTimingDerate),
    SetUnits(SetUnits),
    SetSdcVersion(f64),
    SetVoltage(SetVoltage),
    SetWireLoadMinBlockSize(SetWireLoadMinBlockSize),
    SetWireLoadMode(SetWireLoadMode),
    SetWireLoadModel(SetWireLoadModel),
    SetWireLoadSelectionGroup(SetWireLoadSelectionGroup),
    Whitespace,
}

// -----------------------------------------------------------------------------

enum CommandArg {
    Add,
    AddDelay,
    AllowPaths,
    AnalysisType(String),
    Asynchronous,
    CaseValue(CaseValue),
    Capacitance(UnitValue),
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
    Coordinate(Vec<f64>),
    Current(UnitValue),
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
    Hold,
    HoldVal(f64),
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
    MaxStr(String),
    MaxLibrary(Object),
    Min,
    MinStr(String),
    MinLibrary(Object),
    MinVal(f64),
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
    PhysicallyExclusive,
    Pin(Object),
    PinLoad,
    Positive,
    Power(UnitValue),
    Pulse(String),
    ReferencePin(Object),
    Resistance(UnitValue),
    Rise,
    RiseFrom(Object),
    RiseThrough(Object),
    RiseTo(Object),
    Rule(String),
    Setup,
    SetupVal(f64),
    Source,
    SourceLatencyIncluded,
    SourceObj(Object),
    Start,
    Static,
    StopPropagation,
    String(String),
    SubtractPinLoad,
    Through(Object),
    Time(UnitValue),
    To(Object),
    Type(String),
    Value(f64),
    Voltage(f64),
    VoltageUV(UnitValue),
    Waveform(Vec<f64>),
    Weight(f64),
    WireLoad,
}

// -----------------------------------------------------------------------------

pub(crate) fn sdc<I>(input: &mut I) -> ParseResult<Sdc, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(parser(command))
        .map(|x| Sdc { commands: x })
        .parse_stream(input)
}

fn command<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let c = (
        attempt(parser(create_clock)),
        attempt(parser(create_generated_clock)),
        attempt(parser(create_voltage_area)),
        attempt(parser(current_instance)),
    );
    let g = (parser(group_path),);
    let set = (attempt(parser(set_sdc_version)), attempt(parser(set)));
    let set_ca = (parser(set_case_analysis),);
    let set_cl = (
        attempt(parser(set_clock_gating_check)),
        attempt(parser(set_clock_groups)),
        attempt(parser(set_clock_latency)),
        attempt(parser(set_clock_transition)),
        attempt(parser(set_clock_uncertainty)),
    );
    let set_d = (
        attempt(parser(set_data_check)),
        attempt(parser(set_disable_timing)),
        attempt(parser(set_drive)),
        attempt(parser(set_driving_cell)),
    );
    let set_f = (
        attempt(parser(set_false_path)),
        attempt(parser(set_fanout_load)),
    );
    let set_id = (
        attempt(parser(set_ideal_latency)),
        attempt(parser(set_ideal_network)),
        attempt(parser(set_ideal_transition)),
    );
    let set_in = (
        attempt(parser(set_input_delay)),
        attempt(parser(set_input_transition)),
    );
    let set_le = (
        attempt(parser(set_level_shifter_strategy)),
        attempt(parser(set_level_shifter_threshold)),
    );
    let set_lo = (
        attempt(parser(set_load)),
        attempt(parser(set_logic_dc)),
        attempt(parser(set_logic_one)),
        attempt(parser(set_logic_zero)),
    );
    let set_ma = (
        attempt(parser(set_max_delay)),
        attempt(parser(set_max_time_borrow)),
        attempt(parser(set_max_area)),
        attempt(parser(set_max_capacitance)),
        attempt(parser(set_max_fanout)),
        attempt(parser(set_max_transition)),
        attempt(parser(set_max_dynamic_power)),
        attempt(parser(set_max_leakage_power)),
    );
    let set_mi = (
        attempt(parser(set_min_delay)),
        attempt(parser(set_min_pulse_width)),
        attempt(parser(set_min_capacitance)),
        attempt(parser(set_min_porosity)),
    );
    let set_mu = (parser(set_multicycle_path),);
    let set_o = (
        attempt(parser(set_operating_conditions)),
        attempt(parser(set_output_delay)),
    );
    let set_p = (
        attempt(parser(set_propagated_clock)),
        attempt(parser(set_port_fanout_number)),
    );
    let set_w = (
        attempt(parser(set_wire_load_min_block_size)),
        attempt(parser(set_wire_load_model)),
        attempt(parser(set_wire_load_mode)),
        attempt(parser(set_wire_load_selection_group)),
    );
    let set__ = (
        attempt(parser(set_resistance)),
        attempt(parser(set_sense)),
        attempt(parser(set_timing_derate)),
        attempt(parser(set_units)),
        attempt(parser(set_voltage)),
    );

    choice((
        look_ahead(space()).with(parser(whitespace)),
        look_ahead(char('\n')).with(parser(linebreak)),
        look_ahead(string("\r\n")).with(parser(linebreak)),
        look_ahead(char('#')).with(parser(comment)),
        look_ahead(char('c')).with(choice(c)),
        look_ahead(char('g')).with(choice(g)),
        attempt(look_ahead(string("set ")).with(choice(set))),
        attempt(look_ahead(string("set_ca")).with(choice(set_ca))),
        attempt(look_ahead(string("set_cl")).with(choice(set_cl))),
        attempt(look_ahead(string("set_d")).with(choice(set_d))),
        attempt(look_ahead(string("set_f")).with(choice(set_f))),
        attempt(look_ahead(string("set_id")).with(choice(set_id))),
        attempt(look_ahead(string("set_in")).with(choice(set_in))),
        attempt(look_ahead(string("set_le")).with(choice(set_le))),
        attempt(look_ahead(string("set_lo")).with(choice(set_lo))),
        attempt(look_ahead(string("set_ma")).with(choice(set_ma))),
        attempt(look_ahead(string("set_mi")).with(choice(set_mi))),
        attempt(look_ahead(string("set_mu")).with(choice(set_mu))),
        attempt(look_ahead(string("set_o")).with(choice(set_o))),
        attempt(look_ahead(string("set_p")).with(choice(set_p))),
        attempt(look_ahead(string("set_w")).with(choice(set_w))),
        choice(set__),
    ))
    .parse_stream(input)
}

// -----------------------------------------------------------------------------

fn whitespace<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let mut command = lex(space()).map(|_| Command::Whitespace);
    command.parse_stream(input)
}

// -----------------------------------------------------------------------------

fn linebreak<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let mut command = lex(string("\n").or(string("\r\n"))).map(|_| Command::LineBreak);
    command.parse_stream(input)
}

// -----------------------------------------------------------------------------

fn comment<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let mut command = token('#')
        .and(many(none_of("\n".chars())))
        .map(|(_, x)| Command::Comment(x));
    command.parse_stream(input)
}

// -----------------------------------------------------------------------------

/// A type containing information of `current_instance`
#[derive(Debug, Default, PartialEq)]
pub struct CurrentInstance {
    pub instance: Option<String>,
}

fn current_instance<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("current_instance");
    let instance = item().map(|x| CommandArg::String(x));
    let args = (attempt(instance),);
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut instance = None;
            for x in xs {
                match x {
                    CommandArg::String(x) => instance = Some(x),
                    _ => unreachable!(),
                }
            }
            Command::CurrentInstance(CurrentInstance { instance })
        })
        .parse_stream(input)
}

#[test]
fn test_current_instance() {
    let mut parser = parser(command);
    let tgt = "current_instance dut";
    assert_eq!(
        Command::CurrentInstance(CurrentInstance {
            instance: Some(String::from("dut")),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

fn set_sdc_version<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set").with(symbol("sdc_version"));
    let version = float().map(|x| Command::SetSdcVersion(x));
    command.with(version).parse_stream(input)
}

#[test]
fn test_set_sdc_version() {
    let mut parser = parser(command);
    let tgt = "set sdc_version 2.1";
    assert_eq!(Command::SetSdcVersion(2.1), parser.parse(tgt).unwrap().0);
}

// -----------------------------------------------------------------------------

/// A type containing information of `set`
#[derive(Debug, Default, PartialEq)]
pub struct Set {
    pub variable_name: String,
    pub value: Object,
}

fn set<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let mut command = symbol("set")
        .with(item())
        .and(parser(object))
        .map(|(x, y)| {
            Command::Set(Set {
                variable_name: x,
                value: y,
            })
        });
    command.parse_stream(input)
}

#[test]
fn test_set() {
    let mut parser = parser(command);
    let tgt = "set a b";
    assert_eq!(
        Command::Set(Set {
            variable_name: String::from("a"),
            value: Object::String(vec![String::from("b")])
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_units`
#[derive(Debug, Default, PartialEq)]
pub struct SetUnits {
    pub capacitance: Option<UnitValue>,
    pub resistance: Option<UnitValue>,
    pub time: Option<UnitValue>,
    pub voltage: Option<UnitValue>,
    pub current: Option<UnitValue>,
    pub power: Option<UnitValue>,
}

/// UnitValue
#[derive(Debug, Default, PartialEq)]
pub struct UnitValue {
    pub unit: String,
    pub value: f64,
}

fn unit_value<I>(input: &mut I) -> ParseResult<UnitValue, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let mut unit_value = optional(float()).and(item()).map(|(x, y)| match x {
        Some(x) => UnitValue { value: x, unit: y },
        None => UnitValue {
            value: 1.0,
            unit: y,
        },
    });
    unit_value.parse_stream(input)
}

fn set_units<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = attempt(symbol("set_units")).or(symbol("set_unit"));
    let capacitance = symbol("-capacitance")
        .with(parser(unit_value))
        .map(|x| CommandArg::Capacitance(x));
    let resistance = symbol("-resistance")
        .with(parser(unit_value))
        .map(|x| CommandArg::Resistance(x));
    let time = symbol("-time")
        .with(parser(unit_value))
        .map(|x| CommandArg::Time(x));
    let voltage = symbol("-voltage")
        .with(parser(unit_value))
        .map(|x| CommandArg::VoltageUV(x));
    let current = symbol("-current")
        .with(parser(unit_value))
        .map(|x| CommandArg::Current(x));
    let power = symbol("-power")
        .with(parser(unit_value))
        .map(|x| CommandArg::Power(x));
    let args = (
        attempt(capacitance),
        attempt(resistance),
        attempt(time),
        attempt(voltage),
        attempt(current),
        attempt(power),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut capacitance = None;
            let mut resistance = None;
            let mut time = None;
            let mut voltage = None;
            let mut current = None;
            let mut power = None;
            for x in xs {
                match x {
                    CommandArg::Capacitance(x) => capacitance = Some(x),
                    CommandArg::Resistance(x) => resistance = Some(x),
                    CommandArg::Time(x) => time = Some(x),
                    CommandArg::VoltageUV(x) => voltage = Some(x),
                    CommandArg::Current(x) => current = Some(x),
                    CommandArg::Power(x) => power = Some(x),
                    _ => unreachable!(),
                }
            }
            Command::SetUnits(SetUnits {
                capacitance,
                resistance,
                time,
                voltage,
                current,
                power,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_units() {
    let mut parser = parser(command);
    let tgt =
        "set_units -capacitance 1.2pF -resistance 10MOhm -time ns -voltage V -current mA -power mW";
    assert_eq!(
        Command::SetUnits(SetUnits {
            capacitance: Some(UnitValue {
                value: 1.2,
                unit: String::from("pF")
            }),
            resistance: Some(UnitValue {
                value: 10.0,
                unit: String::from("MOhm")
            }),
            time: Some(UnitValue {
                value: 1.0,
                unit: String::from("ns")
            }),
            voltage: Some(UnitValue {
                value: 1.0,
                unit: String::from("V")
            }),
            current: Some(UnitValue {
                value: 1.0,
                unit: String::from("mA")
            }),
            power: Some(UnitValue {
                value: 1.0,
                unit: String::from("mW")
            }),
        }),
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
    let command = symbol("create_clock");
    let period = symbol("-period")
        .with(float())
        .map(|x| CommandArg::Period(x));
    let name = symbol("-name").with(item()).map(|x| CommandArg::Name(x));
    let waveform = symbol("-waveform")
        .with(braces(many1(float())))
        .map(|x| CommandArg::Waveform(x));
    let add = symbol("-add").map(|_| CommandArg::Add);
    let comment = symbol("-comment")
        .with(item())
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
        .map(|xs: Vec<_>| {
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
            let period = period.expect("create_clock:period");
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
    let tgt = "create_clock -period 10 -name clk -waveform {0 5} -add -comment \"aaa\" source";
    assert_eq!(
        Command::CreateClock(CreateClock {
            period: 10.0,
            name: Some(String::from("clk")),
            waveform: vec![0.0, 5.0],
            add: true,
            comment: Some(String::from("aaa")),
            source_objects: Some(Object::String(vec![String::from("source")])),
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
    let command = symbol("create_generated_clock");
    let name = symbol("-name").with(item()).map(|x| CommandArg::Name(x));
    let source = symbol("-source")
        .with(parser(object))
        .map(|x| CommandArg::SourceObj(x));
    let edges = symbol("-edges")
        .with(braces(many1(float())))
        .map(|x| CommandArg::Edges(x));
    let divide_by = symbol("-divide_by")
        .with(float())
        .map(|x| CommandArg::DivideBy(x));
    let multiply_by = symbol("-multiply_by")
        .with(float())
        .map(|x| CommandArg::MultiplyBy(x));
    let duty_cycle = symbol("-duty_cycle")
        .with(float())
        .map(|x| CommandArg::DutyCycle(x));
    let invert = symbol("-invert").map(|_| CommandArg::Invert);
    let edge_shift = symbol("-edge_shift")
        .with(braces(many1(float())))
        .map(|x| CommandArg::EdgeShift(x));
    let add = symbol("-add").map(|_| CommandArg::Add);
    let master_clock = symbol("-master_clock")
        .with(parser(object))
        .map(|x| CommandArg::MasterClock(x));
    let combinational = symbol("-combinational").map(|_| CommandArg::Combinational);
    let comment = symbol("-comment")
        .with(item())
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
        .map(|xs: Vec<_>| {
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
            let source = source.expect("create_generated_clock:source");
            let source_objects = source_objects.expect("create_generated_clock:source_objects");
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
    let tgt = "create_generated_clock -name clk -source src -edges {0 0.5} -divide_by 3 -multiply_by 2 -duty_cycle 0.4 -invert -edge_shift {0 1} -add -master_clock mclk -combinational -comment \"aaa\" clk";
    assert_eq!(
        Command::CreateGeneratedClock(CreateGeneratedClock {
            name: Some(String::from("clk")),
            source: Object::String(vec![String::from("src")]),
            edges: vec![0.0, 0.5],
            divide_by: Some(3.0),
            multiply_by: Some(2.0),
            duty_cycle: Some(0.4),
            invert: true,
            edge_shift: vec![0.0, 1.0],
            add: true,
            master_clock: Some(Object::String(vec![String::from("mclk")])),
            combinational: true,
            comment: Some(String::from("aaa")),
            source_objects: Object::String(vec![String::from("clk")]),
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
    let command = symbol("group_path");
    let name = symbol("-name").with(item()).map(|x| CommandArg::Name(x));
    let default = symbol("-default").map(|_| CommandArg::Default);
    let weight = symbol("-weight")
        .with(float())
        .map(|x| CommandArg::Weight(x));
    let from = symbol("-from")
        .with(parser(object))
        .map(|x| CommandArg::From(x));
    let rise_from = symbol("-rise_from")
        .with(parser(object))
        .map(|x| CommandArg::RiseFrom(x));
    let fall_from = symbol("-fall_from")
        .with(parser(object))
        .map(|x| CommandArg::FallFrom(x));
    let to = symbol("-to")
        .with(parser(object))
        .map(|x| CommandArg::To(x));
    let rise_to = symbol("-rise_to")
        .with(parser(object))
        .map(|x| CommandArg::RiseTo(x));
    let fall_to = symbol("-fall_to")
        .with(parser(object))
        .map(|x| CommandArg::FallTo(x));
    let through = symbol("-through")
        .with(parser(object))
        .map(|x| CommandArg::Through(x));
    let rise_through = symbol("-rise_through")
        .with(parser(object))
        .map(|x| CommandArg::RiseThrough(x));
    let fall_through = symbol("-fall_through")
        .with(parser(object))
        .map(|x| CommandArg::FallThrough(x));
    let comment = symbol("-comment")
        .with(item())
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
        .map(|xs: Vec<_>| {
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
    let tgt = "group_path -name path -default -weight 2.0 -from a -rise_from a -fall_from a -to b -rise_to b -fall_to b -through c -rise_through c -fall_through c -comment \"aaa\"";
    assert_eq!(
        Command::GroupPath(GroupPath {
            name: Some(String::from("path")),
            default: true,
            weight: Some(2.0),
            from: Some(Object::String(vec![String::from("a")])),
            rise_from: Some(Object::String(vec![String::from("a")])),
            fall_from: Some(Object::String(vec![String::from("a")])),
            to: Some(Object::String(vec![String::from("b")])),
            rise_to: Some(Object::String(vec![String::from("b")])),
            fall_to: Some(Object::String(vec![String::from("b")])),
            through: Some(Object::String(vec![String::from("c")])),
            rise_through: Some(Object::String(vec![String::from("c")])),
            fall_through: Some(Object::String(vec![String::from("c")])),
            comment: Some(String::from("aaa")),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_clock_gating_check`
#[derive(Debug, Default, PartialEq)]
pub struct SetClockGatingCheck {
    pub setup: Option<f64>,
    pub hold: Option<f64>,
    pub rise: bool,
    pub fall: bool,
    pub high: bool,
    pub low: bool,
    pub object_list: Option<Object>,
}

fn set_clock_gating_check<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_clock_gating_check");
    let setup = symbol("-setup")
        .with(float())
        .map(|x| CommandArg::SetupVal(x));
    let hold = symbol("-hold")
        .with(float())
        .map(|x| CommandArg::HoldVal(x));
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let high = symbol("-high").map(|_| CommandArg::High);
    let low = symbol("-low").map(|_| CommandArg::Low);
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(setup),
        attempt(hold),
        attempt(rise),
        attempt(fall),
        attempt(high),
        attempt(low),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut setup = None;
            let mut hold = None;
            let mut rise = false;
            let mut fall = false;
            let mut high = false;
            let mut low = false;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::SetupVal(x) => setup = Some(x),
                    CommandArg::HoldVal(x) => hold = Some(x),
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::High => high = true,
                    CommandArg::Low => low = true,
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            Command::SetClockGatingCheck(SetClockGatingCheck {
                setup,
                hold,
                rise,
                fall,
                high,
                low,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_clock_gating_check() {
    let mut parser = parser(command);
    let tgt = "set_clock_gating_check -setup 1.2 -hold 0.5 -rise -fall -high -low a";
    assert_eq!(
        Command::SetClockGatingCheck(SetClockGatingCheck {
            setup: Some(1.2),
            hold: Some(0.5),
            rise: true,
            fall: true,
            high: true,
            low: true,
            object_list: Some(Object::String(vec![String::from("a")])),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_clock_groups`
#[derive(Debug, Default, PartialEq)]
pub struct SetClockGroups {
    pub group: Object,
    pub logically_exclusive: bool,
    pub physically_exclusive: bool,
    pub asynchronous: bool,
    pub allow_paths: bool,
    pub name: Option<String>,
    pub comment: Option<String>,
}

fn set_clock_groups<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = attempt(symbol("set_clock_groups")).or(symbol("set_clock_groups"));
    let group = symbol("-group")
        .with(parser(object))
        .map(|x| CommandArg::Group(x));
    let logically_exclusive =
        symbol("-logically_exclusive").map(|_| CommandArg::LogicallyExclusive);
    let physically_exclusive =
        symbol("-physically_exclusive").map(|_| CommandArg::PhysicallyExclusive);
    let asynchronous = symbol("-asynchronous").map(|_| CommandArg::Asynchronous);
    let allow_paths = symbol("-allow_paths").map(|_| CommandArg::AllowPaths);
    let name = symbol("-name").with(item()).map(|x| CommandArg::Name(x));
    let comment = symbol("-comment")
        .with(item())
        .map(|x| CommandArg::Comment(x));
    let args = (
        attempt(group),
        attempt(logically_exclusive),
        attempt(physically_exclusive),
        attempt(asynchronous),
        attempt(allow_paths),
        attempt(name),
        attempt(comment),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut group = None;
            let mut logically_exclusive = false;
            let mut physically_exclusive = false;
            let mut asynchronous = false;
            let mut allow_paths = false;
            let mut name = None;
            let mut comment = None;
            for x in xs {
                match x {
                    CommandArg::Group(x) => group = Some(x),
                    CommandArg::LogicallyExclusive => logically_exclusive = true,
                    CommandArg::PhysicallyExclusive => physically_exclusive = true,
                    CommandArg::Asynchronous => asynchronous = true,
                    CommandArg::AllowPaths => allow_paths = true,
                    CommandArg::Name(x) => name = Some(x),
                    CommandArg::Comment(x) => comment = Some(x),
                    _ => unreachable!(),
                }
            }
            let group = group.expect("set_clock_groups:group");
            Command::SetClockGroups(SetClockGroups {
                group,
                logically_exclusive,
                physically_exclusive,
                asynchronous,
                allow_paths,
                name,
                comment,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_clock_groups() {
    let mut parser = parser(command);
    let tgt = "set_clock_groups -group clk -logically_exclusive -physically_exclusive -asynchronous -allow_paths -name clk -comment \"aaa\"";
    assert_eq!(
        Command::SetClockGroups(SetClockGroups {
            group: Object::String(vec![String::from("clk")]),
            logically_exclusive: true,
            physically_exclusive: true,
            asynchronous: true,
            allow_paths: true,
            name: Some(String::from("clk")),
            comment: Some(String::from("aaa")),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_clock_latency`
#[derive(Debug, Default, PartialEq)]
pub struct SetClockLatency {
    pub rise: bool,
    pub fall: bool,
    pub min: bool,
    pub max: bool,
    pub source: bool,
    pub dynamic: bool,
    pub late: bool,
    pub early: bool,
    pub clock: Option<Object>,
    pub delay: f64,
    pub object_list: Object,
}

fn set_clock_latency<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_clock_latency");
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let source = symbol("-source").map(|_| CommandArg::Source);
    let dynamic = symbol("-dynamic").map(|_| CommandArg::Dynamic);
    let late = symbol("-late").map(|_| CommandArg::Late);
    let early = symbol("-early").map(|_| CommandArg::Early);
    let clock = symbol("-clock")
        .with(parser(object))
        .map(|x| CommandArg::ClockObj(x));
    let delay = float().map(|x| CommandArg::Value(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(rise),
        attempt(fall),
        attempt(min),
        attempt(max),
        attempt(source),
        attempt(dynamic),
        attempt(late),
        attempt(early),
        attempt(clock),
        attempt(delay),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut rise = false;
            let mut fall = false;
            let mut min = false;
            let mut max = false;
            let mut source = false;
            let mut dynamic = false;
            let mut late = false;
            let mut early = false;
            let mut delay = None;
            let mut clock = None;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::Source => source = true,
                    CommandArg::Dynamic => dynamic = true,
                    CommandArg::Late => late = true,
                    CommandArg::Early => early = true,
                    CommandArg::ClockObj(x) => clock = Some(x),
                    CommandArg::Value(x) => delay = Some(x),
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let delay = delay.expect("set_clock_latency:delay");
            let object_list = object_list.expect("set_clock_latency:object_list");
            Command::SetClockLatency(SetClockLatency {
                rise,
                fall,
                min,
                max,
                source,
                dynamic,
                late,
                early,
                clock,
                delay,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_clock_latency() {
    let mut parser = parser(command);
    let tgt =
        "set_clock_latency -rise -fall -min -max -source -dynamic -late -early -clock clk 0.12 obj";
    assert_eq!(
        Command::SetClockLatency(SetClockLatency {
            rise: true,
            fall: true,
            min: true,
            max: true,
            source: true,
            dynamic: true,
            late: true,
            early: true,
            clock: Some(Object::String(vec![String::from("clk")])),
            delay: 0.12,
            object_list: Object::String(vec![String::from("obj")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_sense`
#[derive(Debug, Default, PartialEq)]
pub struct SetSense {
    pub r#type: Option<String>,
    pub non_unate: bool,
    pub positive: bool,
    pub negative: bool,
    pub clock_leaf: bool,
    pub stop_propagation: bool,
    pub pulse: Option<String>,
    pub clocks: Option<Object>,
    pub pin_list: Object,
}

fn set_sense<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_sense");
    let r#type = symbol("-type").with(item()).map(|x| CommandArg::Type(x));
    let non_unate = symbol("-non_unate").map(|_| CommandArg::NonUnate);
    let positive = symbol("-positive").map(|_| CommandArg::Positive);
    let negative = symbol("-negative").map(|_| CommandArg::Negative);
    let clock_leaf = symbol("-clock_leaf").map(|_| CommandArg::ClockLeaf);
    let stop_propagation = symbol("-stop_propagation").map(|_| CommandArg::StopPropagation);
    let pulse = symbol("-pulse").with(item()).map(|x| CommandArg::Pulse(x));
    let clocks = symbol("-clocks")
        .with(parser(object))
        .map(|x| CommandArg::Clocks(x));
    let pin_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(r#type),
        attempt(non_unate),
        attempt(positive),
        attempt(negative),
        attempt(clock_leaf),
        attempt(stop_propagation),
        attempt(pulse),
        attempt(clocks),
        attempt(pin_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut r#type = None;
            let mut non_unate = false;
            let mut positive = false;
            let mut negative = false;
            let mut clock_leaf = false;
            let mut stop_propagation = false;
            let mut pulse = None;
            let mut clocks = None;
            let mut pin_list = None;
            for x in xs {
                match x {
                    CommandArg::Type(x) => r#type = Some(x),
                    CommandArg::NonUnate => non_unate = true,
                    CommandArg::Positive => positive = true,
                    CommandArg::Negative => negative = true,
                    CommandArg::ClockLeaf => clock_leaf = true,
                    CommandArg::StopPropagation => stop_propagation = true,
                    CommandArg::Pulse(x) => pulse = Some(x),
                    CommandArg::Clocks(x) => clocks = Some(x),
                    CommandArg::Object(x) => pin_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let pin_list = pin_list.expect("set_sense:pin_list");
            Command::SetSense(SetSense {
                r#type,
                non_unate,
                positive,
                negative,
                clock_leaf,
                stop_propagation,
                pulse,
                clocks,
                pin_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_sense() {
    let mut parser = parser(command);
    let tgt =
        "set_sense -type clock -non_unate -positive -negative -clock_leaf -stop_propagation -pulse a -clocks clk pin";
    assert_eq!(
        Command::SetSense(SetSense {
            r#type: Some(String::from("clock")),
            non_unate: true,
            positive: true,
            negative: true,
            clock_leaf: true,
            stop_propagation: true,
            pulse: Some(String::from("a")),
            clocks: Some(Object::String(vec![String::from("clk")])),
            pin_list: Object::String(vec![String::from("pin")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_clock_transition`
#[derive(Debug, Default, PartialEq)]
pub struct SetClockTransition {
    pub rise: bool,
    pub fall: bool,
    pub min: bool,
    pub max: bool,
    pub transition: f64,
    pub clock_list: Object,
}

fn set_clock_transition<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_clock_transition");
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let transition = float().map(|x| CommandArg::Value(x));
    let clock_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(rise),
        attempt(fall),
        attempt(min),
        attempt(max),
        attempt(transition),
        attempt(clock_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut rise = false;
            let mut fall = false;
            let mut min = false;
            let mut max = false;
            let mut transition = None;
            let mut clock_list = None;
            for x in xs {
                match x {
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::Value(x) => transition = Some(x),
                    CommandArg::Object(x) => clock_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let transition = transition.expect("set_clock_transition:transition");
            let clock_list = clock_list.expect("set_clock_transition:clock_list");
            Command::SetClockTransition(SetClockTransition {
                rise,
                fall,
                min,
                max,
                transition,
                clock_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_clock_transition() {
    let mut parser = parser(command);
    let tgt = "set_clock_transition -rise -fall -min -max 12e-3 clk";
    assert_eq!(
        Command::SetClockTransition(SetClockTransition {
            rise: true,
            fall: true,
            min: true,
            max: true,
            transition: 12e-3,
            clock_list: Object::String(vec![String::from("clk")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_clock_uncertainty`
#[derive(Debug, Default, PartialEq)]
pub struct SetClockUncertainty {
    pub from: Option<Object>,
    pub rise_from: Option<Object>,
    pub fall_from: Option<Object>,
    pub to: Option<Object>,
    pub rise_to: Option<Object>,
    pub fall_to: Option<Object>,
    pub rise: bool,
    pub fall: bool,
    pub setup: bool,
    pub hold: bool,
    pub uncertainty: f64,
    pub object_list: Option<Object>,
}

fn set_clock_uncertainty<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_clock_uncertainty");
    let from = symbol("-from")
        .with(parser(object))
        .map(|x| CommandArg::From(x));
    let rise_from = symbol("-rise_from")
        .with(parser(object))
        .map(|x| CommandArg::RiseFrom(x));
    let fall_from = symbol("-fall_from")
        .with(parser(object))
        .map(|x| CommandArg::FallFrom(x));
    let to = symbol("-to")
        .with(parser(object))
        .map(|x| CommandArg::To(x));
    let rise_to = symbol("-rise_to")
        .with(parser(object))
        .map(|x| CommandArg::RiseTo(x));
    let fall_to = symbol("-fall_to")
        .with(parser(object))
        .map(|x| CommandArg::FallTo(x));
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let setup = symbol("-setup").map(|_| CommandArg::Setup);
    let hold = symbol("-hold").map(|_| CommandArg::Hold);
    let uncertainty = float().map(|x| CommandArg::Value(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(from),
        attempt(rise_from),
        attempt(fall_from),
        attempt(to),
        attempt(rise_to),
        attempt(fall_to),
        attempt(rise),
        attempt(fall),
        attempt(setup),
        attempt(hold),
        attempt(uncertainty),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut from = None;
            let mut rise_from = None;
            let mut fall_from = None;
            let mut to = None;
            let mut rise_to = None;
            let mut fall_to = None;
            let mut rise = false;
            let mut fall = false;
            let mut setup = false;
            let mut hold = false;
            let mut uncertainty = None;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::From(x) => from = Some(x),
                    CommandArg::RiseFrom(x) => rise_from = Some(x),
                    CommandArg::FallFrom(x) => fall_from = Some(x),
                    CommandArg::To(x) => to = Some(x),
                    CommandArg::RiseTo(x) => rise_to = Some(x),
                    CommandArg::FallTo(x) => fall_to = Some(x),
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Setup => setup = true,
                    CommandArg::Hold => hold = true,
                    CommandArg::Value(x) => uncertainty = Some(x),
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let uncertainty = uncertainty.expect("set_clock_uncertainty:uncertainty");
            Command::SetClockUncertainty(SetClockUncertainty {
                from,
                rise_from,
                fall_from,
                to,
                rise_to,
                fall_to,
                rise,
                fall,
                setup,
                hold,
                uncertainty,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_clock_uncertainty() {
    let mut parser = parser(command);
    let tgt = "set_clock_uncertainty -from a -rise_from a -fall_from a -to a -rise_to a -fall_to a -rise -fall -setup -hold 0.1 a";
    assert_eq!(
        Command::SetClockUncertainty(SetClockUncertainty {
            from: Some(Object::String(vec![String::from("a")])),
            rise_from: Some(Object::String(vec![String::from("a")])),
            fall_from: Some(Object::String(vec![String::from("a")])),
            to: Some(Object::String(vec![String::from("a")])),
            rise_to: Some(Object::String(vec![String::from("a")])),
            fall_to: Some(Object::String(vec![String::from("a")])),
            rise: true,
            fall: true,
            setup: true,
            hold: true,
            uncertainty: 0.1,
            object_list: Some(Object::String(vec![String::from("a")])),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_data_check`
#[derive(Debug, Default, PartialEq)]
pub struct SetDataCheck {
    pub from: Option<Object>,
    pub to: Option<Object>,
    pub rise_from: Option<Object>,
    pub fall_from: Option<Object>,
    pub rise_to: Option<Object>,
    pub fall_to: Option<Object>,
    pub setup: bool,
    pub hold: bool,
    pub clock: Option<Object>,
    pub value: f64,
}

fn set_data_check<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_data_check");
    let from = symbol("-from")
        .with(parser(object))
        .map(|x| CommandArg::From(x));
    let to = symbol("-to")
        .with(parser(object))
        .map(|x| CommandArg::To(x));
    let rise_from = symbol("-rise_from")
        .with(parser(object))
        .map(|x| CommandArg::RiseFrom(x));
    let fall_from = symbol("-fall_from")
        .with(parser(object))
        .map(|x| CommandArg::FallFrom(x));
    let rise_to = symbol("-rise_to")
        .with(parser(object))
        .map(|x| CommandArg::RiseTo(x));
    let fall_to = symbol("-fall_to")
        .with(parser(object))
        .map(|x| CommandArg::FallTo(x));
    let setup = symbol("-setup").map(|_| CommandArg::Setup);
    let hold = symbol("-hold").map(|_| CommandArg::Hold);
    let clock = symbol("-clock")
        .with(parser(object))
        .map(|x| CommandArg::ClockObj(x));
    let value = float().map(|x| CommandArg::Value(x));
    let args = (
        attempt(from),
        attempt(to),
        attempt(rise_from),
        attempt(fall_from),
        attempt(rise_to),
        attempt(fall_to),
        attempt(setup),
        attempt(hold),
        attempt(clock),
        attempt(value),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut from = None;
            let mut to = None;
            let mut rise_from = None;
            let mut fall_from = None;
            let mut rise_to = None;
            let mut fall_to = None;
            let mut setup = false;
            let mut hold = false;
            let mut clock = None;
            let mut value = None;
            for x in xs {
                match x {
                    CommandArg::From(x) => from = Some(x),
                    CommandArg::To(x) => to = Some(x),
                    CommandArg::RiseFrom(x) => rise_from = Some(x),
                    CommandArg::FallFrom(x) => fall_from = Some(x),
                    CommandArg::RiseTo(x) => rise_to = Some(x),
                    CommandArg::FallTo(x) => fall_to = Some(x),
                    CommandArg::Setup => setup = true,
                    CommandArg::Hold => hold = true,
                    CommandArg::ClockObj(x) => clock = Some(x),
                    CommandArg::Value(x) => value = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_data_check:value");
            Command::SetDataCheck(SetDataCheck {
                from,
                to,
                rise_from,
                fall_from,
                rise_to,
                fall_to,
                setup,
                hold,
                clock,
                value,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_data_check() {
    let mut parser = parser(command);
    let tgt = "set_data_check -from a -to a -rise_from a -fall_from a -rise_to a -fall_to a -setup -hold -clock a 0.1";
    assert_eq!(
        Command::SetDataCheck(SetDataCheck {
            from: Some(Object::String(vec![String::from("a")])),
            to: Some(Object::String(vec![String::from("a")])),
            rise_from: Some(Object::String(vec![String::from("a")])),
            fall_from: Some(Object::String(vec![String::from("a")])),
            rise_to: Some(Object::String(vec![String::from("a")])),
            fall_to: Some(Object::String(vec![String::from("a")])),
            setup: true,
            hold: true,
            clock: Some(Object::String(vec![String::from("a")])),
            value: 0.1,
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_disable_timing`
#[derive(Debug, Default, PartialEq)]
pub struct SetDisableTiming {
    pub from: Option<Object>,
    pub to: Option<Object>,
    pub cell_pin_list: Object,
}

fn set_disable_timing<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_disable_timing");
    let from = symbol("-from")
        .with(parser(object))
        .map(|x| CommandArg::From(x));
    let to = symbol("-to")
        .with(parser(object))
        .map(|x| CommandArg::To(x));
    let cell_pin_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(from), attempt(to), attempt(cell_pin_list));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut from = None;
            let mut to = None;
            let mut cell_pin_list = None;
            for x in xs {
                match x {
                    CommandArg::From(x) => from = Some(x),
                    CommandArg::To(x) => to = Some(x),
                    CommandArg::Object(x) => cell_pin_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let cell_pin_list = cell_pin_list.expect("set_disable_timing:cell_pin_list");
            Command::SetDisableTiming(SetDisableTiming {
                from,
                to,
                cell_pin_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_disable_timing() {
    let mut parser = parser(command);
    let tgt = "set_disable_timing -from a -to a a";
    assert_eq!(
        Command::SetDisableTiming(SetDisableTiming {
            from: Some(Object::String(vec![String::from("a")])),
            to: Some(Object::String(vec![String::from("a")])),
            cell_pin_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_false_path`
#[derive(Debug, Default, PartialEq)]
pub struct SetFalsePath {
    pub setup: bool,
    pub hold: bool,
    pub rise: bool,
    pub fall: bool,
    pub from: Option<Object>,
    pub to: Option<Object>,
    pub through: Option<Object>,
    pub rise_from: Option<Object>,
    pub rise_to: Option<Object>,
    pub rise_through: Option<Object>,
    pub fall_from: Option<Object>,
    pub fall_to: Option<Object>,
    pub fall_through: Option<Object>,
    pub comment: Option<String>,
}

fn set_false_path<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_false_path");
    let setup = symbol("-setup").map(|_| CommandArg::Setup);
    let hold = symbol("-hold").map(|_| CommandArg::Hold);
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let from = symbol("-from")
        .with(parser(object))
        .map(|x| CommandArg::From(x));
    let to = symbol("-to")
        .with(parser(object))
        .map(|x| CommandArg::To(x));
    let through = symbol("-through")
        .with(parser(object))
        .map(|x| CommandArg::Through(x));
    let rise_from = symbol("-rise_from")
        .with(parser(object))
        .map(|x| CommandArg::RiseFrom(x));
    let rise_to = symbol("-rise_to")
        .with(parser(object))
        .map(|x| CommandArg::RiseTo(x));
    let rise_through = symbol("-rise_through")
        .with(parser(object))
        .map(|x| CommandArg::RiseThrough(x));
    let fall_from = symbol("-fall_from")
        .with(parser(object))
        .map(|x| CommandArg::FallFrom(x));
    let fall_to = symbol("-fall_to")
        .with(parser(object))
        .map(|x| CommandArg::FallTo(x));
    let fall_through = symbol("-fall_through")
        .with(parser(object))
        .map(|x| CommandArg::FallThrough(x));
    let comment = symbol("-comment")
        .with(item())
        .map(|x| CommandArg::Comment(x));
    let args = (
        attempt(setup),
        attempt(hold),
        attempt(from),
        attempt(to),
        attempt(through),
        attempt(rise_from),
        attempt(rise_to),
        attempt(rise_through),
        attempt(fall_from),
        attempt(fall_to),
        attempt(fall_through),
        attempt(rise),
        attempt(fall),
        attempt(comment),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut setup = false;
            let mut hold = false;
            let mut rise = false;
            let mut fall = false;
            let mut from = None;
            let mut to = None;
            let mut through = None;
            let mut rise_from = None;
            let mut rise_to = None;
            let mut rise_through = None;
            let mut fall_from = None;
            let mut fall_to = None;
            let mut fall_through = None;
            let mut comment = None;
            for x in xs {
                match x {
                    CommandArg::Setup => setup = true,
                    CommandArg::Hold => hold = true,
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::From(x) => from = Some(x),
                    CommandArg::To(x) => to = Some(x),
                    CommandArg::Through(x) => through = Some(x),
                    CommandArg::RiseFrom(x) => rise_from = Some(x),
                    CommandArg::RiseTo(x) => rise_to = Some(x),
                    CommandArg::RiseThrough(x) => rise_through = Some(x),
                    CommandArg::FallFrom(x) => fall_from = Some(x),
                    CommandArg::FallTo(x) => fall_to = Some(x),
                    CommandArg::FallThrough(x) => fall_through = Some(x),
                    CommandArg::Comment(x) => comment = Some(x),
                    _ => unreachable!(),
                }
            }
            Command::SetFalsePath(SetFalsePath {
                setup,
                hold,
                rise,
                fall,
                from,
                to,
                through,
                rise_from,
                rise_to,
                rise_through,
                fall_from,
                fall_to,
                fall_through,
                comment,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_false_path() {
    let mut parser = parser(command);
    let tgt = "set_false_path -setup -hold -rise -fall -from a -to a -through a -rise_from a -rise_to a -rise_through a -fall_from a -fall_to a -fall_through a -comment \"aaa\"";
    assert_eq!(
        Command::SetFalsePath(SetFalsePath {
            setup: true,
            hold: true,
            rise: true,
            fall: true,
            from: Some(Object::String(vec![String::from("a")])),
            to: Some(Object::String(vec![String::from("a")])),
            through: Some(Object::String(vec![String::from("a")])),
            rise_from: Some(Object::String(vec![String::from("a")])),
            rise_to: Some(Object::String(vec![String::from("a")])),
            rise_through: Some(Object::String(vec![String::from("a")])),
            fall_from: Some(Object::String(vec![String::from("a")])),
            fall_to: Some(Object::String(vec![String::from("a")])),
            fall_through: Some(Object::String(vec![String::from("a")])),
            comment: Some(String::from("aaa")),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_ideal_latency`
#[derive(Debug, Default, PartialEq)]
pub struct SetIdealLatency {
    pub rise: bool,
    pub fall: bool,
    pub min: bool,
    pub max: bool,
    pub delay: f64,
    pub object_list: Object,
}

fn set_ideal_latency<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_ideal_latency");
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let delay = float().map(|x| CommandArg::Value(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(rise),
        attempt(fall),
        attempt(min),
        attempt(max),
        attempt(delay),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut rise = false;
            let mut fall = false;
            let mut min = false;
            let mut max = false;
            let mut delay = None;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::Value(x) => delay = Some(x),
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let delay = delay.expect("set_ideal_latency:delay");
            let object_list = object_list.expect("set_ideal_latency:object_list");
            Command::SetIdealLatency(SetIdealLatency {
                rise,
                fall,
                min,
                max,
                delay,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_ideal_latency() {
    let mut parser = parser(command);
    let tgt = "set_ideal_latency -rise -fall -min -max 0.1 a";
    assert_eq!(
        Command::SetIdealLatency(SetIdealLatency {
            rise: true,
            fall: true,
            min: true,
            max: true,
            delay: 0.1,
            object_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_ideal_network`
#[derive(Debug, Default, PartialEq)]
pub struct SetIdealNetwork {
    pub no_propagate: bool,
    pub object_list: Object,
}

fn set_ideal_network<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_ideal_network");
    let no_propagate = symbol("-no_propagate").map(|_| CommandArg::NoPropagate);
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(no_propagate), attempt(object_list));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut no_propagate = false;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::NoPropagate => no_propagate = true,
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let object_list = object_list.expect("set_ideal_network:object_list");
            Command::SetIdealNetwork(SetIdealNetwork {
                no_propagate,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_ideal_network() {
    let mut parser = parser(command);
    let tgt = "set_ideal_network -no_propagate a";
    assert_eq!(
        Command::SetIdealNetwork(SetIdealNetwork {
            no_propagate: true,
            object_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_ideal_transition`
#[derive(Debug, Default, PartialEq)]
pub struct SetIdealTransition {
    pub rise: bool,
    pub fall: bool,
    pub min: bool,
    pub max: bool,
    pub transition_time: f64,
    pub object_list: Object,
}

fn set_ideal_transition<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_ideal_transition");
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let transition_time = float().map(|x| CommandArg::Value(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(rise),
        attempt(fall),
        attempt(min),
        attempt(max),
        attempt(transition_time),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut rise = false;
            let mut fall = false;
            let mut min = false;
            let mut max = false;
            let mut transition_time = None;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::Value(x) => transition_time = Some(x),
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let transition_time = transition_time.expect("set_ideal_transition:transition_time");
            let object_list = object_list.expect("set_ideal_transition:object_list");
            Command::SetIdealTransition(SetIdealTransition {
                rise,
                fall,
                min,
                max,
                transition_time,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_ideal_transition() {
    let mut parser = parser(command);
    let tgt = "set_ideal_transition -rise -fall -min -max 0.1 a";
    assert_eq!(
        Command::SetIdealTransition(SetIdealTransition {
            rise: true,
            fall: true,
            min: true,
            max: true,
            transition_time: 0.1,
            object_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_input_delay`
#[derive(Debug, Default, PartialEq)]
pub struct SetInputDelay {
    pub clock: Option<Object>,
    pub reference_pin: Option<Object>,
    pub clock_fall: bool,
    pub level_sensitive: bool,
    pub rise: bool,
    pub fall: bool,
    pub max: bool,
    pub min: bool,
    pub add_delay: bool,
    pub network_latency_included: bool,
    pub source_latency_included: bool,
    pub delay_value: f64,
    pub port_pin_list: Object,
}

fn set_input_delay<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_input_delay");
    let clock = symbol("-clock")
        .with(parser(object))
        .map(|x| CommandArg::ClockObj(x));
    let reference_pin = symbol("-reference_pin")
        .with(parser(object))
        .map(|x| CommandArg::ReferencePin(x));
    let clock_fall = symbol("-clock_fall").map(|_| CommandArg::ClockFall);
    let level_sensitive = symbol("-level_sensitive").map(|_| CommandArg::LevelSensitive);
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let min = symbol("-min").map(|_| CommandArg::Min);
    let add_delay = symbol("-add_delay").map(|_| CommandArg::AddDelay);
    let network_latency_included =
        symbol("-network_latency_included").map(|_| CommandArg::NetworkLatencyIncluded);
    let source_latency_included =
        symbol("-source_latency_included").map(|_| CommandArg::SourceLatencyIncluded);
    let delay_value = float().map(|x| CommandArg::Value(x));
    let port_pin_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(clock),
        attempt(reference_pin),
        attempt(clock_fall),
        attempt(level_sensitive),
        attempt(rise),
        attempt(fall),
        attempt(max),
        attempt(min),
        attempt(add_delay),
        attempt(network_latency_included),
        attempt(source_latency_included),
        attempt(delay_value),
        attempt(port_pin_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut clock = None;
            let mut reference_pin = None;
            let mut clock_fall = false;
            let mut level_sensitive = false;
            let mut rise = false;
            let mut fall = false;
            let mut max = false;
            let mut min = false;
            let mut add_delay = false;
            let mut network_latency_included = false;
            let mut source_latency_included = false;
            let mut delay_value = None;
            let mut port_pin_list = None;
            for x in xs {
                match x {
                    CommandArg::ClockObj(x) => clock = Some(x),
                    CommandArg::ReferencePin(x) => reference_pin = Some(x),
                    CommandArg::ClockFall => clock_fall = true,
                    CommandArg::LevelSensitive => level_sensitive = true,
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Max => max = true,
                    CommandArg::Min => min = true,
                    CommandArg::AddDelay => add_delay = true,
                    CommandArg::NetworkLatencyIncluded => network_latency_included = true,
                    CommandArg::SourceLatencyIncluded => source_latency_included = true,
                    CommandArg::Value(x) => delay_value = Some(x),
                    CommandArg::Object(x) => port_pin_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let delay_value = delay_value.expect("set_input_delay:delay_value");
            let port_pin_list = port_pin_list.expect("set_input_delay:port_pin_list");
            Command::SetInputDelay(SetInputDelay {
                clock,
                reference_pin,
                clock_fall,
                level_sensitive,
                rise,
                fall,
                max,
                min,
                add_delay,
                network_latency_included,
                source_latency_included,
                delay_value,
                port_pin_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_input_delay() {
    let mut parser = parser(command);
    let tgt = "set_input_delay -clock a -reference_pin a -clock_fall -level_sensitive -rise -fall -max -min -add_delay -network_latency_included -source_latency_included 0.1 a";
    assert_eq!(
        Command::SetInputDelay(SetInputDelay {
            clock: Some(Object::String(vec![String::from("a")])),
            reference_pin: Some(Object::String(vec![String::from("a")])),
            clock_fall: true,
            level_sensitive: true,
            rise: true,
            fall: true,
            min: true,
            max: true,
            add_delay: true,
            network_latency_included: true,
            source_latency_included: true,
            delay_value: 0.1,
            port_pin_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_max_delay`
#[derive(Debug, Default, PartialEq)]
pub struct SetMaxDelay {
    pub rise: bool,
    pub fall: bool,
    pub from: Option<Object>,
    pub to: Option<Object>,
    pub through: Option<Object>,
    pub rise_from: Option<Object>,
    pub rise_to: Option<Object>,
    pub rise_through: Option<Object>,
    pub fall_from: Option<Object>,
    pub fall_to: Option<Object>,
    pub fall_through: Option<Object>,
    pub ignore_clock_latency: bool,
    pub comment: Option<String>,
    pub delay_value: f64,
}

fn set_max_delay<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_max_delay");
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let from = symbol("-from")
        .with(parser(object))
        .map(|x| CommandArg::From(x));
    let to = symbol("-to")
        .with(parser(object))
        .map(|x| CommandArg::To(x));
    let through = symbol("-through")
        .with(parser(object))
        .map(|x| CommandArg::Through(x));
    let rise_from = symbol("-rise_from")
        .with(parser(object))
        .map(|x| CommandArg::RiseFrom(x));
    let rise_to = symbol("-rise_to")
        .with(parser(object))
        .map(|x| CommandArg::RiseTo(x));
    let rise_through = symbol("-rise_through")
        .with(parser(object))
        .map(|x| CommandArg::RiseThrough(x));
    let fall_from = symbol("-fall_from")
        .with(parser(object))
        .map(|x| CommandArg::FallFrom(x));
    let fall_to = symbol("-fall_to")
        .with(parser(object))
        .map(|x| CommandArg::FallTo(x));
    let fall_through = symbol("-fall_through")
        .with(parser(object))
        .map(|x| CommandArg::FallThrough(x));
    let ignore_clock_latency =
        symbol("-ignore_clock_latency").map(|_| CommandArg::IgnoreClockLatency);
    let comment = symbol("-comment")
        .with(item())
        .map(|x| CommandArg::Comment(x));
    let delay_value = float().map(|x| CommandArg::Value(x));
    let args = (
        attempt(from),
        attempt(to),
        attempt(through),
        attempt(rise_from),
        attempt(rise_to),
        attempt(rise_through),
        attempt(fall_from),
        attempt(fall_to),
        attempt(fall_through),
        attempt(rise),
        attempt(fall),
        attempt(ignore_clock_latency),
        attempt(comment),
        attempt(delay_value),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut rise = false;
            let mut fall = false;
            let mut from = None;
            let mut to = None;
            let mut through = None;
            let mut rise_from = None;
            let mut rise_to = None;
            let mut rise_through = None;
            let mut fall_from = None;
            let mut fall_to = None;
            let mut fall_through = None;
            let mut ignore_clock_latency = false;
            let mut comment = None;
            let mut delay_value = None;
            for x in xs {
                match x {
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::From(x) => from = Some(x),
                    CommandArg::To(x) => to = Some(x),
                    CommandArg::Through(x) => through = Some(x),
                    CommandArg::RiseFrom(x) => rise_from = Some(x),
                    CommandArg::RiseTo(x) => rise_to = Some(x),
                    CommandArg::RiseThrough(x) => rise_through = Some(x),
                    CommandArg::FallFrom(x) => fall_from = Some(x),
                    CommandArg::FallTo(x) => fall_to = Some(x),
                    CommandArg::FallThrough(x) => fall_through = Some(x),
                    CommandArg::IgnoreClockLatency => ignore_clock_latency = true,
                    CommandArg::Comment(x) => comment = Some(x),
                    CommandArg::Value(x) => delay_value = Some(x),
                    _ => unreachable!(),
                }
            }
            let delay_value = delay_value.expect("set_max_delay:delay_value");
            Command::SetMaxDelay(SetMaxDelay {
                rise,
                fall,
                from,
                to,
                through,
                rise_from,
                rise_to,
                rise_through,
                fall_from,
                fall_to,
                fall_through,
                ignore_clock_latency,
                comment,
                delay_value,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_max_delay() {
    let mut parser = parser(command);
    let tgt = "set_max_delay -rise -fall -from a -to a -through a -rise_from a -rise_to a -rise_through a -fall_from a -fall_to a -fall_through a -ignore_clock_latency -comment \"aaa\" 0.1";
    assert_eq!(
        Command::SetMaxDelay(SetMaxDelay {
            rise: true,
            fall: true,
            from: Some(Object::String(vec![String::from("a")])),
            to: Some(Object::String(vec![String::from("a")])),
            through: Some(Object::String(vec![String::from("a")])),
            rise_from: Some(Object::String(vec![String::from("a")])),
            rise_to: Some(Object::String(vec![String::from("a")])),
            rise_through: Some(Object::String(vec![String::from("a")])),
            fall_from: Some(Object::String(vec![String::from("a")])),
            fall_to: Some(Object::String(vec![String::from("a")])),
            fall_through: Some(Object::String(vec![String::from("a")])),
            ignore_clock_latency: true,
            comment: Some(String::from("aaa")),
            delay_value: 0.1,
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_max_time_borrow`
#[derive(Debug, Default, PartialEq)]
pub struct SetMaxTimeBorrow {
    pub delay_value: f64,
    pub object_list: Object,
}

fn set_max_time_borrow<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_max_time_borrow");
    let delay_value = float().map(|x| CommandArg::Value(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(delay_value), attempt(object_list));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut delay_value = None;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => delay_value = Some(x),
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let delay_value = delay_value.expect("set_max_time_borrow:delay_value");
            let object_list = object_list.expect("set_max_time_borrow:object_list");
            Command::SetMaxTimeBorrow(SetMaxTimeBorrow {
                delay_value,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_max_time_borrow() {
    let mut parser = parser(command);
    let tgt = "set_max_time_borrow 0.1 a";
    assert_eq!(
        Command::SetMaxTimeBorrow(SetMaxTimeBorrow {
            delay_value: 0.1,
            object_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_min_delay`
#[derive(Debug, Default, PartialEq)]
pub struct SetMinDelay {
    pub rise: bool,
    pub fall: bool,
    pub from: Option<Object>,
    pub to: Option<Object>,
    pub through: Option<Object>,
    pub rise_from: Option<Object>,
    pub rise_to: Option<Object>,
    pub rise_through: Option<Object>,
    pub fall_from: Option<Object>,
    pub fall_to: Option<Object>,
    pub fall_through: Option<Object>,
    pub ignore_clock_latency: bool,
    pub comment: Option<String>,
    pub delay_value: f64,
}

fn set_min_delay<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_min_delay");
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let from = symbol("-from")
        .with(parser(object))
        .map(|x| CommandArg::From(x));
    let to = symbol("-to")
        .with(parser(object))
        .map(|x| CommandArg::To(x));
    let through = symbol("-through")
        .with(parser(object))
        .map(|x| CommandArg::Through(x));
    let rise_from = symbol("-rise_from")
        .with(parser(object))
        .map(|x| CommandArg::RiseFrom(x));
    let rise_to = symbol("-rise_to")
        .with(parser(object))
        .map(|x| CommandArg::RiseTo(x));
    let rise_through = symbol("-rise_through")
        .with(parser(object))
        .map(|x| CommandArg::RiseThrough(x));
    let fall_from = symbol("-fall_from")
        .with(parser(object))
        .map(|x| CommandArg::FallFrom(x));
    let fall_to = symbol("-fall_to")
        .with(parser(object))
        .map(|x| CommandArg::FallTo(x));
    let fall_through = symbol("-fall_through")
        .with(parser(object))
        .map(|x| CommandArg::FallThrough(x));
    let ignore_clock_latency =
        symbol("-ignore_clock_latency").map(|_| CommandArg::IgnoreClockLatency);
    let comment = symbol("-comment")
        .with(item())
        .map(|x| CommandArg::Comment(x));
    let delay_value = float().map(|x| CommandArg::Value(x));
    let args = (
        attempt(from),
        attempt(to),
        attempt(through),
        attempt(rise_from),
        attempt(rise_to),
        attempt(rise_through),
        attempt(fall_from),
        attempt(fall_to),
        attempt(fall_through),
        attempt(rise),
        attempt(fall),
        attempt(ignore_clock_latency),
        attempt(comment),
        attempt(delay_value),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut rise = false;
            let mut fall = false;
            let mut from = None;
            let mut to = None;
            let mut through = None;
            let mut rise_from = None;
            let mut rise_to = None;
            let mut rise_through = None;
            let mut fall_from = None;
            let mut fall_to = None;
            let mut fall_through = None;
            let mut ignore_clock_latency = false;
            let mut comment = None;
            let mut delay_value = None;
            for x in xs {
                match x {
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::From(x) => from = Some(x),
                    CommandArg::To(x) => to = Some(x),
                    CommandArg::Through(x) => through = Some(x),
                    CommandArg::RiseFrom(x) => rise_from = Some(x),
                    CommandArg::RiseTo(x) => rise_to = Some(x),
                    CommandArg::RiseThrough(x) => rise_through = Some(x),
                    CommandArg::FallFrom(x) => fall_from = Some(x),
                    CommandArg::FallTo(x) => fall_to = Some(x),
                    CommandArg::FallThrough(x) => fall_through = Some(x),
                    CommandArg::IgnoreClockLatency => ignore_clock_latency = true,
                    CommandArg::Comment(x) => comment = Some(x),
                    CommandArg::Value(x) => delay_value = Some(x),
                    _ => unreachable!(),
                }
            }
            let delay_value = delay_value.expect("set_min_delay:delay_value");
            Command::SetMinDelay(SetMinDelay {
                rise,
                fall,
                from,
                to,
                through,
                rise_from,
                rise_to,
                rise_through,
                fall_from,
                fall_to,
                fall_through,
                ignore_clock_latency,
                comment,
                delay_value,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_min_delay() {
    let mut parser = parser(command);
    let tgt = "set_min_delay -rise -fall -from a -to a -through a -rise_from a -rise_to a -rise_through a -fall_from a -fall_to a -fall_through a -ignore_clock_latency -comment \"aaa\" 0.1";
    assert_eq!(
        Command::SetMinDelay(SetMinDelay {
            rise: true,
            fall: true,
            from: Some(Object::String(vec![String::from("a")])),
            to: Some(Object::String(vec![String::from("a")])),
            through: Some(Object::String(vec![String::from("a")])),
            rise_from: Some(Object::String(vec![String::from("a")])),
            rise_to: Some(Object::String(vec![String::from("a")])),
            rise_through: Some(Object::String(vec![String::from("a")])),
            fall_from: Some(Object::String(vec![String::from("a")])),
            fall_to: Some(Object::String(vec![String::from("a")])),
            fall_through: Some(Object::String(vec![String::from("a")])),
            ignore_clock_latency: true,
            comment: Some(String::from("aaa")),
            delay_value: 0.1,
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_min_pulse_width`
#[derive(Debug, Default, PartialEq)]
pub struct SetMinPulseWidth {
    pub low: bool,
    pub high: bool,
    pub value: f64,
    pub object_list: Option<Object>,
}

fn set_min_pulse_width<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_min_pulse_width");
    let low = symbol("-low").map(|_| CommandArg::Low);
    let high = symbol("-high").map(|_| CommandArg::High);
    let value = float().map(|x| CommandArg::Value(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(low),
        attempt(high),
        attempt(value),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut low = false;
            let mut high = false;
            let mut value = None;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::Low => low = true,
                    CommandArg::High => high = true,
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_min_pulse_width:value");
            Command::SetMinPulseWidth(SetMinPulseWidth {
                low,
                high,
                value,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_min_pulse_width() {
    let mut parser = parser(command);
    let tgt = "set_min_pulse_width -low -high 0.1 a";
    assert_eq!(
        Command::SetMinPulseWidth(SetMinPulseWidth {
            low: true,
            high: true,
            value: 0.1,
            object_list: Some(Object::String(vec![String::from("a")])),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_multicycle_path`
#[derive(Debug, Default, PartialEq)]
pub struct SetMulticyclePath {
    pub setup: bool,
    pub hold: bool,
    pub rise: bool,
    pub fall: bool,
    pub start: bool,
    pub end: bool,
    pub from: Option<Object>,
    pub to: Option<Object>,
    pub through: Option<Object>,
    pub rise_from: Option<Object>,
    pub rise_to: Option<Object>,
    pub rise_through: Option<Object>,
    pub fall_from: Option<Object>,
    pub fall_to: Option<Object>,
    pub fall_through: Option<Object>,
    pub comment: Option<String>,
    pub path_multiplier: f64,
}

fn set_multicycle_path<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_multicycle_path");
    let setup = symbol("-setup").map(|_| CommandArg::Setup);
    let hold = symbol("-hold").map(|_| CommandArg::Hold);
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let start = symbol("-start").map(|_| CommandArg::Start);
    let end = symbol("-end").map(|_| CommandArg::End);
    let from = symbol("-from")
        .with(parser(object))
        .map(|x| CommandArg::From(x));
    let to = symbol("-to")
        .with(parser(object))
        .map(|x| CommandArg::To(x));
    let through = symbol("-through")
        .with(parser(object))
        .map(|x| CommandArg::Through(x));
    let rise_from = symbol("-rise_from")
        .with(parser(object))
        .map(|x| CommandArg::RiseFrom(x));
    let rise_to = symbol("-rise_to")
        .with(parser(object))
        .map(|x| CommandArg::RiseTo(x));
    let rise_through = symbol("-rise_through")
        .with(parser(object))
        .map(|x| CommandArg::RiseThrough(x));
    let fall_from = symbol("-fall_from")
        .with(parser(object))
        .map(|x| CommandArg::FallFrom(x));
    let fall_to = symbol("-fall_to")
        .with(parser(object))
        .map(|x| CommandArg::FallTo(x));
    let fall_through = symbol("-fall_through")
        .with(parser(object))
        .map(|x| CommandArg::FallThrough(x));
    let comment = symbol("-comment")
        .with(item())
        .map(|x| CommandArg::Comment(x));
    let path_multiplier = float().map(|x| CommandArg::Value(x));
    let args = (
        attempt(setup),
        attempt(hold),
        attempt(start),
        attempt(end),
        attempt(from),
        attempt(to),
        attempt(through),
        attempt(rise_from),
        attempt(rise_to),
        attempt(rise_through),
        attempt(fall_from),
        attempt(fall_to),
        attempt(fall_through),
        attempt(rise),
        attempt(fall),
        attempt(comment),
        attempt(path_multiplier),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut setup = false;
            let mut hold = false;
            let mut rise = false;
            let mut fall = false;
            let mut start = false;
            let mut end = false;
            let mut from = None;
            let mut to = None;
            let mut through = None;
            let mut rise_from = None;
            let mut rise_to = None;
            let mut rise_through = None;
            let mut fall_from = None;
            let mut fall_to = None;
            let mut fall_through = None;
            let mut comment = None;
            let mut path_multiplier = None;
            for x in xs {
                match x {
                    CommandArg::Setup => setup = true,
                    CommandArg::Hold => hold = true,
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Start => start = true,
                    CommandArg::End => end = true,
                    CommandArg::From(x) => from = Some(x),
                    CommandArg::To(x) => to = Some(x),
                    CommandArg::Through(x) => through = Some(x),
                    CommandArg::RiseFrom(x) => rise_from = Some(x),
                    CommandArg::RiseTo(x) => rise_to = Some(x),
                    CommandArg::RiseThrough(x) => rise_through = Some(x),
                    CommandArg::FallFrom(x) => fall_from = Some(x),
                    CommandArg::FallTo(x) => fall_to = Some(x),
                    CommandArg::FallThrough(x) => fall_through = Some(x),
                    CommandArg::Comment(x) => comment = Some(x),
                    CommandArg::Value(x) => path_multiplier = Some(x),
                    _ => unreachable!(),
                }
            }
            let path_multiplier = path_multiplier.expect("set_multicycle_path:path_multiplier");
            Command::SetMulticyclePath(SetMulticyclePath {
                setup,
                hold,
                rise,
                fall,
                start,
                end,
                from,
                to,
                through,
                rise_from,
                rise_to,
                rise_through,
                fall_from,
                fall_to,
                fall_through,
                comment,
                path_multiplier,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_multicycle_path() {
    let mut parser = parser(command);
    let tgt = "set_multicycle_path -setup -hold -rise -fall -start -end -from a -to a -through a -rise_from a -rise_to a -rise_through a -fall_from a -fall_to a -fall_through a -comment \"aaa\" 0.1";
    assert_eq!(
        Command::SetMulticyclePath(SetMulticyclePath {
            setup: true,
            hold: true,
            rise: true,
            fall: true,
            start: true,
            end: true,
            from: Some(Object::String(vec![String::from("a")])),
            to: Some(Object::String(vec![String::from("a")])),
            through: Some(Object::String(vec![String::from("a")])),
            rise_from: Some(Object::String(vec![String::from("a")])),
            rise_to: Some(Object::String(vec![String::from("a")])),
            rise_through: Some(Object::String(vec![String::from("a")])),
            fall_from: Some(Object::String(vec![String::from("a")])),
            fall_to: Some(Object::String(vec![String::from("a")])),
            fall_through: Some(Object::String(vec![String::from("a")])),
            comment: Some(String::from("aaa")),
            path_multiplier: 0.1,
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_output_delay`
#[derive(Debug, Default, PartialEq)]
pub struct SetOutputDelay {
    pub clock: Option<Object>,
    pub reference_pin: Option<Object>,
    pub clock_fall: bool,
    pub level_sensitive: bool,
    pub rise: bool,
    pub fall: bool,
    pub max: bool,
    pub min: bool,
    pub add_delay: bool,
    pub network_latency_included: bool,
    pub source_latency_included: bool,
    pub delay_value: f64,
    pub port_pin_list: Object,
}

fn set_output_delay<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_output_delay");
    let clock = symbol("-clock")
        .with(parser(object))
        .map(|x| CommandArg::ClockObj(x));
    let reference_pin = symbol("-reference_pin")
        .with(parser(object))
        .map(|x| CommandArg::ReferencePin(x));
    let clock_fall = symbol("-clock_fall").map(|_| CommandArg::ClockFall);
    let level_sensitive = symbol("-level_sensitive").map(|_| CommandArg::LevelSensitive);
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let min = symbol("-min").map(|_| CommandArg::Min);
    let add_delay = symbol("-add_delay").map(|_| CommandArg::AddDelay);
    let network_latency_included =
        symbol("-network_latency_included").map(|_| CommandArg::NetworkLatencyIncluded);
    let source_latency_included =
        symbol("-source_latency_included").map(|_| CommandArg::SourceLatencyIncluded);
    let delay_value = float().map(|x| CommandArg::Value(x));
    let port_pin_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(clock),
        attempt(reference_pin),
        attempt(clock_fall),
        attempt(level_sensitive),
        attempt(rise),
        attempt(fall),
        attempt(max),
        attempt(min),
        attempt(add_delay),
        attempt(network_latency_included),
        attempt(source_latency_included),
        attempt(delay_value),
        attempt(port_pin_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut clock = None;
            let mut reference_pin = None;
            let mut clock_fall = false;
            let mut level_sensitive = false;
            let mut rise = false;
            let mut fall = false;
            let mut max = false;
            let mut min = false;
            let mut add_delay = false;
            let mut network_latency_included = false;
            let mut source_latency_included = false;
            let mut delay_value = None;
            let mut port_pin_list = None;
            for x in xs {
                match x {
                    CommandArg::ClockObj(x) => clock = Some(x),
                    CommandArg::ReferencePin(x) => reference_pin = Some(x),
                    CommandArg::ClockFall => clock_fall = true,
                    CommandArg::LevelSensitive => level_sensitive = true,
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Max => max = true,
                    CommandArg::Min => min = true,
                    CommandArg::AddDelay => add_delay = true,
                    CommandArg::NetworkLatencyIncluded => network_latency_included = true,
                    CommandArg::SourceLatencyIncluded => source_latency_included = true,
                    CommandArg::Value(x) => delay_value = Some(x),
                    CommandArg::Object(x) => port_pin_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let delay_value = delay_value.expect("set_output_delay:delay_value");
            let port_pin_list = port_pin_list.expect("set_output_delay:port_pin_list");
            Command::SetOutputDelay(SetOutputDelay {
                clock,
                reference_pin,
                clock_fall,
                level_sensitive,
                rise,
                fall,
                max,
                min,
                add_delay,
                network_latency_included,
                source_latency_included,
                delay_value,
                port_pin_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_output_delay() {
    let mut parser = parser(command);
    let tgt = "set_output_delay -clock a -reference_pin a -clock_fall -level_sensitive -rise -fall -max -min -add_delay -network_latency_included -source_latency_included 0.1 a";
    assert_eq!(
        Command::SetOutputDelay(SetOutputDelay {
            clock: Some(Object::String(vec![String::from("a")])),
            reference_pin: Some(Object::String(vec![String::from("a")])),
            clock_fall: true,
            level_sensitive: true,
            rise: true,
            fall: true,
            min: true,
            max: true,
            add_delay: true,
            network_latency_included: true,
            source_latency_included: true,
            delay_value: 0.1,
            port_pin_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_propagated_clock`
#[derive(Debug, Default, PartialEq)]
pub struct SetPropagatedClock {
    pub object_list: Object,
}

fn set_propagated_clock<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_propagated_clock");
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(object_list),);
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let object_list = object_list.expect("set_propagated_clock:object_list");
            Command::SetPropagatedClock(SetPropagatedClock { object_list })
        })
        .parse_stream(input)
}

#[test]
fn test_set_propagated_clock() {
    let mut parser = parser(command);
    let tgt = "set_propagated_clock a";
    assert_eq!(
        Command::SetPropagatedClock(SetPropagatedClock {
            object_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_case_analysis`
#[derive(Debug, Default, PartialEq)]
pub struct SetCaseAnalysis {
    pub value: CaseValue,
    pub port_or_pin_list: Object,
}

#[derive(Debug, PartialEq)]
pub enum CaseValue {
    Zero,
    One,
    Rising,
    Falling,
}

impl Default for CaseValue {
    fn default() -> Self {
        CaseValue::Zero
    }
}

fn set_case_analysis<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_case_analysis");
    let value = choice((
        symbol("0"),
        symbol("1"),
        symbol("rising"),
        symbol("falling"),
    ))
    .map(|x| match x {
        "0" => CommandArg::CaseValue(CaseValue::Zero),
        "1" => CommandArg::CaseValue(CaseValue::One),
        "rising" => CommandArg::CaseValue(CaseValue::Rising),
        "falling" => CommandArg::CaseValue(CaseValue::Falling),
        _ => unreachable!(),
    });
    let port_or_pin_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(value), attempt(port_or_pin_list));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut value = None;
            let mut port_or_pin_list = None;
            for x in xs {
                match x {
                    CommandArg::CaseValue(x) => value = Some(x),
                    CommandArg::Object(x) => port_or_pin_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_case_analysis:value");
            let port_or_pin_list = port_or_pin_list.expect("set_case_analysis:port_or_pin_list");
            Command::SetCaseAnalysis(SetCaseAnalysis {
                value,
                port_or_pin_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_case_analysis() {
    let mut parser = parser(command);
    let tgt = "set_case_analysis 0 a";
    assert_eq!(
        Command::SetCaseAnalysis(SetCaseAnalysis {
            value: CaseValue::Zero,
            port_or_pin_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
    let tgt = "set_case_analysis 1 a";
    assert_eq!(
        Command::SetCaseAnalysis(SetCaseAnalysis {
            value: CaseValue::One,
            port_or_pin_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
    let tgt = "set_case_analysis rising a";
    assert_eq!(
        Command::SetCaseAnalysis(SetCaseAnalysis {
            value: CaseValue::Rising,
            port_or_pin_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
    let tgt = "set_case_analysis falling a";
    assert_eq!(
        Command::SetCaseAnalysis(SetCaseAnalysis {
            value: CaseValue::Falling,
            port_or_pin_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_drive`
#[derive(Debug, Default, PartialEq)]
pub struct SetDrive {
    pub rise: bool,
    pub fall: bool,
    pub min: bool,
    pub max: bool,
    pub resistance: f64,
    pub port_list: Object,
}

fn set_drive<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_drive");
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let resistance = float().map(|x| CommandArg::Value(x));
    let port_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(rise),
        attempt(fall),
        attempt(min),
        attempt(max),
        attempt(resistance),
        attempt(port_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut rise = false;
            let mut fall = false;
            let mut min = false;
            let mut max = false;
            let mut resistance = None;
            let mut port_list = None;
            for x in xs {
                match x {
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::Value(x) => resistance = Some(x),
                    CommandArg::Object(x) => port_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let resistance = resistance.expect("set_drive:resistance");
            let port_list = port_list.expect("set_drive:port_list");
            Command::SetDrive(SetDrive {
                rise,
                fall,
                min,
                max,
                resistance,
                port_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_drive() {
    let mut parser = parser(command);
    let tgt = "set_drive -rise -fall -min -max  0.1 a";
    assert_eq!(
        Command::SetDrive(SetDrive {
            rise: true,
            fall: true,
            min: true,
            max: true,
            resistance: 0.1,
            port_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_driving_cell`
#[derive(Debug, Default, PartialEq)]
pub struct SetDrivingCell {
    pub lib_cell: Option<Object>,
    pub rise: bool,
    pub fall: bool,
    pub min: bool,
    pub max: bool,
    pub library: Option<Object>,
    pub pin: Option<Object>,
    pub from_pin: Option<Object>,
    pub dont_scale: bool,
    pub no_design_rule: bool,
    pub clock: Option<Object>,
    pub clock_fall: bool,
    pub input_transition_rise: Option<f64>,
    pub input_transition_fall: Option<f64>,
    pub multiply_by: Option<f64>,
    pub port_list: Object,
}

fn set_driving_cell<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_driving_cell");
    let lib_cell = symbol("-lib_cell")
        .with(parser(object))
        .map(|x| CommandArg::LibCell(x));
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let library = symbol("-library")
        .with(parser(object))
        .map(|x| CommandArg::Library(x));
    let pin = symbol("-pin")
        .with(parser(object))
        .map(|x| CommandArg::Pin(x));
    let from_pin = symbol("-from_pin")
        .with(parser(object))
        .map(|x| CommandArg::FromPin(x));
    let dont_scale = symbol("-dont_scale").map(|_| CommandArg::DontScale);
    let no_design_rule = symbol("-no_design_rule").map(|_| CommandArg::NoDesignRule);
    let clock = symbol("-clock")
        .with(parser(object))
        .map(|x| CommandArg::ClockObj(x));
    let clock_fall = symbol("-clock_fall").map(|_| CommandArg::ClockFall);
    let input_transition_rise = symbol("-input_transition_rise")
        .with(float())
        .map(|x| CommandArg::InputTransitionRise(x));
    let input_transition_fall = symbol("-input_transition_fall")
        .with(float())
        .map(|x| CommandArg::InputTransitionFall(x));
    let multiply_by = symbol("-multiply_by")
        .with(float())
        .map(|x| CommandArg::MultiplyBy(x));
    let port_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(lib_cell),
        attempt(rise),
        attempt(fall),
        attempt(min),
        attempt(max),
        attempt(library),
        attempt(pin),
        attempt(from_pin),
        attempt(dont_scale),
        attempt(no_design_rule),
        attempt(clock),
        attempt(clock_fall),
        attempt(input_transition_rise),
        attempt(input_transition_fall),
        attempt(multiply_by),
        attempt(port_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut lib_cell = None;
            let mut rise = false;
            let mut fall = false;
            let mut min = false;
            let mut max = false;
            let mut library = None;
            let mut pin = None;
            let mut from_pin = None;
            let mut dont_scale = false;
            let mut no_design_rule = false;
            let mut clock = None;
            let mut clock_fall = false;
            let mut input_transition_rise = None;
            let mut input_transition_fall = None;
            let mut multiply_by = None;
            let mut port_list = None;
            for x in xs {
                match x {
                    CommandArg::LibCell(x) => lib_cell = Some(x),
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::Library(x) => library = Some(x),
                    CommandArg::Pin(x) => pin = Some(x),
                    CommandArg::FromPin(x) => from_pin = Some(x),
                    CommandArg::DontScale => dont_scale = true,
                    CommandArg::NoDesignRule => no_design_rule = true,
                    CommandArg::ClockObj(x) => clock = Some(x),
                    CommandArg::ClockFall => clock_fall = true,
                    CommandArg::InputTransitionRise(x) => input_transition_rise = Some(x),
                    CommandArg::InputTransitionFall(x) => input_transition_fall = Some(x),
                    CommandArg::MultiplyBy(x) => multiply_by = Some(x),
                    CommandArg::Object(x) => port_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let port_list = port_list.expect("set_driving_cell:port_list");
            Command::SetDrivingCell(SetDrivingCell {
                lib_cell,
                rise,
                fall,
                min,
                max,
                library,
                pin,
                from_pin,
                dont_scale,
                no_design_rule,
                clock,
                clock_fall,
                input_transition_rise,
                input_transition_fall,
                multiply_by,
                port_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_driving_cell() {
    let mut parser = parser(command);
    let tgt = "set_driving_cell -lib_cell a -rise -fall -min -max -library a -pin a -from_pin a -dont_scale -no_design_rule -clock a -clock_fall -input_transition_rise 0.1 -input_transition_fall 0.1 -multiply_by 0.1 a";
    assert_eq!(
        Command::SetDrivingCell(SetDrivingCell {
            lib_cell: Some(Object::String(vec![String::from("a")])),
            rise: true,
            fall: true,
            min: true,
            max: true,
            library: Some(Object::String(vec![String::from("a")])),
            pin: Some(Object::String(vec![String::from("a")])),
            from_pin: Some(Object::String(vec![String::from("a")])),
            dont_scale: true,
            no_design_rule: true,
            clock: Some(Object::String(vec![String::from("a")])),
            clock_fall: true,
            input_transition_rise: Some(0.1),
            input_transition_fall: Some(0.1),
            multiply_by: Some(0.1),
            port_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_fanout_load`
#[derive(Debug, Default, PartialEq)]
pub struct SetFanoutLoad {
    pub value: f64,
    pub port_list: Object,
}

fn set_fanout_load<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_fanout_load");
    let value = float().map(|x| CommandArg::Value(x));
    let port_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(value), attempt(port_list));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut value = None;
            let mut port_list = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => port_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_fanout_load:value");
            let port_list = port_list.expect("set_fanout_load:port_list");
            Command::SetFanoutLoad(SetFanoutLoad { value, port_list })
        })
        .parse_stream(input)
}

#[test]
fn test_set_fanout_load() {
    let mut parser = parser(command);
    let tgt = "set_fanout_load 0.1 a";
    assert_eq!(
        Command::SetFanoutLoad(SetFanoutLoad {
            value: 0.1,
            port_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_input_transition`
#[derive(Debug, Default, PartialEq)]
pub struct SetInputTransition {
    pub rise: bool,
    pub fall: bool,
    pub min: bool,
    pub max: bool,
    pub clock: Option<Object>,
    pub clock_fall: bool,
    pub transition: f64,
    pub port_list: Object,
}

fn set_input_transition<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_input_transition");
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let clock = symbol("-clock")
        .with(parser(object))
        .map(|x| CommandArg::ClockObj(x));
    let clock_fall = symbol("-clock_fall").map(|_| CommandArg::ClockFall);
    let transition = float().map(|x| CommandArg::Value(x));
    let port_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(rise),
        attempt(fall),
        attempt(min),
        attempt(max),
        attempt(clock),
        attempt(clock_fall),
        attempt(transition),
        attempt(port_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut rise = false;
            let mut fall = false;
            let mut min = false;
            let mut max = false;
            let mut clock = None;
            let mut clock_fall = false;
            let mut transition = None;
            let mut port_list = None;
            for x in xs {
                match x {
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::ClockObj(x) => clock = Some(x),
                    CommandArg::ClockFall => clock_fall = true,
                    CommandArg::Value(x) => transition = Some(x),
                    CommandArg::Object(x) => port_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let transition = transition.expect("set_input_transition:transition");
            let port_list = port_list.expect("set_input_transition:port_list");
            Command::SetInputTransition(SetInputTransition {
                rise,
                fall,
                min,
                max,
                clock,
                clock_fall,
                transition,
                port_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_input_transition() {
    let mut parser = parser(command);
    let tgt = "set_input_transition -rise -fall -min -max -clock a -clock_fall 0.1 a";
    assert_eq!(
        Command::SetInputTransition(SetInputTransition {
            rise: true,
            fall: true,
            min: true,
            max: true,
            clock: Some(Object::String(vec![String::from("a")])),
            clock_fall: true,
            transition: 0.1,
            port_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_load`
#[derive(Debug, Default, PartialEq)]
pub struct SetLoad {
    pub min: bool,
    pub max: bool,
    pub subtract_pin_load: bool,
    pub pin_load: bool,
    pub wire_load: bool,
    pub value: f64,
    pub objects: Object,
}

fn set_load<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_load");
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let subtract_pin_load = symbol("-subtract_pin_load").map(|_| CommandArg::SubtractPinLoad);
    let pin_load = symbol("-pin_load").map(|_| CommandArg::PinLoad);
    let wire_load = symbol("-wire_load").map(|_| CommandArg::WireLoad);
    let value = float().map(|x| CommandArg::Value(x));
    let objects = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(min),
        attempt(max),
        attempt(subtract_pin_load),
        attempt(pin_load),
        attempt(wire_load),
        attempt(value),
        attempt(objects),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut min = false;
            let mut max = false;
            let mut subtract_pin_load = false;
            let mut pin_load = false;
            let mut wire_load = false;
            let mut value = None;
            let mut objects = None;
            for x in xs {
                match x {
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::SubtractPinLoad => subtract_pin_load = true,
                    CommandArg::PinLoad => pin_load = true,
                    CommandArg::WireLoad => wire_load = true,
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => objects = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_load:value");
            let objects = objects.expect("set_load:objects");
            Command::SetLoad(SetLoad {
                min,
                max,
                subtract_pin_load,
                pin_load,
                wire_load,
                value,
                objects,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_load() {
    let mut parser = parser(command);
    let tgt = "set_load -min -max -subtract_pin_load -pin_load -wire_load 0.1 a";
    assert_eq!(
        Command::SetLoad(SetLoad {
            min: true,
            max: true,
            subtract_pin_load: true,
            pin_load: true,
            wire_load: true,
            value: 0.1,
            objects: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_logic_dc`
#[derive(Debug, Default, PartialEq)]
pub struct SetLogicDc {
    pub port_list: Object,
}

fn set_logic_dc<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_logic_dc");
    let port_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(port_list),);
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut port_list = None;
            for x in xs {
                match x {
                    CommandArg::Object(x) => port_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let port_list = port_list.expect("set_logic_dc:port_list");
            Command::SetLogicDc(SetLogicDc { port_list })
        })
        .parse_stream(input)
}

#[test]
fn test_set_logic_dc() {
    let mut parser = parser(command);
    let tgt = "set_logic_dc a";
    assert_eq!(
        Command::SetLogicDc(SetLogicDc {
            port_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_logic_one`
#[derive(Debug, Default, PartialEq)]
pub struct SetLogicOne {
    pub port_list: Object,
}

fn set_logic_one<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_logic_one");
    let port_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(port_list),);
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut port_list = None;
            for x in xs {
                match x {
                    CommandArg::Object(x) => port_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let port_list = port_list.expect("set_logic_one:port_list");
            Command::SetLogicOne(SetLogicOne { port_list })
        })
        .parse_stream(input)
}

#[test]
fn test_set_logic_one() {
    let mut parser = parser(command);
    let tgt = "set_logic_one a";
    assert_eq!(
        Command::SetLogicOne(SetLogicOne {
            port_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_logic_zero`
#[derive(Debug, Default, PartialEq)]
pub struct SetLogicZero {
    pub port_list: Object,
}

fn set_logic_zero<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_logic_zero");
    let port_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(port_list),);
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut port_list = None;
            for x in xs {
                match x {
                    CommandArg::Object(x) => port_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let port_list = port_list.expect("set_logic_zero:port_list");
            Command::SetLogicZero(SetLogicZero { port_list })
        })
        .parse_stream(input)
}

#[test]
fn test_set_logic_zero() {
    let mut parser = parser(command);
    let tgt = "set_logic_zero a";
    assert_eq!(
        Command::SetLogicZero(SetLogicZero {
            port_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_max_area`
#[derive(Debug, Default, PartialEq)]
pub struct SetMaxArea {
    pub area_value: f64,
}

fn set_max_area<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_max_area");
    let area_value = float().map(|x| CommandArg::Value(x));
    let args = (attempt(area_value),);
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut area_value = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => area_value = Some(x),
                    _ => unreachable!(),
                }
            }
            let area_value = area_value.expect("set_max_area:area_value");
            Command::SetMaxArea(SetMaxArea { area_value })
        })
        .parse_stream(input)
}

#[test]
fn test_set_max_area() {
    let mut parser = parser(command);
    let tgt = "set_max_area 0.1";
    assert_eq!(
        Command::SetMaxArea(SetMaxArea { area_value: 0.1 }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_max_capacitance`
#[derive(Debug, Default, PartialEq)]
pub struct SetMaxCapacitance {
    pub value: f64,
    pub objects: Object,
}

fn set_max_capacitance<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_max_capacitance");
    let value = float().map(|x| CommandArg::Value(x));
    let objects = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(value), attempt(objects));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut value = None;
            let mut objects = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => objects = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_max_capacitance:value");
            let objects = objects.expect("set_max_capacitance:objects");
            Command::SetMaxCapacitance(SetMaxCapacitance { value, objects })
        })
        .parse_stream(input)
}

#[test]
fn test_set_max_capacitance() {
    let mut parser = parser(command);
    let tgt = "set_max_capacitance 0.1 a";
    assert_eq!(
        Command::SetMaxCapacitance(SetMaxCapacitance {
            value: 0.1,
            objects: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_max_fanout`
#[derive(Debug, Default, PartialEq)]
pub struct SetMaxFanout {
    pub value: f64,
    pub objects: Object,
}

fn set_max_fanout<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_max_fanout");
    let value = float().map(|x| CommandArg::Value(x));
    let objects = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(value), attempt(objects));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut value = None;
            let mut objects = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => objects = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_max_fanout:value");
            let objects = objects.expect("set_max_fanout:objects");
            Command::SetMaxFanout(SetMaxFanout { value, objects })
        })
        .parse_stream(input)
}

#[test]
fn test_set_max_fanout() {
    let mut parser = parser(command);
    let tgt = "set_max_fanout 0.1 a";
    assert_eq!(
        Command::SetMaxFanout(SetMaxFanout {
            value: 0.1,
            objects: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_max_transition`
#[derive(Debug, Default, PartialEq)]
pub struct SetMaxTransition {
    pub clock_path: bool,
    pub data_path: bool,
    pub rise: bool,
    pub fall: bool,
    pub value: f64,
    pub object_list: Object,
}

fn set_max_transition<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_max_transition");
    let clock_path = symbol("-clock_path").map(|_| CommandArg::ClockPath);
    let data_path = symbol("-data_path").map(|_| CommandArg::DataPath);
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let value = float().map(|x| CommandArg::Value(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(clock_path),
        attempt(data_path),
        attempt(rise),
        attempt(fall),
        attempt(value),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut clock_path = false;
            let mut data_path = false;
            let mut rise = false;
            let mut fall = false;
            let mut value = None;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::ClockPath => clock_path = true,
                    CommandArg::DataPath => data_path = true,
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_max_transition:value");
            let object_list = object_list.expect("set_max_transition:object_list");
            Command::SetMaxTransition(SetMaxTransition {
                clock_path,
                data_path,
                rise,
                fall,
                value,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_max_transition() {
    let mut parser = parser(command);
    let tgt = "set_max_transition -clock_path -data_path -rise -fall 0.1 a";
    assert_eq!(
        Command::SetMaxTransition(SetMaxTransition {
            clock_path: true,
            data_path: true,
            rise: true,
            fall: true,
            value: 0.1,
            object_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_min_capacitance`
#[derive(Debug, Default, PartialEq)]
pub struct SetMinCapacitance {
    pub value: f64,
    pub objects: Object,
}

fn set_min_capacitance<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_min_capacitance");
    let value = float().map(|x| CommandArg::Value(x));
    let objects = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(value), attempt(objects));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut value = None;
            let mut objects = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => objects = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_min_capacitance:value");
            let objects = objects.expect("set_min_capacitance:objects");
            Command::SetMinCapacitance(SetMinCapacitance { value, objects })
        })
        .parse_stream(input)
}

#[test]
fn test_set_min_capacitance() {
    let mut parser = parser(command);
    let tgt = "set_min_capacitance 0.1 a";
    assert_eq!(
        Command::SetMinCapacitance(SetMinCapacitance {
            value: 0.1,
            objects: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_min_porosity`
#[derive(Debug, Default, PartialEq)]
pub struct SetMinPorosity {
    pub porosity_value: f64,
    pub object_list: Object,
}

fn set_min_porosity<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_min_porosity");
    let porosity_value = float().map(|x| CommandArg::Value(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(porosity_value), attempt(object_list));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut value = None;
            let mut objects = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => objects = Some(x),
                    _ => unreachable!(),
                }
            }
            let porosity_value = value.expect("set_min_porosity:value");
            let object_list = objects.expect("set_min_porosity:objects");
            Command::SetMinPorosity(SetMinPorosity {
                porosity_value,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_min_porosity() {
    let mut parser = parser(command);
    let tgt = "set_min_porosity 0.1 a";
    assert_eq!(
        Command::SetMinPorosity(SetMinPorosity {
            porosity_value: 0.1,
            object_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_operating_conditions`
#[derive(Debug, PartialEq)]
pub struct SetOperatingConditions {
    pub library: Option<Object>,
    pub analysis_type: Option<String>,
    pub max: Option<String>,
    pub min: Option<String>,
    pub max_library: Option<Object>,
    pub min_library: Option<Object>,
    pub object_list: Option<Object>,
    pub condition: Option<String>,
}

fn set_operating_conditions<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_operating_conditions");
    let library = symbol("-library")
        .with(parser(object))
        .map(|x| CommandArg::Library(x));
    let analysis_type = symbol("-analysis_type")
        .with(item())
        .map(|x| CommandArg::AnalysisType(x));
    let max = symbol("-max").with(item()).map(|x| CommandArg::MaxStr(x));
    let min = symbol("-min").with(item()).map(|x| CommandArg::MinStr(x));
    let max_library = symbol("-max_library")
        .with(parser(object))
        .map(|x| CommandArg::MaxLibrary(x));
    let min_library = symbol("-min_library")
        .with(parser(object))
        .map(|x| CommandArg::MinLibrary(x));
    let object_list = symbol("-object_list")
        .with(parser(object))
        .map(|x| CommandArg::ObjectList(x));
    let condition = item().map(|x| CommandArg::String(x));
    let args = (
        attempt(library),
        attempt(analysis_type),
        attempt(max),
        attempt(min),
        attempt(max_library),
        attempt(min_library),
        attempt(object_list),
        attempt(condition),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut library = None;
            let mut analysis_type = None;
            let mut max = None;
            let mut min = None;
            let mut max_library = None;
            let mut min_library = None;
            let mut object_list = None;
            let mut condition = None;
            for x in xs {
                match x {
                    CommandArg::Library(x) => library = Some(x),
                    CommandArg::AnalysisType(x) => analysis_type = Some(x),
                    CommandArg::MaxStr(x) => max = Some(x),
                    CommandArg::MinStr(x) => min = Some(x),
                    CommandArg::MaxLibrary(x) => max_library = Some(x),
                    CommandArg::MinLibrary(x) => min_library = Some(x),
                    CommandArg::ObjectList(x) => object_list = Some(x),
                    CommandArg::String(x) => condition = Some(x),
                    _ => unreachable!(),
                }
            }
            Command::SetOperatingConditions(SetOperatingConditions {
                library,
                analysis_type,
                max,
                min,
                max_library,
                min_library,
                object_list,
                condition,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_operating_conditions() {
    let mut parser = parser(set_operating_conditions);
    let tgt =
        "set_operating_conditions -library a -analysis_type a -max a -min a -max_library a -min_library a -object_list a a";
    assert_eq!(
        Command::SetOperatingConditions(SetOperatingConditions {
            library: Some(Object::String(vec![String::from("a")])),
            analysis_type: Some(String::from("a")),
            max: Some(String::from("a")),
            min: Some(String::from("a")),
            max_library: Some(Object::String(vec![String::from("a")])),
            min_library: Some(Object::String(vec![String::from("a")])),
            object_list: Some(Object::String(vec![String::from("a")])),
            condition: Some(String::from("a")),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_port_fanout_number`
#[derive(Debug, Default, PartialEq)]
pub struct SetPortFanoutNumber {
    pub value: f64,
    pub port_list: Object,
}

fn set_port_fanout_number<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_port_fanout_number");
    let value = float().map(|x| CommandArg::Value(x));
    let port_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (attempt(value), attempt(port_list));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut value = None;
            let mut port_list = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => port_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_port_fanout_number:value");
            let port_list = port_list.expect("set_port_fanout_number:port_list");
            Command::SetPortFanoutNumber(SetPortFanoutNumber { value, port_list })
        })
        .parse_stream(input)
}

#[test]
fn test_set_port_fanout_number() {
    let mut parser = parser(command);
    let tgt = "set_port_fanout_number 0.1 a";
    assert_eq!(
        Command::SetPortFanoutNumber(SetPortFanoutNumber {
            value: 0.1,
            port_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_resistance`
#[derive(Debug, Default, PartialEq)]
pub struct SetResistance {
    pub min: bool,
    pub max: bool,
    pub value: f64,
    pub net_list: Object,
}

fn set_resistance<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_resistance");
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let value = float().map(|x| CommandArg::Value(x));
    let net_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(min),
        attempt(max),
        attempt(value),
        attempt(net_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut min = false;
            let mut max = false;
            let mut value = None;
            let mut net_list = None;
            for x in xs {
                match x {
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::Value(x) => value = Some(x),
                    CommandArg::Object(x) => net_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let value = value.expect("set_resistance:value");
            let net_list = net_list.expect("set_resistance:net_list");
            Command::SetResistance(SetResistance {
                min,
                max,
                value,
                net_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_resistance() {
    let mut parser = parser(command);
    let tgt = "set_resistance -min -max 0.1 a";
    assert_eq!(
        Command::SetResistance(SetResistance {
            min: true,
            max: true,
            value: 0.1,
            net_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_timing_derate`
#[derive(Debug, Default, PartialEq)]
pub struct SetTimingDerate {
    pub cell_delay: bool,
    pub cell_check: bool,
    pub net_delay: bool,
    pub data: bool,
    pub clock: bool,
    pub early: bool,
    pub late: bool,
    pub rise: bool,
    pub fall: bool,
    pub r#static: bool,
    pub dynamic: bool,
    pub increment: bool,
    pub derate_value: f64,
    pub object_list: Option<Object>,
}

fn set_timing_derate<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_timing_derate");
    let cell_delay = symbol("-cell_delay").map(|_| CommandArg::CellDelay);
    let cell_check = symbol("-cell_check").map(|_| CommandArg::CellCheck);
    let net_delay = symbol("-net_delay").map(|_| CommandArg::NetDelay);
    let data = symbol("-data").map(|_| CommandArg::Data);
    let clock = symbol("-clock").map(|_| CommandArg::Clock);
    let early = symbol("-early").map(|_| CommandArg::Early);
    let late = symbol("-late").map(|_| CommandArg::Late);
    let rise = symbol("-rise").map(|_| CommandArg::Rise);
    let fall = symbol("-fall").map(|_| CommandArg::Fall);
    let r#static = symbol("-static").map(|_| CommandArg::Static);
    let dynamic = symbol("-dynamic").map(|_| CommandArg::Dynamic);
    let increment = symbol("-increment").map(|_| CommandArg::Increment);
    let derate_value = float().map(|x| CommandArg::Value(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(cell_delay),
        attempt(cell_check),
        attempt(net_delay),
        attempt(data),
        attempt(clock),
        attempt(early),
        attempt(late),
        attempt(rise),
        attempt(fall),
        attempt(r#static),
        attempt(dynamic),
        attempt(increment),
        attempt(derate_value),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut cell_delay = false;
            let mut cell_check = false;
            let mut net_delay = false;
            let mut data = false;
            let mut clock = false;
            let mut early = false;
            let mut late = false;
            let mut rise = false;
            let mut fall = false;
            let mut r#static = false;
            let mut dynamic = false;
            let mut increment = false;
            let mut derate_value = None;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::CellDelay => cell_delay = true,
                    CommandArg::CellCheck => cell_check = true,
                    CommandArg::NetDelay => net_delay = true,
                    CommandArg::Data => data = true,
                    CommandArg::Clock => clock = true,
                    CommandArg::Early => early = true,
                    CommandArg::Late => late = true,
                    CommandArg::Rise => rise = true,
                    CommandArg::Fall => fall = true,
                    CommandArg::Static => r#static = true,
                    CommandArg::Dynamic => dynamic = true,
                    CommandArg::Increment => increment = true,
                    CommandArg::Value(x) => derate_value = Some(x),
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let derate_value = derate_value.expect("set_timing_derate:derate_value");
            Command::SetTimingDerate(SetTimingDerate {
                cell_delay,
                cell_check,
                net_delay,
                data,
                clock,
                early,
                late,
                rise,
                fall,
                r#static,
                dynamic,
                increment,
                derate_value,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_timing_derate() {
    let mut parser = parser(command);
    let tgt = "set_timing_derate -cell_delay -cell_check -net_delay -data -clock -early -late -rise -fall -static -dynamic -increment 0.1 a";
    assert_eq!(
        Command::SetTimingDerate(SetTimingDerate {
            cell_delay: true,
            cell_check: true,
            net_delay: true,
            data: true,
            clock: true,
            early: true,
            late: true,
            rise: true,
            fall: true,
            r#static: true,
            dynamic: true,
            increment: true,
            derate_value: 0.1,
            object_list: Some(Object::String(vec![String::from("a")])),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_voltage`
#[derive(Debug, Default, PartialEq)]
pub struct SetVoltage {
    pub min: Option<f64>,
    pub object_list: Option<Object>,
    pub max_case_voltage: f64,
}

fn set_voltage<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_voltage");
    let min = symbol("-min").with(float()).map(|x| CommandArg::MinVal(x));
    let object_list = symbol("-object_list")
        .with(parser(object))
        .map(|x| CommandArg::ObjectList(x));
    let max_case_voltage = float().map(|x| CommandArg::Value(x));
    let args = (
        attempt(min),
        attempt(object_list),
        attempt(max_case_voltage),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut min = None;
            let mut object_list = None;
            let mut max_case_voltage = None;
            for x in xs {
                match x {
                    CommandArg::MinVal(x) => min = Some(x),
                    CommandArg::ObjectList(x) => object_list = Some(x),
                    CommandArg::Value(x) => max_case_voltage = Some(x),
                    _ => unreachable!(),
                }
            }
            let max_case_voltage = max_case_voltage.expect("set_voltage:max_case_voltage");
            Command::SetVoltage(SetVoltage {
                min,
                object_list,
                max_case_voltage,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_voltage() {
    let mut parser = parser(command);
    let tgt = "set_voltage -min 0.1 -object_list a 0.1";
    assert_eq!(
        Command::SetVoltage(SetVoltage {
            min: Some(0.1),
            object_list: Some(Object::String(vec![String::from("a")])),
            max_case_voltage: 0.1,
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_wire_load_min_block_size`
#[derive(Debug, Default, PartialEq)]
pub struct SetWireLoadMinBlockSize {
    pub size: f64,
}

fn set_wire_load_min_block_size<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_wire_load_min_block_size");
    let size = float().map(|x| CommandArg::Value(x));
    let args = (attempt(size),);
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut size = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => size = Some(x),
                    _ => unreachable!(),
                }
            }
            let size = size.expect("set_wire_load_min_block_size:size");
            Command::SetWireLoadMinBlockSize(SetWireLoadMinBlockSize { size })
        })
        .parse_stream(input)
}

#[test]
fn test_set_wire_load_min_block_size() {
    let mut parser = parser(command);
    let tgt = "set_wire_load_min_block_size 0.1";
    assert_eq!(
        Command::SetWireLoadMinBlockSize(SetWireLoadMinBlockSize { size: 0.1 }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_wire_load_mode`
#[derive(Debug, Default, PartialEq)]
pub struct SetWireLoadMode {
    pub mode_name: String,
}

fn set_wire_load_mode<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_wire_load_mode");
    let mode_name = item().map(|x| CommandArg::String(x));
    let args = (attempt(mode_name),);
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut mode_name = None;
            for x in xs {
                match x {
                    CommandArg::String(x) => mode_name = Some(x),
                    _ => unreachable!(),
                }
            }
            let mode_name = mode_name.expect("set_wire_load_mode:mode_name");
            Command::SetWireLoadMode(SetWireLoadMode { mode_name })
        })
        .parse_stream(input)
}

#[test]
fn test_set_wire_load_mode() {
    let mut parser = parser(command);
    let tgt = "set_wire_load_mode a";
    assert_eq!(
        Command::SetWireLoadMode(SetWireLoadMode {
            mode_name: String::from("a")
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_wire_load_model`
#[derive(Debug, Default, PartialEq)]
pub struct SetWireLoadModel {
    pub name: String,
    pub library: Option<Object>,
    pub min: bool,
    pub max: bool,
    pub object_list: Option<Object>,
}

fn set_wire_load_model<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_wire_load_model");
    let name = symbol("-name").with(item()).map(|x| CommandArg::Name(x));
    let library = symbol("-library")
        .with(parser(object))
        .map(|x| CommandArg::Library(x));
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(name),
        attempt(library),
        attempt(min),
        attempt(max),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut name = None;
            let mut library = None;
            let mut min = false;
            let mut max = false;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::Name(x) => name = Some(x),
                    CommandArg::Library(x) => library = Some(x),
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let name = name.expect("set_wire_load_model:name");
            Command::SetWireLoadModel(SetWireLoadModel {
                name,
                library,
                min,
                max,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_wire_load_model() {
    let mut parser = parser(command);
    let tgt = "set_wire_load_model -name a -library a -min -max a";
    assert_eq!(
        Command::SetWireLoadModel(SetWireLoadModel {
            name: String::from("a"),
            library: Some(Object::String(vec![String::from("a")])),
            min: true,
            max: true,
            object_list: Some(Object::String(vec![String::from("a")])),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_wire_load_selection_group`
#[derive(Debug, Default, PartialEq)]
pub struct SetWireLoadSelectionGroup {
    pub library: Option<Object>,
    pub min: bool,
    pub max: bool,
    pub group_name: String,
    pub object_list: Option<Object>,
}

fn set_wire_load_selection_group<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_wire_load_selection_group");
    let library = symbol("-library")
        .with(parser(object))
        .map(|x| CommandArg::Library(x));
    let min = symbol("-min").map(|_| CommandArg::Min);
    let max = symbol("-max").map(|_| CommandArg::Max);
    let group_name = item().map(|x| CommandArg::String(x));
    let object_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(library),
        attempt(min),
        attempt(max),
        attempt(group_name),
        attempt(object_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut library = None;
            let mut min = false;
            let mut max = false;
            let mut group_name = None;
            let mut object_list = None;
            for x in xs {
                match x {
                    CommandArg::Library(x) => library = Some(x),
                    CommandArg::Min => min = true,
                    CommandArg::Max => max = true,
                    CommandArg::String(x) => group_name = Some(x),
                    CommandArg::Object(x) => object_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let group_name = group_name.expect("set_wire_load_selection_group:group_name");
            Command::SetWireLoadSelectionGroup(SetWireLoadSelectionGroup {
                library,
                min,
                max,
                group_name,
                object_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_set_wire_load_selection_group() {
    let mut parser = parser(command);
    let tgt = "set_wire_load_selection_group -library a -min -max a [all_clocks]";
    assert_eq!(
        Command::SetWireLoadSelectionGroup(SetWireLoadSelectionGroup {
            library: Some(Object::String(vec![String::from("a")])),
            min: true,
            max: true,
            group_name: String::from("a"),
            object_list: Some(Object::AllClocks),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `create_voltage_area`
#[derive(Debug, Default, PartialEq)]
pub struct CreateVoltageArea {
    pub name: String,
    pub coordinate: Vec<f64>,
    pub guard_band_x: Option<f64>,
    pub guard_band_y: Option<f64>,
    pub cell_list: Object,
}

fn create_voltage_area<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("create_voltage_area");
    let name = symbol("-name").with(item()).map(|x| CommandArg::Name(x));
    let coordinate = symbol("-coordinate")
        .with(braces(many1(float())))
        .map(|x| CommandArg::Coordinate(x));
    let guard_band_x = symbol("-guard_band_x")
        .with(float())
        .map(|x| CommandArg::GuardBandX(x));
    let guard_band_y = symbol("-guard_band_y")
        .with(float())
        .map(|x| CommandArg::GuardBandY(x));
    let cell_list = parser(object).map(|x| CommandArg::Object(x));
    let args = (
        attempt(name),
        attempt(coordinate),
        attempt(guard_band_x),
        attempt(guard_band_y),
        attempt(cell_list),
    );
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut name = None;
            let mut coordinate = Vec::new();
            let mut guard_band_x = None;
            let mut guard_band_y = None;
            let mut cell_list = None;
            for x in xs {
                match x {
                    CommandArg::Name(x) => name = Some(x),
                    CommandArg::Coordinate(x) => coordinate = x,
                    CommandArg::GuardBandX(x) => guard_band_x = Some(x),
                    CommandArg::GuardBandY(x) => guard_band_y = Some(x),
                    CommandArg::Object(x) => cell_list = Some(x),
                    _ => unreachable!(),
                }
            }
            let name = name.expect("create_voltage_area:name");
            let cell_list = cell_list.expect("create_voltage_area:cell_list");
            Command::CreateVoltageArea(CreateVoltageArea {
                name,
                coordinate,
                guard_band_x,
                guard_band_y,
                cell_list,
            })
        })
        .parse_stream(input)
}

#[test]
fn test_create_voltage_area() {
    let mut parser = parser(command);
    let tgt = "create_voltage_area -name a -coordinate {10 20 30 40} -guard_band_x 0.1 -guard_band_y 0.1 a";
    assert_eq!(
        Command::CreateVoltageArea(CreateVoltageArea {
            name: String::from("a"),
            coordinate: vec![10.0, 20.0, 30.0, 40.0],
            guard_band_x: Some(0.1),
            guard_band_y: Some(0.1),
            cell_list: Object::String(vec![String::from("a")]),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_level_shifter_strategy`
#[derive(Debug, Default, PartialEq)]
pub struct SetLevelShifterStrategy {
    pub rule: Option<String>,
}

fn set_level_shifter_strategy<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_level_shifter_strategy");
    let rule = symbol("-rule").with(item()).map(|x| CommandArg::Rule(x));
    let args = (attempt(rule),);
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut rule = None;
            for x in xs {
                match x {
                    CommandArg::Rule(x) => rule = Some(x),
                    _ => unreachable!(),
                }
            }
            Command::SetLevelShifterStrategy(SetLevelShifterStrategy { rule })
        })
        .parse_stream(input)
}

#[test]
fn test_set_level_shifter_strategy() {
    let mut parser = parser(command);
    let tgt = "set_level_shifter_strategy -rule a";
    assert_eq!(
        Command::SetLevelShifterStrategy(SetLevelShifterStrategy {
            rule: Some(String::from("a")),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_level_shifter_threshold`
#[derive(Debug, Default, PartialEq)]
pub struct SetLevelShifterThreshold {
    pub voltage: Option<f64>,
    pub percent: Option<f64>,
}

fn set_level_shifter_threshold<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_level_shifter_threshold");
    let voltage = symbol("-voltage")
        .with(float())
        .map(|x| CommandArg::Voltage(x));
    let percent = symbol("-percent")
        .with(float())
        .map(|x| CommandArg::Percent(x));
    let args = (attempt(voltage), attempt(percent));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut voltage = None;
            let mut percent = None;
            for x in xs {
                match x {
                    CommandArg::Voltage(x) => voltage = Some(x),
                    CommandArg::Percent(x) => percent = Some(x),
                    _ => unreachable!(),
                }
            }
            Command::SetLevelShifterThreshold(SetLevelShifterThreshold { voltage, percent })
        })
        .parse_stream(input)
}

#[test]
fn test_set_level_shifter_threshold() {
    let mut parser = parser(command);
    let tgt = "set_level_shifter_threshold -voltage 0.1 -percent 0.1";
    assert_eq!(
        Command::SetLevelShifterThreshold(SetLevelShifterThreshold {
            voltage: Some(0.1),
            percent: Some(0.1),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_max_dynamic_power`
#[derive(Debug, Default, PartialEq)]
pub struct SetMaxDynamicPower {
    pub power: f64,
    pub unit: Option<String>,
}

fn set_max_dynamic_power<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_max_dynamic_power");
    let power = float().map(|x| CommandArg::Value(x));
    let unit = item().map(|x| CommandArg::String(x));
    let args = (attempt(power), attempt(unit));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut power = None;
            let mut unit = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => power = Some(x),
                    CommandArg::String(x) => unit = Some(x),
                    _ => unreachable!(),
                }
            }
            let power = power.expect("set_max_dynamic_power:power");
            Command::SetMaxDynamicPower(SetMaxDynamicPower { power, unit })
        })
        .parse_stream(input)
}

#[test]
fn test_set_max_dynamic_power() {
    let mut parser = parser(command);
    let tgt = "set_max_dynamic_power 0.1 mW";
    assert_eq!(
        Command::SetMaxDynamicPower(SetMaxDynamicPower {
            power: 0.1,
            unit: Some(String::from("mW")),
        }),
        parser.parse(tgt).unwrap().0
    );
}

// -----------------------------------------------------------------------------

/// A type containing information of `set_max_leakage_power`
#[derive(Debug, Default, PartialEq)]
pub struct SetMaxLeakagePower {
    pub power: f64,
    pub unit: Option<String>,
}

fn set_max_leakage_power<I>(input: &mut I) -> ParseResult<Command, I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let command = symbol("set_max_leakage_power");
    let power = float().map(|x| CommandArg::Value(x));
    let unit = item().map(|x| CommandArg::String(x));
    let args = (attempt(power), attempt(unit));
    command
        .with(many(choice(args)))
        .map(|xs: Vec<_>| {
            let mut power = None;
            let mut unit = None;
            for x in xs {
                match x {
                    CommandArg::Value(x) => power = Some(x),
                    CommandArg::String(x) => unit = Some(x),
                    _ => unreachable!(),
                }
            }
            let power = power.expect("set_max_leakage_power:power");
            Command::SetMaxLeakagePower(SetMaxLeakagePower { power, unit })
        })
        .parse_stream(input)
}

#[test]
fn test_set_max_leakage_power() {
    let mut parser = parser(command);
    let tgt = "set_max_leakage_power 0.1 mW";
    assert_eq!(
        Command::SetMaxLeakagePower(SetMaxLeakagePower {
            power: 0.1,
            unit: Some(String::from("mW")),
        }),
        parser.parse(tgt).unwrap().0
    );
}
