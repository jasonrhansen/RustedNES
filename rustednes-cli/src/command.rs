use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, digit1, space0, space1};
use nom::combinator::{all_consuming, map, map_res, opt};
use nom::sequence::{preceded, tuple};
use nom::IResult;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub enum Command {
    ShowRegs,
    Step(u16),
    Continue,
    Goto(u16),
    ShowMem(Option<u16>),
    ShowPpuMem(u16),
    ShowStack,
    Disassemble(u16),
    Label,
    AddLabel(String, u16),
    RemoveLabel(String),
    Breakpoint,
    AddBreakpoint(u16),
    RemoveBreakpoint(u16),
    Watchpoint,
    AddWatchpoint(u16),
    RemoveWatchpoint(u16),
    Exit,
    Repeat,
}

impl FromStr for Command {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_, command) =
            command(s).map_err(|err| format!("Unable to parse command: {:?}", err))?;
        Ok(command)
    }
}

fn u16_(input: &str) -> IResult<&str, u16> {
    map_res(digit1, |s: &str| s.parse::<u16>())(input)
}

fn u16_hex(input: &str) -> IResult<&str, u16> {
    let prefix = alt((tag("0x"), tag("$")));
    let digits = map_res(alphanumeric1, |s: &str| u16::from_str_radix(&s, 16));
    preceded(opt(prefix), digits)(input)
}

fn command(input: &str) -> IResult<&str, Command> {
    let show_regs = all_consuming(alt((tag("showregs"), tag("r"))));

    let step = all_consuming(preceded(
        alt((tag("step"), tag("s"))),
        opt(preceded(space1, u16_)),
    ));

    let continue_ = all_consuming(alt((tag("continue"), tag("c"))));

    let goto = all_consuming(preceded(
        alt((tag("goto"), tag("g"))),
        preceded(space1, u16_hex),
    ));

    let show_mem = all_consuming(preceded(
        alt((tag("showmem"), tag("m"))),
        opt(preceded(space1, u16_hex)),
    ));

    let show_ppu_mem = all_consuming(preceded(
        alt((tag("showppumem"), tag("pm"))),
        preceded(space1, u16_hex),
    ));

    let show_stack = all_consuming(alt((tag("showstack"), tag("ss"))));

    let disassemble = all_consuming(preceded(
        alt((tag("disassemble"), tag("d"))),
        opt(preceded(space1, u16_)),
    ));

    let label = all_consuming(alt((tag("label"), tag("l"))));

    let add_label = all_consuming(preceded(
        alt((tag("addlabel"), tag("al"))),
        tuple((preceded(space1, alphanumeric1), preceded(space1, u16_hex))),
    ));

    let remove_label = all_consuming(preceded(
        alt((tag("removelabel"), tag("rl"))),
        preceded(space1, alphanumeric1),
    ));

    let breakpoint = all_consuming(alt((tag("breakpoint"), tag("b"))));

    let add_breakpoint = all_consuming(preceded(
        alt((tag("addbreakpoint"), tag("ab"))),
        preceded(space1, u16_hex),
    ));

    let remove_breakpoint = all_consuming(preceded(
        alt((tag("removebreakpoint"), tag("rb"))),
        preceded(space1, u16_hex),
    ));

    let watchpoint = all_consuming(alt((tag("watchpoint"), tag("w"))));

    let add_watchpoint = all_consuming(preceded(
        alt((tag("addwatchpoint"), tag("aw"))),
        preceded(space1, u16_hex),
    ));

    let remove_watchpoint = all_consuming(preceded(
        alt((tag("removewatchpoint"), tag("rw"))),
        preceded(space1, u16_hex),
    ));

    let exit = all_consuming(alt((
        tag("exit"),
        tag("quit"),
        tag("e"),
        tag("x"),
        tag("q"),
    )));

    let repeat = all_consuming(space0);

    let mut commands = alt((
        map(show_regs, |_| Command::ShowRegs),
        map(step, |count| Command::Step(count.unwrap_or(1))),
        map(continue_, |_| Command::Continue),
        map(goto, Command::Goto),
        map(show_mem, Command::ShowMem),
        map(show_ppu_mem, Command::ShowPpuMem),
        map(show_stack, |_| Command::ShowStack),
        map(disassemble, |count| {
            Command::Disassemble(count.unwrap_or(4))
        }),
        map(label, |_| Command::Label),
        map(add_label, |(name, addr)| {
            Command::AddLabel(name.into(), addr)
        }),
        map(remove_label, |name: &str| Command::RemoveLabel(name.into())),
        map(breakpoint, |_| Command::Breakpoint),
        map(add_breakpoint, Command::AddBreakpoint),
        map(remove_breakpoint, Command::RemoveBreakpoint),
        map(watchpoint, |_| Command::Watchpoint),
        map(add_watchpoint, Command::AddWatchpoint),
        map(remove_watchpoint, Command::RemoveWatchpoint),
        map(exit, |_| Command::Exit),
        map(repeat, |_| Command::Repeat),
    ));

    commands(input)
}
