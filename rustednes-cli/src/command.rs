// Based on command.rs from https://github.com/emu-rs/rustual-boy

use combine::char::{alpha_num, digit, hex_digit, space, spaces, string};
use combine::primitives::{ParseResult, Stream};
use combine::{choice, eof, many1, optional, parser, r#try, value, Parser};

use std::borrow::Cow;
use std::str::{self, FromStr};

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
    type Err = Cow<'static, str>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parser(command).parse(s) {
            Ok((c, _)) => Ok(c),
            err => Err(format!("Unable to parse command: {:?}", err).into()),
        }
    }
}

fn command<I: Stream<Item = char>>(input: I) -> ParseResult<Command, I> {
    let show_regs = choice([r#try(string("showregs")), r#try(string("r"))])
        .map(|_| Command::ShowRegs)
        .boxed();

    let step = (
        choice([r#try(string("step")), r#try(string("s"))]),
        optional((spaces(), u16_()).map(|x| x.1)),
    )
        .map(|(_, count)| Command::Step(count.unwrap_or(1)))
        .boxed();

    let continue_ = choice([r#try(string("continue")), r#try(string("c"))])
        .map(|_| Command::Continue)
        .boxed();

    let goto = (
        choice([r#try(string("goto")), r#try(string("g"))]),
        spaces(),
        u16_hex(),
    )
        .map(|(_, _, addr)| Command::Goto(addr))
        .boxed();

    let show_mem = (
        choice([r#try(string("showmem")), r#try(string("m"))]),
        optional((spaces(), u16_hex()).map(|x| x.1)),
    )
        .map(|(_, addr)| Command::ShowMem(addr))
        .boxed();

    let show_ppu_mem = (
        choice([r#try(string("showppumem")), r#try(string("pm"))]),
        space(),
        u16_hex(),
    )
        .map(|(_, _, addr)| Command::ShowPpuMem(addr))
        .boxed();

    let show_stack = choice([r#try(string("showstack")), r#try(string("ss"))])
        .map(|_| Command::ShowStack)
        .boxed();

    let disassemble = (
        choice([r#try(string("disassemble")), r#try(string("d"))]),
        optional((spaces(), u16_()).map(|x| x.1)),
    )
        .map(|(_, count)| Command::Disassemble(count.unwrap_or(4)))
        .boxed();

    let label = choice([r#try(string("label")), r#try(string("l"))])
        .map(|_| Command::Label)
        .boxed();

    let add_label = (
        choice([r#try(string("addlabel")), r#try(string("al"))]),
        space(),
        label_name(),
        space(),
        u16_hex(),
    )
        .map(|(_, _, name, _, addr)| Command::AddLabel(name, addr))
        .boxed();

    let remove_label = (
        choice([r#try(string("removelabel")), r#try(string("rl"))]),
        space(),
        label_name(),
    )
        .map(|(_, _, name)| Command::RemoveLabel(name))
        .boxed();

    let breakpoint = choice([r#try(string("breakpoint")), r#try(string("b"))])
        .map(|_| Command::Breakpoint)
        .boxed();

    let add_breakpoint = (
        choice([r#try(string("addbreakpoint")), r#try(string("ab"))]),
        space(),
        u16_hex(),
    )
        .map(|(_, _, addr)| Command::AddBreakpoint(addr))
        .boxed();

    let remove_breakpoint = (
        choice([r#try(string("removebreakpoint")), r#try(string("rb"))]),
        space(),
        u16_hex(),
    )
        .map(|(_, _, addr)| Command::RemoveBreakpoint(addr))
        .boxed();

    let watchpoint = choice([r#try(string("watchpoint")), r#try(string("w"))])
        .map(|_| Command::Watchpoint)
        .boxed();

    let add_watchpoint = (
        choice([r#try(string("addwatchpoint")), r#try(string("aw"))]),
        space(),
        u16_hex(),
    )
        .map(|(_, _, addr)| Command::AddWatchpoint(addr))
        .boxed();

    let remove_watchpoint = (
        choice([r#try(string("removewatchpoint")), r#try(string("rw"))]),
        space(),
        u16_hex(),
    )
        .map(|(_, _, addr)| Command::RemoveWatchpoint(addr))
        .boxed();

    let exit = choice([
        r#try(string("exit")),
        r#try(string("quit")),
        r#try(string("e")),
        r#try(string("x")),
        r#try(string("q")),
    ])
    .map(|_| Command::Exit)
    .boxed();

    let repeat = value(Command::Repeat).boxed();

    choice(
        vec![
            show_regs,
            step,
            continue_,
            goto,
            show_mem,
            show_ppu_mem,
            show_stack,
            disassemble,
            label,
            add_label,
            remove_label,
            breakpoint,
            add_breakpoint,
            remove_breakpoint,
            watchpoint,
            add_watchpoint,
            remove_watchpoint,
            exit,
            repeat,
        ]
        .into_iter()
        .map(|parser| (parser, eof()).map(|x| x.0))
        .map(|parser| r#try(parser))
        .collect::<Vec<_>>(),
    )
    .parse_stream(input)
}

fn u16_<'a, I: Stream<Item = char> + 'a>() -> Box<Parser<Input = I, Output = u16> + 'a> {
    many1(digit())
        .and_then(|s: String| s.parse::<u16>())
        .boxed()
}

fn u16_hex<'a, I: Stream<Item = char> + 'a>() -> Box<Parser<Input = I, Output = u16> + 'a> {
    let hex_prefix = choice([r#try(string("0x")), r#try(string("$"))]);
    (optional(hex_prefix), many1(hex_digit()))
        .map(|x| x.1)
        .and_then(|s: String| u16::from_str_radix(&s, 16))
        .boxed()
}

fn label_name<'a, I: Stream<Item = char> + 'a>() -> Box<Parser<Input = I, Output = String> + 'a> {
    many1::<String, _>(alpha_num()).boxed()
}
