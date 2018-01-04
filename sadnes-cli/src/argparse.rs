use clap::{App, Arg};

pub struct CommandLineConfig {
    pub rom_path: String,
    pub debug: bool,
}

pub fn parse_args() -> CommandLineConfig {
    let app = App::new("sadNES")
        .version(crate_version!())
        .author(crate_authors!())
        .about("A CLI frontend to the sadNES emulator")
        .arg(Arg::with_name("ROM")
            .help("The name of the ROM to load")
            .required(true)
            .index(1)
        )
        .arg(Arg::with_name("debug")
            .help("Start in debug mode")
            .short("d")
        );

    let matches = app.get_matches();
    let rom_path = matches.value_of("ROM").unwrap();
    let debug = matches.is_present("debug");

    CommandLineConfig {
        rom_path: rom_path.into(),
        debug,
    }
}