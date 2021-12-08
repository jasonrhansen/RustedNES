# RustedNES

## Description

RustedNES is a Nintendo Entertainment System emulator written in the Rust programming language. It runs on Windows, MacOS, and Linux. It currently supports many commercial games.

## Screenshots

![screenshot mario](media/screenshot_mario.png)
![screenshot zelda](media/screenshot_zelda.png)
![screenshot metroid](media/screenshot_metroid.png)
![screenshot contra](media/screenshot_contra.png)

## Building

To build RustedNES you will need to have have Rust installed. You can find instructions here https://www.rust-lang.org/en-US/install.html

On Linux you may also need to install build dependencies. For Ubuntu that could be done like this:

```
sudo apt install libxkbcommon-dev libwayland-cursor2 libwayland-dev
```

Then run the following in the project directory to build:

```
cargo build --release
```

Note: the release flag may be necessary to get playable speed.

## Running

There are currently 2 different ways to run RustedNES. There is a simple CLI frontend, and a libretro core that can be used with RetroArch or other libretro frontends.

### CLI

The CLI interface is simple to use, but it's very bare-bones. The window is a fixed size, the controls are not configurable, etc. To run with more features, see [libretro](#libretro) below.

The built executable can be found in the `target/release` directory. The file is named `rustednes-cli` (with .exe extension on Windows).

To play a game, simply pass the ROM file as an argument.

```
$ rustednes-cli --help
RustedNES 0.1.0
Jason Rodney Hansen <jasonrodneyhansen@gmail.com>
A CLI frontend to the RustedNES emulator

USAGE:
    rustednes-cli [FLAGS] <ROM>

FLAGS:
    -d, --debug      Start in debug mode
    -h, --help       Prints help information
        --noaudio    Disable audio
    -V, --version    Prints version information

ARGS:
    <ROM>    The name of the ROM to load
```

Here are the keyboard controls:

| Button | Key |
| --- | --- |
| Left | <kbd>left</kbd> |
| Right | <kbd>right</kbd> |
| Up | <kbd>up</kbd> |
| Down | <kbd>down</kbd> |
| Select | <kbd>space</kbd> |
| Start | <kbd>enter/return</kbd> |
| A | <kbd>X</kbd> |
| B | <kbd>Z</kbd> |

### libretro

Using RustedNES with a libretro frontend, such as RetroArch, allows many additional features, such as:

* Save states
* SRAM saving
* Changing window size / fullscreen
* Gamepads and other input devices
* Configurable buttons
* Cheats

To run on RetroArch you will need to find the shared library of the libretro core that was created in the build step. You can find it in the `target/release` directory. The name of the file will be different depending on the platform:

* `rustednes_libretro.dll` on Windows
* `librustednes_libretro.dylib` on MacOS
* `librustednes_libretro.so` on Linux

You need to pass that file as an argument after -L, as in the following example for MacOS:

```
retroarch -L librustednes_libretro.dylib name_of_rom.nes
```

## License

Duel-license under MIT license ([LICENSE-MIT](LICENSE-MIT)), or Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))

## Special Thanks

Thanks to the people who created the following emulator projects, which were used for ideas and inspiration for this project:

* https://github.com/emu-rs/rustual-boy (Virtual Boy emulator written in Rust)
* https://github.com/pcwalton/sprocketnes (NES emulator written in Rust)
* https://github.com/fogleman/nes (NES emulator written in Go)

Also thanks to nesdev.com for all of the awesome resources on their wiki: http://wiki.nesdev.com/w/index.php/Nesdev_Wiki
