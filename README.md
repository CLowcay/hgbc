# H-GBC

![CI](https://github.com/CLowcay/hgbc/workflows/CI/badge.svg?branch=master)

An emulator for the Nintendo Gameboy Color in Haskell because -- why not?

Work in progress!

Passes all relevant parts of the Blargg test suite (cpu_instrs, instr_timing, interrupt_time, mem_timing, mem_timing-2, cgb_sound, and halt_bug).  Does not pass dmg_sound or oam_bug since H-GBC emulates the Gameboy Color, and the Gameboy Color hardware does not pass those tests.

Also passes most of the relevant parts of the Mooneye GB suite, with the notable exception of the PPU tests.  H-GBC's graphics code works reasonably well, but it is not cycle-accurate.  There may be some glitches or other differences from the original hardware.

The latest test report is at https://hgbc-test-results.netlify.app/ .

# Build

1. Install Haskell Stack from
   <https://docs.haskellstack.org/en/stable/README/>

2. Install dependencies

```
sudo apt install libsdl2-dev
```

3. Build with

```
stack build
```

# Run

Make executables available on your PATH with:

```
stack install
```

Run the SDL frontend with:

```
hgbc-sdl <rom file>
```    

For a list of command line options:

```
hgbc-sdl --help
```

After the first run, there will be a configuration `~/.hgbc/config.toml`
where default options can be set globally. This file also allows the user to
change the key bindings and colour palettes for monochrome ROMs.

For ROMs with backing RAM, RTC, or other state that needs to be saved between
sessions, that state is saved in the directory `~/.hgbc/<rom_file_name>/`
where `<rom_file_name>` is the name of the ROM file. It is also possible to
put a `config.toml` file in the `~/.hgbc/<rom_file_name>` directory to set
default options for a ROM. ROM specific configuration files have the same
format as the global configuration file.

# Debugger

To debug a ROM file, launch H-GBC with the `--debug` flag.  Then open a web
browser at `localhost:8080`.  The port number can be changed by a command line
option (`--debug-port`) or in `~/.hgbc/config.toml`.
