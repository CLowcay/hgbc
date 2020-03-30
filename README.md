# H-GBC

An emulator for the Nintendo Gameboy Color in Haskell because -- why not?

Work in progress!

# Build

Requires Haskell Stack, which you can install from
<https://docs.haskellstack.org/en/stable/README/>

Build with:

    stack build

# Run

Make executables available on your PATH with:

    stack install

Run the SDL frontend with:

    hgbc-sdl <rom file>

For a list of command line options:

    hgbc-sdl --help

After the first run, there will be a configuration `~/.hgbc/config.toml`
where default options can be set globally. This file also allows the user to
change the key bindings and colour palettes for monochrome ROMs.

For ROMs with backing RAM, RTC, or other state that needs to be saved between
sessions, that state is saved in the directory `~/.hgbc/<rom_file_name>/`
where `<rom_file_name>` is the name of the ROM file. It is also possible to
put a `config.toml` file in the `~/.hgbc/<rom_file_name>` directory to set
default options for a ROM. ROM specific configuration files have the same
format as the global configuration file.
