# H-GBC

An emulator for the Nintendo Gameboy in Haskell because -- why not?

Work in progress!

TODO:
 * Graphics subsystem.
 * Further Optimisation.

# Instructions

Build with

    stack build

Run with 

    hgbc <rom file>

This will launch into the debugger.  Some debugger commands:

 * header - Show the ROM header in human readable format.
 * run - Run the ROM.
 * run _address_ - Run up until the given address.
 * reset - Restart the ROM.
 * registers - Show the CPU registers.
 * reg _register_ = _value_ - Set the value of a register.
 * mem - Dump memory.
 * mem _address_ - Dump memory at the given address.
 * mem _address_ = _value_ - Set a byte of memory.
 * code - Disassemble at current program counter.
 * code _address_ - Disassemble from the given address.
 * step - Execute one instruction.
 * step _n_ - Execute _n_ instructions.
 * step out - Execute until the end of the current procedure.
 * break _address_ - Create a breakpoint at the given address.
 * breakpoints - List all breakpoints.
 * symbol _name_ = _address_ - Create a symbol mapping a name to an address.
 * delete break _address_ = Delete a breakpoint.
 * delete symbol _name_ = Delete a symbol.
 * disable break _address_ = Disable a breakpoint.

