<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# VexRiscv integration

## Building

For building the CPU, the following software needs to be installed and available
in the `PATH`:

- a recent JDK installation
- SBT, the scala build tool
- a recent C compiler
- `verilator`
- `make` for building the verilated library and FFI code

## Notes for using the core:

- VexRiscv has a "reset vector" for the instruction bus. This is the initial
  PC that gets fetched. This address only gets presented to the IBUS after at
  least one cycle of RST being asserted.

- When using the verilated FFI version, there needs to be a delay to prevent a
  combinatorial loop. (input depends on output, which depends on input, which..)
  That means that there are some combinatorial dependencies that don't work as
  they would in regular Haskell/Clash.

  A result of this is that storages _MUST_ make sure to always have at least one
  cycle where ACK is deasserted between any cycles with asserted ACKs.

  This probably does not need to be done when both the core and the Clash code
  gets translated to HDL and simulated together.

- The contents of memories need to be stored in little endian. This means that
  for example the contents of an ELF file need to be endian-swapped before being
  used as the contents of the instruction storage.
  This applies to all storages.
