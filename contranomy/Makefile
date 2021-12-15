TOOLCHAIN ?= riscv64-unknown-elf

# We want to run C code in a freestanding environment, treating the RISC core
# as some generic microcontroller. However, it may be convenient to still allow
# "normal" C programs with a `main` function to be used. To this end, we
# provide a crt0.s which sets up the stack and calls `main`, and use a modified
# version of the default linker script which specifies the address space.

CC = ${TOOLCHAIN}-gcc
CFLAGS = -ffreestanding -nostdlib -mabi=ilp32 -march=rv32im

CABAL ?= cabal

%.elf: %.c
	$(CC) $(CFLAGS) -Wl,-T,linker.ld crt0.s $< -o $@

# In the event we don't want to use this linker script and crt0.s at all,
# `make` can be called with a target of `.free.elf` insetad of `.elf`.

%.free.elf: %.c
	$(CC) $(CFLAGS) $< -o $@

# Building and simulating a file from make can be achieved by calling
#
#   make run_FILE
#
# where FILE is the basename (without extension) of a file which can be
# compiled and passed to the simulator (e.g. a C file or an ELF file).

.PHONY: simulate
run_%: %.elf
	$(CABAL) run -- simcontranomy $<

.PHONY: clean
clean:
	rm -f *.elf

