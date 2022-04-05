CARGO_TARGET_DIR=target

.PHONY: build-sim
build-sim:
	cd contranomy; cabal build simcontranomy


.PHONY: build-firmware-example-hello
build-firmware-example-hello:
	cd firmware/examples/hello; cargo build --release --target-dir ../../../$(CARGO_TARGET_DIR)

.PHONY: sim-firmware-example-hello
sim-firmware-example-hello: build-sim build-firmware-example-hello
	cp target/riscv32imc-unknown-none-elf/release/hello contranomy/main.elf
	cd contranomy; cabal run simcontranomy -- main.elf
