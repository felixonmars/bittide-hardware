GHC=ghc-9.0.2
CARGO_TARGET_DIR=target

.PHONY: build-sim
build-sim:
	cd contranomy; cabal build simcontranomy -w $(GHC)


.PHONY: build-loader
build-loader: build-loaded-program
	cd contranomy-loader; cargo build --release --target-dir ../$(CARGO_TARGET_DIR)

.PHONY: build-loaded-program
build-loaded-program:
	cd contranomy-loaded-program; cargo build --release --target-dir ../$(CARGO_TARGET_DIR)

.PHONY: sim-loader
sim-loader: build-sim build-loader
	cp target/riscv32imc-unknown-none-elf/release/contranomy-loader contranomy/main.elf
	cd contranomy; cabal run simcontranomy -w $(GHC)

.PHONY: sim-loaded-program
sim-loaded-program: build-sim build-loader
	cp target/riscv32imc-unknown-none-elf/release/contranomy-loaded-program contranomy/main.elf
	cd contranomy; cabal run simcontranomy -w $(GHC)
