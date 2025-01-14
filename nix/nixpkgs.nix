{ sources ? import ./sources.nix }:

let
  rust_overlay = import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz");
  overlay = _: nixpkgs: {
    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (nixpkgs) lib; };

    verilog-ethernet = import ./verilog-ethernet.nix { inherit (nixpkgs) pkgs; };

    # Haskell overrides
    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        # Disables libray profiling, documentation building, and building/running
        # test suites to speed up compilation times. This should only be applied
        # to local packages. Adding them to others will defeat caching.
        applyPrefs = p:
          (nixpkgs.haskell.lib.disableLibraryProfiling
            (nixpkgs.haskell.lib.dontHaddock
              (nixpkgs.haskell.lib.dontCheck p)));

        # External overrides
        # .. no external overrides yet ..

        # Internal overrides
        # .. no internal overrides yet ..
      };
    };
  };

in import sources.nixpkgs { overlays = [ rust_overlay overlay ]; }
