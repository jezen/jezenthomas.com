{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
          ];
        };
      in {
        packages.default = pkgs.lib.trivial.pipe pkgs.haskellPackages.jgt
          [
            pkgs.haskell.lib.dontHaddock
            pkgs.haskell.lib.enableStaticLibraries
            pkgs.haskell.lib.justStaticExecutables
            pkgs.haskell.lib.disableLibraryProfiling
            pkgs.haskell.lib.disableExecutableProfiling
          ];
      }) // {
      overlays.default = _: prev: {
        haskell = prev.haskell // {
          packageOverrides = prev.lib.composeExtensions prev.haskell.packageOverrides (_: hprev: {

            jgt =
              let
                haskellSourceFilter = prev.lib.sourceFilesBySuffices ./. [
                  ".cabal"
                  ".hs"
                  "LICENSE"
                ];
              in
              hprev.callCabal2nix "jgt" haskellSourceFilter { };
          });
        };
      };
    };
}
