let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "7283740218a5178185a8c1bf0ecfa861f5f9f0f7";
    sha256 = "1rch0whlswwqfj4z9ijwc0hj9xrbwc04zzgrd4y52fw87s4r4zz5";
  };
  pkgs = import nixpkgs {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      happy = pkgs.haskell.lib.dontCheck super.happy;
    };
  };

in
  pkgs.stdenv.mkDerivation {
    name = "jgt";
    src = ./src;
    phases = "unpackPhase buildPhase";
    buildInputs = [
      (haskellPackages.ghcWithPackages (p: with p; [ hakyll ]))
    ];
    buildPhase = ''
      mkdir -p $out/bin
      ghc -O2 -dynamic --make Main.hs -o $out/bin/jgt
    '';
  }
