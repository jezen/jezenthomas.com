let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a4c4cbb613cc3e15186de0fdb04082fa7e38f6a0";
    sha256 = "1lagfycy2lvfc8cdxk98dz2rxjlrbmv9hj42x0x40sy66bck1w0y";
  };
  pkgs = import nixpkgs {};
in
  pkgs.stdenv.mkDerivation {
    name = "jgt";
    src = ./src;
    phases = "unpackPhase buildPhase";
    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (p: with p; [ hakyll ]))
    ];
    buildPhase = ''
      mkdir -p $out/bin
      ghc -O2 -dynamic --make Main.hs -o $out/bin/jgt
    '';
  }
