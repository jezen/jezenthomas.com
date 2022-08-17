let
  pkgs =
    import (builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs";
      rev = "dbb62c34bbb5cdf05f1aeab07638b24b0824d605";
      ref = "nixos-22.05";
    }) {};
in
  pkgs.stdenv.mkDerivation {
    name = "jgt";
    src = ./src;
    phases = "unpackPhase buildPhase";
    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (p: with p; [ hakyll pureMD5 ]))
    ];
    buildPhase = ''
      mkdir -p $out/bin
      ghc -O2 -dynamic --make Main.hs -o $out/bin/jgt
    '';
  }
