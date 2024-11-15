{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/dbb62c34bbb5cdf05f1aeab07638b24b0824d605";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { };
    in {
      packages = {
        default = pkgs.stdenv.mkDerivation {
          pname = "jgt";
          version = "0.1.0";

          src = ./src;

          phases = "unpackPhase buildPhase";
          buildInputs = [
            (pkgs.haskellPackages.ghcWithPackages (p: with p; [ hakyll pureMD5 ]))
          ];

          buildPhase = ''
            mkdir -p $out/bin
            ghc -O2 -dynamic --make Main.hs -o $out/bin/jgt
          '';
        };
      };
    };
}
