{ pkgs ? import <nixpkgs> {},
  hc ? "ghc844"
}:

pkgs.stdenv.mkDerivation rec {
  name = "shower";
  buildInputs = [
    pkgs.haskell.compiler.${hc}
    pkgs.git
    pkgs.cabal-install
    pkgs.pkgconfig
  ];
}
