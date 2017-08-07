{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "datastructures";
  buildInputs = [pkgconfig zlib ncurses];
  inherit ghc;
}
