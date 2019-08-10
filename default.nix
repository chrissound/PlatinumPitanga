{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" } :
nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "platinumpitanga" (./.) {}

