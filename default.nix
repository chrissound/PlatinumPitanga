{
  nixpkgs ? import <nixpkgs> {}
, sources ? import ./nix/sources.nix
, compiler ? "ghc864" } :
let
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
in
pkgs.haskell.lib.buildStackProject {
  name = "platinumpitanga";
  src = ./.;
}
