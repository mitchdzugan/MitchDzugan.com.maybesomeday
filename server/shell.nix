{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskell = pkgs.haskellPackages;

  drv = haskell.callPackage ./default.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
