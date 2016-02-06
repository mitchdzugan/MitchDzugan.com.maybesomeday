{ nixpkgs ? import <nixpkgs> {}, mkDerivation, aeson, base, servant-server, stdenv, wai, warp }:

let 
  inherit (nixpkgs) pkgs;
  shared = pkgs.haskellPackages.callPackage ../shared {}; 
in

mkDerivation {
  pname = "server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base servant-server wai warp shared ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "http://github.com/githubuser/server#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
