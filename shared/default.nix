{ mkDerivation, base, servant, aeson, stdenv }:
mkDerivation {
  pname = "shared";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base servant aeson ];
  license = null;
}
