{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "shared";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = null;
}
