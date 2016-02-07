{ mkDerivation, aeson, attoparsec, base, bytestring
, case-insensitive, deepseq, exceptions, hspec, http-api-data
, http-client, http-client-tls, http-media, http-types, HUnit
, mockery, network, network-uri, process, QuickCheck, safe, servant
, servant-server, stdenv, string-conversions, text, transformers
, transformers-compat, wai, warp
}:
mkDerivation {
  pname = "servant-client";
  version = "0.5";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring case-insensitive exceptions
    http-api-data http-client http-client-tls http-media http-types
    network-uri safe servant string-conversions text transformers
    transformers-compat
  ];
  testHaskellDepends = [
    aeson base bytestring deepseq hspec http-client http-media
    http-types HUnit mockery network process QuickCheck safe servant
    servant-server text transformers transformers-compat wai warp
  ];
  homepage = "http://haskell-servant.github.io/";
  description = "automatical derivation of querying functions for servant webservices";
  license = stdenv.lib.licenses.bsd3;
}
