{ mkDerivation
, base
, bytestring
, HUnit
, brotli-pkg
, QuickCheck
, stdenv
, tasty
, tasty-hunit
, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "brotli";
  version = "0.0.0.0";
  sha256 = "32e52ad9ebb1848ced38b1cc3919376005f9f65937498b2dcb6102ca0a8f38d1";
  revision = "1";
  editedCabalFile = "0fw26rv8i9zz4qyr32paz2y0psdppdaz427jp8mpbanwmg763024";
  libraryHaskellDepends = [ base bytestring transformers ];
  libraryPkgconfigDepends = [ brotli-pkg ];
  testHaskellDepends = [
    base
    bytestring
    HUnit
    QuickCheck
    tasty
    tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/hvr/brotli";
  description = "Brotli (RFC7932) compression and decompression";
  license = stdenv.lib.licenses.gpl3;
}
