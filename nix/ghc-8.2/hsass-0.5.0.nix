{ mkDerivation, base, bytestring, data-default-class, filepath
, hlibsass, hspec, hspec-discover, monad-loops, stdenv, temporary
, transformers
}:
mkDerivation {
  pname = "hsass";
  version = "0.5.0";
  sha256 = "8dc4c6a7455a1182ec2dba36c489f89d7e5c1053388b2c63c4ddba6080b7501e";
  revision = "1";
  editedCabalFile = "00va8x51p6rbg8jbkxpyz74pa2pfm5dsrpi57icbyickx39iaydd";
  libraryHaskellDepends = [
    base bytestring data-default-class filepath hlibsass monad-loops
    transformers
  ];
  testHaskellDepends = [
    base bytestring data-default-class hspec hspec-discover temporary
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "https://github.com/jakubfijalkowski/hsass";
  description = "Integrating Sass into Haskell applications";
  license = stdenv.lib.licenses.mit;
}
