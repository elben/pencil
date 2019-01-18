{ mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, conduit, containers, directory, filepath, hspec, HUnit, mockery
, resourcet, scientific, semigroups, stdenv, template-haskell
, temporary, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "yaml";
  version = "0.8.32";
  sha256 = "dc20f863deb4ee75395bf1f78268781db76be6209af67b70c05f6e1a09f47a31";
  configureFlags = [ "-f-system-libyaml" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring conduit containers directory
    filepath resourcet scientific semigroups template-haskell text
    transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson attoparsec base base-compat bytestring conduit containers
    directory filepath hspec HUnit mockery resourcet scientific
    semigroups template-haskell temporary text transformers
    unordered-containers vector
  ];
  homepage = "https://github.com/snoyberg/yaml#readme";
  description = "Support for parsing and rendering YAML documents";
  license = stdenv.lib.licenses.bsd3;
}
