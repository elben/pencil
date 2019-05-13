{ mkDerivation, base, data-default, directory, doctest
, edit-distance, filepath, hashable, hsass, mtl, pandoc, parsec
, semigroups, stdenv, text, time, unordered-containers, vector, xml
, yaml
}:
mkDerivation {
  pname = "pencil";
  version = "0.1.4";
  src = ./.;
  libraryHaskellDepends = [
    base data-default directory edit-distance filepath hashable hsass
    mtl pandoc parsec semigroups text time unordered-containers vector
    xml yaml
  ];
  testHaskellDepends = [
    base doctest mtl text unordered-containers
  ];
  homepage = "https://github.com/elben/pencil";
  description = "Static site generator";
  license = stdenv.lib.licenses.bsd3;
}
