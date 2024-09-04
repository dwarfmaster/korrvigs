{ mkDerivation, aeson, array, async, base, base16-bytestring
, binary, bytestring, case-insensitive, conduit, containers
, data-default, data-endian, directory, extra, file-embed, filepath
, hmatrix-glpk, http-conduit, http-types, HUnit, lens, lens-aeson
, lib, linear, mime-types, monad-loops, mtl, network-uri, opaleye
, optparse-applicative, pandoc, pandoc-types, parsec
, parsec3-numbers, postgresql-simple, process, product-profunctors
, profunctors, temporary, text, text-builder, text-manipulate, time
, unix, utf8-string, vector, xdg-basedir, xml-conduit
}:
mkDerivation {
  pname = "korrvigs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base base16-bytestring binary bytestring
    case-insensitive conduit containers data-default data-endian
    directory extra file-embed filepath hmatrix-glpk http-conduit
    http-types HUnit lens lens-aeson linear mime-types monad-loops mtl
    network-uri opaleye optparse-applicative pandoc pandoc-types parsec
    parsec3-numbers postgresql-simple process product-profunctors
    profunctors temporary text text-builder text-manipulate time unix
    utf8-string vector xdg-basedir xml-conduit
  ];
  testHaskellDepends = [ base HUnit ];
  doHaddock = false;
  description = "A wiki system for my personal use";
  license = lib.licenses.mit;
  mainProgram = "korrvigs-cli";
}
