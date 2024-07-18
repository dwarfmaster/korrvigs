{ mkDerivation, aeson, base, base16-bytestring, binary, bytestring
, containers, data-default, data-endian, directory, extra, filepath
, HUnit, lens, lib, linear, monad-loops, mtl, network-uri, opaleye
, optparse-applicative, pandoc, pandoc-types, parsec
, parsec3-numbers, postgresql-simple, product-profunctors
, profunctors, text, text-builder, time
}:
mkDerivation {
  pname = "korrvigs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring binary bytestring containers
    data-default data-endian directory extra filepath HUnit lens linear
    monad-loops mtl network-uri opaleye optparse-applicative pandoc
    pandoc-types parsec parsec3-numbers postgresql-simple
    product-profunctors profunctors text text-builder time
  ];
  doHaddock = false;
  description = "A wiki system for my personal use";
  license = lib.licenses.mit;
  mainProgram = "korrvigs-cli";
}
