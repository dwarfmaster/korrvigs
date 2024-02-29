{ mkDerivation, aeson, base, base16-bytestring, binary, bytestring
, containers, data-endian, lens, lib, linear, mtl, opaleye
, postgresql-simple, product-profunctors, profunctors, text, time
}:
mkDerivation {
  pname = "korrvigs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring binary bytestring containers
    data-endian lens linear mtl opaleye postgresql-simple
    product-profunctors profunctors text time
  ];
  executableHaskellDepends = [
    aeson base bytestring containers lens linear mtl opaleye
    postgresql-simple product-profunctors profunctors text time
  ];
  testHaskellDepends = [
    aeson base bytestring containers lens linear mtl opaleye
    postgresql-simple product-profunctors profunctors text time
  ];
  doHaddock = false;
  description = "A wiki system for my personal use";
  license = lib.licenses.mit;
  mainProgram = "korrvigs-cli";
}
