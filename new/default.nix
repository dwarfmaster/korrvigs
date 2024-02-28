{ mkDerivation, aeson, base, binary, bytestring, bytestring-lexing
, containers, data-endian, lens, lib, linear, mtl, opaleye
, postgresql-simple, product-profunctors, profunctors, text, time
}:
mkDerivation {
  pname = "korrvigs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytestring bytestring-lexing containers
    data-endian lens linear mtl opaleye postgresql-simple
    product-profunctors profunctors text time
  ];
  testHaskellDepends = [ base ];
  description = "A wiki system for my personal use";
  license = lib.licenses.mit;
}
