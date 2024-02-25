{ mkDerivation, aeson, base, containers, lens, lib, linear, opaleye
, postgresql-simple, product-profunctors, text, time
}:
mkDerivation {
  pname = "korrvigs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers lens linear opaleye postgresql-simple
    product-profunctors text time
  ];
  testHaskellDepends = [ base ];
  description = "A wiki system for my personal use";
  license = lib.licenses.mit;
}
