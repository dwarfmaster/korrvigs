{ mkDerivation, base, containers, filepath, lib, opaleye
, postgresql-simple, product-profunctors, text, uuid, yesod
}:
mkDerivation {
  pname = "korrvigs-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers filepath opaleye postgresql-simple
    product-profunctors text uuid yesod
  ];
  license = lib.licenses.mit;
  mainProgram = "korrvigs-web";
}
