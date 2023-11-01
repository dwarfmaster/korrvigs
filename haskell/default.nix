{ mkDerivation, aeson, base, clientsession, containers, filepath
, lib, opaleye, postgresql-simple, product-profunctors, shakespeare
, text, uuid, yesod, yesod-core
}:
mkDerivation {
  pname = "korrvigs-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base clientsession containers filepath opaleye
    postgresql-simple product-profunctors shakespeare text uuid yesod
    yesod-core
  ];
  license = lib.licenses.mit;
  mainProgram = "korrvigs-web";
}
