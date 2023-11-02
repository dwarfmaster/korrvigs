{ mkDerivation, aeson, base, clientsession, containers, filepath
, hashable, lib, opaleye, postgresql-simple, product-profunctors
, random, shakespeare, text, uuid, yesod, yesod-core
}:
mkDerivation {
  pname = "korrvigs-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base clientsession containers filepath hashable opaleye
    postgresql-simple product-profunctors random shakespeare text uuid
    yesod yesod-core
  ];
  license = lib.licenses.mit;
  mainProgram = "korrvigs-web";
}
