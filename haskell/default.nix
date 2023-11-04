{ mkDerivation, aeson, array, base, clientsession, containers
, data-default, directory, filepath, hashable, lib, opaleye, pandoc
, postgresql-simple, product-profunctors, random, shakespeare, text
, uuid, yesod, yesod-core
}:
mkDerivation {
  pname = "korrvigs-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson array base clientsession containers data-default directory
    filepath hashable opaleye pandoc postgresql-simple
    product-profunctors random shakespeare text uuid yesod yesod-core
  ];
  license = lib.licenses.mit;
  mainProgram = "korrvigs-web";
}
