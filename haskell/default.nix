{ mkDerivation, aeson, array, base, bytestring, clientsession
, containers, data-default, dhall, directory, file-embed, filepath
, hashable, http-types, lib, opaleye, pandoc, pandoc-types, parsec
, postgresql-simple, product-profunctors, random, shakespeare
, temporary, text, uuid, yesod, yesod-core
}:
mkDerivation {
  pname = "korrvigs-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson array base bytestring clientsession containers data-default
    dhall directory file-embed filepath hashable http-types opaleye
    pandoc pandoc-types parsec postgresql-simple product-profunctors
    random shakespeare temporary text uuid yesod yesod-core
  ];
  license = lib.licenses.mit;
  mainProgram = "korrvigs-web";
}
