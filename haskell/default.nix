{
  mkDerivation,
  aeson,
  array,
  base,
  bytestring,
  clientsession,
  containers,
  data-default,
  dhall,
  directory,
  file-embed,
  filepath,
  hashable,
  http-types,
  lib,
  mime-types,
  opaleye,
  pandoc,
  pandoc-types,
  parsec,
  postgresql-simple,
  product-profunctors,
  random,
  shakespeare,
  skylighting,
  temporary,
  text,
  transformers,
  uuid,
  yesod,
  yesod-core,
}:
mkDerivation {
  pname = "korrvigs-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    array
    base
    bytestring
    clientsession
    containers
    data-default
    dhall
    directory
    file-embed
    filepath
    hashable
    http-types
    mime-types
    opaleye
    pandoc
    pandoc-types
    parsec
    postgresql-simple
    product-profunctors
    random
    shakespeare
    skylighting
    temporary
    text
    transformers
    uuid
    yesod
    yesod-core
  ];
  license = lib.licenses.mit;
  mainProgram = "korrvigs-web";
}
