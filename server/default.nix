{
  mkDerivation,
  base,
  lib,
  parsec,
  text,
  uuid,
}:
mkDerivation {
  pname = "korrvigs-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [base parsec text uuid];
  license = lib.licenses.mit;
  mainProgram = "korrvigs-server";
}
