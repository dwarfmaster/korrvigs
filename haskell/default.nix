{ mkDerivation, base, lib, yesod }:
mkDerivation {
  pname = "korrvigs-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base yesod ];
  license = lib.licenses.mit;
  mainProgram = "korrvigs-web";
}
