{ mkDerivation, base, filepath, fsnotify, lib, reflex, regex-base
, regex-posix
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base filepath fsnotify reflex regex-base regex-posix
  ];
  description = "Knowledge management system";
  license = lib.licenses.mit;
  mainProgram = "backend";
}
