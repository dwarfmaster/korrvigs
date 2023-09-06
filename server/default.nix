{
  lib,
  stdenv,
  cmake,
  ...
}:
stdenv.mkDerivation {
  pname = "Korrvigs server";
  version = "0.1";
  src = ./.;

  buildInputs = [cmake];

  meta = {
    license = lib.licenses.mit;
    maintainers = [lib.maintainers.dwarfmaster];
    description = "A datalog-engine backed wiki system";
  };
}
