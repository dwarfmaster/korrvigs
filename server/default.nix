{
  lib,
  stdenv,
  cmake,
  boost,
  ...
}:
stdenv.mkDerivation {
  pname = "Korrvigs server";
  version = "0.1";
  src = ./.;

  buildInputs = [cmake boost.dev];

  meta = {
    license = lib.licenses.mit;
    maintainers = [lib.maintainers.dwarfmaster];
    description = "A datalog-engine backed wiki system";
  };
}
