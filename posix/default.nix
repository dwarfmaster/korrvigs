{
  stdenv,
  lib,
  pkg-config,
  swiProlog,
}:
stdenv.mkDerivation {
  pname = "korrvigs-posix";
  version = "1.0";
  src = ./.;

  buildInputs = [
    pkg-config
    swiProlog
  ];

  installPhase = ''
    mkdir -p $out/lib/korrvigs/modules
    install -m 444 posix.pl $out/lib/korrvigs/modules
    mkdir -p $out/lib/korrvigs/foreign
    install -m 444 posix.so $out/lib/korrvigs/foreign
  '';

  meta = {
    description = "Missing posix functions from swiProlog libraries";
    maintainers = [lib.maintainers.dwarfmaster];
    license = lib.licenses.mit;
  };
}
