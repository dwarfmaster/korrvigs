{ stdenv, lib }:

stdenv.mkDerivation {
  pname = "korrvigs";
  version = "0.5";
  src = ./.;

  installPhase = ''
    mkdir -p $out/lib/korrvigs/modules
    install -m 444 main.pl $out/lib/korrvigs.pl
    for f in $(ls modules/*.pl); do
      install -m 444 $f $out/lib/korrvigs/modules
    done
  '';

  meta = {
    description = "Missing posix functions from swiProlog libraries";
    maintainers = [ lib.maintainers.dwarfmaster ];
    license = lib.licenses.mit;
  };
}
