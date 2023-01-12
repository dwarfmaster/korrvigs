{ stdenv, lib }:

stdenv.mkDerivation {
  pname = "piper";
  version = "1.0";
  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    install -m 555 piper.out $out/bin/piper
  '';

  meta = {
    description = "Bind command stdin and stdout to socket";
    maintainers = [ lib.maintainers.dwarfmaster ];
    license = lib.licenses.mit;
  };
}
