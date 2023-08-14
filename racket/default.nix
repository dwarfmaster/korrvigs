{
  stdenv,
  lib,
  racket,
}: let
  src = ./.;
in
  stdenv.mkDerivation {
    pname = "korrvigs-server";
    version = "0.1";

    inherit src;
    buildInputs = [racket];
    buildPhase = ''
      patchRequire () {
        query_start='s:^\(require "(.*)"\)$:(require (file "'
        query_end='/source/\1")):'
        sed -E "$query_start$out$query_end" -i $1
      }

      mkdir -p $out/source
      mkdir -p $out/bin
      install -m444 ${src}/datalog-utils.rkt $out/source
      patchRequire $out/source/datalog-utils.rkt
      install -m444 ${src}/files.rkt $out/source
      patchRequire $out/source/files.rkt
      install -m444 ${src}/parser.rkt $out/source
      patchRequire $out/source/parser.rkt
      install -m444 ${src}/server.rkt $out/source
      patchRequire $out/source/server.rkt
      install -m555 ${src}/main.rktl $out/bin
      patchRequire $out/bin/main.rktl
      patchShebangs $out/bin/main.rktl
    '';

    meta = {
      description = "A semantic wiki engine";
      maintainers = [lib.maintainers.dwarfmaster];
      license = lib.licenses.mit;
    };
  }
