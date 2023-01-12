{ stdenv, lib, pkg-config, swiProlog, tree-sitter, tree-sitter-norg-source }:

stdenv.mkDerivation {
  pname = "korrvigs-norg-treesitter";
  version = "1.0";
  src = ./.;

  NORG_PARSER_SOURCE="${tree-sitter-norg-source}/src";
  buildInputs = [
    swiProlog
    tree-sitter
    pkg-config
  ];

  installPhase = ''
    mkdir -p $out/lib/korrvigs/modules
    install -m 444 norg-parser.pl $out/lib/korrvigs/modules
    mkdir -p $out/lib/korrvigs/foreign
    install -m 444 norg_parser.so $out/lib/korrvigs/foreign
  '';

  meta = {
    description = "Norg parser using tree-sitter for swi prolog";
    maintainers = [ lib.maintainers.dwarfmaster ];
    license = lib.licenses.mit;
  };
}
