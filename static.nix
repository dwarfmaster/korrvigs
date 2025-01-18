{
  runCommand,
  nodePackages_latest,
  fetchzip,
  fetchurl,
  fetchFromGitHub,
  gnused,
  theme ? {
    base01 = "#302b25";
    base02 = "#48413a";
    base03 = "#9d8b70";
    base04 = "#b4a490";
    base05 = "#cabcb1";
    base06 = "#d7c8bc";
    base07 = "#e4d4c8";
    base08 = "#d35c5c";
    base09 = "#ca7f32";
    base0A = "#e0ac16";
    base0B = "#b7ba53";
    base0C = "#6eb958";
    base0D = "#88a4d3";
    base0E = "#bb90e2";
    base0F = "#b49368";
  },
}: let
  leaflet = fetchzip {
    url = "https://leafletjs-cdn.s3.amazonaws.com/content/leaflet/v1.9.4/leaflet.zip";
    sha256 = "0bjl1f911qdmnsrxl2jprnaz0sgkbm5dq4fqmh6mr6r90brnlf5x";
    stripRoot = false;
  };
  vis-network = fetchurl {
    url = "https://unpkg.com/vis-network/standalone/umd/vis-network.min.js";
    sha256 = "1rd2l7l8vz4a9d5fkiwrk7ikk3z89xyn7c3bhpz7xycvvcyq6gzm";
  };
  vis-timeline = fetchurl {
    url = "https://unpkg.com/vis-timeline/standalone/umd/vis-timeline-graph2d.min.js";
    sha256 = "1mk5x9fjnppv5l42m225qnplymbxn3565rn572jrcdhhaxsn5xam";
  };
  ace = fetchFromGitHub {
    owner = "ajaxorg";
    repo = "ace-builds";
    rev = "da6219a75f73d63e2298c9b4c293793faf98e6d9";
    sha256 = "1pbfz7bqrpliwc2n0lzmssmpykh5qszvmfr6q149syvvdznklhd3";
  };
  sed = "${gnused}/bin/sed";
in
  runCommand "korrvigs-static" {} ''
    mkdir -p $out
    cp -r ${nodePackages_latest.mathjax}/lib/node_modules/mathjax $out/mathjax
    cp -r ${leaflet} $out/leaflet
    mkdir -p $out/vis
    cp ${vis-network} $out/vis/vis-network.min.js
    cp ${vis-timeline} $out/vis/vis-timeline-graph2d.min.js
    cp -r ${ace}/src-min $out/ace
    cp ${./ressources/favicon.ico} $out/favicon.ico
    mkdir $out/icons
    ${sed} 's/stroke="#000000"/stroke="${theme.base09}"/' ${./ressources/icons/checkbox-todo.svg} > $out/icons/checkbox-todo.svg
    ${sed} 's/stroke="#000000"/stroke="${theme.base0E}"/' ${./ressources/icons/checkbox-ongoing.svg} > $out/icons/checkbox-ongoing.svg
    ${sed} 's/stroke="#000000"/stroke="${theme.base0B}"/' ${./ressources/icons/checkbox-done.svg} > $out/icons/checkbox-done.svg
  ''
