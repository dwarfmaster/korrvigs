{
  runCommand,
  nodePackages_latest,
  fetchzip,
  fetchurl,
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
in
  runCommand "korrvigs-static" {} ''
    mkdir -p $out
    cp -r ${nodePackages_latest.mathjax}/lib/node_modules/mathjax $out/mathjax
    cp -r ${leaflet} $out/leaflet
    mkdir -p $out/vis
    cp ${vis-network} $out/vis/vis-network.min.js
    cp ${vis-timeline} $out/vis/vis-timeline-graph2d.min.js
  ''
