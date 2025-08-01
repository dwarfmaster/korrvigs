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
    url = "https://unpkg.com/vis-network@9.1.10/standalone/umd/vis-network.min.js";
    sha256 = "1dq8iyahkz3yv1019zz98qm6lbab1bsiwm2fyq9jvcvsvdxl6sx0";
  };
  vis-timeline = fetchurl {
    url = "https://unpkg.com/vis-timeline@7.7.4/standalone/umd/vis-timeline-graph2d.min.js";
    sha256 = "0k1xzb3rc3xkn9msgis9f1znkrwzrhmy4k7bmdlbraa6a7mil89x";
  };
  fuse = fetchurl {
    url = "https://cdn.jsdelivr.net/npm/fuse.js/dist/fuse.basic.min.js";
    sha256 = "1x96hgn2mqrkf97jvksylhr8lb4wbwh64z60mf9dkxmqsjj79chs";
  };
  ace = fetchFromGitHub {
    owner = "ajaxorg";
    repo = "ace-builds";
    rev = "da6219a75f73d63e2298c9b4c293793faf98e6d9";
    sha256 = "1pbfz7bqrpliwc2n0lzmssmpykh5qszvmfr6q149syvvdznklhd3";
  };
  photoswipe = fetchFromGitHub {
    owner = "dimsemenov";
    repo = "PhotoSwipe";
    rev = "d80c32a62b169e776ad1c983d1fcdc6eea8b48e0";
    sha256 = "1a2rfvid8iqsnq582myjbbz3mjavwy7566lgfbds0badzf9l5srm";
  };
  photoswipe-video = fetchFromGitHub {
    owner = "dimsemenov";
    repo = "photoswipe-video-plugin";
    rev = "5e32d6589df53df2887900bcd55267d72aee57a6";
    sha256 = "0l9rr3pb5mh7iv5m5d4b1x8qd26jr7nxzjwnqx4zk406bcqv672a";
  };
  fullcalendar = fetchurl {
    url = "https://cdn.jsdelivr.net/npm/fullcalendar@6.1.17/index.global.min.js";
    sha256 = "0x0hx6yazpp0qcqw103y14gr7pvd5r1g7638c6cpxa6yp3fimypr";
  };
  sed = "${gnused}/bin/sed";
  charis = fetchzip {
    url = "https://software.sil.org/downloads/r/charis/CharisSIL-6.200.zip";
    sha256 = "0yl0cjfnbgzjj0aa9gnimgfsp477qapz1avrdba5v5rzwyapb3mb";
    stripRoot = true;
  };
in
  runCommand "korrvigs-static" {} ''
    mkdir -p $out
    cp -r ${nodePackages_latest.mathjax}/lib/node_modules/mathjax $out/mathjax
    cp -r ${leaflet} $out/leaflet
    mkdir -p $out/vis
    cp ${vis-network} $out/vis/vis-network.min.js
    cp ${vis-timeline} $out/vis/vis-timeline-graph2d.min.js
    cp -r ${ace}/src-min $out/ace
    cp -r ${photoswipe}/dist $out/photoswipe
    cp -r ${photoswipe-video}/dist $out/photoswipe-video-plugin
    mkdir -p $out/fuse
    cp ${fuse} $out/fuse/fuse.js
    mkdir -p $out/fullcalendar
    cp -r ${fullcalendar} $out/fullcalendar/index.global.min.js
    cp ${./ressources/favicon.ico} $out/favicon.ico
    mkdir $out/icons
    ${sed} 's/stroke="#000000"/stroke="${theme.base09}"/' ${./ressources/icons/checkbox-todo.svg} > $out/icons/checkbox-todo.svg
    ${sed} 's/stroke="#000000"/stroke="${theme.base0A}"/' ${./ressources/icons/checkbox-important.svg} > $out/icons/checkbox-important.svg
    ${sed} 's/fill="#000000"/fill="${theme.base0E}"/' ${./ressources/icons/checkbox-ongoing.svg} > $out/icons/checkbox-ongoing.svg
    ${sed} 's/stroke="#000000"/stroke="${theme.base0D}"/' ${./ressources/icons/checkbox-blocked.svg} > $out/icons/checkbox-blocked.svg
    ${sed} 's/stroke="#000000"/stroke="${theme.base0B}"/' ${./ressources/icons/checkbox-done.svg} > $out/icons/checkbox-done.svg
    ${sed} 's/fill="#000000"/fill="${theme.base08}"/' ${./ressources/icons/checkbox-dont.svg} > $out/icons/checkbox-dont.svg
    ${sed} 's/fill="#0F0F0F"/fill="${theme.base01}"/' ${./ressources/icons/edit-save.svg} > $out/icons/edit-save.svg
    ${sed} 's/fill="#0D0D0D"/fill="${theme.base01}"/' ${./ressources/icons/edit-quit.svg} > $out/icons/edit-quit.svg
    ln -s ${./ressources/icons/file.png} $out/icons/file.png
    ln -s ${./ressources/icons/note.png} $out/icons/note.png
    ln -s ${./ressources/icons/link.png} $out/icons/link.png
    ln -s ${./ressources/icons/media.png} $out/icons/media.png
    ln -s ${./ressources/icons/share.png} $out/icons/share.png
    ln -s ${./ressources/icons/parent.png} $out/icons/parent.png
    ln -s ${./ressources/icons/remove.png} $out/icons/remove.png
    ln -s ${./ressources/icons/upload.png} $out/icons/upload.png
    ln -s ${./ressources/icons/eventsync.png} $out/icons/eventsync.png
    ln -s ${./ressources/icons/export.png} $out/icons/export.png
    ln -s ${./ressources/icons/open.png} $out/icons/open.png
    ln -s ${./ressources/icons/open-white.png} $out/icons/open-white.png
    ln -s ${./ressources/icons/collection.png} $out/icons/collection.png
    mkdir -p $out/font
    cp ${charis}/web/*.woff2 $out/font
  ''
