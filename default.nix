{ mkDerivation, aeson, aeson-pretty, array, async, base
, base16-bytestring, base64, binary, blaze-html, blaze-markup
, bytestring, case-insensitive, citeproc, clientsession, clock
, conduit, conduit-aeson, conduit-extra, containers, crypton
, crypton-connection, data-default, data-endian, deepseq, directory
, entropy, exceptions, extra, feed, file-embed, filepath
, hmatrix-glpk, http-client, http-client-tls, http-conduit
, http-types, HUnit, iconv, isbn, lens, lens-aeson, lib, linear
, memory, mime-types, monad-loops, mtl, network-uri, opaleye
, optparse-applicative, pandoc, pandoc-types, parsec
, parsec3-numbers, password, postgresql-simple, process
, product-profunctors, profunctors, random, shakespeare, split, stm
, tagsoup, template-haskell, temporary, text, text-builder
, text-manipulate, time, tls, transformers, unix, utf8-string
, vector, xdg-basedir, xml-conduit, xml-types, yaml, yesod
, yesod-core, yesod-static
}:
mkDerivation {
  pname = "korrvigs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty array async base base16-bytestring base64 binary
    blaze-html blaze-markup bytestring case-insensitive citeproc
    clientsession clock conduit conduit-aeson conduit-extra containers
    crypton crypton-connection data-default data-endian deepseq
    directory entropy exceptions extra feed file-embed filepath
    hmatrix-glpk http-client http-client-tls http-conduit http-types
    HUnit iconv isbn lens lens-aeson linear memory mime-types
    monad-loops mtl network-uri opaleye optparse-applicative pandoc
    pandoc-types parsec parsec3-numbers password postgresql-simple
    process product-profunctors profunctors random shakespeare split
    stm tagsoup template-haskell temporary text text-builder
    text-manipulate time tls transformers unix utf8-string vector
    xdg-basedir xml-conduit xml-types yaml yesod yesod-core
    yesod-static
  ];
  testHaskellDepends = [ base HUnit ];
  doHaddock = false;
  description = "A wiki system for my personal use";
  license = lib.licenses.mit;
  mainProgram = "korrvigs-cli";
}
