module Korrvigs.Note.Languages
  ( Language (..),
    langName,
    langExt,
    langExe,
    langAce,
    langSkylight,
    languagesMap,
    knownLanguages,
  )
where

import Control.Arrow
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Compute.Runnable

data Language = Language
  { _langName :: Text,
    _langExt :: Text,
    _langExe :: Maybe Executable,
    _langAce :: Maybe Text,
    _langSkylight :: Maybe Text
  }

makeLenses ''Language

languagesMap :: Map Text Language
languagesMap = M.fromList $ (view langName &&& id) <$> knownLanguages

l :: Text -> Text -> Maybe Executable -> Maybe Text -> Maybe Text -> Language
l = Language

j :: a -> Maybe a
j = Just

n :: Maybe a
n = Nothing

knownLanguages :: [Language]
knownLanguages =
  [ -- Markup
    l "markdown" "md" n (j "markdown") (j "Markdown"),
    l "pandoc" "pd" n (j "markdown") (j "Markdown"),
    l "html" "html" n (j "html") (j "HTML"),
    l "css" "css" n (j "css") (j "CSS"),
    l "latex" "tex" (j LaTeX) (j "latex") (j "LaTeX"),
    l "context" "tex" (j ConTeXt) (j "latex") (j "LaTeX"),
    l "bibtex" "bib" n (j "bibtex") (j "BibTeX"),
    -- Drawing
    l "dot" "dot" (j Graphviz) (j "dot") (j "dot"),
    l "tikz" "tex" (j TiKZ) (j "latex") (j "LaTeX"),
    l "haskell-diagrams" "hs" (j HaskellDiagrams) (j "haskell") (j "Haskell"),
    l "asymptote" "asy" (j Asymptote) (j "c_cpp") (j "C++"),
    l "gnuplot" "plt" (j GnuPlot) n n,
    l "openscad" "scad" (j OpenScad) (Just "scad") n,
    l "povray" "pov" (j Povray) n (j "POV-Ray"),
    -- Languages
    l "c" "c" (j CLang) (j "c_cpp") (j "C"),
    l "cpp" "cpp" (j CPPLang) (j "c_cpp") (j "C++"),
    l "haskell" "hs" (j Haskell) (j "haskell") (j "Haskell"),
    l "rust" "rs" (j Rust) (j "rust") (j "Rust"),
    l "zig" "zig" n (j "zig") (j "Zig"),
    l "ada" "ada" n (j "ada") (j "Ada"),
    l "javascript" "js" n (j "javascript") (j "JavaScript"),
    l "ocaml" "ml" (j OCaml) (j "ocaml") (j "Objective Caml"),
    l "prolog" "pl" (j SwiProlog) (j "prolog") (j "Prolog"),
    l "python" "py" (j Python) (j "python") (j "Python"),
    l "raku" "raku" (j Raku) (j "raku") (j "Raku"),
    l "perl" "pl" (j Perl) (j "perl") (j "Perl"),
    l "r" "r" n (j "r") (j "R Script"),
    l "ruby" "rb" n (j "ruby") (j "Ruby"),
    -- Script
    l "lua" "lua" (j Lua) (j "lua") (j "Lua"),
    l "julia" "jl" (j Julia) (j "julia") (j "Julia"),
    l "sh" "sh" n (j "sh") (j "Bash"),
    l "bash" "sh" (j Bash) (j "sh") (j "Bash"),
    l "zsh" "sh" n (j "sh") (j "Zsh"),
    -- Build
    l "makefile" "make" n (j "makefile") (j "Makefile"),
    l "cabal" "cabal" n (j "haskell_cabal") n,
    -- Data
    l "yaml" "yaml" n (j "yaml") (j "YAML"),
    l "json" "json" (j PlainJson) (j "json") (j "JSON"),
    l "toml" "toml" n (j "toml") (j "TOML"),
    l "xml" "xml" n (j "xml") (j "XML"),
    l "csv" "csv" (j PlainCsv) n n,
    l "dhall" "dhall" (j Dhall) n n,
    -- Misc
    l "nix" "nix" (j NixData) (j "nix") (j "Nix"),
    l "mysql" "sql" n (j "mysql") (j "SQL (MySQL)"),
    l "pgsql" "pgsql" n (j "pgsql") (j "SQL (PostgreSQL)"),
    l "text" "txt" (j PlainText) (j "text") n
  ]
