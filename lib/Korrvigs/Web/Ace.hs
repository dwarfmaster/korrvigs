module Korrvigs.Web.Ace (setup, preview, isLanguage) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Text.Julius
import Yesod

modeMap :: Map Text Text
modeMap =
  M.fromList
    [ -- Markup
      ("markdown", "markdown"),
      ("html", "html"),
      ("css", "css"),
      ("dot", "dot"),
      ("latex", "latex"),
      ("bibtex", "bibtex"),
      -- Languages
      ("c", "c_cpp"),
      ("cpp", "c_cpp"),
      ("haskell", "haskell"),
      ("rust", "rust"),
      ("zig", "zig"),
      ("ada", "ada"),
      ("javascript", "javascript"),
      ("ocaml", "ocaml"),
      ("prolog", "prolog"),
      ("python", "python"),
      ("raku", "raku"),
      ("perl", "perl"),
      ("r", "r"),
      ("ruby", "ruby"),
      -- Script
      ("lua", "lua"),
      ("julia", "julia"),
      ("sh", "sh"),
      ("bash", "sh"),
      ("zsh", "sh"),
      -- Build
      ("makefile", "makefile"),
      ("cabal", "haskell_cabal"),
      -- Data
      ("yaml", "yaml"),
      ("json", "json"),
      ("toml", "toml"),
      ("xml", "xml"),
      -- Misc
      ("nix", "nix"),
      ("mysql", "mysql"),
      ("pgsql", "pgsql")
    ]

languageMode :: Text -> Text
languageMode language = fromMaybe "text" $ M.lookup language modeMap

isLanguage :: Text -> Bool
isLanguage = flip M.member modeMap

setupAceJs :: JavascriptUrl url
setupAceJs =
  [julius|
  function setupAceEditor(id, mode, readOnly) {
    var editor = ace.edit(id)
    editor.setTheme("ace/theme/github_dark")
    editor.session.setMode("ace/mode/" + mode)
    editor.setOptions({
      maxLines: Infinity,
    })
    if(readOnly) {
      editor.setOptions({
        readOnly: true,
        highlightActiveLine: false,
        highlightGutterLine: false
      })
      editor.renderer.$cursorLayer.element.style.display = "none"
      editor.session.setUseWorker(false)
    } else {
      editor.setKeyboardHandler("ace/keyboard/vim")
    }
    return editor
  }
|]

-- Setup must be included once in every page using the ace editor
setup :: Widget
setup = do
  Rcs.ace StaticR
  toWidget setupAceJs

-- Use Ace for read-only syntax highlighting
preview :: Text -> Text -> Handler Widget
preview code language = do
  ident <- newIdent
  jsEditor <- rawJS <$> newIdent
  pure $ do
    toWidget
      [cassius|
      ##{ident}
        width: 100%
    |]
    toWidget
      [julius|
      var #{jsEditor} = setupAceEditor(#{ident}, #{languageMode language}, true)
    |]
    [whamlet|
      <div ##{ident}>
        #{code}
    |]
