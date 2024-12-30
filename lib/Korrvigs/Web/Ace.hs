module Korrvigs.Web.Ace (preview, isLanguage) where

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

-- Use Ace for read-only syntax highlighting
preview :: Text -> Text -> Handler Widget
preview code language = do
  ident <- newIdent
  jsEditor <- rawJS <$> newIdent
  pure $ do
    Rcs.ace StaticR
    toWidget
      [cassius|
      ##{ident}
        width: 100%
    |]
    toWidget
      [julius|
      function setup#{jsEditor}() {
        var editor = ace.edit(#{ident})
        editor.setTheme("ace/theme/github_dark")
        editor.session.setMode("ace/mode/" + #{languageMode language})
        // editor.setKeyboardHandler("ace/keyboard/vim")
        editor.setOptions({
          maxLines: Infinity,
          readOnly: true,
          highlightActiveLine: false,
          highlightGutterLine: false
        })
        editor.renderer.$cursorLayer.element.style.display = "none"
        editor.session.setUseWorker(false)
        return editor
      }
      var #{jsEditor} = setup#{jsEditor}()
    |]
    [whamlet|
      <div ##{ident}>
        #{code}
    |]
