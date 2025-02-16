module Korrvigs.Web.Ace (setup, preview, isLanguage, editOnClick) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Text.Julius
import Yesod
import Yesod.Static

modeMap :: Map Text Text
modeMap =
  M.fromList
    [ -- Markup
      ("markdown", "markdown"),
      ("pandoc", "markdown"),
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

saveIcon :: Route WebData
saveIcon = StaticR $ StaticRoute ["icons", "edit-save.svg"] []

quitIcon :: Route WebData
quitIcon = StaticR $ StaticRoute ["icons", "edit-quit.svg"] []

setupAceJs :: JavascriptUrl (Route WebData)
setupAceJs =
  [julius|
  function aceSave(editor) {
    const content = editor.getValue()
    const url = editor.korrvigs.postUrl
    return fetch(url, {
      method: "POST",
      body: content,
      headers: {
        "Content-Type": "text/plain; charset=UTF-8"
      }
    })
  }
  function aceQuit(editor) {
    const url = editor.korrvigs.redirectUrl
    location.assign(url)
  }
  function setupAceVimMode() {
    ace.config.loadModule("ace/keyboard/vim", function(module) {
      var VimApi = module.CodeMirror.Vim
      VimApi.defineEx("write", "w", function(cm) {
        aceSave(cm.ace)
      })
      VimApi.defineEx("quit", "q", function(cm) {
        aceQuit(cm.ace)
      })
      VimApi.defineEx("wq", null, function(cm) {
        aceSave(cm.ace).then(function() { aceQuit(cm.ace) })
      })
    })
  }
  function setupAceEditor(id, mode, readOnly) {
    var editor = ace.edit(id)
    editor.setTheme("ace/theme/github_dark")
    editor.session.setMode("ace/mode/" + mode)
    const commonOptions = {
      maxLines: Infinity,
      printMarginColumn: 81,
    }
    const roOptions = {
      readOnly: true,
      highlightActiveLine: false,
      highlightGutterLine: false
    }
    const rwOptions = {
      autoScrollEditorIntoView: true,
    }
    if(readOnly) {
      editor.setOptions({ ...commonOptions, ...roOptions })
      editor.renderer.$cursorLayer.element.style.display = "none"
      editor.session.setUseWorker(false)
    } else if ('ontouchstart' in document.documentElement) {
      editor.setOptions({ ...commonOptions, ...rwOptions })

      let saveBtm = document.createElement("img")
      saveBtm.setAttribute("src", "@{saveIcon}")
      saveBtm.classList.add("edit-button")
      saveBtm.classList.add("edit-save-button")
      saveBtm.addEventListener('click', function() { aceSave(editor) })

      let quitBtm = document.createElement("img")
      quitBtm.setAttribute("src", "@{quitIcon}")
      quitBtm.classList.add("edit-button")
      quitBtm.classList.add("edit-quit-button")
      quitBtm.addEventListener('click', function() { 
        aceSave(editor).then(function() { aceQuit(editor) })
      })

      let p = document.createElement("p")
      p.classList.add("edit-button-tray")
      p.appendChild(quitBtm)
      p.appendChild(saveBtm)
      document.getElementById(id).after(p)
    } else {
      editor.setOptions({ ...commonOptions, ...rwOptions })
      editor.setKeyboardHandler("ace/keyboard/vim")
    }
    return editor
  }
  function aceEdit(id, mode, url, nextUrl) {
    var div = document.getElementById(id)
    while(div.firstChild) {
      div.removeChild(div.firstChild)
    }
    let nid = Date.now().toString(36) + Math.random().toString(36).substr(2)
    let edit = document.createElement("div")
    edit.id = nid
    div.appendChild(edit)

    fetch(url).then((response) => response.text()).then((content) => {
      edit.textContent = content
      var editor = setupAceEditor(nid, mode, false)
      editor.korrvigs = {
        postUrl: url,
        redirectUrl: nextUrl
      }
      setupAceVimMode()
      editor.focus()
    })
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
      [julius|
      var #{jsEditor} = setupAceEditor(#{ident}, #{languageMode language}, true)
    |]
    [whamlet|
      <div ##{ident}>
        #{code}
    |]

editOnClick :: Text -> Text -> Text -> Route WebData -> Text -> Handler Widget
editOnClick buttonId divId language url redirUrl = do
  pure $
    toWidget
      [julius|
    document.getElementById(#{buttonId}).addEventListener("click", (event) => {
      aceEdit(#{divId}, #{languageMode language}, "@{url}", #{redirUrl})
      event.currentTarget.remove()
    })
  |]
