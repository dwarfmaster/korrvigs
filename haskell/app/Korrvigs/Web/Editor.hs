module Korrvigs.Web.Editor (EditorLanguage (..), EditorConfig (..), editorHandler) where

import Data.Default
import Data.Text (Text)
import Korrvigs.Web.Backend
import Korrvigs.Web.Method (Method, methodGet, methodPost)
import qualified Korrvigs.Web.Ressources as Rcs
import Yesod

data EditorLanguage
  = Raw
  | Markdown

data EditorConfig = EditorConfig
  { editorContent :: Handler Text,
    editorLanguage :: EditorLanguage,
    editorSave :: Text -> Handler TypedContent
  }

instance Default EditorConfig where
  def =
    EditorConfig
      { editorContent = pure "",
        editorLanguage = Raw,
        editorSave = const $ notFound
      }

editorHandler ::
  Method ->
  EditorConfig ->
  (Widget -> Handler Html) ->
  Handler TypedContent
editorHandler mth cfg page | mth == methodGet = selectRep $ do
  provideRep $ page $ do
    Rcs.editor
    [whamlet|
      <div #editor>
      <div #editor-save>
        <button>
          Save
    |]
  provideRep $ String <$> editorContent cfg
editorHandler mth cfg _ | mth == methodPost = do
  postData <- fst <$> runRequestBody
  liftIO $ putStrLn $ show postData
  case lookup "content" postData of
    Nothing -> notFound
    Just content -> editorSave cfg content
editorHandler _ _ _ = notFound
