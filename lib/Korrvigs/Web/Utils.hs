module Korrvigs.Web.Utils
  ( colorKind,
    htmlKind,
    htmlKind',
    edgeSubColor,
    edgeRefColor,
    fileAFormMult,
  )
where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Kind
import Korrvigs.Utils.Base16
import Korrvigs.Web.Backend
import Yesod

mkHtmlKind :: Text -> Base16Index -> Handler Html
mkHtmlKind nm col = do
  theme <- getBase
  render <- getUrlRender
  let html =
        [hamlet|
    <span .entry-kind style="background-color:#{theme col}">
      #{nm}
  |]
  pure $ html render

colorKind :: Kind -> Base16Index
colorKind Note = Base08
colorKind File = Base09
colorKind Event = Base0B
colorKind Calendar = Base0C
colorKind Syndicate = Base0D
colorKind AddressBook = Base0E

htmlKind' :: Kind -> Handler Html
htmlKind' Note = mkHtmlKind "Note" $ colorKind Note
htmlKind' File = mkHtmlKind "File" $ colorKind File
htmlKind' Event = mkHtmlKind "Event" $ colorKind Event
htmlKind' Calendar = mkHtmlKind "Calendar" $ colorKind Calendar
htmlKind' Syndicate = mkHtmlKind "Syndicate" $ colorKind Syndicate
htmlKind' AddressBook = mkHtmlKind "AddressBook" $ colorKind AddressBook

htmlKind :: Kind -> Widget
htmlKind kd = do
  html <- handlerToWidget $ htmlKind' kd
  toWidget html

edgeSubColor :: Base16Index
edgeSubColor = Base0E

edgeRefColor :: Base16Index
edgeRefColor = Base0F

fileAFormMult ::
  (MonadHandler m) =>
  FieldSettings (HandlerSite m) ->
  AForm m [FileInfo]
fileAFormMult fs = AForm $ \(master, langs) menvs ints -> do
  let (name, ints') =
        case fsName fs of
          Just x -> (x, ints)
          Nothing ->
            let i' = incrInts ints
             in (T.pack $ 'f' : show i', i')
  id' <- maybe newIdent return $ fsId fs
  let (res, errs) =
        case menvs of
          Nothing -> (FormMissing, Nothing)
          Just (_, fenv) ->
            case M.lookup name fenv of
              Just fi -> (FormSuccess fi, Nothing)
              _ -> (FormSuccess [], Nothing)
  let fv =
        FieldView
          { fvLabel = toHtml $ renderMessage master langs $ fsLabel fs,
            fvTooltip = fmap (toHtml . renderMessage master langs) $ fsTooltip fs,
            fvId = id',
            fvInput =
              [whamlet|
$newline never
<input type=file name=#{name} ##{id'} *{fsAttrs fs} multiple>
|],
            fvErrors = errs,
            fvRequired = False
          }
  return (res, (fv :), ints', Multipart)
  where
    incrInts :: Ints -> Ints
    incrInts (IntSingle i) = IntSingle $ i + 1
    incrInts (IntCons i is) = (i + 1) `IntCons` is
