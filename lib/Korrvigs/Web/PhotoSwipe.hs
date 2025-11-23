module Korrvigs.Web.PhotoSwipe where

import Control.Lens
import Control.Monad
import Data.Aeson.Lens
import Data.Default
import Data.List.NonEmpty (NonEmpty (..), groupBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Korrvigs.Compute.Run
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Monad.Computation
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Public.Crypto as Public
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Text.Cassius
import Yesod
import Yesod.Static

data PhotoswipeContentType
  = PSPicture
  | PSVideo Text

data PhotoswipeEntry = PhotoswipeEntry
  { _swpUrl :: Route WebData,
    _swpUrlPublic :: Route WebData,
    _swpType :: PhotoswipeContentType,
    _swpMiniature :: Route WebData,
    _swpMiniaturePublic :: Route WebData,
    _swpRedirect :: Maybe (Route WebData),
    _swpCaption :: Widget,
    _swpWidth :: Int,
    _swpHeight :: Int,
    _swpDate :: Maybe Day
  }

makeLenses ''PhotoswipeEntry

miniatureEntryImpl :: PhotoswipeContentType -> Maybe Day -> Id -> Handler (Maybe PhotoswipeEntry)
miniatureEntryImpl tp day i = do
  szM <- getComputation i "size"
  szR <- forM szM runVeryLazy
  let url = EntryDownloadR (WId i)
  let miniatureUrl = EntryComputeR (WId i) "miniature"
  urlSignature <- Public.signRoute url []
  miniatureSignature <- Public.signRoute miniatureUrl []
  pure $ do
    sz :: Map Text Int <- szR ^? _Just . _Right . _ResultJson . _JSON
    width <- M.lookup "width" sz
    height <- M.lookup "height" sz
    pure $
      PhotoswipeEntry
        { _swpUrl = url,
          _swpUrlPublic = PublicEntryDownloadR urlSignature (WId i),
          _swpMiniature = miniatureUrl,
          _swpMiniaturePublic = PublicEntryComputeR miniatureSignature (WId i) "miniature",
          _swpType = tp,
          _swpRedirect = Just $ EntryR (WId i),
          _swpCaption = mempty,
          _swpWidth = width,
          _swpHeight = height,
          _swpDate = day
        }

miniatureEntry :: Maybe Text -> Maybe Day -> Id -> Handler (Maybe PhotoswipeEntry)
miniatureEntry (Just mime) = miniatureFileEntry mime
miniatureEntry _ = miniatureEntryImpl PSPicture

miniatureFileEntry :: Text -> Maybe Day -> Id -> Handler (Maybe PhotoswipeEntry)
miniatureFileEntry mime | T.isPrefixOf "video/" mime = miniatureEntryImpl $ PSVideo mime
miniatureFileEntry _ = miniatureEntryImpl PSPicture

photoswipeHeader :: Widget
photoswipeHeader = do
  Rcs.photoswipe StaticR
  toWidget
    [cassius|
    a.pswp__button--open-entry
      font-size: 20px
      color: #fff
      display: flex
      justify-content: center
      align-items: center
  |]
  toWidget
    [julius|
    function setupPhotoswipeFor(id) {
      import('@{StaticR $ StaticRoute ["photoswipe", "photoswipe-lightbox.esm.js"] []}')
        .then((module) => import('@{StaticR $ StaticRoute ["photoswipe-video-plugin", "photoswipe-video-plugin.esm.js"] []}').then((video) => {
          const PhotoSwipeLightbox = module.default;
          const PhotoSwipeVideoPlugin = video.default;
          const lightbox = new PhotoSwipeLightbox({
            gallery: "#" + id,
            children: 'a.photoswipe-entry',
            pswpModule: () => import('@{StaticR $ StaticRoute ["photoswipe", "photoswipe.esm.js"] []}')
          });
          const videoPlugin = new PhotoSwipeVideoPlugin(lightbox, {});
          lightbox.on('uiRegister', function () {
            lightbox.pswp.ui.registerElement({
              name: 'open-entry',
              ariaLabel: 'Open entry page',
              order: 9,
              isButton: true,
              tagName: 'a',
              html: '<img height=32 width=32 style="display: inline;" src=@{StaticR $ StaticRoute ["icons", "open-white.png"] []}>',
              onInit: (el, pswp) => {
                pswp.on('change', () => {
                  el.href = pswp.currSlide.data.element.getAttribute('data-korrvigs-target')
                })
              }
            });
          });
          lightbox.init()
        }))
    }
  |]

groupEntries :: [PhotoswipeEntry] -> [NonEmpty PhotoswipeEntry]
groupEntries = groupBy (\e1 e2 -> e1 ^. swpDate == e2 ^. swpDate)

displayDate :: Maybe Day -> Text
displayDate Nothing = "No Date"
displayDate (Just d) = T.pack $ formatTime defaultTimeLocale "%e %B %0Y - %A" d

displayDateOfGroup :: NonEmpty PhotoswipeEntry -> Text
displayDateOfGroup (e :| _) = displayDate $ view swpDate e

libraryCSS :: p -> Css
libraryCSS =
  [cassius|
    .library-item
      display: inline-block
      border-radius: 0.1em
      border: 2px solid var(--base06)
      margin: 0.2em
      width: 10em
      height: 12em
    .library-miniature
      width: 10em
      height: 10em
      display: flex
      align-items: center
      justify-content: center
      img
        max-width: 9.5em
        max-height: 9.5em
    .library-caption
      padding: 0.1em
      width: 100%
      height: 2em
      text-align: center
      font-size: small
      overflow: hidden
      p
        margin: 0
        text-align: left
  |]

data PhotoswipeSettings = PhotoswipeSettings
  { _swpGroup :: Bool,
    _swpLibrary :: Bool
  }

makeLenses ''PhotoswipeSettings

instance Default PhotoswipeSettings where
  def = PhotoswipeSettings False False

photoswipe :: PhotoswipeSettings -> [PhotoswipeEntry] -> Handler Widget
photoswipe _ [] = pure mempty
photoswipe settings (item : items) = do
  let togroup = settings ^. swpGroup
  let displayLib = settings ^. swpLibrary
  i <- newIdent
  public <- isPublic
  let getUrl = if public then view swpUrlPublic else view swpUrl
  let getMiniature = if public then view swpMiniaturePublic else view swpMiniature
  let grouped = if togroup then groupEntries (item : items) else [item :| items]
  let widgetFn = if displayLib then libWidget else itemWidget
  pure $ do
    when displayLib $ toWidget libraryCSS
    toWidget [julius|setupPhotoswipeFor(#{i});|]
    toWidget
      [cassius|
      ##{i}
        width: 100%
        summary
          width: 100%
          background-color: var(--base0F)
          color: var(--base00)
    |]
    case grouped of
      [group] ->
        [whamlet|
        <div ##{i} .pswp-gallery>
          $forall item <- group 
            ^{widgetFn getUrl getMiniature item}
      |]
      _ ->
        [whamlet|
        <div ##{i} .pswp-gallery>
          $forall group <- grouped
            <details>
              <summary>
                #{displayDateOfGroup group}
              $forall item <- group 
                ^{widgetFn getUrl getMiniature item}
      |]
  where
    itemTarget getUrl entry = fromMaybe (getUrl entry) (entry ^. swpRedirect)
    videoAttrs :: PhotoswipeEntry -> [(Text, Text)]
    videoAttrs it = case it ^. swpType of
      PSPicture -> []
      PSVideo _ -> [("data-pswp-type", "video")]
    mkLink :: (PhotoswipeEntry -> Route WebData) -> (PhotoswipeEntry -> Route WebData) -> PhotoswipeEntry -> Widget -> Widget
    mkLink getUrl getMiniature it widget =
      [whamlet|
        <a href=@{getUrl it} .photoswipe-entry data-pswp-width=#{_swpWidth it} data-pswp-height=#{_swpHeight it} data-korrvigs-target=@{itemTarget getUrl it} target="_blank" *{videoAttrs it}>
          <img loading=lazy src=@{getMiniature it} alt="">
          ^{widget}
      |]
    itemWidget getUrl getMiniature it = mkLink getUrl getMiniature it $ _swpCaption it
    libWidget getUrl getMiniature it =
      [whamlet|
        <div .library-item>
          <div .library-miniature>
            ^{mkLink getUrl getMiniature it mempty}
          <div .library-caption>
            ^{_swpCaption it}
      |]
