module Korrvigs.Web.PhotoSwipe where

import Control.Lens
import Data.List.NonEmpty (NonEmpty (..), groupBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Korrvigs.Compute
import Korrvigs.Compute.Action
import Korrvigs.Entry
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Public.Crypto as Public
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
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

miniatureEntryImpl :: PhotoswipeContentType -> Maybe Day -> Id -> Action -> Handler (Maybe PhotoswipeEntry)
miniatureEntryImpl tp day i sizeA = do
  szM <- lazyRunJSON i "size" sizeA
  let url = EntryDownloadR (WId i)
  let miniatureUrl = EntryComputeR (WId i) "miniature"
  urlSignature <- Public.signRoute url []
  miniatureSignature <- Public.signRoute miniatureUrl []
  pure $ do
    sz :: Map Text Int <- szM
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

miniatureEntry :: Maybe Text -> Maybe Day -> Id -> Action -> Handler (Maybe PhotoswipeEntry)
miniatureEntry (Just mime) = miniatureFileEntry mime
miniatureEntry _ = miniatureEntryImpl PSPicture

miniatureFileEntry :: Text -> Maybe Day -> Id -> Action -> Handler (Maybe PhotoswipeEntry)
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
            children: 'a',
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

photoswipe :: Bool -> [PhotoswipeEntry] -> Handler Widget
photoswipe _ [] = pure mempty
photoswipe togroup (item : items) = do
  i <- newIdent
  public <- isPublic
  let getUrl = if public then view swpUrlPublic else view swpUrl
  let getMiniature = if public then view swpMiniaturePublic else view swpMiniature
  let grouped = if togroup then groupEntries (item : items) else [item :| items]
  pure $ do
    toWidget [julius|setupPhotoswipeFor(#{i})|]
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
            ^{itemWidget getUrl getMiniature item}
      |]
      _ ->
        [whamlet|
        <div ##{i} .pswp-gallery>
          $forall group <- grouped
            <details>
              <summary>
                #{displayDateOfGroup group}
              $forall item <- group 
                ^{itemWidget getUrl getMiniature item}
      |]
  where
    itemTarget getUrl entry = fromMaybe (getUrl entry) (entry ^. swpRedirect)
    itemWidget getUrl getMiniature it = case it ^. swpType of
      PSPicture ->
        [whamlet|
        <a href=@{getUrl it} data-pswp-width=#{_swpWidth it} data-pswp-height=#{_swpHeight it} data-korrvigs-target=@{itemTarget getUrl it} target="_blank">
          <img loading=lazy src=@{getMiniature it} alt="">
          ^{_swpCaption it}
        |]
      PSVideo _ ->
        [whamlet|
        <a href=@{getUrl it} data-pswp-width=#{_swpWidth it} data-pswp-height=#{_swpHeight it} data-pswp-type=video data-korrvigs-target=@{itemTarget getUrl it} target="_blank">
          <img loading=lazy src=@{getMiniature it} alt="">
          ^{_swpCaption it}
        |]
