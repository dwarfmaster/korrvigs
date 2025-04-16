module Korrvigs.Web.PhotoSwipe where

import Control.Lens
import Data.List.NonEmpty (NonEmpty (..), groupBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Korrvigs.Compute
import Korrvigs.Compute.Action
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Yesod
import Yesod.Static

data PhotoswipeEntry = PhotoswipeEntry
  { _swpUrl :: Route WebData,
    _swpMiniature :: Route WebData,
    _swpRedirect :: Route WebData,
    _swpCaption :: Widget,
    _swpWidth :: Int,
    _swpHeight :: Int,
    _swpDate :: Maybe Day
  }

makeLenses ''PhotoswipeEntry

miniatureEntry :: (MonadKorrvigs m) => Maybe Day -> Id -> m (Maybe PhotoswipeEntry)
miniatureEntry day i = do
  query <- rSelectOne $ do
    miniature <- selComp (sqlId i) "miniature"
    size <- selComp (sqlId i) "size"
    pure (miniature ^. sqlCompAction, size ^. sqlCompAction)
  case query :: Maybe (Action, Action) of
    Nothing -> pure Nothing
    Just (_, sizeA) -> miniatureEntryCached day i sizeA

miniatureEntryCached :: (MonadKorrvigs m) => Maybe Day -> Id -> Action -> m (Maybe PhotoswipeEntry)
miniatureEntryCached day i sizeA = do
  szM <- runJSON sizeA
  pure $ do
    sz :: Map Text Int <- szM
    width <- M.lookup "width" sz
    height <- M.lookup "height" sz
    pure $
      PhotoswipeEntry
        { _swpUrl = EntryDownloadR (WId i),
          _swpMiniature = EntryComputeR (WId i) "miniature",
          _swpRedirect = EntryR (WId i),
          _swpCaption = mempty,
          _swpWidth = width,
          _swpHeight = height,
          _swpDate = day
        }

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
        .then((module) => {
          const PhotoSwipeLightbox = module.default
          const lightbox = new PhotoSwipeLightbox({
            gallery: "#" + id,
            children: 'a',
            pswpModule: () => import('@{StaticR $ StaticRoute ["photoswipe", "photoswipe.esm.js"] []}')
          });
          lightbox.on('uiRegister', function () {
            lightbox.pswp.ui.registerElement({
              name: 'open-entry',
              ariaLabel: 'Open entry page',
              order: 9,
              isButton: true,
              tagName: 'a',
              html: '<svg width="32" height="32" viewBox="0 0 48 48" aria-hidden="true"><path fill="#ffffff" d="M 40.960938 4.9804688 A 2.0002 2.0002 0 0 0 40.740234 5 L 28 5 A 2.0002 2.0002 0 1 0 28 9 L 36.171875 9 L 22.585938 22.585938 A 2.0002 2.0002 0 1 0 25.414062 25.414062 L 39 11.828125 L 39 20 A 2.0002 2.0002 0 1 0 43 20 L 43 7.2460938 A 2.0002 2.0002 0 0 0 40.960938 4.9804688 z M 12.5 8 C 8.3826878 8 5 11.382688 5 15.5 L 5 35.5 C 5 39.617312 8.3826878 43 12.5 43 L 32.5 43 C 36.617312 43 40 39.617312 40 35.5 L 40 26 A 2.0002 2.0002 0 1 0 36 26 L 36 35.5 C 36 37.446688 34.446688 39 32.5 39 L 12.5 39 C 10.553312 39 9 37.446688 9 35.5 L 9 15.5 C 9 13.553312 10.553312 12 12.5 12 L 22 12 A 2.0002 2.0002 0 1 0 22 8 L 12.5 8 z"/></svg>',
              onClick: (event, el, pswp) => {
                window.open(pswp.currSlide.data.element.getAttribute('data-korrvigs-target'), '_blank')
              }
            });
          })
          lightbox.init()
        })
    }
  |]

groupEntries :: [PhotoswipeEntry] -> [NonEmpty PhotoswipeEntry]
groupEntries = groupBy (\e1 e2 -> e1 ^. swpDate == e2 ^. swpDate)

displayDate :: Maybe Day -> Text
displayDate Nothing = "No Date"
displayDate (Just d) = T.pack $ formatTime defaultTimeLocale "%e %B %0Y - %A" d

displayDateOfGroup :: NonEmpty PhotoswipeEntry -> Text
displayDateOfGroup (e :| _) = displayDate $ view swpDate e

photoswipe :: [PhotoswipeEntry] -> Handler Widget
photoswipe items = do
  i <- newIdent
  let grouped = groupEntries items
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
    [whamlet|
      <div ##{i} .pswp-gallery>
        $forall group <- grouped
          <details>
            <summary>
              #{displayDateOfGroup group}
            $forall item <- group 
              <a href=@{_swpUrl item} data-pswp-width=#{_swpWidth item} data-pswp-height=#{_swpHeight item} data-korrvigs-target=@{_swpRedirect item} target="_blank">
                <img loading=lazy src=@{_swpMiniature item} alt="">
                ^{_swpCaption item}
    |]
