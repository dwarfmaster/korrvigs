module Korrvigs.Web.PhotoSwipe where

import Control.Lens
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Compute
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
    _swpHeight :: Int
  }

makeLenses ''PhotoswipeEntry

miniatureEntry :: (MonadKorrvigs m) => Id -> m (Maybe PhotoswipeEntry)
miniatureEntry i = do
  comps <- entryStoredComputations i
  szM <- maybe (pure Nothing) getJsonComp $ M.lookup "size" comps
  pure $ do
    void $ M.lookup "miniature" comps
    sz <- szM :: Maybe (Map Text Int)
    width <- M.lookup "width" sz
    height <- M.lookup "height" sz
    pure $
      PhotoswipeEntry
        { _swpUrl = EntryDownloadR (WId i),
          _swpMiniature = EntryComputeR (WId i) "miniature",
          _swpRedirect = EntryR (WId i),
          _swpCaption = mempty,
          _swpWidth = width,
          _swpHeight = height
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
                window.location = pswp.currSlide.data.element.getAttribute('data-korrvigs-target')
              }
            });
          })
          lightbox.init()
        })
    }
  |]

photoswipe :: [PhotoswipeEntry] -> Handler Widget
photoswipe items = do
  i <- newIdent
  pure $ do
    toWidget [julius|setupPhotoswipeFor(#{i})|]
    toWidget
      [cassius|
      ##{i}
        width: 100%
    |]
    [whamlet|
      <div ##{i} .pswp-gallery>
        $forall item <- items
          <a href=@{_swpUrl item} data-pswp-width=#{_swpWidth item} data-pswp-height=#{_swpHeight item} data-korrvigs-target=@{_swpRedirect item} target="_blank">
            <img loading=lazy src=@{_swpMiniature item} alt="">
            ^{_swpCaption item}
    |]
