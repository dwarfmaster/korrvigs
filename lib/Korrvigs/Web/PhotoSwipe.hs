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
          _swpCaption = mempty,
          _swpWidth = width,
          _swpHeight = height
        }

photoswipeHeader :: Widget
photoswipeHeader = do
  Rcs.photoswipe StaticR
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
          <a href=@{_swpUrl item} data-pswp-width=#{_swpWidth item} data-pswp-height=#{_swpHeight item} target="_blank">
            <img src=@{_swpMiniature item} alt="">
            ^{_swpCaption item}
    |]
