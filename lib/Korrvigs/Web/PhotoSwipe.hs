module Korrvigs.Web.PhotoSwipe where

import Control.Lens
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
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
