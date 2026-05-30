module Korrvigs.Metadata.Blog.Content (postTextRender) where

import Control.Lens hiding (pre)
import Control.Monad
import Control.Monad.RWS.Strict
import Data.Default
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Korrvigs.Metadata.Blog.Export
import Korrvigs.Metadata.Blog.Structure
import Korrvigs.Monad
import Korrvigs.Note hiding (code, sub)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Renderer.Text
import Prelude hiding (div)

postTextRender :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> FilePath -> m (Text, Text)
postTextRender renderUrl path = do
  doc <- readNote path >>= throwEither (\e -> KMiscError $ "Failed to load note for blog post: " <> e)
  let ctx =
        RenderContext
          { _rdrDoc = doc,
            _rdrRenderUrl = renderUrl,
            _rdrHdOffset = 0,
            _rdrCurLevel = 0
          }
  summary <- postSummary ctx $ doc ^. docContent
  content <- postContent ctx $ doc ^. docContent
  pure (rdr summary, rdr content)
  where
    rdr :: Html -> Text
    rdr = LT.toStrict . renderMarkup

postContent :: (MonadKorrvigs m) => RenderContext m -> [Block] -> m Html
postContent ctx bks = fst <$> evalRWST (renderBlocks bks) ctx def

postSummary :: (MonadKorrvigs m) => RenderContext m -> [Block] -> m Html
postSummary _ [] = pure mempty
postSummary ctx (bk : _) = fst <$> evalRWST (renderBlock bk) ctx def
