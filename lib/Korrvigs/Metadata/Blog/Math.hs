module Korrvigs.Metadata.Blog.Math (renderMath) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Profunctor.Product
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Monad
import Korrvigs.Utils.Process
import Opaleye
import System.Exit
import System.Process
import Text.Blaze.Html5 hiding (table)

cacheTable :: Table (Field SqlText, Field SqlText) (Field SqlText, Field SqlText)
cacheTable =
  table "blog_math_cache" $
    p2 (tableField "math", tableField "svg")

runCached :: (MonadKorrvigs m) => Text -> CreateProcess -> m (Maybe Html)
runCached mth prc = do
  cached :: Maybe Text <- rSelectOne $ do
    cached <- selectTable cacheTable
    where_ $ cached ^. _1 .== sqlStrictText mth
    pure $ cached ^. _2
  case cached of
    Just svg -> pure $ Just $ preEscapedText svg
    Nothing -> do
      (exit, r) <- liftIO $ runStdout prc
      case exit of
        ExitFailure _ -> pure Nothing
        ExitSuccess -> do
          let svg = LEnc.decodeUtf8 r
          withSQL $ \conn ->
            void $
              liftIO $
                runInsert conn $
                  Insert
                    { iTable = cacheTable,
                      iRows = [(sqlStrictText mth, sqlLazyText svg)],
                      iReturning = rCount,
                      iOnConflict = Just doNothing
                    }
          pure $ Just $ preEscapedLazyText svg

renderMath :: (MonadKorrvigs m) => Bool -> Text -> m Html
renderMath inline mth = do
  msvg <- runCached mth $ proc "tex2svg" $ ["--inline" | inline] <> [T.unpack mth]
  case msvg of
    Just svg -> pure svg
    Nothing -> pure $ code $ toMarkup mth
