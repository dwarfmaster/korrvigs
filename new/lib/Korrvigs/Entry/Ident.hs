{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Entry.Ident
  ( Id (..),
    IdMaker,
    idPrefix,
    idTitle,
    idParent,
    idSeq,
    idDate,
    imk,
    newId,
  )
where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Extra (findM)
import Control.Monad.IO.Class
import Data.Char
import Data.List (unfoldr)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Korrvigs.Entry.Ident.Stopwords
import Korrvigs.Entry.SQL
import Korrvigs.Monad
import Opaleye hiding (not, null)
import Text.Printf

newtype Id = MkId {unId :: Text}
  deriving (Eq, Ord, Show)

data IdMaker = IdMaker
  { _idPrefix :: Text,
    _idTitle :: Maybe Text,
    _idParent :: Maybe Id,
    _idSeq :: Maybe Int,
    _idDate :: Maybe ZonedTime
  }
  deriving (Show)

makeLenses ''IdMaker

capitalize :: Text -> Text
capitalize = T.pack . toCap . T.unpack
  where
    toCap :: String -> String
    toCap [] = []
    toCap (l : ls) = toUpper l : fmap toLower ls

imk :: Text -> IdMaker
imk prefix = IdMaker prefix Nothing Nothing Nothing Nothing

prepTitle :: Text -> Text
prepTitle title = foldl (<>) "" content
  where
    wds :: [Text]
    wds = T.map toLower <$> T.split (\c -> isPunctuation c || isSpace c) title
    toDrop :: Text -> Bool
    toDrop t = S.member t stopwords || T.length t == 1
    content :: [Text]
    content = capitalize . T.take 6 <$> (take 3 . filter (not . toDrop)) wds

prepDate :: Bool -> ZonedTime -> Text
prepDate full dt =
  T.pack $ if full then printf "%04d%02d%02d" year month day else cutYear
  where
    date :: Day
    date = localDay $ zonedTimeToLocalTime dt
    year :: Year
    month :: MonthOfYear
    day :: DayOfMonth
    (year, month, day) = toGregorian date
    cutYear :: String
    cutYear =
      if year < 1975 || year >= 1975 then printf "%04d" year else printf "%02d" (year `mod` 50)

prep :: IdMaker -> Text
prep mk = T.intercalate ":" $ filter (not . T.null) [mk ^. idPrefix, stub, date, sq]
  where
    stub :: Text
    stub =
      case mk ^. idTitle of
        Just title -> prepTitle title
        Nothing ->
          case mk ^. idParent of
            Just (MkId parent) ->
              case T.split (== ':') parent of
                _ : stb : _ -> stb
                _ -> parent
            Nothing -> ""
    date :: Text
    date = maybe "" (prepDate (isNothing (mk ^. idTitle) && isNothing (mk ^. idParent))) $ mk ^. idDate
    sq :: Text
    sq = maybe "" (T.pack . show) $ mk ^. idSeq

nextMaker :: IdMaker -> IdMaker
nextMaker = idSeq %~ maybe (Just 1) (Just . (+ 1))

createId :: Monad m => (Text -> m Bool) -> IdMaker -> m Text
createId check mk =
  fmap fromJust $ findM check $ prep <$> unfoldr (Just . (id &&& nextMaker)) mk

newId :: MonadKorrvigs m => IdMaker -> m Id
newId =
  fmap MkId
    . createId
      ( \candidate -> do
          conn <- pgSQL
          r <- liftIO $ runSelect conn $ limit 1 $ do
            EntryRow i _ _ _ _ _ _ <- selectTable entriesTable
            where_ $ i .== sqlStrictText candidate
            pure ()
          pure $ null r
      )
