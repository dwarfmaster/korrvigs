{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Entry.Ident
  ( Id (..),
    IdMaker,
    idPrefix,
    idTitle,
    idParent,
    idLanguage,
    idSeq,
    idDate,
    imk,
    createId,
    sqlId,
  )
where

import Codec.Text.IConv
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Extra (findM)
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.List (find, unfoldr)
import Data.Maybe (fromJust, isNothing)
import Data.Profunctor.Product.Default
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time
import qualified Korrvigs.Entry.Ident.English as En
import qualified Korrvigs.Entry.Ident.French as Fr
import Opaleye (DefaultFromField (..), Field, SqlText, ToFields, sqlStrictText)
import Text.Printf

commonStopwords :: Set Text
commonStopwords =
  S.fromList
    [ "file",
      "img",
      "dmisc",
      "vid",
      "video",
      "dsc",
      "pxl"
    ]

newtype Id = MkId {unId :: Text}
  deriving (Eq, Ord, Show, Read)

instance Default ToFields Id (Field SqlText) where
  def = dimap unId id def

instance DefaultFromField SqlText Id where
  defaultFromField = MkId <$> defaultFromField

sqlId :: Id -> Field SqlText
sqlId = sqlStrictText . unId

data IdMaker = IdMaker
  { _idPrefix :: Text,
    _idTitle :: Maybe Text,
    _idParent :: Maybe Id,
    _idSeq :: Maybe Int,
    _idDate :: Maybe ZonedTime,
    _idLanguage :: Maybe Text
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
imk prefix = IdMaker prefix Nothing Nothing Nothing Nothing Nothing

prepTitle :: Maybe Text -> Text -> Text
prepTitle language title = foldl (<>) "" content
  where
    ascii :: Text
    ascii =
      T.map sanitize $
        Enc.decodeASCII $
          LBS.toStrict $
            convertFuzzy Transliterate "utf-8" "ascii" $
              LBS.fromStrict $
                Enc.encodeUtf8 title
    sanitize :: Char -> Char
    sanitize c | isAlpha c = c
    sanitize _ = ' '
    wds :: [Text]
    wds = T.map toLower <$> T.split (\c -> isPunctuation c || isSpace c) ascii
    stopwords :: Set Text
    stopwords = S.union commonStopwords $ case language of
      Just "fr" -> Fr.stopwords
      _ -> En.stopwords
    toDrop :: Text -> Bool
    toDrop t = S.member t stopwords || T.length t <= 1
    content :: [Text]
    content = capitalize . T.take 10 <$> (take 3 . filter (not . toDrop)) wds

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
prep mk =
  T.intercalate ":" $ filter (not . T.null) [prefix, stub, date, sq]
  where
    (prefix, stub) =
      case prepTitle (mk ^. idLanguage) <$> mk ^. idTitle of
        Just title | not (T.null title) -> ("", title)
        _ ->
          case mk ^. idParent of
            Just (MkId parent) ->
              case find isStub $ T.split (== ':') parent of
                Just stb -> (mk ^. idPrefix, stb)
                _ -> (mk ^. idPrefix, "")
            Nothing -> (mk ^. idPrefix, "")
    isStub :: Text -> Bool
    isStub txt = not (T.null txt) && isUpperCase (T.head txt)
    date :: Text
    date = maybe "" (prepDate (isNothing (mk ^. idTitle) && isNothing (mk ^. idParent))) $ mk ^. idDate
    sq :: Text
    sq = maybe "" (T.pack . show) $ mk ^. idSeq

nextMaker :: IdMaker -> IdMaker
nextMaker = idSeq %~ maybe (Just 1) (Just . (+ 1))

createId :: (Monad m) => (Text -> m Bool) -> IdMaker -> m Text
createId check mk =
  fmap fromJust $ findM check $ prep <$> unfoldr (Just . (id &&& nextMaker)) mk
