module Korrvigs.Metadata.Media.Export (export) where

import Control.Lens
import Control.Monad (join)
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.ISBN
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad

data BibData = BibData
  { _bibKind :: Text,
    _bibKey :: Text,
    _bibTitle :: Maybe Text,
    _bibAuthors :: [Text],
    _bibAbstract :: Maybe Text,
    _bibDOI :: Maybe Text,
    _bibISBN :: Maybe ISBN,
    _bibMonth :: Maybe MonthOfYear,
    _bibYear :: Maybe Year,
    _bibUrl :: Maybe Text,
    _bibJournal :: Maybe Text,
    _bibPublisher :: Maybe Text,
    _bibContainer :: Maybe MediaContainer,
    _bibCollection :: Maybe Text,
    _bibInstitution :: [Text]
  }

makeLenses ''BibData

mkBibKind :: MediaType -> Text
mkBibKind Article = "Article"
mkBibKind Book = "Book"
mkBibKind Comic = "Book"
mkBibKind Booklet = "Booklet"
mkBibKind Incollection = "Incollection"
mkBibKind Inproceedings = "Inproceedings"
mkBibKind Manual = "Manual"
mkBibKind MastersThesis = "Mastersthesis"
mkBibKind PhdThesis = "Phdthesis"
mkBibKind Unpublished = "Unpublished"
mkBibKind Manga = "Book"
mkBibKind Chapter = "Inbook"
mkBibKind Page = "Inbook"
mkBibKind _ = "Misc"

rSelectBibData :: (MonadKorrvigs m) => Id -> m (Maybe BibData)
rSelectBibData i =
  rSelectMtdt MediaMtdt si >>= \case
    Nothing -> pure Nothing
    Just tp -> do
      key <- rSelectMtdt BibtexKey si
      entry <- load i
      bd <-
        BibData
          (mkBibKind tp)
          (fromMaybe (unId i) key)
          (entry ^? _Just . entryTitle . _Just)
          . join
          . toList
          <$> rSelectMtdt Authors si
          <*> rSelectMtdt Abstract si
          <*> ((listToMaybe =<<) <$> rSelectMtdt DOI si)
          <*> ((listToMaybe =<<) <$> rSelectMtdt ISBNMtdt si)
          <*> pure (entry ^? _Just . entryDate . _Just . to greg . _2)
          <*> pure (entry ^? _Just . entryDate . _Just . to greg . _1)
          <*> rSelectMtdt Url si
          <*> rSelectMtdt Journal si
          <*> ((listToMaybe =<<) <$> rSelectMtdt Publisher si)
          <*> rSelectMtdt InContainer si
          <*> rSelectMtdt InCollection si
          <*> (join . toList <$> rSelectMtdt Institution si)
      pure $ Just bd
  where
    si = sqlId i
    greg = toGregorian . localDay . zonedTimeToLocalTime

prepareBibData :: (MonadKorrvigs m) => [Id] -> m [BibData]
prepareBibData ids = do
  bibs <- mapM rSelectBibData ids
  pure $ sortBy (\b1 b2 -> compare (b1 ^. bibKey) (b2 ^. bibKey)) $ catMaybes bibs

renderBib :: BibData -> Builder
renderBib bib =
  mconcat
    [ charUtf8 '@',
      stringUtf8 $ T.unpack $ bib ^. bibKind,
      charUtf8 '{',
      stringUtf8 $ T.unpack $ bib ^. bibKey,
      stringUtf8 ",\n",
      mt (bib ^. bibTitle) "title",
      m (Just $ bib ^. bibAuthors) "authors" (stringUtf8 . T.unpack . T.intercalate ", "),
      mt (bib ^. bibAbstract) "abstract",
      mt (bib ^. bibDOI) "doi",
      mt (renderISBN <$> bib ^. bibISBN) "ISBN",
      m (bib ^. bibMonth) "month" intDec,
      m (bib ^. bibYear) "year" integerDec,
      mt (bib ^. bibUrl) "url",
      mt (bib ^. bibJournal) "journal",
      mt (bib ^. bibPublisher) "publisher",
      mt (bib ^? bibContainer . _Just . conTitle) "container",
      mt (bib ^? bibContainer . _Just . conCollection . _Just) "booktitle",
      mt (bib ^? bibContainer . _Just . conChapter . _Just) "chapter",
      m (bib ^? bibContainer . _Just . conPages . _Just) "pages" $ \(p1, p2) -> intDec p1 <> stringUtf8 "--" <> intDec p2,
      m (bib ^? bibContainer . _Just . conVolume . _Just) "volume" intDec,
      mt (bib ^. bibCollection) "booktitle",
      m (Just $ bib ^. bibInstitution) "organisation" (stringUtf8 . T.unpack . T.intercalate ", "),
      stringUtf8 "}\n"
    ]
  where
    m :: Maybe a -> Text -> (a -> Builder) -> Builder
    m Nothing _ _ = mempty
    m (Just v) k bld = stringUtf8 ("  " <> T.unpack k <> " = {") <> bld v <> stringUtf8 "},\n"
    mt v k = m v k $ stringUtf8 . T.unpack

renderBibliography :: [BibData] -> Builder
renderBibliography bibs = mconcat $ intersperse (charUtf8 '\n') $ renderBib <$> bibs

export :: (MonadKorrvigs m) => [Id] -> m ByteString
export ids = toLazyByteString . renderBibliography <$> prepareBibData ids
