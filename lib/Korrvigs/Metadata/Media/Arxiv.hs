module Korrvigs.Metadata.Media.Arxiv (parseQuery, queryArxiv) where

import Conduit
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson hiding (decode)
import Data.Conduit.Text
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format.ISO8601
import qualified Data.XML.Types as XML
import Korrvigs.Entry.New
import Korrvigs.File.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import qualified Korrvigs.Metadata.Media.Pandoc as Pandoc
import Korrvigs.Monad
import Korrvigs.Utils (simpleHttpM)
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Time
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.URI
import Text.Atom.Feed
import Text.Atom.Feed.Import (elementFeed)
import Text.XML

type ArxivId = Text

parseQuery :: Text -> Maybe ArxivId
parseQuery = T.stripPrefix "https://arxiv.org/abs/"

parseDate :: Date -> Maybe (Year, MonthOfYear)
parseDate dt = case iso8601ParseM $ T.unpack dt of
  Nothing -> Nothing
  Just u -> let (yr, mth, _) = toGregorian $ utctDay u in Just (yr, mth)

insertOnce :: (Eq a) => a -> [a] -> [a]
insertOnce x xs = if x `elem` xs then xs else x : xs

processLink :: Link -> NewEntry -> NewEntry
processLink lnk | linkTitle lnk == Just "doi" =
  case T.stripPrefix "http://dx.doi.org/" href <|> T.stripPrefix "https://dx.doi.org" href of
    Nothing -> id
    Just doi -> neMtdt . at (mtdtName DOI) %~ Just . toJSON . insertOnce doi . fromMaybe [] . (>>= fromJSONM)
  where
    href = linkHref lnk
processLink _ = id

processLinks :: Entry -> NewEntry -> NewEntry
processLinks entry = foldr (\lnk f -> processLink lnk . f) id $ entryLinks entry

extractFile :: Link -> Maybe Text
extractFile lnk | linkTitle lnk == Just "pdf" = Just $ linkHref lnk
extractFile _ = Nothing

extractFiles :: Entry -> [Text]
extractFiles = mapMaybe extractFile . entryLinks

elementTxt :: XML.Element -> Text
elementTxt = T.strip . mconcat . fmap ndTxt . XML.elementNodes
  where
    ndTxt (XML.NodeContent (XML.ContentText txt)) = txt
    ndTxt _ = ""

processArxivMtdt :: XML.Element -> NewEntry -> NewEntry
processArxivMtdt el
  | XML.nameLocalName (XML.elementName el) == "journal_ref" =
      neMtdt . at (mtdtName InContainer) %~ Just . toJSON . maybe mc (conTitle .~ con) . (>>= fromJSONM)
  where
    con = elementTxt el
    mc = MediaContainer con Nothing Nothing Nothing Nothing
processArxivMtdt el
  | XML.nameLocalName (XML.elementName el) == "doi" =
      neMtdt . at (mtdtName DOI) %~ Just . toJSON . insertOnce (elementTxt el) . fromMaybe [] . (>>= fromJSONM)
processArxivMtdt _ = id

processArxivMtdts :: Entry -> NewEntry -> NewEntry
processArxivMtdts entry =
  foldr (\el f -> processArxivMtdt el . f) id $ entryOther entry

queryArxivBibtex :: (MonadKorrvigs m) => ArxivId -> m (Maybe (NewEntry -> NewEntry))
queryArxivBibtex i = do
  let url = escapeURIString isUnescapedInURI $ "https://arxiv.org/bibtex/" <> T.unpack i
  bibtex <- simpleHttpM $ T.pack url
  case bibtex of
    Nothing -> pure Nothing
    Just bib -> do
      meds <- liftIO $ Pandoc.importBibtex bib
      case M.toList meds of
        [] -> pure Nothing
        ((_, med) : _) -> pure $ Just med

queryArxiv :: (MonadKorrvigs m) => ArxivId -> m (Maybe (NewEntry -> NewEntry))
queryArxiv i = do
  let url = escapeURIString isUnescapedInURI $ "https://export.arxiv.org/api/query?id_list=" <> T.unpack i
  req <- parseRequest url
  man <- manager
  queryArxivBibtex i >>= \case
    Nothing -> pure Nothing
    Just med -> do
      content <- liftIO $ runResourceT $ do
        resp <- http req man
        let scode = statusCode (responseStatus resp)
        if scode == 200
          then fmap Just $ runConduit $ responseBody resp .| decode utf8 .| sinkTextDoc def
          else pure Nothing
      case content >>= elementFeed . toXMLElement . documentRoot <&> feedEntries of
        Nothing -> pure Nothing
        Just [] -> pure Nothing
        Just (entry : _) -> do
          let files = extractFiles entry
          let title = T.pack $ txtToString $ entryTitle entry
          dls <- fmap catMaybes $ forM files $ \f ->
            newFromUrl $ NewDownloadedFile f $ def & neTitle ?~ title <> " PDF"
          pure $
            Just $
              foldr
                (.)
                med
                [ setMtdtValueM Abstract $ prepAbstract . T.pack . txtToString <$> entrySummary entry,
                  setMtdtValue DOI ["10.48550/arXiv." <> i],
                  maybe id (\(yr, mth) -> neDate ?~ fromGreg yr (Just mth) Nothing) $ entryPublished entry >>= parseDate,
                  setMtdtValue Url $ entryId entry,
                  processLinks entry,
                  processArxivMtdts entry,
                  neChildren %~ (dls <>)
                ]
  where
    prepAbstract :: Text -> Text
    prepAbstract = T.strip . T.replace "\n" " " . T.replace "\\\"" "\""
