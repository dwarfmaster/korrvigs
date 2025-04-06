module Korrvigs.Metadata.Media.Arxiv (parseQuery, queryArxiv) where

import Conduit
import Control.Applicative
import Control.Lens
import Data.Conduit.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format.ISO8601
import qualified Data.XML.Types as XML
import Korrvigs.Metadata.Media.Ontology
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

processLink :: Link -> Media -> Media
processLink lnk | linkTitle lnk == Just "doi" =
  case T.stripPrefix "http://dx.doi.org/" href <|> T.stripPrefix "https://dx.doi.org" href of
    Nothing -> id
    Just doi -> medDOI %~ insertOnce doi
  where
    href = linkHref lnk
processLink lnk | linkTitle lnk == Just "pdf" = id -- TODO download pdf
processLink _ = id

processLinks :: Entry -> Media -> Media
processLinks entry = foldr (\lnk f -> processLink lnk . f) id $ entryLinks entry

elementTxt :: XML.Element -> Text
elementTxt = T.strip . mconcat . fmap ndTxt . XML.elementNodes
  where
    ndTxt (XML.NodeContent (XML.ContentText txt)) = txt
    ndTxt _ = ""

processArxivMtdt :: XML.Element -> Media -> Media
processArxivMtdt el
  | XML.nameLocalName (XML.elementName el) == "journal_ref" =
      medContainer %~ Just . maybe mc (conTitle .~ con)
  where
    con = elementTxt el
    mc = MediaContainer con Nothing Nothing Nothing Nothing
processArxivMtdt el
  | XML.nameLocalName (XML.elementName el) == "doi" =
      medDOI %~ insertOnce (elementTxt el)
processArxivMtdt _ = id

processArxivMtdts :: Entry -> Media -> Media
processArxivMtdts entry =
  foldr (\el f -> processArxivMtdt el . f) id $ entryOther entry

queryArxiv :: ArxivId -> IO (Maybe Media)
queryArxiv i = do
  let url = escapeURIString isUnescapedInURI $ "https://export.arxiv.org/api/query?id_list=" <> T.unpack i
  req <- parseRequest url
  man <- newManager tlsManagerSettings
  content <- runResourceT $ do
    resp <- http req man
    let scode = statusCode (responseStatus resp)
    if scode == 200
      then fmap Just $ runConduit $ responseBody resp .| decode utf8 .| sinkTextDoc def
      else pure Nothing
  case content >>= elementFeed . toXMLElement . documentRoot <&> feedEntries of
    Nothing -> pure Nothing
    Just [] -> pure Nothing
    Just (entry : _) -> do
      pure $
        Just $
          Media
            { _medType = Article,
              _medAbstract = prepAbstract . T.pack . txtToString <$> entrySummary entry,
              _medBibtex = Nothing,
              _medDOI = ["10.48550/arXiv." <> i],
              _medISBN = [],
              _medISSN = [],
              _medTitle = Just $ T.pack $ txtToString $ entryTitle entry,
              _medAuthors = personName <$> entryAuthors entry,
              _medMonth = entryPublished entry >>= parseDate <&> snd,
              _medYear = entryPublished entry >>= parseDate <&> fst,
              _medUrl = Just $ entryId entry,
              _medRSS = Nothing,
              _medSource = [],
              _medPublisher = [],
              _medContainer = Nothing,
              _medInstitution = [],
              _medLicense = []
            }
            & processLinks entry
            & processArxivMtdts entry
  where
    prepAbstract :: Text -> Text
    prepAbstract = T.strip . T.replace "\n" " " . T.replace "\\\"" "\""
