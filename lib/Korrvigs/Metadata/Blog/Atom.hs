module Korrvigs.Metadata.Blog.Atom where

import Conduit (throwM)
import Control.Lens
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.XML.Types as XML
import Korrvigs.Metadata.Blog.Content
import Korrvigs.Metadata.Blog.Structure
import Korrvigs.Monad
import qualified Text.Atom.Feed as A
import Text.Atom.Feed.Export (xmlFeed)
import Text.XML (fromXMLDocument, renderLBS)

renderAtom :: (MonadKorrvigs m) => Bool -> (BlogUrl -> m Text) -> Maybe Text -> Text -> m LBS.ByteString
renderAtom onlyPublished renderUrl tag title = do
  entries <- loadForTag onlyPublished tag $ Just 10
  xml <- xmlFeed <$> generateAtomFor renderUrl tag title entries
  let doc =
        fromXMLDocument $
          XML.Document
            { XML.documentPrologue =
                XML.Prologue
                  { XML.prologueDoctype = Nothing,
                    XML.prologueBefore = [],
                    XML.prologueAfter = []
                  },
              XML.documentRoot = xml,
              XML.documentEpilogue = []
            }
  case doc of
    Left _ -> throwM $ KMiscError "Failed to convert from atom xml"
    Right d -> pure $ renderLBS def d

atomLinkTo :: Text -> A.Link
atomLinkTo uri =
  A.Link uri (Just $ Left "self") (Just "application/rss+xml") Nothing Nothing Nothing [] []

generateAtomFor :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> Maybe Text -> Text -> [(Text, Day, Text, FilePath)] -> m A.Feed
generateAtomFor renderUrl tag title entries = do
  uri <- renderUrl $ maybe BlogAtom BlogAtomTag tag
  atomEntries <- mapM (generateAtomEntryFor uri renderUrl) entries
  let lastDay = fromMaybe (fromGregorian 1970 01 01) $ view _2 <$> listToMaybe entries
  let date = T.pack $ iso8601Show $ UTCTime lastDay 0
  pure $
    (A.nullFeed uri (A.TextString title) date)
      { A.feedGenerator =
          Just $
            A.Generator
              { A.genURI = Just "https://github.com/dwarfmaster/korrvigs",
                A.genVersion = Nothing,
                A.genText = "korrvigs"
              },
        A.feedEntries = atomEntries,
        A.feedLinks = [atomLinkTo uri]
      }

generateAtomEntryFor :: (MonadKorrvigs m) => Text -> (BlogUrl -> m Text) -> (Text, Day, Text, FilePath) -> m A.Entry
generateAtomEntryFor feedURI renderUrl (nm, day, title, path) = do
  uri <- renderUrl $ BlogPostNote nm
  blogURI <- renderUrl $ BlogTopLevel "default.html"
  let date = T.pack $ iso8601Show $ UTCTime day 0
  (summary, content) <- postTextRender renderUrl path
  pure $
    (A.nullEntry uri (A.TextString title) date)
      { A.entryPublished = Just date,
        A.entrySummary = Just (A.HTMLString summary),
        A.entryContent = Just (A.HTMLContent content),
        A.entryAuthors = [A.Person "DwarfMaster" (Just blogURI) Nothing []],
        A.entryLinks = [atomLinkTo feedURI]
      }
