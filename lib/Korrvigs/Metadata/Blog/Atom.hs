module Korrvigs.Metadata.Blog.Atom where

import Conduit (throwM)
import Control.Lens
import Data.Binary.Builder
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.XML.Types as XML
import Korrvigs.Metadata.Blog.Structure
import Korrvigs.Monad
import qualified Text.Atom.Feed as A
import Text.Atom.Feed.Export (xmlFeed)
import Text.XML (fromXMLDocument, renderLBS)
import Yesod

renderAtom :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> Maybe Text -> Text -> [(Text, Day, Text)] -> m TypedContent
renderAtom renderUrl tag title entries = do
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
    Right d -> do
      let lbs = renderLBS def d
      pure $ toTypedContent (typeAtom, ContentBuilder (fromLazyByteString lbs) Nothing)

generateAtomFor :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> Maybe Text -> Text -> [(Text, Day, Text)] -> m A.Feed
generateAtomFor renderUrl tag title entries = do
  uri <- renderUrl $ maybe BlogAtom BlogAtomTag tag
  atomEntries <- mapM (generateAtomEntryFor renderUrl) entries
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
        A.feedEntries = atomEntries
      }

generateAtomEntryFor :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> (Text, Day, Text) -> m A.Entry
generateAtomEntryFor renderUrl (nm, day, title) = do
  uri <- renderUrl $ BlogPostNote nm
  let date = T.pack $ iso8601Show $ UTCTime day 0
  pure $
    (A.nullEntry uri (A.TextString title) date)
      { A.entryPublished = Just date
      }
