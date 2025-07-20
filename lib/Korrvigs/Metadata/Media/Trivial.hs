module Korrvigs.Metadata.Media.Trivial where

import Conduit
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Korrvigs.Utils (firstJust)
import Text.Parsec

type TrivialId = (MediaType, Text)

type TrivialExtractor = ParsecT Text () Identity (MediaType, Text)

ytExtractor :: TrivialExtractor
ytExtractor = string (T.unpack ytUrl) >> (urlVideoP <|> urlChannelP)
  where
    ytUrl = "https://www.youtube.com/"
    urlVideoP = do
      void $ string "watch?v="
      i <- manyTill anyChar eof
      pure (Video, ytUrl <> "watch?v=" <> T.pack i)
    urlChannelP = do
      void $ char '@'
      i <- manyTill anyChar eof
      pure (Channel, ytUrl <> "@" <> T.pack i)

extractors :: [TrivialExtractor]
extractors = [ytExtractor]

parseQuery :: Text -> Maybe TrivialId
parseQuery url = firstJust $ extract <$> extractors
  where
    extract :: TrivialExtractor -> Maybe TrivialId
    extract ex = case runParser ex () "" url of
      Left _ -> Nothing
      Right r -> pure r

query :: (MonadKorrvigs m) => TrivialId -> m (Maybe (Media, [Id]))
query (tp, url) =
  pure $
    Just
      ( Media
          { _medType = tp,
            _medAbstract = Nothing,
            _medBibtex = Nothing,
            _medDOI = [],
            _medISBN = [],
            _medISSN = [],
            _medTitle = Nothing,
            _medAuthors = [],
            _medMonth = Nothing,
            _medYear = Nothing,
            _medUrl = Just url,
            _medRSS = Nothing,
            _medSource = [],
            _medPublisher = [],
            _medContainer = Nothing,
            _medInstitution = [],
            _medLicense = [],
            _medCover = Nothing,
            _medDiscussion = []
          },
        []
      )
