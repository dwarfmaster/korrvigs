module Korrvigs.Metadata.Media.Trivial where

import Conduit
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry.New
import Korrvigs.Metadata.Media
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

nebulaExtractor :: TrivialExtractor
nebulaExtractor = string (T.unpack nebulaUrl) >> (urlVideoP <|> urlChannelP)
  where
    nebulaUrl = "https://nebula.tv/"
    urlVideoP = do
      void $ try $ string "videos/"
      i <- manyTill anyChar eof
      pure (Video, nebulaUrl <> "videos/" <> T.pack i)
    urlChannelP = do
      i <- manyTill anyChar eof
      pure (Channel, nebulaUrl <> T.pack i)

mediapartExtractor :: TrivialExtractor
mediapartExtractor = do
  void $ string journalUrl
  i <- manyTill anyChar eof
  pure (Article, T.pack $ journalUrl <> i)
  where
    journalUrl = "https://www.mediapart.fr/journal"

mondeDiploExtractor :: TrivialExtractor
mondeDiploExtractor = do
  void $ string journalUrl
  i <- manyTill anyChar eof
  pure (Article, T.pack $ journalUrl <> i)
  where
    journalUrl = "https://www.monde-diplomatique.fr/"

extractors :: [TrivialExtractor]
extractors = [ytExtractor, nebulaExtractor, mediapartExtractor, mondeDiploExtractor]

parseQuery :: Text -> Maybe TrivialId
parseQuery url = firstJust $ extract <$> extractors
  where
    extract :: TrivialExtractor -> Maybe TrivialId
    extract ex = case runParser ex () "" url of
      Left _ -> Nothing
      Right r -> pure r

query :: (MonadKorrvigs m) => TrivialId -> m (Maybe (NewEntry -> NewEntry))
query (tp, url) = pure $ Just $ setMtdtValue MediaMtdt tp . setMtdtValue Url url
