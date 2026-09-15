{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Korrvigs.Utils.DAV.Card where

import Control.Lens
import Control.Monad.IO.Class
import Data.List (singleton)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Utils.DAV.Web
import Network.HTTP.Conduit
import Network.URI
import System.FilePath
import Text.XML

data CardDavData = CardDavData
  { _cardUser :: Text,
    _cardPwd :: Text,
    _cardManager :: Manager,
    _cardServer :: Text,
    _cardAbook :: Text
  }

makeLenses ''CardDavData

toDavData :: CardDavData -> DavData
toDavData cdd = DavData (cdd ^. cardUser) (cdd ^. cardPwd) (cdd ^. cardManager)

makeCardURL :: CardDavData -> Text
makeCardURL cdd = T.pack $ joinPath $ T.unpack <$> [cdd ^. cardServer, "addressbooks", "users", cdd ^. cardUser, cdd ^. cardAbook]

getCTag :: (MonadIO m) => CardDavData -> m (Either DavError DavTag)
getCTag cdd =
  propfind dav url [CalDavProp "getctag"] Depth0 >>= \case
    Left err -> pure $ Left err
    Right props -> case M.lookup uri props >>= M.lookup "getctag" . view statProps of
      Nothing -> pure $ Left $ DavError 207 "No CTAG in returned value"
      Just ctag -> pure $ Right $ DavTag ctag
  where
    dav = toDavData cdd
    url = makeCardURL cdd
    uri = DavRc $ maybe url ((<> "/") . T.pack . uriPath) $ parseURI (T.unpack url)

processVCF :: (Text -> a) -> (PropStat -> Maybe Text) -> (DavRessource, PropStat) -> Maybe (DavRessource, a)
processVCF f ext (vcf, metag) = (vcf,) . f <$> ext metag

getETags :: (MonadIO m) => CardDavData -> m (Either DavError (Map DavRessource DavTag))
getETags cdd =
  report dav url (CardProp "addressbook-query") [DavProp "getetag"] filtr Depth1 >>= \case
    Left err -> pure $ Left err
    Right props -> pure $ Right $ M.fromList $ mapMaybe (processVCF DavTag fromStat) $ M.toList props
  where
    dav = toDavData cdd
    url = makeCardURL cdd
    filtr p2n =
      singleton $
        Element (p2n $ CardProp "filter") M.empty $
          singleton $
            NodeElement $
              Element
                (p2n $ CardProp "comp-filter")
                (M.singleton "name" "VCARD")
                []
    fromStat :: PropStat -> Maybe Text
    fromStat stat = M.lookup "getetag" $ stat ^. statProps

getCardData' :: (MonadIO m) => CardDavData -> [DavRessource] -> m (Either DavError (Map DavRessource Text))
getCardData' cdd ids =
  report dav url (CardProp "addressbook-multiget") [CardProp "address-data"] filtr Depth1 >>= \case
    Left err -> pure $ Left err
    Right props -> pure $ Right $ M.fromList $ mapMaybe (processVCF id fromStat) $ M.toList props
  where
    dav = toDavData cdd
    url = makeCardURL cdd
    filtr p2n = flip map ids $ \i ->
      Element (p2n $ DavProp "href") M.empty [NodeContent $ extractDavRc i]
    fromStat :: PropStat -> Maybe Text
    fromStat stat = M.lookup "address-data" $ stat ^. statProps

getCardData :: (MonadIO m) => CardDavData -> [DavRessource] -> m (Either DavError (Map DavRessource Text))
getCardData cdd ids = do
  rs <- mapM (getCardData' cdd) $ chunksOf 15 ids
  let r = sequence rs
  case r of
    Left err -> pure $ Left err
    Right mps -> pure $ Right $ foldr M.union M.empty mps
