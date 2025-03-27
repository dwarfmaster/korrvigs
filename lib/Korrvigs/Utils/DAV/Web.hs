{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Korrvigs.Utils.DAV.Web
  ( DavData (..),
    DavError (..),
    DavRessource (..),
    DavTag (..),
    davStatusCode,
    davError,
    PropfindDepth (..),
    Property (..),
    propfind,
    report,
    put,
    delete,
    PropStat (..),
    statStatus,
    statProps,
  )
where

import Conduit
import Control.Applicative
import Control.Lens hiding (element)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Text
import Data.List (singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Korrvigs.Utils.XML
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.FilePath
import Text.XML
import Text.XML.Cursor (fromDocument)

data DavData = DavData
  { _davUser :: Text,
    _davPwd :: Text,
    _davManager :: Manager
  }

makeLenses ''DavData

data DavError = DavError
  { _davStatusCode :: Int,
    _davError :: Text
  }
  deriving (Show)

makeLenses ''DavError

newtype DavRessource = DavRc {extractDavRc :: Text} deriving (Ord, Eq, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype DavTag = DavTag {extractDavTag :: Text} deriving (Ord, Eq, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

type DavM = ExceptT DavError (ReaderT DavData IO)

runDavM :: (MonadIO m) => DavData -> DavM a -> m (Either DavError a)
runDavM auth x = liftIO $ runReaderT (runExceptT x) auth

davXML :: Text -> Text -> Maybe Document -> [Header] -> DavM Document
davXML url mtd body hds = do
  initReq <- parseRequest $ T.unpack url
  user <- view davUser
  pwd <- view davPwd
  let req =
        applyBasicAuth (Enc.encodeUtf8 user) (Enc.encodeUtf8 pwd) $
          initReq
            { method = Enc.encodeUtf8 mtd
            }
  let hdContent = case body of
        Just _ -> [("Content-type", "application/xml; charset=utf-8")]
        Nothing -> []
  let reqWithBody =
        req
          { requestBody = case body of
              Just doc -> RequestBodyLBS $ renderLBS def doc
              Nothing -> RequestBodyBS "",
            requestHeaders = hdContent ++ hds ++ requestHeaders req
          }
  man <- view davManager
  mxml <- liftIO $ runResourceT $ do
    resp <- http reqWithBody man
    let scode = statusCode (responseStatus resp)
    if scode == 200 || scode == 207
      then fmap Right $ runConduit $ responseBody resp .| decode utf8 .| sinkTextDoc def
      else do
        let msg = "Failed with status code " <> T.pack (show scode)
        msgs <- runConduit $ responseBody resp .| decode utf8 .| sinkList
        pure $ Left $ DavError scode $ mconcat $ msg : msgs
  case mxml of
    Left err -> throwError err
    Right xml -> pure xml

data PropStat = PropStat
  { _statStatus :: Text,
    _statProps :: Map Text Text
  }
  deriving (Eq, Show)

makeLenses ''PropStat

bname :: Element -> Text
bname = nameLocalName . elementName

guardName :: Element -> Text -> XMLParser ()
guardName el nm | bname el == nm = pure ()
guardName el nm = fail $ "Expected \"" <> T.unpack nm <> "\" element, got \"" <> T.unpack (bname el) <> "\""

parsePropFind :: XMLParser [(DavRessource, PropStat)]
parsePropFind = do
  onElementChildren $ \mstatus -> do
    guardName mstatus "multistatus"
    fmap mconcat . tillEof $ onElementChildren $ \resp -> do
      guardName resp "response"
      tillEof $ do
        lnk <- onElementChildren $ \href -> do
          guardName href "href"
          textContent
        pstat <- onElementChildren $ \stat -> do
          guardName stat "propstat"
          parsePropStat
        pure (DavRc lnk, pstat)

textContent :: XMLParser Text
textContent = mconcat . catMaybes <$> tillEof (optional content)

parsePropStat :: XMLParser PropStat
parsePropStat = do
  props <- tillEof $ optional $ statusParser <|> propsParser
  pure $ foldr (fromMaybe id) (PropStat "" M.empty) props
  where
    statusParser :: XMLParser (PropStat -> PropStat)
    statusParser = onElementChildren $ \st -> do
      guardName st "status"
      txt <- textContent
      pure $ statStatus .~ txt
    propsParser :: XMLParser (PropStat -> PropStat)
    propsParser = onElementChildren $ \prop -> do
      guardName prop "prop"
      props <- tillEof $ optional $ onElementChildren $ \prp -> do
        val <- textContent
        pure (nameLocalName (elementName prp), val)
      let propMap = M.fromList $ catMaybes props
      pure $ statProps .~ propMap

data PropfindDepth
  = Depth0
  | Depth1
  | DepthInfinite

depthHeader :: PropfindDepth -> [Header]
depthHeader depth = singleton $ ("Depth",) $ case depth of
  Depth0 -> "0"
  Depth1 -> "1"
  DepthInfinite -> "infinite"

data Property
  = DavProp Text
  | CalDavProp Text
  | CalProp Text
  | MiscProp Text Text

propToName :: Property -> Name
propToName (DavProp nm) = Name nm (Just "DAV:") (Just "d")
propToName (CalDavProp nm) = Name nm (Just "http://calendarserver.org/ns/") (Just "cs")
propToName (CalProp nm) = Name nm (Just "urn:ietf:params:xml:ns:caldav") (Just "c")
propToName (MiscProp nm ns) = Name nm (Just ns) Nothing

propfind :: (MonadIO m) => DavData -> Text -> [Property] -> PropfindDepth -> m (Either DavError (Map DavRessource PropStat))
propfind dav server properties depth =
  runDavM dav davAct >>= \case
    Left err -> pure $ Left err
    Right resp -> do
      case runIdentity $ runTreeParser parsePropFind $ fromDocument resp of
        Left perr -> pure $ Left $ DavError 0 $ perr ^. errMsg
        Right v -> pure $ Right $ M.fromList v
  where
    davAct =
      davXML
        server
        "PROPFIND"
        propXml
        (depthHeader depth ++ [("Prefer", "return-minimal")])
    propXml :: Maybe Document
    propXml
      | null properties = Nothing
      | otherwise = Just $ Document (Prologue [] Nothing []) propXmlContent []
    propXmlContent :: Element
    propXmlContent =
      Element
        (propToName $ DavProp "propfind")
        M.empty
        [ NodeElement $
            Element
              (propToName $ DavProp "prop")
              M.empty
              [NodeElement (Element (propToName prop) M.empty [])]
          | prop <- properties
        ]

report :: (MonadIO m) => DavData -> Text -> Property -> [Property] -> ((Property -> Name) -> [Element]) -> PropfindDepth -> m (Either DavError (Map DavRessource PropStat))
report dav server query properties filtr depth =
  runDavM dav davAct >>= \case
    Left err -> pure $ Left err
    Right resp -> do
      case runIdentity $ runTreeParser parsePropFind $ fromDocument resp of
        Left perr -> pure $ Left $ DavError 0 $ perr ^. errMsg
        Right v -> pure $ Right $ M.fromList v
  where
    davAct =
      davXML server "REPORT" (Just repXml) (depthHeader depth ++ [("Prefer", "return-minimal")])
    repXml = Document (Prologue [] Nothing []) xmlContent []
    xmlContent =
      Element
        (propToName query)
        M.empty
        $ NodeElement
          <$> ( Element (propToName $ DavProp "prop") M.empty propXml
                  : filtr propToName
              )
    propXml =
      [NodeElement $ Element (propToName prop) M.empty [] | prop <- properties]

-- Return new ETAG if the information is present in the response
put :: (MonadIO m, MonadThrow m) => DavData -> Text -> DavRessource -> Maybe DavTag -> LBS.ByteString -> m (Either DavError (Maybe DavTag))
put dav server rc etag dat = do
  let url = T.unpack $ server <> extractDavRc rc
  initReq <- parseRequest url
  let user = dav ^. davUser
  let pwd = dav ^. davPwd
  let req =
        applyBasicAuth (Enc.encodeUtf8 user) (Enc.encodeUtf8 pwd) $
          initReq
            { method = "PUT"
            }
  let hdContent = [("Content-Type", "text/calendar; charset=utf-8")]
  let matchETag = case etag of
        Just e -> [("If-Match", Enc.encodeUtf8 $ extractDavTag e)]
        Nothing -> []
  let reqWithBody =
        req
          { requestBody = RequestBodyLBS dat,
            requestHeaders = hdContent ++ matchETag ++ requestHeaders req
          }
  let man = dav ^. davManager
  liftIO $ runResourceT $ do
    resp <- http reqWithBody man
    let scode = statusCode (responseStatus resp)
    if scode == 200 || scode == 204
      then pure $ case getResponseHeader "ETag" resp of
        [netag] -> Right $ Just $ DavTag $ Enc.decodeUtf8 netag
        _ -> Right Nothing
      else
        if scode == 412
          then do
            liftIO $ putStrLn $ "Precondition failed for " <> url
            pure (Right Nothing)
          else pure $ Left $ DavError scode $ "Failed with status code " <> T.pack (show scode)

delete :: (MonadIO m, MonadThrow m) => DavData -> Text -> DavRessource -> DavTag -> m (Either DavError ())
delete dav server rc etag = do
  let url = T.unpack server </> T.unpack (extractDavRc rc)
  initReq <- parseRequest url
  let user = dav ^. davUser
  let pwd = dav ^. davPwd
  let req =
        applyBasicAuth (Enc.encodeUtf8 user) (Enc.encodeUtf8 pwd) $
          initReq
            { method = "DELETE"
            }
  let hdContent = [("Content-type", "text/calendar; charset=utf-8")]
  let matchETag = [("If-Match", Enc.encodeUtf8 $ extractDavTag etag)]
  let reqWithHds = req {requestHeaders = hdContent ++ matchETag ++ requestHeaders req}
  let man = dav ^. davManager
  liftIO $ runResourceT $ do
    resp <- http reqWithHds man
    let scode = statusCode (responseStatus resp)
    if scode == 200 || scode == 202 || scode == 204
      then pure $ Right ()
      else pure $ Left $ DavError scode $ "Failed with status code " <> T.pack (show scode)
