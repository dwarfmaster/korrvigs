module Korrvigs.Utils.DAV.Web
  ( DavData (..),
    DavError (..),
    davStatusCode,
    davError,
    PropfindDepth (..),
    Property (..),
    propfind,
    report,
    put,
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
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status
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

parsePropFind :: XMLParser [(Text, PropStat)]
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
        pure (lnk, pstat)

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

propfind :: (MonadIO m) => DavData -> Text -> [Property] -> PropfindDepth -> m (Either DavError (Map Text PropStat))
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

report :: (MonadIO m) => DavData -> Text -> Property -> [Property] -> ((Property -> Name) -> [Element]) -> PropfindDepth -> m (Either DavError (Map Text PropStat))
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

put :: (MonadIO m, MonadThrow m) => DavData -> Text -> Text -> LBS.ByteString -> m (Either DavError ())
put dav server etag dat = do
  initReq <- parseRequest $ T.unpack server
  let user = dav ^. davUser
  let pwd = dav ^. davPwd
  let req =
        applyBasicAuth (Enc.encodeUtf8 user) (Enc.encodeUtf8 pwd) $
          initReq
            { method = "PUT"
            }
  let hdContent = [("Content-type", "text/calendar; charset=utf-8")]
  let matchETag = [("If-Match", Enc.encodeUtf8 etag)]
  let reqWithBody =
        req
          { requestBody = RequestBodyLBS dat,
            requestHeaders = hdContent ++ matchETag ++ requestHeaders req
          }
  let man = dav ^. davManager
  liftIO $ runResourceT $ do
    resp <- http reqWithBody man
    let scode = statusCode (responseStatus resp)
    if scode == 200 || scode == 207
      then pure $ Right ()
      else pure $ Left $ DavError scode $ "Failed with status code " <> T.pack (show scode)
