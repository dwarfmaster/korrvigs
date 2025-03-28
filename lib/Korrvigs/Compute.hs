module Korrvigs.Compute where

import Conduit (throwM)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Actions.SQL
import qualified Korrvigs.Compute.Builtin as Builtin
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils (recursiveRemoveFile)
import Korrvigs.Utils.Git.Annex
import Korrvigs.Utils.JSON (writeJsonToFile)
import System.Directory
import System.FilePath

newtype Action = Builtin Builtin.Action
  deriving (Eq, Show)

instance ToJSON Action where
  toJSON (Builtin act) =
    object
      [ "kind" .= String "builtin",
        "value" .= toJSON act
      ]

instance FromJSON Action where
  parseJSON = withObject "Action" $ \act -> do
    kd <- act .: "kind"
    case kd of
      String "builtin" -> Builtin <$> (parseJSON =<< act .: "value")
      String str -> fail $ T.unpack $ "\"" <> str <> "\" is not a valid computation kind name"
      obj -> unexpected obj

data CompType
  = ScalarImage
  | Picture
  | VectorImage
  | Json
  deriving (Eq, Show)

instance FromJSON CompType where
  parseJSON = withText "CompType" $ \case
    "scalar" -> pure ScalarImage
    "picture" -> pure Picture
    "vector" -> pure VectorImage
    "json" -> pure Json
    str -> fail $ T.unpack $ "\"" <> str <> "\" is not a valid computation type name"

instance ToJSON CompType where
  toJSON ScalarImage = "scalar"
  toJSON Picture = "picture"
  toJSON VectorImage = "vector"
  toJSON Json = "json"

data Computation = Computation
  { _cmpEntry :: Id,
    _cmpId :: Text,
    _cmpAction :: Action,
    _cmpType :: CompType
  }
  deriving (Eq, Show)

data ComputationJSON = CompJSON
  { _cmpJsAction :: Action,
    _cmpJsType :: CompType
  }
  deriving (Eq, Show)

instance FromJSON ComputationJSON where
  parseJSON = withObject "Computation" $ \obj ->
    CompJSON
      <$> obj .: "action"
      <*> obj .: "type"

instance ToJSON ComputationJSON where
  toJSON (CompJSON action tp) =
    object
      [ "action" .= toJSON action,
        "type" .= toJSON tp
      ]

type EntryCompsJSON = Map Text ComputationJSON

type EntryComps = Map Text Computation

makeLenses ''Computation
makeLenses ''ComputationJSON
makePrisms ''Action
makePrisms ''CompType

cacheDir :: (MonadKorrvigs m) => m FilePath
cacheDir = do
  rt <- root
  pure $ joinPath [rt, "cache"]

compsDir :: (MonadKorrvigs m) => Id -> m FilePath
compsDir i = do
  rt <- cacheDir
  let dir = fmap T.unpack $ preparePart =<< T.split (== ':') (unId i)
  pure $ joinPath $ rt : dir
  where
    preparePart :: Text -> [Text]
    preparePart p | not (T.null p) && isUpperCase (T.head p) = [T.take 2 p, p]
    preparePart p = [p]

compsFile :: (MonadKorrvigs m) => Id -> m FilePath
compsFile i = do
  dir <- compsDir i
  pure $ joinPath [dir, "computations.json"]

compFile :: (MonadKorrvigs m) => Computation -> m FilePath
compFile cmp = do
  dir <- compsDir $ cmp ^. cmpEntry
  pure $ joinPath [dir, T.unpack $ cmp ^. cmpId <> ext]
  where
    ext :: Text
    ext = case cmp ^. cmpType of
      ScalarImage -> ".png"
      Picture -> ".jpg"
      VectorImage -> ".svg"
      Json -> ".json"

loadComputations :: Id -> FilePath -> IO (Either Text EntryComps)
loadComputations i file = do
  parsed <- eitherDecode <$> LBS.readFile file
  case parsed of
    Left err -> pure $ Left $ "Can't parse computation file " <> unId i <> " : " <> T.pack err
    Right js -> pure $ Right $ M.mapWithKey cmpFromJSON js
  where
    cmpFromJSON :: Text -> ComputationJSON -> Computation
    cmpFromJSON key cmp =
      Computation
        { _cmpEntry = i,
          _cmpId = key,
          _cmpAction = cmp ^. cmpJsAction,
          _cmpType = cmp ^. cmpJsType
        }

entryStoredComputations :: (MonadKorrvigs m) => Id -> m EntryComps
entryStoredComputations i = entryStoredComputations' i =<< compsFile i

entryStoredComputations' :: (MonadKorrvigs m) => Id -> FilePath -> m EntryComps
entryStoredComputations' i file = do
  ex <- liftIO $ doesFileExist file
  if not ex
    then pure M.empty
    else do
      parsed <- liftIO $ loadComputations i file
      case parsed of
        Left err -> throwM $ KMiscError err
        Right js -> pure js

getJsonComp :: (MonadKorrvigs m, FromJSON a) => Computation -> m (Maybe a)
getJsonComp cmp = getJsonComp' =<< compFile cmp

getJsonComp' :: (MonadKorrvigs m, FromJSON a) => FilePath -> m (Maybe a)
getJsonComp' file = do
  ex <- liftIO $ doesFileExist file
  if ex
    then do
      parsed <- liftIO $ eitherDecode <$> LBS.readFile file
      case parsed of
        Left err -> throwM $ KMiscError $ "Can't parse JSON computation file " <> T.pack file <> " : " <> T.pack err
        Right (x :: Value) -> pure $ case fromJSON x of
          Error _ -> Nothing
          Success v -> Just v
    else pure Nothing

storeComputations :: (MonadKorrvigs m) => Id -> EntryComps -> m ()
storeComputations i cmps = storeComputations' cmps =<< compsFile i

storeComputations' :: (MonadKorrvigs m) => EntryComps -> FilePath -> m ()
storeComputations' cmps file | M.null cmps = do
  ex <- liftIO $ doesFileExist file
  rt <- cacheDir
  when ex $ recursiveRemoveFile rt file
storeComputations' cmps file = do
  let dir = takeDirectory file
  liftIO $ createDirectoryIfMissing True dir
  writeJsonToFile file $ cmpToJSON <$> cmps

cmpToJSON :: Computation -> ComputationJSON
cmpToJSON cmp =
  CompJSON
    { _cmpJsAction = cmp ^. cmpAction,
      _cmpJsType = cmp ^. cmpType
    }

run :: (MonadKorrvigs m) => Computation -> m ()
run cmp =
  load (cmp ^. cmpEntry) >>= \case
    Nothing -> pure ()
    Just entry -> do
      dir <- compsDir $ entry ^. name
      liftIO $ createDirectoryIfMissing True dir
      tgt <- compFile cmp
      case cmp ^. cmpAction of
        Builtin act -> Builtin.run act entry tgt
      ex <- liftIO $ doesFileExist tgt
      rt <- root
      when (ex && shouldAnnex (cmp ^. cmpType)) $ annexAdd rt tgt
  where
    shouldAnnex :: CompType -> Bool
    shouldAnnex ScalarImage = True
    shouldAnnex Picture = True
    shouldAnnex VectorImage = True
    shouldAnnex Json = False

syncComputations :: (MonadKorrvigs m) => Id -> EntryComps -> m ()
syncComputations i cmps = do
  stored <- entryStoredComputations i
  let toRm = M.difference stored cmps
  forM_ (M.toList toRm) $ \(_, cmp) -> do
    file <- compFile cmp
    ex <- liftIO $ doesFileExist file
    rt <- cacheDir
    when ex $ recursiveRemoveFile rt file
  storeComputations i cmps

listComputations :: (MonadKorrvigs m) => m (Map Id EntryComps)
listComputations = (liftIO . recurse []) =<< cacheDir
  where
    recurse :: [Text] -> FilePath -> IO (Map Id EntryComps)
    recurse prts path = do
      let file = joinPath [path, "computations.json"]
      fileEx <- doesFileExist file
      fileComp <-
        if fileEx
          then do
            let i = idFromPath prts
            parsed <- loadComputations i file
            pure $ case parsed of
              Left _ -> M.empty
              Right js -> M.singleton i js
          else pure M.empty
      contents <- getDirectoryContents path
      comps <- forM contents $ \part -> do
        let partPath = joinPath [path, part]
        partDir <- doesDirectoryExist partPath
        if partDir
          then recurse (T.pack part : prts) partPath
          else pure M.empty
      pure $ mconcat $ fileComp : comps
    idFromPath :: [Text] -> Id
    idFromPath = MkId . T.intercalate ":" . filterParts
    filterParts :: [Text] -> [Text]
    filterParts [] = []
    filterParts (prefix : prt : prts)
      | T.length prefix <= 2
          && isUpperCase (T.head prefix)
          && T.isPrefixOf prefix prt =
          prt : filterParts prts
    filterParts (prt : prts) = prt : filterParts prts

rmComputations :: (MonadKorrvigs m) => Id -> m ()
rmComputations i = do
  dir <- compsDir i
  ex <- liftIO $ doesFileExist dir
  when ex $ liftIO $ removeDirectory dir
