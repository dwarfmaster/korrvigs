module Data.ERIS.Test where

import Conduit
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.State
import Crypto.Hash
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString.Base32
import qualified Data.ByteString.Lazy as LBS
import Data.ERIS.Crypto
import Data.ERIS.DB.Class
import Data.ERIS.Decode
import Data.ERIS.Encode
import Data.ERIS.StreamingEncode
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Text.IO (putStr, putStrLn)
import System.Directory
import System.FilePath
import Prelude hiding (putStr, putStrLn)

data ERISTestType
  = ERISTestPositive
      { _erisTestContent :: ByteString,
        _erisTestConvergence :: ConvergenceSecret,
        _erisTestBlockSize :: Int
      }
  | ERISTestNegative

data ERISTestVector = ERISTestVector
  { _erisTestId :: Int,
    _erisTestType :: ERISTestType,
    _erisTestSpec :: Text,
    _erisTestName :: Text,
    _erisTextDescription :: Text,
    _erisTestCapability :: ERISCapability,
    _erisTestUrn :: Text,
    _erisTestBlocks :: Map ERISHash ERISBlock
  }

makeLenses ''ERISTestType
makeLenses ''ERISTestVector

parseJsonB32 :: Text -> Parser ByteString
parseJsonB32 txt = case decodeBase32 $ Enc.encodeUtf8 txt of
  Left err -> fail $ T.unpack err
  Right bs -> pure bs

parseJsonDigest :: (HashAlgorithm a) => Text -> Parser (Digest a)
parseJsonDigest txt = do
  bs <- parseJsonB32 txt
  case digestFromByteString bs of
    Nothing -> fail "Digest of unexpected length"
    Just d -> pure d

parseHashKey :: Text -> Parser ERISHashKey
parseHashKey txt = do
  bs <- parseJsonB32 txt
  case mkErisHashKey bs of
    Nothing -> fail "Hash key of unexpected length"
    Just k -> pure k

parseCapability :: Object -> Parser ERISCapability
parseCapability obj =
  ERISCapability
    <$> obj .: "block-size"
    <*> obj .: "level"
    <*> (obj .: "root-reference" >>= parseJsonDigest)
    <*> (obj .: "root-key" >>= parseJsonDigest)

parseBlocks :: Map Text Text -> Parser (Map ERISHash ERISBlock)
parseBlocks mp = fmap M.fromList $ mapM parseBk $ M.toList mp
  where
    parseBk (ref, bk) =
      (,)
        <$> parseJsonDigest ref
        <*> parseJsonB32 bk

instance FromJSON ERISTestVector where
  parseJSON = withObject "ERISTestVector" $ \obj -> do
    tp <-
      obj .: "type" >>= \case
        "positive" ->
          ERISTestPositive
            <$> (obj .: "content" >>= parseJsonB32)
            <*> (obj .: "convergence-secret" >>= parseHashKey)
            <*> obj .: "block-size"
        "negative" -> pure ERISTestNegative
        s -> fail $ "Invalid ERIS test type: " <> T.unpack s
    ERISTestVector
      <$> obj .: "id"
      <*> pure tp
      <*> obj .: "spec-version"
      <*> obj .: "name"
      <*> obj .: "description"
      <*> (obj .: "read-capability" >>= parseCapability)
      <*> obj .: "urn"
      <*> (obj .: "blocks" >>= parseBlocks)

runTests :: FilePath -> IO ()
runTests dir = do
  files <- listDirectory dir
  successes <- newIORef 0
  total <- newIORef 0
  forM_ files $ \file -> when (takeExtension file == ".json") $ do
    let path = dir </> file
    r <- eitherDecode <$> LBS.readFile path
    forM_ r $ \testVector -> do
      putStrLn $
        "> "
          <> showPolarity (testVector ^. erisTestType)
          <> " "
          <> testVector ^. erisTestName
      (nsuccesses, ntotal) <- runOneTest testVector
      modifyIORef successes (+ nsuccesses)
      modifyIORef total (+ ntotal)
  finalSuccess <- readIORef successes
  finalTotal <- readIORef total
  putStrLn $ T.pack (show finalSuccess) <> "/" <> T.pack (show finalTotal)

showPolarity :: ERISTestType -> Text
showPolarity (ERISTestPositive {}) = "[+]"
showPolarity ERISTestNegative = "[-]"

displaySuccess :: Bool -> IO ()
displaySuccess True = putStrLn " [✓]"
displaySuccess False = putStrLn " [╳]"

runOneTest :: ERISTestVector -> IO (Int, Int)
runOneTest vector = case vector ^. erisTestType of
  ERISTestPositive content convSecret blockSize ->
    runTestPositive vector content convSecret blockSize
  ERISTestNegative -> runTestNegative vector

wrapPositiveTest :: Text -> IO Bool -> IO Bool
wrapPositiveTest nm test = do
  putStr nm
  let toRun = do
        r <- test >>= evaluate
        displaySuccess r
        pure r
  catch
    toRun
    (\(e :: SomeException) -> putStrLn (" [╳]: " <> T.pack (show e)) >> pure False)

-- First value is number of success, second is total number of tests
runTestPositive :: ERISTestVector -> ByteString -> ConvergenceSecret -> Int -> IO (Int, Int)
runTestPositive vector content convergenceSecret blockSize = do
  let lcontent = LBS.fromStrict content
  -- Plain encode
  encPlainSuccess <- wrapPositiveTest "Plain encode" $ do
    capPlain <-
      evalStateT
        (erisEncode ERISInMemoryDB lcontent convergenceSecret blockSize)
        (M.empty :: InMemoryDB)
    pure $ capPlain == vector ^. erisTestCapability
  -- Stream encode
  encStreamSuccess <- wrapPositiveTest "Stream encode" $ do
    capStream <-
      evalStateT
        (runConduit $ yield content .| erisEncodeStreaming ERISInMemoryDB convergenceSecret blockSize)
        (M.empty :: InMemoryDB)
    pure $ capStream == vector ^. erisTestCapability
  -- Plain decode
  decPlainSuccess <- wrapPositiveTest "Plain decode" $ do
    lbsPlain <-
      evalStateT
        (erisDecode ERISInMemoryDB $ vector ^. erisTestCapability)
        (vector ^. erisTestBlocks)
    pure $ lbsPlain == lcontent
  -- Stream decode
  decStreamSuccess <- wrapPositiveTest "Stream decode" $ do
    lbsStream <-
      evalStateT
        (runConduit $ erisDecodeStreaming ERISInMemoryDB (vector ^. erisTestCapability) .| sinkLazy)
        (vector ^. erisTestBlocks)
    pure $ lbsStream == lcontent
  -- Conclusion
  let results = [encPlainSuccess, encStreamSuccess, decPlainSuccess, decStreamSuccess]
  pure (sum $ fromEnum <$> results, length results)

wrapNegativeTest :: Text -> IO Bool -> IO Bool
wrapNegativeTest nm test = do
  putStr nm
  let toRun = do
        r <- test >>= evaluate
        displaySuccess r
        pure r
  catch
    toRun
    (\(e :: SomeException) -> putStrLn (" [✓]: " <> T.pack (show e)) >> pure True)

runTestNegative :: ERISTestVector -> IO (Int, Int)
runTestNegative vector = do
  -- Plain decode
  decPlainSuccess <- wrapNegativeTest "Plain decode" $ do
    void $
      evalStateT
        (erisDecode ERISInMemoryDB $ vector ^. erisTestCapability)
        (vector ^. erisTestBlocks)
    pure False
  -- Stream decode
  decStreamSuccess <- wrapNegativeTest "Stream decode" $ do
    void $
      evalStateT
        (runConduit $ erisDecodeStreaming ERISInMemoryDB (vector ^. erisTestCapability) .| sinkLazy)
        (vector ^. erisTestBlocks)
    pure False
  -- Conclusion
  let results = [decPlainSuccess, decStreamSuccess]
  pure (sum $ fromEnum <$> results, length results)
