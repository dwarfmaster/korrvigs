module Korrvigs.Cli.ERIS where

import Control.Lens hiding (argument, ignored)
import Control.Monad.IO.Class
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Base32
import Data.ERIS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Korrvigs.Cli.Monad
import Korrvigs.Monad
import Options.Applicative

data Cmd
  = Encode FilePath (Maybe FilePath) Bool
  | Decode FilePath Text
  | Inspect Text

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "encode"
      ( info
          ( ( Encode
                <$> argument str (metavar "DB" <> help "ERIS plain database path")
                <*> optional (argument str $ metavar "FILE" <> help "File to encode")
                <*> switch (long "small-size" <> short 's' <> help "Use small block size")
            )
              <**> helper
          )
          (progDesc "Encode file to ERIS database" <> header "korr eris encode -- Encode files to ERIS")
      )
      <> command
        "decode"
        ( info
            ( ( Decode
                  <$> argument str (metavar "DB" <> help "ERIS plain database path")
                  <*> argument str (metavar "CAP" <> help "ERIS read capability")
              )
                <**> helper
            )
            (progDesc "Decode file from ERIS capability" <> header "korr eris decode -- Decode file from ERIS")
        )
      <> command
        "inspect"
        ( info
            ( (Inspect <$> argument str (metavar "CAP" <> help "ERIS capability to inspect"))
                <**> helper
            )
            (progDesc "Inspect capability" <> header "korr eris inspect -- Inspect capability")
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with ERIS encoded files"
      <> header "korr eris -- Deal with ERIS encoded files"

run :: Cmd -> KorrM ()
run (Encode rt file isSmall) = do
  content <- liftIO $ maybe BS.getContents BS.readFile file
  let blockSize = if isSmall then erisSmallBlockSize else erisBlockSize
  let db = ERISFileDB rt
  conv <- throwMaybe (KMiscError "Invalid convergent secret") $ mkErisHashKey $ BS.replicate 32 0x00
  cap <- erisEncode db content conv blockSize
  liftIO $ TIO.putStrLn $ erisEncodeCapabilityToText cap
run (Decode rt cap) = case erisDecodeCapabilityFromText cap of
  Nothing -> liftIO $ putStrLn "Could not recognise capability"
  Just erisCap -> do
    bs <- erisDecode (ERISFileDB rt) erisCap
    liftIO $ BS.putStr bs
run (Inspect cap) = case erisDecodeCapabilityFromText cap of
  Nothing -> liftIO $ putStrLn "Could not recognise capability"
  Just erisCap -> liftIO $ do
    putStrLn $ "Level: " <> show (erisCap ^. erisCapLevel)
    putStrLn $ "Block size: " <> show (erisCap ^. erisCapBlockSize)
    putStrLn $ "Reference: " <> T.unpack (encodeBase32Unpadded $ BS.pack $ BA.unpack $ erisCap ^. erisCapRootRef)
    putStrLn $ "Key: " <> T.unpack (encodeBase32Unpadded $ BS.pack $ BA.unpack $ erisCap ^. erisCapRootKey)
