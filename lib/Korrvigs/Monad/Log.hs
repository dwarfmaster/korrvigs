module Korrvigs.Monad.Log where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Korrvigs.Log
import Korrvigs.Monad.Class
import Language.Haskell.TH

withLoc :: Name -> Q Exp
withLoc nm = do
  loc <- location
  pure $
    AppE
      (AppE (VarE nm) (LitE $ StringL $ loc_module loc))
      (LitE $ IntegerL $ toInteger $ fst $ loc_start loc)

logDat :: (MonadKorrvigs m) => LogLevel -> Text -> Int -> LogEventData -> m ()
logDat lvl file line dat = do
  parent <- getLogContext
  withSQL $ \conn -> void $ liftIO $ logEvent conn parent file line lvl dat

logDatTrace :: (MonadKorrvigs m) => Text -> Int -> LogEventData -> m ()
logDatTrace = logDat LogTrace

logDatInfo :: (MonadKorrvigs m) => Text -> Int -> LogEventData -> m ()
logDatInfo = logDat LogInfo

logDatWarning :: (MonadKorrvigs m) => Text -> Int -> LogEventData -> m ()
logDatWarning = logDat LogWarning

logDatError :: (MonadKorrvigs m) => Text -> Int -> LogEventData -> m ()
logDatError = logDat LogError

logTrace :: Q Exp
logTrace = withLoc $ mkName "logDatTrace"

log :: Q Exp
log = withLoc $ mkName "logDatInfo"

logWarning :: Q Exp
logWarning = withLoc $ mkName "logDatWarning"

logError :: Q Exp
logError = withLoc $ mkName "logDatError"

withLogContextImpl :: (MonadKorrvigs m) => Text -> Int -> Text -> m a -> m a
withLogContextImpl file line msg act = do
  oldParent <- getLogContext
  parent <- withSQL $ \conn -> liftIO $ logEvent conn oldParent file line LogInfo $ MiscEvent msg
  registerLogContext $ Just parent
  r <- act
  registerLogContext oldParent
  pure r

withLogContext :: Q Exp
withLogContext = withLoc $ mkName "withLogContextImpl"
