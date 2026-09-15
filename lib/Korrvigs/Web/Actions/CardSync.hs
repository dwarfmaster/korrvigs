module Korrvigs.Web.Actions.CardSync
  ( cardPullTarget,
    cardPullForm,
    cardPullTitle,
    runCardPull,
  )
where

import Control.Lens
import Control.Monad
import Data.Default
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Korrvigs.AddressBook.DAV as DAV
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Opaleye
import Yesod

cardPullTarget :: ActionTarget -> ActionCond
cardPullTarget TargetHome = ActCondAlways
cardPullTarget (TargetEntry entry) | isJust (entry ^? _AddressBook) = ActCondAlways
cardPullTarget _ = ActCondNever

cardPullForm :: AForm Handler Text
cardPullForm = areq passwordField "dav-password" Nothing

cardPullTitle :: ActionTarget -> Text
cardPullTitle = const "Pull contacts"

mkMsg :: Bool -> Text -> (Route WebData -> [(Text, Text)] -> Text) -> Html
mkMsg r msg =
  [hamlet|
  <p> #{status} 
  <pre>
    <code>
      #{msg}
|]
  where
    status :: Text
    status = if r then "Success" else "Failure"

withLogging :: (MonadIO m) => ((Text -> m ()) -> m a) -> m (a, Text)
withLogging act = do
  logging <- liftIO $ newIORef ""
  r <- act $ \txt -> liftIO $ modifyIORef logging (<> "\n" <> txt)
  msg <- liftIO $ readIORef logging
  pure (r, msg)

runCardPull :: Text -> ActionTarget -> Handler ActionReaction
runCardPull pwd TargetHome = do
  (rs, msg) <- withLogging $ \report -> do
    abooks <- rSelect $ do
      entry <- selectTable entriesTable
      where_ $ entry ^. sqlEntryKind .== sqlKind AddressBook
      pure (entry ^. sqlEntryName, entry ^. sqlEntryTitle, entry ^. sqlEntryId)
    forM abooks $ \(i, title, sqlI :: Int) -> do
      report $ "> Pulling from " <> fromMaybe ("@" <> unId i) title
      entry <- load sqlI
      let mabook = entry >>= (^? _AddressBook)
      case mabook of
        Nothing -> do
          report "Failed to load addressbook"
          pure False
        Just abook -> DAV.pullAddressBook report abook pwd
  render <- getUrlRenderParams
  pure $ def & reactMsg ?~ mkMsg (all id rs) msg render
runCardPull pwd (TargetEntry entry) = do
  abook <- maybe notFound pure (entry ^? _AddressBook)
  (r, msg) <- withLogging $ \report -> DAV.pullAddressBook report abook pwd
  render <- getUrlRenderParams
  pure $ def & reactMsg ?~ mkMsg r msg render
runCardPull _ _ = pure def
