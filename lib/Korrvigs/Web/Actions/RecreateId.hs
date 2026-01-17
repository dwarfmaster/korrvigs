module Korrvigs.Web.Actions.RecreateId where

import Control.Lens
import Control.Monad.Extra
import Data.Default
import Data.Text (Text)
import qualified Korrvigs.Calendar.New as Cal
import Korrvigs.Entry
import qualified Korrvigs.Event.New as Ev
import qualified Korrvigs.File.New as File
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Monad.Metadata (updateRefs)
import qualified Korrvigs.Note.New as Note
import qualified Korrvigs.Syndicate.New as Syn
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Opaleye
import Yesod hiding (Update)

recreateIdTarget :: ActionTarget -> ActionCond
recreateIdTarget (TargetEntry _) = ActCondAlways
recreateIdTarget _ = ActCondNever

recreateIdForm :: AForm Handler Bool
recreateIdForm = areq checkBoxField "Recreate ?" (Just False)

recreateIdTitle :: ActionTarget -> Text
recreateIdTitle = const "Recreate ID"

runRecreateId :: Bool -> ActionTarget -> Handler ActionReaction
runRecreateId False _ = pure $ def & reactMsg ?~ [shamlet|<p>Check the box to recreate ID|]
runRecreateId True (TargetEntry entry) = do
  let sqlI = sqlInt4 $ entry ^. entryId
  parents <- rSelect $ nameFor =<< selectTargetsFor entriesSubTable sqlI
  language <- rSelectTextMtdt Language $ sqlId $ entry ^. entryName
  let idMaker =
        imk prefix
          & idTitle .~ entry ^. entryTitle
          & idParent .~ mkParent parents
          & idDate .~ entry ^. entryDate
          & idLanguage .~ language
  ni <- newId idMaker
  when (ni /= entry ^. entryName) $ do
    updateRefs entry $ Just ni
    moveFile ni
    void $ atomicSQL $ \conn ->
      runUpdate conn $
        Update
          { uTable = entriesTable,
            uUpdateWith = updateEasy $ sqlEntryName .~ sqlId ni,
            uWhere = \e -> e ^. sqlEntryId .== sqlI,
            uReturning = rCount
          }
  render <- getUrlRender
  pure $
    def
      & reactMsg ?~ [shamlet|<p>Recreated id: #{unId ni}|]
      & reactRedirect ?~ render (EntryR $ WId ni)
  where
    mkParent [i] = Just i
    mkParent _ = Nothing
    prefix = choosePrefix $ case entry ^. entryKindData of
      NoteD _ -> PrefixNote
      FileD file -> PrefixFile $ file ^. fileMime
      EventD _ -> PrefixEvent
      CalendarD _ -> PrefixCalendar
      SyndicateD _ -> PrefixSyndicate
    moveFile :: Id -> Handler ()
    moveFile ni = case entry ^. entryKindData of
      FileD file -> File.moveFile file ni
      NoteD note -> Note.moveFile note ni
      EventD ev -> Ev.moveFile ev ni
      CalendarD cal -> Cal.moveFile cal ni
      SyndicateD syn -> Syn.moveFile syn ni
runRecreateId True _ = pure def
