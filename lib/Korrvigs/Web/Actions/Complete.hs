module Korrvigs.Web.Actions.Complete where

import Control.Lens
import Control.Monad
import Data.Default
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Monad.Metadata
import Korrvigs.Note
import qualified Korrvigs.Note.Sync as Note
import Korrvigs.Query
import Korrvigs.Utils
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

completeTarget :: ActionTarget -> ActionCond
completeTarget (TargetEntry _) =
  ActCondQuery $
    def
      & queryKind ?~ KindQueryNote NoteQuery
      & queryMtdt .~ [(mtdtSqlName TaskMtdt, AnyQuery)]
completeTarget _ = ActCondNever

completeForm :: AForm Handler Bool
completeForm = areq checkBoxField "Abandon" Nothing

completeTitle :: ActionTarget -> Text
completeTitle = const "Complete"

runComplete :: Bool -> ActionTarget -> Handler ActionReaction
runComplete dont (TargetEntry entry) = do
  render <- getUrlRender
  forM_ (entry ^? _Note) $ \note ->
    Note.updateImpl note $ pure . (docContent %~ filter (not . shouldDelete))
  updateMetadata entry (M.singleton (mtdtSqlName TaskMtdt) (toJSON ntask)) mtdtToRm
  fromMaybeT () $ do
    syn <- hoistLift $ rSelectTextMtdt SyndicateMtdt $ sqlId i
    entrySyn <- hoistLift $ load $ MkId syn
    lift $ updateMetadata entrySyn M.empty [mtdtSqlName AutoRun]
  pure $ def & reactRedirect ?~ render (EntryR $ WId i)
  where
    i = entry ^. entryName
    ntask = renderTaskStatus $ if dont then TaskDont else TaskDone
    mtdtToRm :: [Text]
    mtdtToRm =
      [ mtdtSqlName AggregateMethod,
        mtdtSqlName AggregateCount,
        mtdtSqlName FirstUnread,
        mtdtSqlName LastRead
      ]
    shouldDelete :: Block -> Bool
    shouldDelete (Para inls) = any shouldDelInline inls
    shouldDelete _ = False
    shouldDelInline :: Inline -> Bool
    shouldDelInline (MtdtLink _ mtdt) = mtdt `elem` mtdtToRm
    shouldDelInline _ = False
runComplete _ _ = pure def
