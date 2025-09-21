module Korrvigs.Web.Entry.Syndicate where

import Control.Lens
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Syndicate.SQL
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Widgets (checkBoxDWIM, openIcon)
import Opaleye hiding (not)
import qualified Opaleye as O
import Yesod hiding (Field)

embed :: Int -> Syndicate -> Handler Widget
embed lvl syn = do
  let url = syn ^. synUrl
  public <- isPublic
  itemsSQL :: [(Text, Text, Maybe Id, Maybe UTCTime, Int, Maybe Text)] <- rSelect $ orderBy (descNullsFirst $ view _4) $ do
    item <- selectTable syndicatedItemsTable
    where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
    task <- fmap joinMField $ optional $ limit 1 $ fromNullableSelect $ do
      task <- selectTable entriesMetadataTable
      entry <- selectTable entriesTable
      where_ $ matchNullable (sqlBool False) (entry ^. sqlEntryName .==) (item ^. sqlSynItInstance)
      where_ $ task ^. sqlKey .== sqlStrictText (mtdtSqlName TaskMtdt)
      where_ $ task ^. sqlEntry .== (entry ^. sqlEntryId)
      pure $ sqlJsonToText $ toNullable $ task ^. sqlValue
    pure (item ^. sqlSynItTitle, item ^. sqlSynItUrl, item ^. sqlSynItInstance, item ^. sqlSynItDate, item ^. sqlSynItSequence, task)
  items <- forM itemsSQL $ \item -> do
    cb <- maybe (pure mempty) (flip checkBoxDWIM $ item ^. _6) $ item ^. _3
    pure $ item & _6 .~ cb
  pure $ do
    Rcs.checkboxCode
    [whamlet|
    $if lvl == 0
      <p>
        <a href=#{url}>#{url}
        $maybe (entry,code) <- view synFilter syn
          <code>
            |>
            $if public
              #{unId entry}
            $else
              <a href=@{EntryR $ WId $ entry}>
                #{unId entry}
            #
            #{code}
    <ul>
      $forall (title,url,inst,_,sq,cb) <- items
        <li>
          $if not public
            ^{cb}
            #{T.pack " "}
          <a href=#{url}>#{title}
          $if not public
            $maybe i <- inst
              <a href=@{EntryR $ WId i}>
                ^{openIcon}
            $nothing
              <a href=@{SynItemR (WId $ view (synEntry . entryName) syn) sq}>
                ⤓
  |]
  where
    joinMField :: MaybeFields (Field a) -> FieldNullable a
    joinMField mfield = matchMaybe mfield $ \case
      Just f -> toNullable f
      Nothing -> O.null

content :: Syndicate -> Handler Widget
content = embed 0
