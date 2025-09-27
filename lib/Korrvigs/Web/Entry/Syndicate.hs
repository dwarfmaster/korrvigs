module Korrvigs.Web.Entry.Syndicate where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
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

data RenderSpec = RenderSpec
  { _renderOnlyNew :: Bool,
    _renderShowSyndicate :: Bool
  }

makeLenses ''RenderSpec

instance Default RenderSpec where
  def = RenderSpec False False

embed :: Int -> Syndicate -> Handler Widget
embed lvl syn = do
  let url = syn ^. synUrl
  itemsWidget <- renderItems [syn] def
  public <- isPublic
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
    ^{itemsWidget}
  |]

renderItems :: [Syndicate] -> RenderSpec -> Handler Widget
renderItems syns spec = do
  let onlyNew = spec ^. renderOnlyNew
  let showTitle = spec ^. renderShowSyndicate
  public <- isPublic
  itemsSQL :: [(Text, Text, Text, Maybe Id, Maybe UTCTime, Int, Bool, Maybe Text, Maybe Text)] <-
    rSelect $ orderBy (descNullsFirst $ view _5) $ do
      (synName, synId, synTitle :: FieldNullable SqlText) <- values $ (\entry -> (sqlId $ entry ^. entryName, sqlInt4 $ entry ^. entryId, toFields $ entry ^. entryTitle)) . view synEntry <$> syns
      item <- selectTable syndicatedItemsTable
      where_ $ item ^. sqlSynItSyndicate .== synId
      task <- fmap joinMField $ optional $ limit 1 $ fromNullableSelect $ do
        task <- selectTable entriesMetadataTable
        entry <- selectTable entriesTable
        where_ $ matchNullable (sqlBool False) (entry ^. sqlEntryName .==) (item ^. sqlSynItInstance)
        where_ $ task ^. sqlKey .== sqlStrictText (mtdtSqlName TaskMtdt)
        where_ $ task ^. sqlEntry .== (entry ^. sqlEntryId)
        pure $ sqlJsonToText $ toNullable $ task ^. sqlValue
      when onlyNew $ do
        where_ $ isNull $ item ^. sqlSynItInstance
        where_ $ item ^. sqlSynItRead .== sqlBool False
      pure (synName, item ^. sqlSynItTitle, item ^. sqlSynItUrl, item ^. sqlSynItInstance, item ^. sqlSynItDate, item ^. sqlSynItSequence, item ^. sqlSynItRead, task, synTitle)
  items <- forM itemsSQL $ \item -> do
    cb <- maybe (pure mempty) (flip checkBoxDWIM $ item ^. _8) $ item ^. _4
    pure $ item & _8 .~ cb
  pure
    [whamlet|
    <ul>
      $forall (synName,title,url,inst,_,sq,read,cb,synTitle) <- items
        <li>
          $if not public
            ^{cb}
            $if read && isNothing inst
              ✓
            #{T.pack " "}
            $if showTitle
              <a href=@{EntryR $ WId $ MkId synName}>
                [
                $maybe title <- synTitle
                  #{title}
                $nothing
                  @#{synName}
                ]
          <a href=#{url}>#{title}
          $if not public
            $maybe i <- inst
              <a href=@{EntryR $ WId i}>
                ^{openIcon}
            $nothing
              <a href=@{SynItemImportR (WId $ MkId synName) sq}>
                ⤓
              $if not read
                <a href=@{SynItemReadR (WId $ MkId synName) sq}>
                  ✓
  |]
  where
    joinMField :: MaybeFields (Field a) -> FieldNullable a
    joinMField mfield = matchMaybe mfield $ \case
      Just f -> toNullable f
      Nothing -> O.null

content :: Syndicate -> Handler Widget
content = embed 0
