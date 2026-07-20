module Korrvigs.Web.Entry.Syndicate where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Syndicate.SQL
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Opaleye hiding (not)
import Yesod hiding (Field)

data RenderSpec = RenderSpec
  { _renderOnlyNew :: Bool,
    _renderShowSyndicate :: Bool,
    _renderLimitPerSyn :: Maybe Int
  }

makeLenses ''RenderSpec

instance Default RenderSpec where
  def = RenderSpec False False Nothing

embed :: Int -> Syndicate -> Handler Widget
embed lvl syn = do
  let murl = syn ^. synUrl
  itemsWidget <- renderItems [(syn ^. synEntry . entryName, syn ^. synEntry . entryTitle, syn)] def
  public <- isPublic
  pure $ do
    Rcs.checkboxCode StaticR
    [whamlet|
    $if lvl == 0
      <p>
        $maybe url <- murl
          <a href=#{url}>#{url}
        $forall (entry,code) <- view synFilters syn
          <code>
            #{T.pack "|> "}
            $if public
              #{unId entry}
            $else
              <a href=@{EntryR $ WId $ entry}>
                #{unId entry}
            #{T.pack "#"}
            #{code}
    ^{itemsWidget}
  |]

renderItems :: [(Id, Maybe Text, Syndicate)] -> RenderSpec -> Handler Widget
renderItems syns spec = do
  let onlyNew = spec ^. renderOnlyNew
  let showTitle = spec ^. renderShowSyndicate
  public <- isPublic
  let ordByDate = descNullsFirst $ view _5
  let ordBySeq = desc $ view _6
  itemsSQL :: [(Text, Text, Text, Maybe UTCTime, Int, Bool, Id, Maybe Text)] <-
    rSelect $ orderBy (ordByDate <> ordBySeq) $ do
      let mkVal (i, title, syn) =
            let e = syn ^. synEntry
             in ( sqlId $ e ^. entryName,
                  sqlInt4 $ e ^. entryId,
                  toFields $ title,
                  sqlId i
                )
      (synName, synId, synTitle :: FieldNullable SqlText, synParentId) <- values $ mkVal <$> syns
      let ordItByDate = descNullsFirst $ view sqlSynItDate
      let ordItBySeq = desc $ view sqlSynItSequence
      let ordIt = orderBy $ ordItByDate <> ordItBySeq
      item <- maybe id (\l -> limit l . ordIt) (spec ^. renderLimitPerSyn) $ do
        item <- selectTable syndicatedItemsTable
        where_ $ item ^. sqlSynItSyndicate .== synId
        pure item
      when onlyNew $ where_ $ item ^. sqlSynItRead .== sqlBool False
      pure (synName, item ^. sqlSynItTitle, item ^. sqlSynItUrl, item ^. sqlSynItDate, item ^. sqlSynItSequence, item ^. sqlSynItRead, synParentId, synTitle)
  items <- forM itemsSQL $ \item -> do
    liId <- if onlyNew then Just <$> newIdent else pure Nothing
    spanId :: Text <- newIdent
    pure $ item & _4 .~ (liId, spanId)
  let optId someId = [("id" :: Text, i) | i <- toList someId]
  render <- getUrlRender
  let mkRead nm sq liId spanId = "readItem(this,\"" <> render (SynItemReadR (WId $ MkId nm) sq) <> "\"," <> maybe "null" (\t -> "\"" <> t <> "\"") liId <> ", \"" <> spanId <> "\")"
  pure $ do
    Rcs.itemCode StaticR
    [whamlet|
    <div .syndicate>
      <ul>
        $forall (synName,title,url,(liId,spanId),sq,read,synParentId,synTitle) <- items
          <li *{optId liId}>
            $if not public
              <span ##{spanId}>
                $if read
                  ✓
              $if not read
                <span .item-read onclick=#{mkRead synName sq liId spanId}>
                  ✓
              #{T.pack " "}
            <a href=#{url}>#{title}
            $if not public
              $if showTitle
                <a .syndicate-ref href=@{EntryR $ WId synParentId}>
                  [
                  $maybe title <- synTitle
                    #{title}
                  $nothing
                    @#{unId synParentId}
                  ]
  |]

content :: Syndicate -> Handler Widget
content = embed 0
