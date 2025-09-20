module Korrvigs.Web.Entry.Syndicate where

import Control.Lens
import Data.Text (Text)
import Data.Time.Clock
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Syndicate.SQL
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Korrvigs.Web.Widgets (openIcon)
import Opaleye hiding (not)
import Yesod

embed :: Int -> Syndicate -> Handler Widget
embed lvl syn = do
  let url = syn ^. synUrl
  public <- isPublic
  items :: [(Text, Text, Maybe Id, Maybe UTCTime, Int)] <- rSelect $ orderBy (descNullsFirst $ view _4) $ do
    item <- selectTable syndicatedItemsTable
    where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
    pure (item ^. sqlSynItTitle, item ^. sqlSynItUrl, item ^. sqlSynItInstance, item ^. sqlSynItDate, item ^. sqlSynItSequence)
  pure
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
      $forall (title,url,inst,_,sq) <- items
        <li>
          <a href=#{url}>#{title}
            $if not public
              $maybe i <- inst
                <a href=@{EntryR $ WId i}>
                  ^{openIcon}
              $nothing
                <a href=@{SynItemR (WId $ view (synEntry . entryName) syn) sq}>
                  ⤓
  |]

content :: Syndicate -> Handler Widget
content = embed 0
