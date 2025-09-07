module Korrvigs.Web.Note.Col (getNoteColEditR, postNoteColEditR) where

import Control.Lens
import Control.Monad
import Data.ByteString.Lazy (writeFile)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Note
import Korrvigs.Note.AST
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Search.Form
import Yesod
import Prelude hiding (writeFile)

getNoteColEditR :: WebId -> Text -> Handler Html
getNoteColEditR (WId i) col = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? entryKindData . _NoteD
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load note " <> T.pack (note ^. notePath) <> ": " <> err)
  (display, _, items) <- maybe notFound pure $ md ^? docContent . each . bkCollection col
  let numbered :: [(Int, CollectionItem)] = zip [1 ..] items
  widgets <- forM numbered $ \(nb, item) -> case item of
    ColItemEntry it -> do
      itEntry <- load it >>= throwMaybe (KCantLoad it "When importing from collection")
      pure
        [whamlet|
        <li>
          <input type=checkbox name=#{"item-" <> show nb}>
          <a href=@{EntryR $ WId it}>
            #{fromMaybe ("#" <> unId it) $ view entryTitle itEntry}
      |]
    ColItemInclude it included -> do
      itEntry <- load it >>= throwMaybe (KCantLoad it "When importing from collection")
      vals <- loadCollectionItem ColList $ ColItemInclude it included
      pure
        [whamlet|
        <li>
          <p>
            Importing from
            <a href=@{EntryR $ WId it}>
              #{fromMaybe ("#" <> unId (view entryName itEntry)) (view entryTitle itEntry)}
            ##{included}:
          <ul>
            $forall (row,_) <- vals
              <li>
                <a href=@{EntryR $ WId $ view sqlEntryName row}>
                  #{fromMaybe ("#" <> unId (view sqlEntryName row)) (view sqlEntryTitle row)}
      |]
    ColItemQuery q -> do
      vals <- loadCollectionItem ColList $ ColItemQuery q
      render <- getUrlRenderParams
      let params = getParameters Nothing q display
      pure
        [whamlet|
        <li>
          <p>
            <a href=#{render SearchR params}>
              Query:
          <ul>
            $forall (row,_) <- vals
              <li>
                <a href=@{EntryR $ WId $ view sqlEntryName row}>
                  #{fromMaybe ("#" <> unId (view sqlEntryName row)) (view sqlEntryTitle row)}
      |]
    ColItemSubOf it -> do
      itEntry <- load it >>= throwMaybe (KCantLoad it "When importing from collection")
      vals <- loadCollectionItem ColList $ ColItemSubOf it
      pure
        [whamlet|
        <li>
          <p>
            Subs of
            <a href=@{EntryR $ WId it}>
              #{fromMaybe ("#" <> unId (view entryName itEntry)) (view entryTitle itEntry)}
            :
          <ul>
            $forall (row,_) <- vals
              <li>
                <a href=@{EntryR $ WId $ view sqlEntryName row}>
                  #{fromMaybe ("#" <> unId (view sqlEntryName row)) (view sqlEntryTitle row)}
      |]
    ColItemComment _ -> pure mempty
  defaultLayout $ do
    Rcs.formsStyle
    [whamlet|
      <form action=@{NoteColEditR (WId i) col} method=POST>
        <ul>
          $forall w <- widgets
            ^{w}
        <input type=submit value="Remove selected">
    |]

postNoteColEditR :: WebId -> Text -> Handler ()
postNoteColEditR (WId i) col = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? entryKindData . _NoteD
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load note " <> T.pack (note ^. notePath) <> ": " <> err)
  items <- maybe notFound pure $ md ^? docContent . each . bkCollection col . _3
  let numbered :: [(Int, CollectionItem)] = zip [1 ..] items
  nitems <- fmap catMaybes $ forM numbered $ \(nb, item) -> do
    let param = T.pack $ "item-" <> show nb
    paramV <- lookupPostParam param
    pure $ if paramV == Just "on" then Nothing else Just item
  let nmd = md & docContent . each . bkCollection col . _3 .~ nitems
  liftIO $ writeFile (note ^. notePath) $ writeNoteLazy nmd
  redirect $ NoteColR (WId i) col
