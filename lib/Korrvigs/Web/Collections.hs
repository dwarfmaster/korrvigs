module Korrvigs.Web.Collections where

import Control.Lens
import Control.Monad
import Data.Foldable
import Data.List (elemIndex, sortBy)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Compute.Action
import Korrvigs.Entry
import Korrvigs.Metadata.Collections
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Note.Loc hiding (subs)
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Note as Note
import Korrvigs.Web.PhotoSwipe (PhotoswipeEntry (..), swpMiniature, swpRedirect, swpUrl)
import qualified Korrvigs.Web.PhotoSwipe as PhotoSwipe
import Korrvigs.Web.Public.Crypto (mkPublic)
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Widget
import qualified Korrvigs.Web.Widgets as Widgets
import Opaleye hiding (not, null)
import Text.Blaze (textValue, toMarkup, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Yesod

selectDisplay :: [Text] -> Maybe Text -> Handler Widget
selectDisplay prefix disp = do
  i <- newIdent
  pure $ do
    toWidget
      [julius|
      document.getElementById(#{i}).addEventListener("change", (event) => {
        const url = `@{ColR prefix}?display=${event.target.value}`
        window.location.replace(url)
      })
    |]
    [whamlet|
      <form>
        <select ##{i}>
          <option value="misc" *{mkSel "misc"}>Collection
          <option value="gallery" *{mkSel "gallery"}>Gallery
          <option value="todo" *{mkSel "todo"}>Todo
    |]
  where
    sel = case disp of
      Just "gallery" -> "gallery"
      Just "todo" -> "todo"
      _ -> "misc"
    mkSel :: Text -> [(Text, Text)]
    mkSel v = [("selected", "") | sel == v]

getColR :: [Text] -> Handler Html
getColR prefix = do
  pub <- isPublic
  toDisp <- lookupGetParam "display"
  disp <- case toDisp of
    Just "gallery" -> displayGal prefix
    Just "todo" -> displayTask prefix
    _ -> displayMisc prefix
  sel <- selectDisplay prefix toDisp
  actions <- actionsWidget $ TargetCollection prefix
  defaultLayout $ do
    setTitle $ toMarkup title
    Rcs.entryStyle
    Widgets.sectionLogic
    unless pub $ do
      Rcs.formsStyle
      sel
      actions
    disp
  where
    rprefix = reverse prefix
    title = case rprefix of
      [] -> "Collections"
      (lp : _) -> lp

displayTree :: Maybe Text -> Int -> Int -> Html -> [Text] -> ColTree -> Handler Widget
displayTree = displayTreeImpl mkLeafs
  where
    mkLeafs :: Int -> [(Id, Maybe Text)] -> Handler Widget
    mkLeafs _ ents = do
      pents <- forM ents $ \(i, title) -> (i,title,) <$> mkPublic (EntryR $ WId i)
      pure
        [whamlet|
        <ul>
          $forall (i,title,url) <- pents
            <li>
              <a href=@{url}>
                $maybe t <- title
                  #{t}
                $nothing
                  @#{unId i}
      |]

symbols :: [Text] -> Widget
symbols rprefix = case prefix of
  "Favourite" : _ -> Widget.headerSymbol "★"
  "Reading list" : _ -> Widget.headerSymbol "✓"
  _ -> Widget.headerSymbol "•"
  where
    prefix = reverse rprefix

displayTreeImpl ::
  (Int -> [(Id, Maybe Text)] -> Handler Widget) ->
  Maybe Text ->
  Int ->
  Int ->
  Html ->
  [Text] ->
  ColTree ->
  Handler Widget
displayTreeImpl mkLeafs disp lvl threshold cat rprefix favs = do
  let entries = favs ^. colEntries
  subs <-
    isPublic >>= \case
      True -> pure []
      False -> forM (M.toList $ favs ^. colSubs) $ \(subHd, sb) -> do
        render <- getUrlRenderParams
        let url = ColR $ reverse $ subHd : rprefix
        let subH = Html.a (toMarkup subHd) ! Attr.href (textValue $ render url [("display", d) | d <- toList disp])
        displayTreeImpl mkLeafs disp (lvl + 1) threshold subH (subHd : rprefix) sb
  leafs <- mkLeafs lvl entries
  let content =
        [whamlet|
    ^{leafs}
    $forall sb <- subs
      ^{sb}
  |]
  pure $ void $ Widgets.mkSection lvl [("class", "collapsed") | lvl > threshold] [] (header lvl) content
  where
    header :: Int -> Widget
    header 0 = [whamlet|<h1> ^{symbols rprefix} ^{cat}|]
    header 1 = [whamlet|<h2> ^{symbols rprefix} ^{cat}|]
    header 2 = [whamlet|<h3> ^{symbols rprefix} ^{cat}|]
    header 3 = [whamlet|<h4> ^{symbols rprefix} ^{cat}|]
    header 4 = [whamlet|<h5> ^{symbols rprefix} ^{cat}|]
    header _ = [whamlet|<h6> ^{symbols rprefix} ^{cat}|]

mkTitle :: ([Text] -> Text) -> Text -> [Text] -> Handler Html
mkTitle renderUrl base rprefix = do
  public <- isPublic
  pure $ case rprefix of
    [] -> toMarkup base
    (lp : prs) -> buildTitle public prs <> toMarkup lp
  where
    buildTitle :: Bool -> [Text] -> Html
    buildTitle _ [] = mempty
    buildTitle public (hd : rpr) =
      let rec = buildTitle public rpr
       in let lnk = Html.a (toMarkup hd) ! Attr.href (textValue $ renderUrl $ reverse $ hd : rpr)
           in rec <> (if public then toMarkup hd else lnk) <> " > "

displayMisc :: [Text] -> Handler Widget
displayMisc prefix = do
  render <- getUrlRender
  titleH <- mkTitle (render . ColR) "Collections" rprefix
  displayTree Nothing 0 0 titleH rprefix =<< colTree MiscCollection prefix True
  where
    rprefix = reverse prefix

displayGal :: [Text] -> Handler Widget
displayGal prefix = do
  render <- getUrlRenderParams
  titleH <- mkTitle (flip render [("display", "gallery")] . ColR) "Collections" rprefix
  subTree <- colCatTree MiscCollection prefix
  subs <- displayTree (Just "gallery") 1 0 "Sub galleries" rprefix subTree
  pictures <- rSelect $ orderBy (ascNullsFirst (view _2) <> asc (view _1)) $ do
    (i, _) <- selectCol MiscCollection prefix False
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== i
    void $ selComp i "miniature"
    size <- selComp i "size"
    pure (i, entry ^. sqlEntryDate, size ^. sqlCompAction)
  entries <- mapM mkEntry pictures
  photoswipe <- PhotoSwipe.photoswipe $ catMaybes entries
  pure $ do
    PhotoSwipe.photoswipeHeader
    [whamlet|
      <h1>📷 ^{titleH}
      $if not (nullTree subTree)
        ^{subs}
      <div #common-gallery>
        ^{photoswipe}
    |]
  where
    rprefix = reverse prefix
    mkEntry :: (Id, Maybe ZonedTime, Action) -> Handler (Maybe PhotoswipeEntry)
    mkEntry (i, dt, sizeA) =
      PhotoSwipe.miniatureEntry (localDay . zonedTimeToLocalTime <$> dt) i sizeA >>= \case
        Nothing -> pure Nothing
        Just entry -> do
          url <- mkPublic $ entry ^. swpUrl
          mini <- mkPublic $ entry ^. swpMiniature
          public <- isPublic
          let redir = if public then url else entry ^. swpRedirect
          pure . Just $ entry & swpUrl .~ url & swpMiniature .~ mini & swpRedirect .~ redir

displayTask :: [Text] -> Handler Widget
displayTask prefix = do
  tree <- colTree MiscCollection prefix True
  render <- getUrlRender
  titleH <- mkTitle (render . ColR) "Collection" rprefix
  widget <- displayTreeImpl mkLeafs (Just "todo") 0 0 titleH prefix tree
  pure $ do
    Rcs.checkboxCode
    widget
  where
    rprefix = reverse prefix
    mkLeafs :: Int -> [(Id, Maybe Text)] -> Handler Widget
    mkLeafs tlvl entries = do
      let lvl = tlvl + 1
      widgets <- forM entries $ \(i, titleText) -> do
        (widget, _) <- Note.embedBody i lvl
        tsk <- loadTask i
        taskW <- Widgets.taskWidget i (SubLoc []) tsk
        let titleT = fromMaybe (unId i) titleText
        let titleW = [whamlet|<a href=@{EntryR $ WId i}>#{titleT}|]
        let t = header lvl [whamlet|^{Widgets.headerSymbol "📖"} ^{taskW} ^{titleW}|]
        let w = void $ Widgets.mkSection lvl [("class", "collapsed")] [] t widget
        pure (view tskStatus <$> tsk, w)
      let ordered = sortBy (ordTaskStatus fst) widgets
      pure $ mconcat $ snd <$> ordered
    header :: Int -> Widget -> Widget
    header 0 content = [whamlet|<h1> ^{content}|]
    header 1 content = [whamlet|<h2> ^{content}|]
    header 2 content = [whamlet|<h3> ^{content}|]
    header 3 content = [whamlet|<h4> ^{content}|]
    header 4 content = [whamlet|<h5> ^{content}|]
    header _ content = [whamlet|<h6> ^{content}|]
    ordTaskStatus :: (a -> Maybe TaskStatus) -> a -> a -> Ordering
    ordTaskStatus f t1 t2 =
      let ordered = Nothing : (Just <$> [TaskOngoing, TaskImportant, TaskBlocked, TaskTodo, TaskDone, TaskDont])
       in let i1 = elemIndex (f t1) ordered
           in let i2 = elemIndex (f t2) ordered
               in compare i1 i2
