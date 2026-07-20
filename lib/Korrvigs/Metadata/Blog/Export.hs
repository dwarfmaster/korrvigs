module Korrvigs.Metadata.Blog.Export where

import Control.Lens hiding (pre)
import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Default as D
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Blog.Citations (prepareCitations)
import Korrvigs.Metadata.Blog.Math
import Korrvigs.Metadata.Blog.Mtdt
import Korrvigs.Metadata.Blog.Structure
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Note hiding (code, sub)
import Korrvigs.Utils
import Korrvigs.Utils.JSON
import qualified Korrvigs.Web.Widgets as Wdgs
import Opaleye
import qualified Opaleye as O
import Text.Blaze
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import Prelude hiding (div, span)

data RenderContext m = RenderContext
  { _rdrDoc :: Document,
    _rdrRenderUrl :: BlogUrl -> m Text,
    _rdrHdOffset :: Int,
    _rdrCurLevel :: Int,
    _rdrTopEntries :: Id -> m (Maybe BlogUrl),
    _rdrCitations :: Map Id Html,
    _rdrOnlyPublished :: Bool
  }

data RenderState = RenderState
  { _rdrstCount :: Int,
    _rdrstFootNotes :: Map Int [Block]
  }

instance D.Default RenderState where
  def = RenderState 1 M.empty

data BlogPageContent m = BlogPageContent
  { _blogPageContent :: Html,
    _blogPageMetadata :: Map Text Text,
    _blogPageTitle :: Text,
    _blogPageRenderUrl :: BlogUrl -> m Text,
    _blogPageMenu :: BlogMenuContent,
    _blogPageUrl :: BlogUrl
  }

type RenderMonad m = RWST (RenderContext m) () RenderState m

makeLenses ''RenderContext
makeLenses ''RenderState
makeLenses ''BlogPageContent

shiftOffset :: Int -> RenderMonad m a -> RenderMonad m a
shiftOffset noffset = withRWST $ \r st -> (r & rdrHdOffset .~ noffset, st)

renderPost :: (MonadKorrvigs m) => BlogUrl -> BlogMenuContent -> Maybe (Id, Text) -> (BlogUrl -> m Text) -> (Id -> m (Maybe BlogUrl)) -> Map Text Text -> Bool -> Id -> m Html
renderPost url menuContent csl renderUrl topEntries mtdt onlyPublished noteId = do
  entry <- load noteId >>= throwMaybe (KCantLoad noteId "Failed to load entry for blog")
  note <- throwMaybe (KMiscError "Blog post is not a note") $ entry ^? _Note
  doc <- readNote (note ^. notePath) >>= throwEither (\e -> KMiscError $ "Failed to load note for blog post: " <> e)
  let t =
        fromMaybe (doc ^. docTitle) $
          M.lookup (mtdtName BlogTitle) (doc ^. docMtdt) >>= fromJSONM
  (citations, bibliography) <- maybe (pure mempty) (flip renderCitations doc) csl
  contentHtml <- renderDocument renderUrl topEntries citations onlyPublished t doc
  renderPageContent $ BlogPageContent (contentHtml <> bibliography) mtdt t renderUrl menuContent url

renderPageContent :: (MonadKorrvigs m) => BlogPageContent m -> m Html
renderPageContent pc = do
  stl <- pc ^. blogPageRenderUrl $ BlogTopLevel "style.css"
  sideMenu <- renderSideBar (pc ^. blogPageUrl) (pc ^. blogPageMenu) $ pc ^. blogPageRenderUrl
  feed <- pc ^. blogPageRenderUrl $ BlogAtom
  let content = div (pc ^. blogPageContent) ! A.class_ "content-div"
  let leftDiv = div mempty ! A.class_ "left-div"
  pure $
    docTypeHtml $
      html $
        renderHead (pc ^. blogPageMetadata) (pc ^. blogPageTitle) stl feed
          <> (body $ div (leftDiv <> content <> sideMenu) ! A.class_ "main-div")

renderCitations :: (MonadKorrvigs m) => (Id, Text) -> Document -> m (Map Id Html, Html)
renderCitations csl doc = do
  citations <- prepareCitations csl doc
  let ids = view _1 <$> citations
  let citationsList = flip foldMap citations $ \(_, ref, content) ->
        li $
          span content ! A.class_ "citation-content" ! A.id (toValue $ "ref-" <> ref)
  let citationsHtml = do
        h2 "Citations"
        ul citationsList ! A.class_ "citations"
  pure (ids, if M.null ids then mempty else citationsHtml)

renderHead :: Map Text Text -> Text -> Text -> Text -> Html
renderHead mtdt t stl feed =
  mconcat
    [ meta ! A.charset "UTF-8",
      title (toMarkup t),
      link ! A.rel "stylesheet" ! A.href (toValue stl),
      link ! A.rel "alternate" ! A.type_ "application/atom+xml" ! A.href (toValue feed),
      renderMeta mtdt,
      Wdgs.skyStyle
    ]

renderRssIcon :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> Maybe Text -> m Html
renderRssIcon renderUrl tag = do
  url <- renderUrl $ maybe BlogAtom BlogAtomTag tag
  pure $ a (img ! A.src "https://upload.wikimedia.org/wikipedia/commons/4/46/Generic_Feed-icon.svg" ! A.class_ "icon") ! A.href (toValue url)

renderSideBar :: (MonadKorrvigs m) => BlogUrl -> BlogMenuContent -> (BlogUrl -> m Text) -> m Html
renderSideBar current menuContent renderUrl = do
  let titleH = h1 $ toMarkup $ menuContent ^. blogMenuTitle
  contentH <- forM (menuContent ^. blogMenuItems) $ \(itemName, itemLink) -> do
    url <- renderUrl $ BlogTopLevel itemLink
    let setCurrent = mkCurrent (== BlogTopLevel itemLink)
    pure $ setCurrent $ p $ a (toMarkup itemName) ! A.href (toValue url)
  archive <- renderUrl BlogArchive
  icon <- renderRssIcon renderUrl Nothing
  let archiveH = p $ mkCurrent matchArchive $ a "Posts" ! A.href (toValue archive) <> " " <> icon
  tagsH <- forM (menuContent ^. blogMenuTags) $ \(tag, tagCount) -> do
    url <- renderUrl $ BlogArchiveTag tag
    rssIcon <- renderRssIcon renderUrl (Just tag)
    let archiveTagH = p $ mkCurrent (== BlogArchiveTag tag) $ a (toMarkup tag) ! A.href (toValue url) <> "(" <> toMarkup tagCount <> ") " <> rssIcon
    pure $ archiveTagH ! A.class_ "archive-tag"
  pure $ div (titleH <> mconcat contentH <> archiveH <> mconcat tagsH) ! A.class_ "menu-div"
  where
    mkCurrent matchUrl = if matchUrl current then (! A.class_ "current-page") else id
    matchArchive BlogArchive = True
    matchArchive (BlogArchiveTag _) = True
    matchArchive _ = False

renderMeta :: Map Text Text -> Html
renderMeta mtdt = mconcat $ render <$> M.toList mtdt
  where
    render (nm, cnt) = meta ! A.name (toValue nm) ! A.content (toValue cnt)

renderDocument :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> (Id -> m (Maybe BlogUrl)) -> Map Id Html -> Bool -> Text -> Document -> m Html
renderDocument renderUrl topEntries citations onlyPublished usedTitle doc = do
  let ctx =
        RenderContext
          { _rdrDoc = doc,
            _rdrRenderUrl = renderUrl,
            _rdrHdOffset = 1,
            _rdrCurLevel = 1,
            _rdrTopEntries = topEntries,
            _rdrCitations = citations,
            _rdrOnlyPublished = onlyPublished
          }
  let date = renderDate doc
  let t = h1 $ toMarkup usedTitle
  tags <- renderTags renderUrl doc
  (content, ()) <- evalRWST (renderBlocksWithFootnotes $ doc ^. docContent) ctx D.def
  pure $ date <> t <> tags <> content

renderDate :: Document -> Html
renderDate doc =
  let docDay :: Maybe Day =
        M.lookup "date" (doc ^. docMtdt)
          >>= fromJSONM
          >>= (pure . localDay . zonedTimeToLocalTime)
   in case docDay of
        Nothing -> mempty
        Just day -> p (toMarkup $ formatTime defaultTimeLocale "%F" day) ! A.class_ "pubdate"

renderTags :: forall m. (MonadKorrvigs m) => (BlogUrl -> m Text) -> Document -> m Html
renderTags renderUrl doc = do
  rendered <- mapM renderTag tags
  pure $ div (mconcat rendered) ! A.class_ "tags-container"
  where
    tags = fromMaybe [] $ M.lookup (mtdtName BlogTags) (doc ^. docMtdt) >>= fromJSONM
    renderTag :: Text -> m Html
    renderTag tag = do
      url <- renderUrl $ BlogArchiveTag tag
      pure $ div (a (toMarkup tag) ! A.href (toValue url)) ! A.class_ "tag"

renderBlocksWithFootnotes :: forall m. (MonadKorrvigs m) => [Block] -> RenderMonad m Html
renderBlocksWithFootnotes bks = do
  content <- renderBlocks bks
  footnotes <- use rdrstFootNotes
  rdrstFootNotes .= M.empty
  ftHtml <- mapM renderFootNote $ M.toList footnotes
  off <- view rdrHdOffset
  let footnotesHtml = if M.null footnotes then mempty else dl (h (off + 1) "Footnotes" <> mconcat ftHtml) ! A.class_ "footnotes"
  pure $ content <> footnotesHtml
  where
    renderFootNote :: (Int, [Block]) -> RenderMonad m Html
    renderFootNote (ftIdC, ftBks) = do
      content <- renderBlocks ftBks
      let ftId = "ft-" <> T.pack (show ftIdC)
      let ftContentId = "ftc-" <> T.pack (show ftIdC)
      let ftIdHtml = dt (a (toMarkup $ show ftIdC) ! A.href (toValue $ "#" <> ftId)) ! A.id (toValue ftContentId)
      pure $ ftIdHtml <> dd content

renderBlocks :: (MonadKorrvigs m) => [Block] -> RenderMonad m Html
renderBlocks bks = fmap mconcat $ mapM renderBlock bks

renderBlock :: (MonadKorrvigs m) => Block -> RenderMonad m Html
renderBlock (Para inls) = p <$> renderInlines inls
renderBlock (LineBlock inls) = do
  lns <- mapM renderInlines inls
  pure $ p $ mconcat $ (<> hr) <$> lns
renderBlock (CodeBlock attr txt) = pure $ Wdgs.skyContent attr txt
renderBlock (BlockQuote bks) = blockquote <$> renderBlocks bks
renderBlock (OrderedList items) = ol . foldMap li <$> mapM renderBlocks items
renderBlock (BulletList items) = ul . foldMap li <$> mapM renderBlocks items
renderBlock (DefinitionList items) = dl . mconcat <$> mapM renderItem items
  where
    renderItem (def, bks) = do
      defH <- dt <$> renderInlines def
      bksH <- foldMap dd <$> mapM renderBlocks bks
      pure $ defH <> bksH
renderBlock (Figure _ cpt fig) = do
  cptH <- renderBlocks cpt
  figH <- renderBlocks fig
  pure $ figure $ figH <> figcaption cptH
renderBlock (Embed ref) = do
  doc <- view rdrDoc
  case resolveEmbedRef doc ref of
    Nothing -> pure mempty
    Just ident -> fromMaybeT mempty $ do
      entry <- hoistLift $ lift $ load ident
      case entry ^. entryKindData of
        NoteD note -> do
          embedded <- hoistEither =<< readNote (note ^. notePath)
          lvl <- view rdrCurLevel
          lift $ shiftOffset lvl $ renderBlocks $ embedded ^. docContent
        FileD file | "image" `BS.isPrefixOf` (file ^. fileMime) -> do
          fileName <- hoistLift $ lift $ rSelectMtdt BlogFile $ sqlId $ entry ^. entryName
          render <- view rdrRenderUrl
          url <- lift $ lift $ render $ BlogFilePlain fileName
          pure $ img ! A.src (toValue url) ! A.width "100%"
        _ -> pure mempty
renderBlock (EmbedHeader ref hdLvl) = do
  doc <- view rdrDoc
  case resolveEmbedRef doc ref of
    Nothing -> pure mempty
    Just ident -> fromMaybeT mempty $ do
      entry <- hoistLift $ lift $ load ident
      note <- hoistMaybe $ entry ^? _Note
      embedded <- hoistEither =<< readNote (note ^. notePath)
      off <- view rdrHdOffset
      let lvl = hdLvl + off
      let hdr = h lvl $ toMarkup $ doc ^. docTitle
      fmap (hdr <>) $ lift $ shiftOffset lvl $ renderBlocks $ embedded ^. docContent
renderBlock (Collection _ "last-posts" _) = do
  onlyPublished <- view rdrOnlyPublished
  render <- view rdrRenderUrl
  posts <- lift $ loadForTag onlyPublished Nothing (Just 10)
  items <- forM posts $ \(post, pub, postTitle, _) -> do
    url <- lift $ render $ BlogPostNote post
    pure $
      li $
        mconcat
          [ span (toMarkup $ formatTime defaultTimeLocale "%F" pub) ! A.class_ "postdate",
            " ",
            a (toMarkup postTitle) ! A.href (toValue url)
          ]
  pure $ ul $ mconcat items
renderBlock (Collection _ _ _) = pure mempty
renderBlock (Syndicate _ _ _ _) = pure mempty
renderBlock (Sub hd) | "blog-ignore" `elem` (hd ^. hdAttr . attrClasses) = pure mempty
renderBlock (Sub hd) = do
  off <- view rdrHdOffset
  let lvl = off + hd ^. hdLevel
  let hdr = h lvl (toMarkup $ hd ^. hdTitle)
  content <- shiftOffset lvl $ renderBlocks $ hd ^. hdContent
  pure $ hdr <> content
renderBlock (Table _) = pure mempty

h :: Int -> Html -> Html
h 1 = h1
h 2 = h2
h 3 = h3
h 4 = h4
h 5 = h5
h _ = h6

resolveEmbedRef :: Document -> Either Text Id -> Maybe Id
resolveEmbedRef _ (Right ident) = Just ident
resolveEmbedRef doc (Left mtdt) = doc ^? docMtdt . ix (CI.mk mtdt) . _JSON . to MkId

renderInlines :: (MonadKorrvigs m) => [Inline] -> RenderMonad m Html
renderInlines inls = fmap mconcat $ mapM renderInline inls

renderInline :: (MonadKorrvigs m) => Inline -> RenderMonad m Html
renderInline (Plain txt) = pure $ toMarkup txt
renderInline (Styled Emph inls) = i <$> renderInlines inls
renderInline (Styled Quote inls) = q <$> renderInlines inls
renderInline (Styled SubScript inls) = sub <$> renderInlines inls
renderInline (Styled SuperScript inls) = sup <$> renderInlines inls
renderInline (Code _ txt) = pure $ code $ toMarkup txt
renderInline (Link _ inls tgt _) = do
  r :: Maybe (Maybe Text, Maybe Text, Bool, Maybe Text) <- lift $ rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId tgt
    let sqlI = entry ^. sqlEntryId
    blogpost <- selectTextMtdt BlogPost sqlI
    blogfile <- selectTextMtdt BlogFile sqlI
    hidden <- selectTextMtdt Hidden sqlI
    url <- selectTextMtdt Url sqlI
    pure (blogpost, blogfile, O.not $ isNull hidden, url)
  capt <- renderInlines inls
  lnk <- case r of
    Nothing -> pure capt -- Should happen only when tgt does not exists
    Just (blogpost, blogfile, isHidden, url) -> exitExceptT $ do
      renderUrl <- lift $ view rdrRenderUrl
      forM_ blogpost $ \post -> do
        urlT <- lift $ lift $ renderUrl $ BlogPostNote post
        throwE $ a capt ! A.href (toValue urlT)
      forM_ blogfile $ \file -> do
        urlT <- lift $ lift $ renderUrl $ BlogFilePlain file
        throwE $ a capt ! A.href (toValue urlT)
      when isHidden $ throwE capt
      forM_ url $ \urlT -> throwE $ a capt ! A.href (toValue urlT)
      topEntries <- lift $ view rdrTopEntries
      entryUrl <- lift $ lift $ topEntries tgt
      forM_ entryUrl $ \tgtUrl -> do
        rendered <- lift $ lift $ renderUrl tgtUrl
        throwE $ a capt ! A.href (toValue rendered)
      pure capt
  citations <- view rdrCitations
  let citation = fromMaybe mempty $ M.lookup tgt citations
  pure $ lnk <> citation
  where
    exitExceptT act =
      runExceptT act >>= \case
        Left e -> pure e
        Right v -> pure v
renderInline (Cite _) = pure mempty
renderInline (MtdtLink txt tgt) = do
  mtdt <- view $ rdrDoc . docMtdt
  case M.lookup (CI.mk tgt) mtdt >>= fromJSONM of
    Just (url :: Text) -> do
      c <- cpt $ toMarkup url
      pure $ a c ! A.href (toValue url)
    Nothing -> cpt mempty
  where
    cpt d = maybe (pure d) renderInlines txt
renderInline (DateLink (Just txt) _) = renderInlines txt
renderInline (DateLink Nothing day) = do
  let txt = formatTime defaultTimeLocale "%Y-%m-%d" day
  renderInline $ Plain $ T.pack txt
renderInline (PlainLink txt tgt) = do
  let url = show tgt
  cpt <- maybe (pure $ toMarkup url) renderInlines txt
  pure $ a cpt ! A.href (toValue url)
renderInline Space = pure $ toMarkup (" " :: Text)
renderInline Break = pure br
renderInline (DisplayMath mth) = lift $ renderMath False mth
renderInline (InlineMath mth) = lift $ renderMath True mth
renderInline (Sidenote note) = do
  idx <- use rdrstCount
  rdrstCount %= (+ 1)
  rdrstFootNotes %= M.insert idx note
  let ftId = "ft-" <> T.pack (show idx)
  let ftContentId = "ftc-" <> T.pack (show idx)
  let lnk = a (toMarkup $ show idx) ! A.href (toValue $ "#" <> ftContentId)
  pure $ span lnk ! A.class_ "footnote" ! A.id (toValue ftId)
renderInline (Check _) = pure mempty
