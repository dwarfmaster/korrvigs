module Korrvigs.Metadata.Blog.Export where

import Control.Lens hiding (pre)
import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson.Lens
import qualified Data.CaseInsensitive as CI
import qualified Data.Default as D
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Blog.Mtdt
import Korrvigs.Metadata.Blog.Structure
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Note hiding (code, sub)
import Korrvigs.Utils
import Korrvigs.Utils.JSON
import Opaleye
import Text.Blaze
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import Prelude hiding (div)

data RenderContext m = RenderContext
  { _rdrDoc :: Document,
    _rdrRenderUrl :: BlogUrl -> m Text,
    _rdrHdOffset :: Int,
    _rdrCurLevel :: Int
  }

data RenderState = RenderState
  { _rdrstCount :: Int,
    _rdrstFootNotes :: Map Int [Block]
  }

instance D.Default RenderState where
  def = RenderState 0 M.empty

data BlogPageContent m = BlogPageContent
  { _blogPageContent :: Html,
    _blogPageMetadata :: Map Text Text,
    _blogPageTitle :: Text,
    _blogPageRenderUrl :: BlogUrl -> m Text
  }

type RenderMonad m = RWST (RenderContext m) () RenderState m

makeLenses ''RenderContext
makeLenses ''RenderState
makeLenses ''BlogPageContent

shiftOffset :: Int -> RenderMonad m a -> RenderMonad m a
shiftOffset noffset = withRWST $ \r st -> (r & rdrHdOffset .~ noffset, st)

renderPost :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> Map Text Text -> Id -> m Html
renderPost renderUrl mtdt noteId = do
  entry <- load noteId >>= throwMaybe (KCantLoad noteId "Failed to load entry for blog")
  note <- throwMaybe (KMiscError "Blog post is not a note") $ entry ^? _Note
  doc <- readNote (note ^. notePath) >>= throwEither (\e -> KMiscError $ "Failed to load note for blog post: " <> e)
  let t =
        fromMaybe (doc ^. docTitle) $
          M.lookup (mtdtName BlogTitle) (doc ^. docMtdt) >>= fromJSONM
  contentHtml <- renderDocument renderUrl t doc
  renderPageContent $ BlogPageContent contentHtml mtdt t renderUrl

renderPageContent :: (MonadKorrvigs m) => BlogPageContent m -> m Html
renderPageContent pc = do
  stl <- pc ^. blogPageRenderUrl $ BlogTopLevel "style.css"
  hd <- renderHeader $ pc ^. blogPageRenderUrl
  feed <- pc ^. blogPageRenderUrl $ BlogAtom
  pure $
    docTypeHtml $
      html $
        renderHead (pc ^. blogPageMetadata) (pc ^. blogPageTitle) stl feed
          <> (body $ hd <> pc ^. blogPageContent)

renderHead :: Map Text Text -> Text -> Text -> Text -> Html
renderHead mtdt t stl feed =
  mconcat
    [ meta ! A.charset "UTF-8",
      title (toMarkup t),
      link ! A.rel "stylesheet" ! A.href (toValue stl),
      link ! A.rel "alternate" ! A.type_ "application/atom+xml" ! A.href (toValue feed),
      renderMeta mtdt
    ]

renderRssIcon :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> Maybe Text -> m Html
renderRssIcon renderUrl tag = do
  url <- renderUrl $ maybe BlogAtom BlogAtomTag tag
  pure $ a (img ! A.src "https://upload.wikimedia.org/wikipedia/commons/4/46/Generic_Feed-icon.svg" ! A.class_ "icon") ! A.href (toValue url)

renderHeader :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> m Html
renderHeader renderUrl = do
  home <- renderUrl $ BlogTopLevel "default.html"
  let homeH = div (a "Home" ! A.href (toValue home)) ! A.class_ "home-div"
  archive <- renderUrl BlogArchive
  icon <- renderRssIcon renderUrl Nothing
  let archiveH = div (a "Archive" ! A.href (toValue archive) <> " " <> icon) ! A.class_ "archive-div"
  pure $ div (homeH <> archiveH) ! A.class_ "header-div"

renderMeta :: Map Text Text -> Html
renderMeta mtdt = mconcat $ render <$> M.toList mtdt
  where
    render (nm, cnt) = meta ! A.name (toValue nm) ! A.content (toValue cnt)

renderDocument :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> Text -> Document -> m Html
renderDocument renderUrl usedTitle doc = do
  let ctx =
        RenderContext
          { _rdrDoc = doc,
            _rdrRenderUrl = renderUrl,
            _rdrHdOffset = 1,
            _rdrCurLevel = 1
          }
  let date = renderDate doc
  let t = h1 $ toMarkup usedTitle
  tags <- renderTags renderUrl doc
  (content, ()) <- evalRWST (renderBlocks $ doc ^. docContent) ctx D.def
  pure $ date <> t <> tags <> content

renderDate :: Document -> Html
renderDate doc =
  let docDay :: Maybe Day =
        M.lookup (mtdtName PublishedDate) (doc ^. docMtdt)
          >>= fromJSONM
          >>= parseTimeM True defaultTimeLocale "%F"
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

renderBlocks :: (MonadKorrvigs m) => [Block] -> RenderMonad m Html
renderBlocks bks = fmap mconcat $ mapM renderBlock bks

renderBlock :: (MonadKorrvigs m) => Block -> RenderMonad m Html
renderBlock (Para inls) = p <$> renderInlines inls
renderBlock (LineBlock inls) = do
  lns <- mapM renderInlines inls
  pure $ p $ mconcat $ (<> hr) <$> lns
renderBlock (CodeBlock _ txt) = pure $ pre $ toMarkup txt
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
      note <- hoistMaybe $ entry ^? _Note
      embedded <- hoistEither =<< readNote (note ^. notePath)
      lvl <- view rdrCurLevel
      lift $ shiftOffset lvl $ renderBlocks $ embedded ^. docContent
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
renderInline (Link _ inls tgt) = do
  r :: Maybe (Maybe Text, Maybe Text, Bool, Maybe Text) <- lift $ rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId tgt
    let sqlI = entry ^. sqlEntryId
    blogpost <- selectTextMtdt BlogPost sqlI
    blogfile <- selectTextMtdt BlogFile sqlI
    hidden <- selectTextMtdt Hidden sqlI
    url <- selectTextMtdt Url sqlI
    pure (blogpost, blogfile, isNull hidden, url)
  capt <- renderInlines inls
  case r of
    Nothing -> pure capt
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
      pure capt
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
renderInline (PlainLink txt tgt) = do
  let url = show tgt
  cpt <- maybe (pure $ toMarkup url) renderInlines txt
  pure $ a cpt ! A.href (toValue url)
renderInline Space = pure $ toMarkup (" " :: Text)
renderInline Break = pure br
renderInline (DisplayMath mth) = pure $ code $ toMarkup mth
renderInline (InlineMath mth) = pure $ code $ toMarkup mth
renderInline (Sidenote _) = pure mempty
renderInline (Check _) = pure mempty
