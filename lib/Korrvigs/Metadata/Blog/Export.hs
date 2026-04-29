module Korrvigs.Metadata.Blog.Export where

import Control.Lens hiding (pre)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson.Lens
import qualified Data.CaseInsensitive as CI
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

data BlogPageContent m = BlogPageContent
  { _blogPageContent :: Html,
    _blogPageMetadata :: Map Text Text,
    _blogPageTitle :: Text,
    _blogPageRenderUrl :: BlogUrl -> m Text
  }

makeLenses ''RenderContext
makeLenses ''BlogPageContent

renderPost :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> Map Text Text -> Id -> m Html
renderPost renderUrl mtdt noteId = do
  entry <- load noteId >>= throwMaybe (KCantLoad noteId "Failed to load entry for blog")
  note <- throwMaybe (KMiscError "Blog post is not a note") $ entry ^? _Note
  doc <- readNote (note ^. notePath) >>= throwEither (\e -> KMiscError $ "Failed to load note for blog post: " <> e)
  let t =
        fromMaybe (doc ^. docTitle) $
          M.lookup (mtdtName BlogTitle) (doc ^. docMtdt) >>= fromJSONM
  contentHtml <- renderDocument renderUrl doc
  renderPageContent $ BlogPageContent contentHtml mtdt t renderUrl

renderPageContent :: (MonadKorrvigs m) => BlogPageContent m -> m Html
renderPageContent pc = do
  stl <- pc ^. blogPageRenderUrl $ BlogTopLevel "style.css"
  hd <- renderHeader $ pc ^. blogPageRenderUrl
  pure $
    docTypeHtml $
      renderHead (pc ^. blogPageMetadata) (pc ^. blogPageTitle) stl
        <> (body $ hd <> pc ^. blogPageContent)

renderHead :: Map Text Text -> Text -> Text -> Html
renderHead mtdt t stl =
  mconcat
    [ meta ! A.charset "UTF-8",
      title (toMarkup t),
      link ! A.rel "stylesheet" ! A.href (toValue stl),
      renderMeta mtdt
    ]

renderHeader :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> m Html
renderHeader renderUrl = do
  home <- renderUrl $ BlogTopLevel "default.html"
  let homeH = div (a "Home" ! A.href (toValue home)) ! A.class_ "home-div"
  archive <- renderUrl BlogArchive
  let archiveH = div (a "Archive" ! A.href (toValue archive)) ! A.class_ "archive-div"
  pure $ div (homeH <> archiveH) ! A.class_ "header-div"

renderMeta :: Map Text Text -> Html
renderMeta mtdt = mconcat $ render <$> M.toList mtdt
  where
    render (nm, cnt) = meta ! A.name (toValue nm) ! A.content (toValue cnt)

renderDocument :: (MonadKorrvigs m) => (BlogUrl -> m Text) -> Document -> m Html
renderDocument renderUrl doc = do
  let ctx =
        RenderContext
          { _rdrDoc = doc,
            _rdrRenderUrl = renderUrl,
            _rdrHdOffset = 1,
            _rdrCurLevel = 1
          }
  date <- renderDate doc
  let t = h1 $ toMarkup $ doc ^. docTitle
  tags <- renderTags renderUrl doc
  content <- renderBlocks ctx $ doc ^. docContent
  pure $ date <> t <> tags <> content

renderDate :: (MonadKorrvigs m) => Document -> m Html
renderDate doc = do
  defaultDay <- utctDay <$> liftIO getCurrentTime
  let docDay =
        M.lookup (mtdtName PublishedDate) (doc ^. docMtdt)
          >>= fromJSONM
          >>= parseTimeM True defaultTimeLocale "%F"
  let day = fromMaybe defaultDay docDay
  pure $ p (toMarkup $ formatTime defaultTimeLocale "%F" day) ! A.class_ "pubdate"

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

renderBlocks :: (MonadKorrvigs m) => RenderContext m -> [Block] -> m Html
renderBlocks ctx bks = fmap mconcat $ mapM (renderBlock ctx) bks

renderBlock :: (MonadKorrvigs m) => RenderContext m -> Block -> m Html
renderBlock ctx (Para inls) = p <$> renderInlines ctx inls
renderBlock ctx (LineBlock inls) = do
  lns <- mapM (renderInlines ctx) inls
  pure $ p $ mconcat $ (<> hr) <$> lns
renderBlock _ (CodeBlock _ txt) = pure $ pre $ toMarkup txt
renderBlock ctx (BlockQuote bks) = blockquote <$> renderBlocks ctx bks
renderBlock ctx (OrderedList items) = ol . foldMap li <$> mapM (renderBlocks ctx) items
renderBlock ctx (BulletList items) = ul . foldMap li <$> mapM (renderBlocks ctx) items
renderBlock ctx (DefinitionList items) = dl . mconcat <$> mapM renderItem items
  where
    renderItem (def, bks) = do
      defH <- dt <$> renderInlines ctx def
      bksH <- foldMap dd <$> mapM (renderBlocks ctx) bks
      pure $ defH <> bksH
renderBlock ctx (Figure _ cpt fig) = do
  cptH <- renderBlocks ctx cpt
  figH <- renderBlocks ctx fig
  pure $ figure $ figH <> figcaption cptH
renderBlock ctx (Embed ref) = case resolveEmbedRef (ctx ^. rdrDoc) ref of
  Nothing -> pure mempty
  Just ident -> fromMaybeT mempty $ do
    entry <- hoistLift $ load ident
    note <- hoistMaybe $ entry ^? _Note
    doc <- hoistEither =<< readNote (note ^. notePath)
    let lvl = ctx ^. rdrCurLevel
    lift $ renderBlocks (ctx & rdrHdOffset .~ lvl) $ doc ^. docContent
renderBlock ctx (EmbedHeader ref hdLvl) = case resolveEmbedRef (ctx ^. rdrDoc) ref of
  Nothing -> pure mempty
  Just ident -> fromMaybeT mempty $ do
    entry <- hoistLift $ load ident
    note <- hoistMaybe $ entry ^? _Note
    doc <- hoistEither =<< readNote (note ^. notePath)
    let lvl = hdLvl + ctx ^. rdrHdOffset
    let hdr = h lvl $ toMarkup $ doc ^. docTitle
    fmap (hdr <>) $ lift $ renderBlocks (ctx & rdrHdOffset .~ lvl) $ doc ^. docContent
renderBlock _ (Collection _ _ _) = pure mempty
renderBlock _ (Syndicate _ _ _ _) = pure mempty
renderBlock ctx (Sub hd) = do
  let lvl = ctx ^. rdrHdOffset + hd ^. hdLevel
  let hdr = h lvl (toMarkup $ hd ^. hdTitle)
  content <- renderBlocks (ctx & rdrCurLevel .~ lvl) $ hd ^. hdContent
  pure $ hdr <> content
renderBlock _ (Table _) = pure mempty

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

renderInlines :: (MonadKorrvigs m) => RenderContext m -> [Inline] -> m Html
renderInlines ctx inls = fmap mconcat $ mapM (renderInline ctx) inls

renderInline :: (MonadKorrvigs m) => RenderContext m -> Inline -> m Html
renderInline _ (Plain txt) = pure $ toMarkup txt
renderInline ctx (Styled Emph inls) = i <$> renderInlines ctx inls
renderInline ctx (Styled Quote inls) = q <$> renderInlines ctx inls
renderInline ctx (Styled SubScript inls) = sub <$> renderInlines ctx inls
renderInline ctx (Styled SuperScript inls) = sup <$> renderInlines ctx inls
renderInline _ (Code _ txt) = pure $ code $ toMarkup txt
renderInline ctx (Link _ inls tgt) = do
  r :: Maybe (Maybe Text, Maybe Text, Bool, Maybe Text) <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId tgt
    let sqlI = entry ^. sqlEntryId
    blogpost <- selectTextMtdt BlogPost sqlI
    blogfile <- selectTextMtdt BlogFile sqlI
    hidden <- selectTextMtdt Hidden sqlI
    url <- selectTextMtdt Url sqlI
    pure (blogpost, blogfile, isNull hidden, url)
  capt <- renderInlines ctx inls
  case r of
    Nothing -> pure capt
    Just (blogpost, blogfile, isHidden, url) -> exitExceptT $ do
      forM_ blogpost $ \post -> do
        urlT <- lift $ ctx ^. rdrRenderUrl $ BlogPostNote post
        throwE $ a capt ! A.href (toValue urlT)
      forM_ blogfile $ \file -> do
        urlT <- lift $ ctx ^. rdrRenderUrl $ BlogFilePlain file
        throwE $ a capt ! A.href (toValue urlT)
      when isHidden $ throwE capt
      forM_ url $ \urlT -> throwE $ a capt ! A.href (toValue urlT)
      pure capt
  where
    exitExceptT act =
      runExceptT act >>= \case
        Left e -> pure e
        Right v -> pure v
renderInline _ (Cite _) = pure mempty
renderInline ctx (MtdtLink txt tgt) =
  case M.lookup (CI.mk tgt) (ctx ^. rdrDoc . docMtdt) >>= fromJSONM of
    Just (url :: Text) -> do
      c <- cpt $ toMarkup url
      pure $ a c ! A.href (toValue url)
    Nothing -> cpt mempty
  where
    cpt d = maybe (pure d) (renderInlines ctx) txt
renderInline ctx (PlainLink txt tgt) = do
  let url = show tgt
  cpt <- maybe (pure $ toMarkup url) (renderInlines ctx) txt
  pure $ a cpt ! A.href (toValue url)
renderInline _ Space = pure $ toMarkup (" " :: Text)
renderInline _ Break = pure br
renderInline _ (DisplayMath mth) = pure $ code $ toMarkup mth
renderInline _ (InlineMath mth) = pure $ code $ toMarkup mth
renderInline _ (Sidenote _) = pure mempty
renderInline _ (Check _) = pure mempty
