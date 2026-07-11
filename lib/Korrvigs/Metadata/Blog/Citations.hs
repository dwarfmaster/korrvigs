{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Metadata.Blog.Citations where

import Citeproc
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import qualified Korrvigs.Metadata.Media.Export as E
import Korrvigs.Monad
import Korrvigs.Note hiding (Style)
import Korrvigs.Note.AST hiding (Style)
import Opaleye
import Text.Blaze
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Text

instance Eq Html where
  _ == _ = False

instance Ord Html where
  compare _ _ = LT

instance Show Html where
  show = LT.unpack . renderMarkup

instance CiteprocOutput Html where
  toText = LT.toStrict . renderMarkup
  fromText = text
  dropTextWhile _ = id
  dropTextWhileEnd _ = id
  addFontVariant NormalVariant h = H.span h ! A.style "font-variant-caps: normal;"
  addFontVariant SmallCapsVariant h = H.span h ! A.style "font-variant-caps: small-caps;"
  addFontStyle NormalFont h = H.span h ! A.style "font-style: normal;"
  addFontStyle ItalicFont h = H.span h ! A.style "font-style: italic;"
  addFontStyle ObliqueFont h = H.span h ! A.style "font-style: oblique;"
  addFontWeight NormalWeight h = H.span h ! A.style "font-weight: normal;"
  addFontWeight BoldWeight h = H.span h ! A.style "font-weight: bold;"
  addFontWeight LightWeight h = H.span h ! A.style "font-weight: 100;"
  addTextDecoration NoDecoration h = H.span h ! A.style "text-decoration: none;"
  addTextDecoration UnderlineDecoration h = H.span h ! A.style "text-decoration: underline;"
  addVerticalAlign BaselineAlign h = H.span h ! A.style "vertical-align: baseline;"
  addVerticalAlign SupAlign h = H.span h ! A.style "vertical-align: top;"
  addVerticalAlign SubAlign h = H.span h ! A.style "vertical-align: sub;"
  addTextCase _ Lowercase h = H.span h ! A.style "text-transform: lowercase;"
  addTextCase _ Uppercase h = H.span h ! A.style "text-transform: uppercase;"
  addTextCase _ CapitalizeFirst h = H.span h ! A.style "text-transform: capitalize;"
  addTextCase _ CapitalizeAll h = H.span h ! A.style "text-transform: capitalize;"
  addTextCase _ _ h = H.span h ! A.style "text-transform: none;"
  addDisplay _ = id
  addQuotes h = "\"" <> h <> "\""
  movePunctuationInsideQuotes = id
  inNote = id
  mapText _ = id
  addHyperlink url h = H.a h ! A.href (toValue url)
  localizeQuotes _ = id

config :: CiteprocOptions
config =
  CiteprocOptions
    { linkCitations = True,
      linkBibliography = True
    }

loadStyle :: (MonadKorrvigs m) => (Id, Text) -> m (Style Html)
loadStyle (i, cd) = do
  entry <- load i >>= throwMaybe (KCantLoad i "Can't load entry to find CSL")
  note <- throwMaybe (KMiscError $ unId i <> " is not a note") $ entry ^? _Note
  let path = note ^. notePath
  doc <- readNote path >>= throwEither (KCantLoad i)
  content <- throwMaybe (KMiscError $ unId i <> " does not have a #" <> cd <> " code block") $ doc ^? docContent . each . bkNamedCode cd . _2
  parseStyle (const $ pure mempty) content >>= throwEither (\e -> KMiscError $ "Failed to load CSL from " <> unId i <> "#" <> cd <> ": " <> T.pack (show e))

loadLang :: (MonadKorrvigs m) => m Lang
loadLang =
  throwEither (\e -> KMiscError $ "Failed to load lang: " <> T.pack e) $ parseLang "en"

buildReference :: Id -> E.BibData -> Reference Html
buildReference i bib =
  Reference
    { referenceId = ItemId (unId i),
      referenceType = (bib ^. E.bibKind),
      referenceDisambiguation = Nothing,
      referenceVariables =
        M.fromList $
          catMaybes
            [ m (bib ^. E.bibTitle) "title" TextVal
            -- TODO
            ]
    }
  where
    m :: Maybe a -> Variable -> (a -> Val Html) -> Maybe (Variable, Val Html)
    m Nothing _ _ = Nothing
    m (Just v) nm mk = Just (nm, mk v)

loadReference :: (MonadKorrvigs m) => Id -> m (Maybe (Reference Html))
loadReference i = do
  bib <- E.rSelectBibData i
  pure $ buildReference i <$> bib

extractCitations :: (MonadKorrvigs m) => Document -> m [Id]
extractCitations doc = rSelect $ do
  i <- values $ sqlId <$> S.toList candidates
  entry <- selectTable entriesTable
  where_ $ entry ^. sqlEntryName .== i
  let sqlI = entry ^. sqlEntryId
  media <- selectTable entriesMetadataTable
  where_ $ media ^. sqlEntry .== sqlI
  where_ $ media ^. sqlKey .== sqlStrictText (mtdtSqlName MediaMtdt)
  pure i
  where
    candidates = S.fromList $ doc ^.. docContent . each . bkSubBlocks . bkInlines . ((_Link . _3) <> _Cite)

-- Extract the citations, build the bibliography. Associate to each cited entry
-- a text key and a formatted bibliography entry.
prepareCitations :: (MonadKorrvigs m) => (Id, Text) -> Document -> m (Map Id (Html, Text, Html))
prepareCitations csl doc = do
  citations <- extractCitations doc
  refs <- fmap catMaybes $ forM citations $ \i -> fmap (i,) <$> loadReference i
  let procCitations = mkCitation . fst <$> refs
  style <- loadStyle csl
  lang <- loadLang
  let r = citeproc config style (Just lang) (snd <$> refs) procCitations
  pure $ M.fromList $ zipWith3 (\i ref (refTxt, bib) -> (i, (ref, refTxt, bib))) (fst <$> refs) (resultCitations r) (resultBibliography r)
  where
    mkCitation :: Id -> Citation Html
    mkCitation i =
      Citation
        { citationId = Just (unId i),
          citationNoteNumber = Nothing,
          citationItems =
            [ CitationItem
                { citationItemId = ItemId (unId i),
                  citationItemLabel = Nothing,
                  citationItemLocator = Nothing,
                  citationItemType = NormalCite,
                  citationItemPrefix = Nothing,
                  citationItemSuffix = Nothing,
                  citationItemData = Nothing
                }
            ]
        }
