module Korrvigs.Metadata.Media.Pandoc
  ( importReferences,
    importBibtex,
    importRIS,
    importRef,
  )
where

import Citeproc.Types
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.ISBN
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LB
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Utils.Pandoc
import Text.Pandoc (PandocIO, ReaderOptions, runIO)
import Text.Pandoc.Builder
import Text.Pandoc.Citeproc (getReferences)
import Text.Pandoc.Readers.BibTeX
import Text.Pandoc.Readers.RIS
import Text.Parsec
import Text.Parsec.Number

refsType :: [(CI Text, MediaType)]
refsType =
  [ ("article-journal", Article),
    ("book", Book),
    ("pamphlet", Booklet),
    ("chapter", Incollection),
    ("paper-conference", Inproceedings),
    ("thesis", PhdThesis),
    ("manuscript", Unpublished)
  ]

lookupType :: Text -> MediaType
lookupType = fromMaybe Misc . flip M.lookup (M.fromList refsType) . CI.mk

inlinesToText :: Inlines -> Text
inlinesToText = LT.toStrict . LB.toLazyText . foldMap pdInlineToText

parseTxt :: Val Inlines -> Maybe Text
parseTxt (TextVal txt) = Just txt
parseTxt (FancyVal inls) = Just $ inlinesToText inls
parseTxt (NumVal i) = Just $ T.pack $ show i
parseTxt _ = Nothing

parseISBN :: Val Inlines -> Maybe ISBN
parseISBN inls = case validateISBN <$> parseTxt inls of
  Nothing -> Nothing
  Just (Left _) -> Nothing
  Just (Right isbn) -> Just isbn

parseIntFromText :: Text -> Maybe Int
parseIntFromText txt = case parse decimal "<integer>" txt of
  Left _ -> Nothing
  Right v -> Just v

parseInt :: Val Inlines -> Maybe Int
parseInt (NumVal i) = Just i
parseInt (TextVal txt) = parseIntFromText txt
parseInt (FancyVal inls) = parseIntFromText $ inlinesToText inls
parseInt _ = Nothing

parseAuthors :: Val Inlines -> [Text]
parseAuthors (TextVal txt) = [txt]
parseAuthors (NamesVal names) = renderName <$> names
parseAuthors _ = []

renderName :: Name -> Text
renderName nm = case nameLiteral nm of
  Just lit -> lit
  Nothing -> T.intercalate " " $ catMaybes [nameGiven nm, nameDroppingParticle nm, nameNonDroppingParticle nm, nameFamily nm, nameSuffix nm]

parsePagesTxt :: Text -> Maybe (Int, Int)
parsePagesTxt txt = case parse parser "<pages>" txt of
  Left _ -> Nothing
  Right v -> Just v
  where
    parser = do
      s <- decimal
      void $ optional $ char ':' >> (decimal :: ParsecT Text u Identity Int)
      e <- option s $ do
        void $ char '-'
        e <- decimal
        void $ optional $ char ':' >> (decimal :: ParsecT Text u Identity Int)
        pure e
      pure (s, e)

parsePages :: Val Inlines -> Maybe (Int, Int)
parsePages (NumVal page) = Just (page, page)
parsePages (TextVal txt) = parsePagesTxt txt
parsePages (FancyVal inls) = parsePagesTxt $ inlinesToText inls
parsePages _ = Nothing

missued :: Val Inlines -> Endo Media
missued (DateVal (Date [DateParts [year]] _ _ _)) = Endo $ medYear ?~ toInteger year
missued (DateVal (Date [DateParts (year : month : _)] _ _ _)) =
  Endo $ (medYear ?~ toInteger year) . (medMonth ?~ month)
missued _ = mempty

mp :: (Val Inlines -> Maybe a) -> ASetter' Media (Maybe a) -> Val Inlines -> Endo Media
mp parser lns val = Endo $ maybe id (lns ?~) $ parser val

mpCon :: (Val Inlines -> Maybe a) -> ASetter' MediaContainer a -> Val Inlines -> Endo Media
mpCon parser lns val =
  Endo $ maybe id (\v -> medContainer %~ Just . (lns .~ v) . fromMaybe def) $ parser val

mp' :: (Val Inlines -> Maybe a) -> ASetter' Media [a] -> Val Inlines -> Endo Media
mp' parser lns val = Endo $ maybe id (\v -> lns %~ (v :)) $ parser val

variablesMapping :: [(Variable, Val Inlines -> Endo Media)]
variablesMapping =
  [ ("abstract", mpTxt medAbstract),
    ("doi", mpTxt' medDOI),
    ("isbn", mp' parseISBN medISBN),
    ("issn", mpTxt' medISSN),
    ("title", mpTxt medTitle),
    ("author", \v -> Endo $ medAuthors %~ (++ parseAuthors v)),
    ("issued", missued),
    ("url", mpTxt medUrl),
    ("publisher", mpTxt' medPublisher),
    ("container-title", mpCon parseTxt conTitle),
    ("collection-title", mpCon (fmap Just . parseTxt) conCollection),
    ("chapter", mpCon (fmap Just . parseTxt) conChapter),
    ("page", mpCon (fmap Just . parsePages) conPages),
    ("volume", mpCon (fmap Just . parseInt) conVolume),
    ("organization", mpTxt' medInstitution)
  ]
  where
    mpTxt = mp parseTxt
    mpTxt' = mp' parseTxt

importReference :: Reference Inlines -> Media
importReference ref = appEndo endo med
  where
    tp = lookupType $ referenceType ref
    med =
      Media
        { _medType = tp,
          _medAbstract = Nothing,
          _medBibtex = Nothing,
          _medDOI = [],
          _medISBN = [],
          _medISSN = [],
          _medTitle = Nothing,
          _medAuthors = [],
          _medMonth = Nothing,
          _medYear = Nothing,
          _medUrl = Nothing,
          _medRSS = Nothing,
          _medSource = [],
          _medPublisher = [],
          _medContainer = Nothing,
          _medInstitution = [],
          _medLicense = [],
          _medCover = Nothing,
          _medDiscussion = []
        }
    handleVariable :: Variable -> Val Inlines -> Endo Media
    handleVariable var val =
      maybe mempty ($ val) $ M.lookup var $ M.fromList variablesMapping
    endo :: Endo Media
    endo = foldMap (uncurry handleVariable) $ M.toList $ referenceVariables ref

importReferences :: [Reference Inlines] -> Map Text Media
importReferences = M.fromList . map (unItemId . referenceId &&& importReference)

importPandoc :: (ReaderOptions -> Text -> PandocIO Pandoc) -> BSL.ByteString -> IO (Map Text Media)
importPandoc reader input =
  runIO act <&> \case
    Left _ -> M.empty
    Right v -> v
  where
    txt = LT.toStrict $ LEnc.decodeUtf8 input
    act = do
      pd <- reader def txt
      refs <- getReferences Nothing pd
      pure $ importReferences refs

importBibtex :: BSL.ByteString -> IO (Map Text Media)
importBibtex = importPandoc readBibLaTeX

importRIS :: BSL.ByteString -> IO (Map Text Media)
importRIS = importPandoc readRIS

importRef :: Text -> IO (Maybe Media)
importRef txt = do
  let bsl = LEnc.encodeUtf8 $ LT.fromStrict txt
  bib <- importBibtex bsl
  case M.toList bib of
    ((_, b) : _) -> pure $ Just b
    [] -> do
      ris <- importRIS bsl
      case M.toList ris of
        ((_, r) : _) -> pure $ Just r
        [] -> pure Nothing
