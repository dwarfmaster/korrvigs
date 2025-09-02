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
import Data.Aeson
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
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Utils.JSON (fromJSONM)
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

missued :: Val Inlines -> Endo NewEntry
missued (DateVal (Date [DateParts [year]] _ _ _)) =
  Endo $ neMtdt . at (mtdtName MedYear) ?~ toJSON (toInteger year)
missued (DateVal (Date [DateParts (year : month : _)] _ _ _)) =
  Endo $
    (neMtdt . at (mtdtName MedYear) ?~ toJSON (toInteger year))
      . (neMtdt . at (mtdtName MedMonth) ?~ toJSON month)
missued _ = mempty

mp :: (ExtraMetadata mtdt, ToJSON (MtdtType mtdt)) => (Val Inlines -> Maybe (MtdtType mtdt)) -> mtdt -> Val Inlines -> Endo NewEntry
mp parser mtdt val = Endo $ maybe id ((neMtdt . at (mtdtName mtdt) ?~) . toJSON) $ parser val

mpCon :: (Val Inlines -> Maybe a) -> ASetter' MediaContainer a -> Val Inlines -> Endo NewEntry
mpCon parser lns val =
  Endo $ maybe id (\v -> neMtdt . at (mtdtName InContainer) %~ Just . toJSON . (lns .~ v) . fromMaybe def . (>>= fromJSONM)) $ parser val

mp' :: (ExtraMetadata mtdt, MtdtType mtdt ~ [a], ToJSON a, FromJSON a) => (Val Inlines -> Maybe a) -> mtdt -> Val Inlines -> Endo NewEntry
mp' parser mtdt val = Endo $ maybe id (\v -> neMtdt . at (mtdtName mtdt) %~ Just . toJSON . maybe [v] (v :) . (>>= fromJSONM)) $ parser val

variablesMapping :: [(Variable, Val Inlines -> Endo NewEntry)]
variablesMapping =
  [ ("abstract", mpTxt Abstract),
    ("doi", mpTxt' DOI),
    ("isbn", mp' parseISBN ISBNMtdt),
    ("issn", mpTxt' ISSN),
    ("title", Endo . maybe id (neTitle ?~) . parseTxt),
    ("author", \v -> Endo $ neMtdt . at (mtdtName Authors) %~ Just . toJSON . (++ parseAuthors v) . fromMaybe [] . (>>= fromJSONM)),
    ("issued", missued),
    ("url", mpTxt Url),
    ("publisher", mpTxt' Publisher),
    ("container-title", mpCon parseTxt conTitle),
    ("collection-title", mpCon (fmap Just . parseTxt) conCollection),
    ("chapter", mpCon (fmap Just . parseTxt) conChapter),
    ("page", mpCon (fmap Just . parsePages) conPages),
    ("volume", mpCon (fmap Just . parseInt) conVolume),
    ("organization", mpTxt' Institution)
  ]
  where
    mpTxt :: (ExtraMetadata mtdt, MtdtType mtdt ~ Text) => mtdt -> Val Inlines -> Endo NewEntry
    mpTxt = mp parseTxt
    mpTxt' :: (ExtraMetadata mtdt, MtdtType mtdt ~ [Text]) => mtdt -> Val Inlines -> Endo NewEntry
    mpTxt' = mp' parseTxt

importReference :: Reference Inlines -> NewEntry -> NewEntry
importReference ref = appEndo endo . med
  where
    tp = lookupType $ referenceType ref
    med = setMtdtValue MediaMtdt tp
    handleVariable :: Variable -> Val Inlines -> Endo NewEntry
    handleVariable var val =
      maybe mempty ($ val) $ M.lookup var $ M.fromList variablesMapping
    endo :: Endo NewEntry
    endo = foldMap (uncurry handleVariable) $ M.toList $ referenceVariables ref

importReferences :: [Reference Inlines] -> Map Text (NewEntry -> NewEntry)
importReferences = M.fromList . map (unItemId . referenceId &&& importReference)

importPandoc :: (ReaderOptions -> Text -> PandocIO Pandoc) -> BSL.ByteString -> IO (Map Text (NewEntry -> NewEntry))
importPandoc reader input =
  runIO act >>= \case
    Left err -> do
      putStrLn $ "Failed to load: " <> show err
      pure M.empty
    Right v -> pure v
  where
    txt = LT.toStrict $ LEnc.decodeUtf8 input
    act = do
      pd <- reader def txt
      refs <- getReferences Nothing pd
      pure $ importReferences refs

importBibtex :: BSL.ByteString -> IO (Map Text (NewEntry -> NewEntry))
importBibtex = importPandoc readBibLaTeX

importRIS :: BSL.ByteString -> IO (Map Text (NewEntry -> NewEntry))
importRIS = importPandoc readRIS

importRef :: Text -> IO (Maybe (NewEntry -> NewEntry))
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
