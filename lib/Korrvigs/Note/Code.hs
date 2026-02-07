module Korrvigs.Note.Code where

import Control.Applicative ((<|>))
import Control.Arrow (second, (***))
import Control.Lens
import Control.Monad
import Data.Foldable (toList)
import Data.List (singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Note.AST
import Text.Parsec hiding ((<|>))
import Text.Parsec.Number

fromId :: (Applicative f) => Text -> ((Attr, Text) -> f (Attr, Text)) -> Document -> f Document
fromId nm = docContent . each . bkSubBlocks . _CodeBlock . filtered ((== nm) . view (_1 . attrId))

knownLanguages :: Map Text Executable
knownLanguages =
  M.fromList
    [ ("bash", Bash),
      ("prolog", SwiProlog),
      ("json", PlainJson),
      ("csv", PlainCsv),
      ("text", PlainText),
      ("dot", Graphviz),
      ("c", CLang),
      ("cpp", CPPLang),
      ("nix", NixData),
      ("python", Python),
      ("lua", Lua),
      ("julia", Julia),
      ("dhall", Dhall),
      ("perl", Perl),
      ("raku", Raku),
      ("haskell", Haskell),
      ("rust", Rust),
      ("ocaml", OCaml),
      ("openscad", OpenScad),
      ("povray", Povray),
      ("latex", LaTeX),
      ("context", ConTeXt),
      ("gnuplot", GnuPlot),
      ("asymptote", Asymptote)
    ]

data AttrData = AttrData
  { _attrArg :: Map Int RunArg,
    _attrEnv :: Map Text RunArg,
    _attrStdIn :: Maybe RunArg,
    _attrType :: Maybe RunnableType,
    _attrSetup :: RunnableSetup
  }
  deriving (Show, Eq)

makeLenses ''AttrData

instance Semigroup AttrData where
  dt1 <> dt2 =
    AttrData
      { _attrArg = M.union (dt1 ^. attrArg) (dt2 ^. attrArg),
        _attrEnv = M.union (dt1 ^. attrEnv) (dt2 ^. attrEnv),
        _attrStdIn = (dt1 ^. attrStdIn) <|> (dt2 ^. attrStdIn),
        _attrType = (dt1 ^. attrType) <|> (dt2 ^. attrType),
        _attrSetup = (dt1 ^. attrSetup) <> (dt2 ^. attrSetup)
      }

instance Monoid AttrData where
  mempty = AttrData M.empty M.empty Nothing Nothing mempty

renderAttrData :: AttrData -> Map Text [Text]
renderAttrData attrData =
  M.fromListWith (<>) . fmap (second singleton) $
    (renderArg <$> M.toList (attrData ^. attrArg))
      ++ (renderEnv <$> M.toList (attrData ^. attrEnv))
      ++ (renderVal "stdin" <$> toList (attrData ^. attrStdIn))
      ++ (renderType <$> toList (attrData ^. attrType))
      ++ (("profile",) <$> toList (stp ^? runStpProfile . _Just . to renderProfile))
      ++ (("o",) <$> toList (stp ^. runStpOpt))
      ++ (("flag",) <$> stp ^. runStpFlags)
      ++ (("version",) <$> toList (stp ^. runStpVersion))
  where
    stp = attrData ^. attrSetup
    renderVal :: Text -> RunArg -> (Text, Text)
    renderVal prefix (ArgPlain val) = (prefix, val)
    renderVal prefix (ArgResult i cmp) = (prefix <> ":comp", unId i <> "#" <> cmp)
    renderVal prefix (ArgResultSame cmp) = (prefix <> ":comp", cmp)
    renderVal prefix (ArgEntry i) = (prefix <> ":entry", unId i)
    renderArg :: (Int, RunArg) -> (Text, Text)
    renderArg (i, arg) = renderVal ("arg:" <> T.pack (show i)) arg
    renderEnv :: (Text, RunArg) -> (Text, Text)
    renderEnv (ev, arg) = renderVal ("env:" <> ev) arg
    renderType :: RunnableType -> (Text, Text)
    renderType tp = ("type", runTypeName tp)
    renderProfile RunRelease = "release"
    renderProfile RunDebug = "debug"

parseAttrMtdt :: Text -> [Text] -> AttrData
parseAttrMtdt key vals = mconcat $ parseAttrMtdt' key <$> vals

parseAttrMtdt' :: Text -> Text -> AttrData
parseAttrMtdt' key val = case parse (parser <* eof) "<codearg>" key of
  Left _ -> mempty
  Right attrDat -> attrDat
  where
    valP :: (Stream s Identity Char) => Parsec s () RunArg
    valP = option (ArgPlain val) $ do
      void $ char ':'
      kd <- many alphaNum
      case kd of
        "comp" ->
          pure $
            if T.any (== '#') val
              then
                uncurry ArgResult $
                  MkId *** (maybe "" snd . T.uncons) $
                    T.break (== '#') val
              else ArgResultSame val
        "entry" -> pure $ ArgEntry $ MkId val
        _ -> fail $ kd <> " is not a recognised RunArg type"
    parser :: (Stream s Identity Char) => Parsec s () AttrData
    parser =
      typeP <|> argP <|> envP <|> stdinP <|> profP <|> optP <|> flagP <|> versionP
    argP :: (Stream s Identity Char) => Parsec s () AttrData
    argP = do
      void $ string "arg:"
      num <- decimal
      ra <- valP
      pure $ mempty & attrArg . at num ?~ ra
    envP :: (Stream s Identity Char) => Parsec s () AttrData
    envP = do
      void $ string "env:"
      nm <- T.pack <$> many (alphaNum <|> char '_')
      ra <- valP
      pure $ mempty & attrEnv . at nm ?~ ra
    stdinP :: (Stream s Identity Char) => Parsec s () AttrData
    stdinP = do
      void $ string "stdin"
      ra <- valP
      pure $ mempty & attrStdIn ?~ ra
    typeP :: (Stream s Identity Char) => Parsec s () AttrData
    typeP = do
      void $ string "type"
      tp <- maybe (fail $ T.unpack val <> " is not a valid runnable type") pure $ parseTypeName val
      pure $ mempty & attrType ?~ tp
    profP :: (Stream s Identity Char) => Parsec s () AttrData
    profP = do
      void $ string "profile"
      prof <- case val of
        "debug" -> pure RunDebug
        "release" -> pure RunRelease
        _ -> fail $ T.unpack val <> " is not a valid profile"
      pure $ mempty & attrSetup . runStpProfile ?~ prof
    optP :: (Stream s Identity Char) => Parsec s () AttrData
    optP = do
      void $ string "o"
      pure $ mempty & attrSetup . runStpOpt ?~ val
    flagP :: (Stream s Identity Char) => Parsec s () AttrData
    flagP = do
      void $ string "flag"
      pure $ mempty & attrSetup . runStpFlags .~ [val]
    versionP :: (Stream s Identity Char) => Parsec s () AttrData
    versionP = do
      void $ string "version"
      pure $ mempty & attrSetup . runStpVersion ?~ val

codeRefs :: Attr -> [Id]
codeRefs attr = argToRef =<< attrDat ^.. (attrArg . each <> attrEnv . each <> attrStdIn . _Just)
  where
    attrDat = foldMap (uncurry parseAttrMtdt) $ M.toList $ attr ^. attrMtdt
    argToRef (ArgPlain _) = []
    argToRef (ArgResult i _) = [i]
    argToRef (ArgResultSame _) = []
    argToRef (ArgEntry i) = [i]

languageToType :: Executable -> Maybe RunnableType
languageToType PlainJson = Just ArbitraryJson
languageToType PlainCsv = Just TabularCsv
languageToType PlainText = Just ArbitraryText
languageToType Graphviz = Just VectorGraphic
languageToType NixData = Just ArbitraryJson
languageToType Dhall = Just ArbitraryJson
languageToType LaTeX = Just VectorDocument
languageToType ConTeXt = Just VectorDocument
languageToType _ = Nothing

toRunnable :: Attr -> Text -> Maybe Runnable
toRunnable attr code = do
  let classes = attr ^. attrClasses
  let attrDat = foldMap (uncurry parseAttrMtdt) $ M.toList $ attr ^. attrMtdt
  language <- foldr ((<|>) . flip M.lookup knownLanguages) Nothing classes
  tp <- attrDat ^. attrType <|> languageToType language
  pure $
    Runnable
      { _runExecutable = language,
        _runCode = code,
        _runType = tp,
        _runArgs = fmap snd $ M.toList $ attrDat ^. attrArg,
        _runEnv = attrDat ^. attrEnv,
        _runStdIn = attrDat ^. attrStdIn,
        _runSetup = attrDat ^. attrSetup
      }

updateRefInAttrData :: Id -> Maybe Id -> AttrData -> AttrData
updateRefInAttrData old new attrData =
  AttrData
    { _attrArg = M.mapMaybe updateRefInVal $ attrData ^. attrArg,
      _attrEnv = M.mapMaybe updateRefInVal $ attrData ^. attrEnv,
      _attrStdIn = (attrData ^. attrStdIn) >>= updateRefInVal,
      _attrType = attrData ^. attrType,
      _attrSetup = attrData ^. attrSetup
    }
  where
    updateRefInVal :: RunArg -> Maybe RunArg
    updateRefInVal (ArgPlain txt) = Just $ ArgPlain txt
    updateRefInVal (ArgResult i cmp) | i == old = flip ArgResult cmp <$> new
    updateRefInVal (ArgResult i cmp) = Just $ ArgResult i cmp
    updateRefInVal (ArgResultSame cmp) = Just $ ArgResultSame cmp
    updateRefInVal (ArgEntry i) | i == old = ArgEntry <$> new
    updateRefInVal (ArgEntry i) = Just $ ArgEntry i

updateRefInAttr :: Id -> Maybe Id -> Attr -> Attr
updateRefInAttr old new = attrMtdt %~ mconcat . map updateRefInMtdt . M.toList
  where
    updateRefInMtdt :: (Text, [Text]) -> Map Text [Text]
    updateRefInMtdt (key, vals) =
      let oldMtdt = parseAttrMtdt key vals
       in let newMtdt = updateRefInAttrData old new oldMtdt
           in let rendered = renderAttrData newMtdt
               in if M.null rendered
                    then (if oldMtdt == newMtdt then M.singleton key vals else M.empty)
                    else rendered
