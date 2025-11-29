module Korrvigs.Note.Code where

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Lens
import Control.Monad
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
      ("ocaml", OCaml)
    ]

data AttrData = AttrData
  { _attrArg :: Map Int RunArg,
    _attrEnv :: Map Text RunArg,
    _attrStdIn :: Maybe RunArg,
    _attrType :: Maybe RunnableType
  }
  deriving (Show)

makeLenses ''AttrData

instance Semigroup AttrData where
  dt1 <> dt2 =
    AttrData
      { _attrArg = M.union (dt1 ^. attrArg) (dt2 ^. attrArg),
        _attrEnv = M.union (dt1 ^. attrEnv) (dt2 ^. attrEnv),
        _attrStdIn = (dt1 ^. attrStdIn) <|> (dt2 ^. attrStdIn),
        _attrType = (dt1 ^. attrType) <|> (dt2 ^. attrType)
      }

instance Monoid AttrData where
  mempty = AttrData M.empty M.empty Nothing Nothing

parseAttrMtdt :: Text -> Text -> AttrData
parseAttrMtdt key val = case parse (typeP <|> argP <|> envP <|> stdinP) "<codearg>" key of
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
        _runStdIn = attrDat ^. attrStdIn
      }
