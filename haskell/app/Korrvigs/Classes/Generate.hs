module Korrvigs.Classes.Generate (generateClassHs) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (toUpper)
import Data.List (intersperse, sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Schema
import Opaleye ((./=))
import qualified Opaleye as O

generateClassHs :: MonadIO m => Connection -> m Text
generateClassHs conn = do
  classes <- findAllClasses conn
  pure $
    toStrict $
      toLazyText $
        unlinesB
          [ header,
            classDefinition classes,
            nameDefinition classes,
            parseDefinition classes,
            isaDefinition classes,
            ""
          ]

unlinesB :: [Builder] -> Builder
unlinesB = mconcat . intersperse "\n"

header :: Builder
header =
  unlinesB
    [ "-- This file has been auto-generated",
      "module Korrvigs.Classes.Generated where",
      "",
      "import Data.Ix (Ix)",
      "import Data.Text (Text)"
    ]

classDefinition :: [(Text, Text)] -> Builder
classDefinition classes =
  mconcat $
    ["\ndata Class = Entity"]
      ++ (map (\(cls, _) -> " | " <> toPascalCase cls) classes)
      ++ [" deriving (Show, Eq, Enum, Bounded, Ord, Ix)"]

nameDefinition :: [(Text, Text)] -> Builder
nameDefinition classes =
  unlinesB $
    [ "\nname :: Class -> Text",
      "name Entity = \"Entity\""
    ]
      ++ (map (\(cls, _) -> "name " <> toPascalCase cls <> " = \"" <> fromText cls <> "\"") classes)

parseDefinition :: [(Text, Text)] -> Builder
parseDefinition classes =
  unlinesB $
    [ "\nparse :: Text -> Maybe Class",
      "parse \"Entity\" = Just Entity"
    ]
      ++ (map (\(cls, _) -> "parse \"" <> fromText cls <> "\" = Just " <> toPascalCase cls) classes)
      ++ ["parse _ = Nothing"]

isaDefinition :: [(Text, Text)] -> Builder
isaDefinition classes =
  unlinesB $
    [ "\nisA :: Class -> Class",
      "isA Entity = Entity"
    ]
      ++ (map (\(cls, parent) -> "isA " <> toPascalCase cls <> " = " <> toPascalCase parent) classes)

-- List all classes with their parent, except entity
findAllClasses :: MonadIO m => Connection -> m [(Text, Text)]
findAllClasses conn = do
  classes <- liftIO $ O.runSelect conn $ do
    (cls_, prt_) <- O.selectTable classesTable
    O.where_ $ cls_ ./= O.sqlStrictText "Entity"
    return (cls_, prt_)
  pure $ sortBy (\(cls1, _) (cls2, _) -> compare cls1 cls2) classes

toPascalCase :: Text -> Builder
toPascalCase = fromString . toPascalCaseStr . T.unpack

toPascalCaseStr :: String -> String
toPascalCaseStr (c : str) = toUpper c : toPascalCaseStr' str
toPascalCaseStr [] = []

toPascalCaseStr' :: String -> String
toPascalCaseStr' (' ' : c : str) = toUpper c : toPascalCaseStr' str
toPascalCaseStr' (c : str) = c : toPascalCaseStr' str
toPascalCaseStr' [] = []
