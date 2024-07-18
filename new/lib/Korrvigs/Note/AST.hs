module Korrvigs.Note.AST where

import Control.Lens.TH (makeLenses)
import Data.Array
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Korrvigs.Entry
import Network.URI

data Document = Document
  { _docMtdt :: Metadata,
    _docContent :: [Block],
    _docTitle :: Text,
    _docRefTo :: Set Id
  }
  deriving (Show, Eq)

data Header = Header
  { _hdAttr :: Attr,
    _hdTitle :: Text,
    _hdRefTo :: Set Id,
    _hdLevel :: Int,
    _hdContent :: [Block],
    _hdParent :: Maybe Header,
    _hdDocument :: Document
  }
  deriving (Show, Eq)

data Block
  = Para [Inline]
  | LineBlock [[Inline]]
  | CodeBlock Attr Text
  | BlockQuote [Block]
  | OrderedList [[Block]]
  | BulletList [[Block]]
  | DefinitionList [([Inline], [[Block]])]
  | Figure Attr [Block] [Block] -- The first block set is the caption
  | Embed Attr Id -- Embed a document
  | Sub Header
  | Table Table
  deriving (Show, Eq)

data Inline
  = Plain Text
  | Styled Style [Inline]
  | Code Attr Text
  | Link Attr [Inline] Id -- Named link to another entry
  | Cite Id -- Citation to entry
  | PlainLink URI -- Unnamed link to URI
  | Space
  | Break
  | DisplayMath Text
  | InlineMath Text
  | Sidenote [Block] -- Foot/side-note
  deriving (Show, Eq)

data Style
  = Emph
  | Quote
  deriving (Show, Eq)

data Attr = MkAttr
  { _attrId :: Text,
    _attrClasses :: [Text],
    _attrMtdt :: Map Text Text
  }
  deriving (Show, Eq)

-- Tables
data Cell = Cell
  { _cellOrig :: (Int, Int),
    _cellWidth :: Int,
    _cellHeight :: Int,
    _cellData :: [Block]
  }
  deriving (Eq, Show)

data Table = MkTable
  { _tableCaption :: [Block],
    _tableCells :: Array (Int, Int) Cell,
    _tableAttr :: Attr,
    _tableHeader :: Int,
    _tableFooter :: Int
  }
  deriving (Eq, Show)

makeLenses ''Document
makeLenses ''Attr
makeLenses ''Header
makeLenses ''Cell
makeLenses ''Table
