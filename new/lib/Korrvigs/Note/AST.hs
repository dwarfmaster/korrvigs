module Korrvigs.Note.AST where

import Control.Lens.TH (makeLenses)
import Data.Map (Map)
import Data.Text (Text)
import Korrvigs.Entry
import Network.URI

data Document = Document
  { _docMtdt :: Metadata,
    _docContent :: [Block],
    _docTitle :: Text,
    _docLevel :: Int,
    _docAttr :: Attr
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
  | Figure Attr Text [Block]
  | Embed Attr Id -- Embed a document
  | Sub Document
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

makeLenses ''Document
makeLenses ''Attr
