module Korrvigs.Note.AST where

import Control.Lens
import Data.Aeson (Value)
import Data.Array
import Data.CaseInsensitive (CI)
import Data.Default
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Metadata.Task
import Network.URI

data Checks = Checks
  { _ckTodo :: Int,
    _ckImportant :: Int,
    _ckOngoing :: Int,
    _ckBlocked :: Int,
    _ckDone :: Int,
    _ckDont :: Int
  }
  deriving (Show, Eq)

instance Default Checks where
  def = Checks 0 0 0 0 0 0

data Document = Document
  { _docMtdt :: Map (CI Text) Value,
    _docContent :: [Block],
    _docTitle :: Text,
    _docRefTo :: Set Id,
    _docTask :: Maybe Task,
    _docTasks :: [Task],
    _docChecks :: Checks,
    _docParents :: Set Id
  }
  deriving (Show)

data Header = Header
  { _hdAttr :: Attr,
    _hdTitle :: Text,
    _hdRefTo :: Set Id,
    _hdTask :: Maybe Task,
    _hdTasks :: [Task],
    _hdChecks :: Checks,
    _hdLevel :: Int,
    _hdContent :: [Block],
    _hdParent :: Maybe Header,
    _hdDocument :: Document
  }
  deriving (Show)

data Block
  = Para [Inline]
  | LineBlock [[Inline]]
  | CodeBlock Attr Text
  | BlockQuote [Block]
  | OrderedList [[Block]]
  | BulletList [[Block]]
  | DefinitionList [([Inline], [[Block]])]
  | Figure Attr [Block] [Block] -- The first block set is the caption
  | Embed Id -- Embed a document
  | EmbedHeader Id -- Embed a document as a sub header
  | Sub Header
  | Table Table
  deriving (Show)

data Inline
  = Plain Text
  | Styled Style [Inline]
  | Code Attr Text
  | Link Attr [Inline] Id -- Named link to another entry
  | Cite Id -- Citation to entry
  | PlainLink (Maybe [Inline]) URI -- Named link to URI
  | Space
  | Break
  | DisplayMath Text
  | InlineMath Text
  | Sidenote [Block] -- Foot/side-note
  | Check TaskStatus
  deriving (Show)

data Style
  = Emph
  | Quote
  | SubScript
  | SuperScript
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
  deriving (Show)

data Table = MkTable
  { _tableCaption :: [Block],
    _tableCells :: Array (Int, Int) Cell,
    _tableAttr :: Attr,
    _tableHeader :: Int,
    _tableFooter :: Int
  }
  deriving (Show)

makeLenses ''Checks
makeLenses ''Document
makeLenses ''Attr
makeLenses ''Header
makeLenses ''Cell
makeLenses ''Table
makePrisms ''Block
makePrisms ''Inline

-- Traversal over all the inlines of a block, not recursing into subs
bkInlines :: Traversal' Block Inline
bkInlines f (Para inls) = Para <$> each f inls
bkInlines f (LineBlock inls) = LineBlock <$> each (each f) inls
bkInlines f (BlockQuote bks) = BlockQuote <$> each (bkInlines f) bks
bkInlines f (OrderedList bks) = OrderedList <$> each (each $ bkInlines f) bks
bkInlines f (BulletList bks) = BulletList <$> each (each $ bkInlines f) bks
bkInlines f (DefinitionList lst) = DefinitionList <$> each (defInlines f) lst
  where
    defInlines :: Traversal' ([Inline], [[Block]]) Inline
    defInlines g (df, content) =
      (,) <$> each g df <*> each (each $ bkInlines g) content
bkInlines f (Figure attr caption content) =
  Figure attr <$> each (bkInlines f) caption <*> each (bkInlines f) content
bkInlines f (Table (MkTable caption cells attr hd ft)) =
  Table
    <$> ( MkTable
            <$> each (bkInlines f) caption
            <*> each (cellInlines f) cells
            <*> pure attr
            <*> pure hd
            <*> pure ft
        )
  where
    cellInlines :: Traversal' Cell Inline
    cellInlines g (Cell orig width height dat) =
      Cell orig width height <$> each (bkInlines g) dat
bkInlines _ b = pure b

-- Traversal over the recursive inlines. It is a singleton on the non-recursive inlines.
inlInlines :: Traversal' Inline Inline
inlInlines f (Styled st inls) = Styled st <$> each f inls
inlInlines f (Link attr txt i) = Link attr <$> each f txt <*> pure i
inlInlines f (PlainLink (Just txt) uri) =
  PlainLink <$> (Just <$> each f txt) <*> pure uri
inlInlines f (Sidenote bks) = Sidenote <$> each (bkInlines $ inlInlines f) bks
inlInlines f i = f i

-- Traversal over all blocks in block
bkBlocks :: Traversal' Block Block
bkBlocks f (BlockQuote bks) = BlockQuote <$> each (bkBlocks f) bks
bkBlocks f (OrderedList bks) = OrderedList <$> each (each $ bkBlocks f) bks
bkBlocks f (BulletList bks) = BulletList <$> each (each $ bkBlocks f) bks
bkBlocks f (DefinitionList defs) = DefinitionList <$> each (defBlocks f) defs
  where
    defBlocks :: Traversal' ([Inline], [[Block]]) Block
    defBlocks g (df, content) = (df,) <$> each (each $ bkBlocks g) content
bkBlocks f (Figure attr caption content) =
  Figure attr <$> each (bkBlocks f) caption <*> each (bkBlocks f) content
bkBlocks f x = f x
