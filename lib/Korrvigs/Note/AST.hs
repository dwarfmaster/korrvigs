{-# LANGUAGE RankNTypes #-}

module Korrvigs.Note.AST where

import Control.Lens hiding ((.=))
import Data.Aeson hiding (Array)
import Data.Array
import Data.CaseInsensitive (CI)
import Data.Default
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Korrvigs.Compute.SQL
import Korrvigs.Entry hiding (_Syndicate)
import Korrvigs.Entry.JSON
import Korrvigs.Metadata.Task
import Korrvigs.Query
import Korrvigs.Utils
import Korrvigs.Utils.Multiplate
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
    _docChecks :: Checks,
    _docParents :: Set Id,
    _docCollections :: Map Text [Id],
    _docNamedSubs :: Set Text,
    _docNamedCode :: Set Text,
    _docComputations :: Map Text ComputationResult
  }
  deriving (Show)

data Header = Header
  { _hdAttr :: Attr,
    _hdTitle :: Text,
    _hdRefTo :: Set Id,
    _hdTask :: Maybe Task,
    _hdChecks :: Checks,
    _hdLevel :: Int,
    _hdContent :: [Block],
    _hdCollections :: Map Text [Id]
  }
  deriving (Show)

data Collection
  = ColList
  | ColMap
  | ColGallery
  | ColTimeline
  | ColNetwork
  | ColFuzzy
  | ColCalendar
  | ColKanban
  | ColTaskList
  | ColLibrary
  | ColPlayList
  | ColContacts
  deriving (Show, Eq, Ord, Bounded, Enum)

collectionText :: Collection -> Text
collectionText ColList = "list"
collectionText ColMap = "map"
collectionText ColGallery = "gallery"
collectionText ColTimeline = "timeline"
collectionText ColNetwork = "network"
collectionText ColFuzzy = "fuzzy"
collectionText ColCalendar = "calendar"
collectionText ColKanban = "kanban"
collectionText ColTaskList = "tasklist"
collectionText ColLibrary = "library"
collectionText ColPlayList = "playlist"
collectionText ColContacts = "contacts"

data CollectionItem
  = ColItemEntry Id
  | ColItemInclude Id Text
  | ColItemQuery Query
  | ColItemSubOf Id
  | ColItemDummy EntryJSON
  | ColItemComment Text
  deriving (Show)

instance ToJSON CollectionItem where
  toJSON (ColItemEntry i) =
    object ["type" .= ("entry" :: Text), "entry" .= unId i]
  toJSON (ColItemInclude i col) =
    object ["type" .= ("include" :: Text), "entry" .= unId i, "col" .= col]
  toJSON (ColItemQuery q) =
    object ["type" .= ("query" :: Text), "query" .= q]
  toJSON (ColItemSubOf i) =
    object ["type" .= ("subof" :: Text), "entry" .= unId i]
  toJSON (ColItemDummy v) =
    object ["type" .= ("dummy" :: Text), "value" .= v]
  toJSON (ColItemComment c) =
    object ["type" .= ("comment" :: Text), "comment" .= c]

instance FromJSON CollectionItem where
  parseJSON = withObject "CollectionItem" $ \obj -> do
    tp <- obj .: "type"
    case tp of
      "entry" -> ColItemEntry . MkId <$> obj .: "entry"
      "include" -> ColItemInclude . MkId <$> obj .: "entry" <*> obj .: "col"
      "query" -> ColItemQuery <$> obj .: "query"
      "subof" -> ColItemSubOf . MkId <$> obj .: "entry"
      "dummy" -> ColItemDummy <$> obj .: "value"
      "comment" -> ColItemComment <$> obj .: "comment"
      _ -> fail $ T.unpack tp <> " is not a valid collection item"

data Block
  = Para [Inline]
  | LineBlock [[Inline]]
  | CodeBlock Attr Text
  | BlockQuote [Block]
  | OrderedList [[Block]]
  | BulletList [[Block]]
  | DefinitionList [([Inline], [[Block]])]
  | Figure Attr [Block] [Block] -- The first block set is the caption
  | Embed (Either Text Id) -- Embed a document
  | EmbedHeader (Either Text Id) Int -- Embed a document as a sub header
  | Collection Collection Text [CollectionItem]
  | Syndicate Text Bool (Maybe Int) [(Id, Maybe Text)]
  | Sub Header
  | Table Table
  deriving (Show)

data EntryView
  = ViewEntry
  | ViewSyndicate
  | ViewSyndicateUnread
  deriving (Show)

data Inline
  = Plain Text
  | Styled Style [Inline]
  | Code Attr Text
  | Link Attr [Inline] Id EntryView -- Named link to another entry
  | Cite Id -- Citation to entry
  | MtdtLink (Maybe [Inline]) Text -- Named link to the content of a metadata
  | DateLink (Maybe [Inline]) Day -- Named link to a day page
  | MailtoLink (Maybe [Inline]) Text -- Named link to a mail address
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
    _attrMtdt :: Map Text [Text]
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
makePrisms ''CollectionItem

-- Plate instances
instance Plated Header where
  plate = hdContent . each . _Sub

data ASTPlate f = ASTPlate
  { block :: Block -> f Block,
    inline :: Inline -> f Inline,
    header :: Header -> f Header
  }

buildBlock :: (Applicative f) => ASTPlate f -> Block -> f Block
buildBlock f (Para inls) = Para <$> each (inline f) inls
buildBlock f (LineBlock inls) = LineBlock <$> each (each $ inline f) inls
buildBlock f (BlockQuote bks) = BlockQuote <$> each (block f) bks
buildBlock f (OrderedList bks) = OrderedList <$> each (each $ block f) bks
buildBlock f (BulletList bks) = BulletList <$> each (each $ block f) bks
buildBlock f (DefinitionList defs) = DefinitionList <$> each defInlines defs
  where
    defInlines (inls, bks) = (,) <$> each (inline f) inls <*> each (each $ block f) bks
buildBlock f (Figure attr caption bks) =
  Figure attr <$> each (block f) caption <*> each (block f) bks
buildBlock f (Sub hd) = Sub <$> header f hd
buildBlock f (Table (MkTable caption cells attr hd ft)) =
  Table
    <$> ( MkTable
            <$> each (block f) caption
            <*> each cell cells
            <*> pure attr
            <*> pure hd
            <*> pure ft
        )
  where
    cell (Cell orig w h dat) = Cell orig w h <$> each (block f) dat
buildBlock _ bk = pure bk

buildInline :: (Applicative f) => ASTPlate f -> Inline -> f Inline
buildInline f (Styled stl inls) = Styled stl <$> each (inline f) inls
buildInline f (Link attr caption i vw) =
  Link attr <$> each (inline f) caption <*> pure i <*> pure vw
buildInline f (MtdtLink (Just inls) txt) =
  MtdtLink . Just <$> each (inline f) inls <*> pure txt
buildInline f (DateLink (Just inls) day) =
  DateLink . Just <$> each (inline f) inls <*> pure day
buildInline f (PlainLink (Just inls) uri) =
  PlainLink . Just <$> each (inline f) inls <*> pure uri
buildInline f (Sidenote bks) =
  Sidenote <$> each (block f) bks
buildInline _ inl = pure inl

buildHeader :: (Applicative f) => ASTPlate f -> Header -> f Header
buildHeader child = hdContent . each $ block child

instance Multiplate ASTPlate where
  multiplate child =
    ASTPlate (buildBlock child) (buildInline child) (buildHeader child)
  mkPlate build = ASTPlate (build block) (build inline) (build header)

-- Traversal over all the inlines of a block, not recursing into subs
bkInlines :: Traversal' Block Inline
bkInlines f =
  block $
    multiplate $
      ASTPlate
        { block = bkInlines f,
          inline = f,
          header = pure
        }

-- Traversal over the recursive inlines. It is a singleton on the non-recursive inlines.
inlInlines :: Traversal' Inline Inline
inlInlines f =
  inline $
    multiplate $
      ASTPlate
        { block = bkInlines $ inlInlines f,
          inline = f,
          header = pure
        }

bkBlocksImpl :: (Applicative f) => ASTPlate f -> (Block -> f Block) -> (Block -> f Block)
bkBlocksImpl _ f bk@(Para _) = f bk
bkBlocksImpl _ f bk@(LineBlock _) = f bk
bkBlocksImpl _ f bk@(CodeBlock _ _) = f bk
bkBlocksImpl _ f bk@(Embed _) = f bk
bkBlocksImpl _ f bk@(EmbedHeader _ _) = f bk
bkBlocksImpl _ f bk@(Collection _ _ _) = f bk
bkBlocksImpl p _ bk = block (multiplate p) bk

-- Traversal over all blocks in block, not descending into headers
bkBlocks :: Traversal' Block Block
bkBlocks f (Sub h) = f $ Sub h
bkBlocks f bk = bkBlocksImpl (purePlate {block = bkBlocks f}) f bk

-- Traversal over all blocks in block, descending into subs, not including headers
bkSubBlocks :: Traversal' Block Block
bkSubBlocks f = bkBlocksImpl plt f
  where
    plt =
      purePlate
        { block = bkSubBlocks f,
          header = hdContent . each $ bkSubBlocks f
        }

-- Traversal over collections
bkCollections' :: Traversal' Block (Collection, Text, [CollectionItem])
bkCollections' f (Collection col nm ids) = uncurry3 Collection <$> f (col, nm, ids)
bkCollections' _ x = pure x

bkCollections :: Traversal' Block (Collection, Text, [CollectionItem])
bkCollections = bkSubBlocks . bkCollections'

-- bkCollection :: Text -> Traversal' Block (Collection, Text, [CollectionItem])
bkCollection :: (Applicative f) => Text -> ((Collection, Text, [CollectionItem]) -> f (Collection, Text, [CollectionItem])) -> Block -> f Block
bkCollection nm = bkCollections . filtered ((== nm) . view _2)

bkNamedSub :: (Applicative f) => Text -> (Header -> f Header) -> Block -> f Block
bkNamedSub i = bkSubBlocks . _Sub . filtered ((== i) . view (hdAttr . attrId))

bkNamedCode :: (Applicative f) => Text -> ((Attr, Text) -> f (Attr, Text)) -> Block -> f Block
bkNamedCode i = bkSubBlocks . _CodeBlock . filtered ((== i) . view (_1 . attrId))

bkNamedSyn :: (Applicative f) => Text -> ((Text, Bool, Maybe Int, [(Id, Maybe Text)]) -> f (Text, Bool, Maybe Int, [(Id, Maybe Text)])) -> Block -> f Block
bkNamedSyn i = bkSubBlocks . _Syndicate . filtered ((== i) . view _1)

splitStartingParagraph :: [Block] -> ([Block], [Block])
splitStartingParagraph = span (not . isSub)
  where
    isSub :: Block -> Bool
    isSub = isJust . (^? _Sub)
