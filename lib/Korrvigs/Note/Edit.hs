module Korrvigs.Note.Edit
  ( addSubHeaderLast,
    addSubHeaderFirst,
    addHeaderAfter,
    addHeaderBefore,
    reRoot,
  )
where

import Control.Arrow ((***))
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Korrvigs.Note.AST
import Korrvigs.Note.Loc

newHeader :: Int -> Header
newHeader lvl =
  Header
    { _hdAttr = MkAttr "" [] M.empty,
      _hdTitle = "NEW HEADER",
      _hdRefTo = S.empty,
      _hdTask = Nothing,
      _hdTasks = [],
      _hdChecks = Checks 0 0 0 0 0 0,
      _hdLevel = lvl,
      _hdContent = [],
      _hdCollections = M.empty
    }

insertSubFirst :: Header -> [Block] -> [Block]
insertSubFirst hd [] = [Sub hd]
insertSubFirst hd (Sub hd' : bks) = Sub hd : Sub hd' : bks
insertSubFirst hd (bk : bks) = bk : insertSubFirst hd bks

insertNewSubFirst :: Header -> Header
insertNewSubFirst hd = hd & hdContent %~ insertSubFirst sb
  where
    sb = newHeader $ 1 + hd ^. hdLevel

addSubHeaderFirst :: SubLoc -> Document -> Document
addSubHeaderFirst loc = sub loc %~ insertNewSubFirst

insertNewSubLast :: Header -> Header
insertNewSubLast hd = hd & hdContent %~ (++ [Sub sb])
  where
    sb = newHeader $ 1 + hd ^. hdLevel

addSubHeaderLast :: SubLoc -> Document -> Document
addSubHeaderLast loc = sub loc %~ insertNewSubLast

insertSecond :: a -> [a] -> [a]
insertSecond x [] = [x]
insertSecond x (a : as) = a : x : as

addHeaderAfter :: SubLoc -> Document -> Document
addHeaderAfter loc = subs loc %~ insertSecond (Sub sb)
  where
    sb = newHeader $ subLvl loc

addHeaderBefore :: SubLoc -> Document -> Document
addHeaderBefore loc = subs loc %~ (Sub sb :)
  where
    sb = newHeader $ subLvl loc

reRoot :: Int -> Block -> Block
reRoot delta (Para inls) = Para $ reRootInline delta <$> inls
reRoot delta (BlockQuote bks) = BlockQuote $ reRoot delta <$> bks
reRoot delta (OrderedList bks) = OrderedList $ map (reRoot delta) <$> bks
reRoot delta (BulletList bks) = BulletList $ map (reRoot delta) <$> bks
reRoot delta (DefinitionList defs) =
  DefinitionList $ (map (reRootInline delta) *** map (map $ reRoot delta)) <$> defs
reRoot delta (Figure attr caption content) =
  Figure attr (reRoot delta <$> caption) (reRoot delta <$> content)
reRoot delta (EmbedHeader i lvl) = EmbedHeader i $ lvl + delta
reRoot delta (Sub hd) =
  Sub $
    hd
      & hdLevel %~ (+ delta)
      & hdContent . each %~ reRoot delta
reRoot delta (Table tbl) =
  Table $
    tbl
      & tableCaption . each %~ reRoot delta
      & tableCells . each . cellData . each %~ reRoot delta
reRoot _ bk = bk

reRootInline :: Int -> Inline -> Inline
reRootInline delta (Sidenote bks) = Sidenote $ reRoot delta <$> bks
reRootInline _ inl = inl
