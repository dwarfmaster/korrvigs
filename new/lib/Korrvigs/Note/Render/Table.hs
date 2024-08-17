module Korrvigs.Note.Render.Table (renderTable) where

import Control.Lens hiding ((#))
import Control.Monad
import Data.Array
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import qualified Data.Text as T
import Korrvigs.Note.AST
import Korrvigs.Note.Render.Monad
import Numeric.LinearProgramming

type Renderer = Int -> [Block] -> BSL.ByteString

data Variable
  = VarRow Int
  | VarCol Int

type VarMaker = Variable -> Int

data RenderCell = RC
  { _rcOrig :: (Int, Int),
    _rcWidth :: Int,
    _rcHeight :: Int,
    _rcRows :: Int,
    _rcCols :: Int,
    _rcContent :: [BSL.ByteString]
  }

makeLenses ''RenderCell

varRange :: (Int -> Int) -> Int -> Int -> [(Double, Int)]
varRange var low size = [1.0 # var i | i <- [low .. (low + size - 1)]]

cellConstraints :: VarMaker -> RenderCell -> [Bound [(Double, Int)]]
cellConstraints mk rc =
  [ varRange (mk . VarCol) (rc ^. rcOrig . _1) (rc ^. rcCols) :>=: (rc ^. rcWidth . to fromIntegral - 3.0 * (fromIntegral (rc ^. rcCols) - 1)),
    varRange (mk . VarRow) (rc ^. rcOrig . _2) (rc ^. rcRows) :>=: (rc ^. rcHeight . to fromIntegral - (fromIntegral (rc ^. rcRows) - 1))
  ]

renderCell :: Int -> Renderer -> Cell -> RenderCell
renderCell colWidth renderer cell =
  RC
    { _rcOrig = cell ^. cellOrig,
      _rcRows = cell ^. cellHeight,
      _rcCols = cols,
      _rcWidth = if null content then 0 else maximum $ BSL8.length <$> content,
      _rcHeight = length content,
      _rcContent = content
    }
  where
    cols = cell ^. cellWidth
    idealWidth = cols * colWidth + (cols - 1) * 3
    rendered = renderer idealWidth (cell ^. cellData)
    content = BSL8.lines rendered

renderTable :: Int -> Renderer -> Table -> RenderM ()
renderTable width renderer tbl = do
  case solution of
    Feasible (_, sol) -> drawTable tbl (fromSol sol) renderedCells
    Optimal (_, sol) -> drawTable tbl (fromSol sol) renderedCells
    _ -> writeText "Unsolved!" >> flush
  where
    (_, (ncols, nrows)) = bounds $ tbl ^. tableCells
    idealColWidth = width `div` ncols
    renderedCells = renderCell idealColWidth renderer <$> tbl ^. tableCells
    mk (VarRow i) = i
    mk (VarCol i) = nrows + i
    objective = Minimize $ replicate (nrows + ncols) 1.0
    bds =
      [mk (VarRow i) :>=: 0.5 | i <- [1 .. nrows]]
        ++ [mk (VarCol i) :>=: 0.5 | i <- [1 .. ncols]]
    uniqCells =
      map (^. _2) $
        filter
          (\((x, y), rc) -> rc ^. rcOrig == (x, y))
          [(i, renderedCells ! i) | i <- range ((1, 1), (ncols, nrows))]
    constraints = Sparse . mconcat $ cellConstraints mk <$> uniqCells
    solution = simplex objective constraints bds
    fromSol :: [Double] -> Variable -> Int
    fromSol sol v = ceiling $ sol !! (mk v - 1)

drawTable :: Table -> (Variable -> Int) -> Array (Int, Int) RenderCell -> RenderM ()
drawTable tbl varS rcs = do
  let (_, (ncols, nrows)) = bounds rcs
  forM_ [0 .. nrows] $ \row -> do
    when (row /= 0) $ do
      forM_ [1 .. (varS $ VarRow row)] $ \line -> do
        forM_ [1 .. ncols] $ \col -> do
          let rc = rcs ! (col, row)
          when (rc ^. rcOrig . _1 == col) $ do
            writeText . T.pack $ [' ' | col > 1] ++ "| "
            let offset = lineOffset (rc ^. rcOrig . _2) row
            let lineToDraw = offset + line - 1
            let width = getAvailableWidth (rc ^. rcOrig . _1) (rc ^. rcCols)
            drawLine width lineToDraw $ rc ^. rcContent
        writeText " |"
        flush >> newline
    let lineChar
          | row == tbl ^. tableHeader = '='
          | nrows - row == tbl ^. tableFooter && row /= nrows = '='
          | row == nrows && tbl ^. tableFooter /= nrows = '='
          | otherwise = '-'
    forM_ [0 .. ncols] $ \col -> do
      let spanLeft =
            row > 0
              && row < nrows
              && ( col == 0
                     || let rc = rcs ! (col, row)
                         in (rc ^. rcOrig . _2) + (rc ^. rcRows) - 1 > row
                 )
      let spanRight =
            row > 0
              && row < nrows
              && ( col == ncols
                     || let rc = rcs ! (col + 1, row)
                         in (rc ^. rcOrig . _2) + (rc ^. rcRows) - 1 > row
                 )
      let spanTop =
            row == 0
              || ( col > 0
                     && col < ncols
                     && let rc = rcs ! (col, row)
                         in (rc ^. rcOrig . _1) + (rc ^. rcCols) - 1 > col
                 )
      let spanBot =
            row == nrows
              || ( col > 0
                     && col < ncols
                     && let rc = rcs ! (col, row + 1)
                         in (rc ^. rcOrig . _1) + (rc ^. rcCols) - 1 > col
                 )
      when (col > 0) $ do
        if row > 0 && spanLeft
          then
            let rc = rcs ! (col, row)
             in when (rc ^. rcOrig . _1 == col) $ do
                  let cWidth = getAvailableWidth col (rc ^. rcCols)
                  let lineToDraw = lineOffset (rc ^. rcOrig . _2) (row + 1) - 1
                  drawLine cWidth lineToDraw $ rc ^. rcContent
          else writeText . T.pack $ replicate (varS $ VarCol col) lineChar
      let centerChar
            | spanTop && spanBot = [lineChar]
            | spanLeft && spanRight = "|"
            | otherwise = "+"
      let leftChar
            | col == 0 = ""
            | spanLeft = " "
            | otherwise = [lineChar]
      let rightChar
            | col == ncols = ""
            | spanRight = " "
            | otherwise = [lineChar]
      unless (spanLeft && spanRight && spanTop && spanBot) $ do
        writeText . T.pack $ leftChar ++ centerChar ++ rightChar
    flush >> newline
  where
    getAvailableWidth :: Int -> Int -> Int
    getAvailableWidth st ncols =
      let s = sum $ varS . VarCol <$> [st .. (st + ncols - 1)]
       in s + (ncols - 1) * 3
    lineOffset :: Int -> Int -> Int
    lineOffset st cur =
      let s = sum $ varS . VarRow <$> [st .. (cur - 1)]
       in s + (cur - st)
    drawLine :: Int -> Int -> [BSL.ByteString] -> RenderM ()
    drawLine width i lns | i < length lns = do
      let ln = lns !! i
      writeBS ln
      writeText . T.pack $ replicate (width - BSL8.length ln) ' '
    drawLine width _ _ = writeText . T.pack $ replicate width ' '
