module SolverSeq (solve, solveDo) where

import Data.List (nub)
import CellSeq (Cell(..), intsToCells)
import Data.Sequence (Seq) -- importing type names, constructors and operators unqualified
import qualified Data.Sequence as Seq
import Data.Foldable
-- import Debug.Trace (trace)

ints = [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]

cells = intsToCells ints

cell = Cell {unCursor = False, unPrefilled = False, unValue = 9, unPositionX = 5, unPositionY = 8}

-- Reading cells
getRow :: Cell -> Seq Cell -> Seq Cell
getRow needle = Seq.filter $ (unPositionY needle ==) . unPositionY

getColumn :: Cell -> Seq Cell -> Seq Cell
getColumn needle = Seq.filter $ (unPositionX needle ==) . unPositionX

-- ... ... ...
-- 0,0 1,0 2,0 
-- ... ... ...
--
-- ... ... ...
-- 0,1 1,1 2,1 
-- ... ... ...
--
-- ... ... ...
-- 0,2 1,2 2,2 
-- ... ... ...
cellToKvadrant :: Cell -> (Int, Int)
cellToKvadrant c = (unPositionX c `div` 3, unPositionY c `div` 3)

getKvadrant :: Cell -> Seq Cell -> Seq Cell
getKvadrant needle = Seq.filter $ (cellToKvadrant needle ==) . cellToKvadrant

moveCursorToNext :: Seq Cell -> Seq Cell
moveCursorToNext cs = cs'' 
  where cNext = setCursorTrue $ nextNotPrefilledCell cs
        c = if isCursor cs then setCursorFalse $ findCellWithCursor cs else Seq.index cs 0 -- just return first cell dont replace it
        cs' = replaceCell cs cNext
        cs'' = replaceCell cs' c

isCursor :: Seq Cell -> Bool
isCursor = not . null . Seq.filter (\c -> (unCursor c) == True)

-- Converting cells to other formats
cellsToInts :: Seq Cell -> [Int]
cellsToInts = toList . fmap unValue

intsToChars :: [Int] -> [Char]
intsToChars xs = map (head . show) xs -- show returns strings like "1" therefore head to convert to '1'

insertBetweenString :: Int -> Char -> String -> String 
insertBetweenString 0 _ xs = xs
insertBetweenString _ _ [] = []
insertBetweenString n char xs
 | length xs <= n = xs
 | otherwise = take n xs ++ [char] ++ insertBetweenString n char (drop n xs)

cellsToString :: Seq Cell -> String
cellsToString = flip (++) "\n" . insertBetweenString 12 '\n' . insertBetweenString 3 ' ' . intsToChars . cellsToInts

findCellWithCursor :: Seq Cell -> Cell
findCellWithCursor = head . toList . Seq.filter ((True ==) . unCursor)

-- Operations
setCursorTrue :: Cell -> Cell
setCursorTrue c = c {unCursor = True}

setCursorFalse :: Cell -> Cell
setCursorFalse c = c {unCursor = False}

incrementCell :: Cell -> Cell
incrementCell c = c { unValue = 1 + unValue c }

replaceCell :: Seq Cell -> Cell -> Seq Cell
replaceCell cs needle = fmap (\c -> 
  if 
    unPositionX c == unPositionX needle &&
    unPositionY c == unPositionY needle
  then
    needle 
  else 
    c
  ) cs

validateField :: Seq Cell -> Cell -> (Cell -> Seq Cell -> Seq Cell) -> Bool
validateField cs c getFunction = ints == nub ints
  where ints = filter (0 /=) $ cellsToInts $ getFunction c cs  

validateCell :: Seq Cell -> Cell -> Bool
validateCell cs c = all (validateField cs c) [getRow, getColumn, getKvadrant]

isBoardValid :: Seq Cell -> Bool
isBoardValid cs = and $ fmap (validateCell cs) cs

nextBoard :: Seq Cell -> Seq Cell
nextBoard cs
  | True == validateCell cs' c' = cs'
  | otherwise = nextBoard cs'
  where c = findCellWithCursor cs
        c' = incrementCell c
        cs' = replaceCell cs c'

isOver9Board :: Seq Cell -> Bool
isOver9Board =  not . null . Seq.filter ((9<) . unValue)

previousNotPrefilledCell :: Seq Cell -> Cell
previousNotPrefilledCell cs = last $ takeWhile ((False ==) . unCursor) $ toList $ Seq.filter ((False ==) . unPrefilled) cs

nextNotPrefilledCell :: Seq Cell -> Cell
nextNotPrefilledCell cs = last $ takeWhile ((False ==) . unCursor) $ toList $ Seq.filter ((False ==) . unPrefilled) $ Seq.reverse cs

trackBackBoard :: Seq Cell -> Seq Cell
trackBackBoard cs = cs''
  where prev = previousNotPrefilledCell cs
        prev' = prev { unCursor = True }
        curr = findCellWithCursor cs
        curr' = curr { unValue = 0, unCursor = False }
        cs' = replaceCell cs curr'
        cs'' = replaceCell cs' prev'

isNo0OnBoard :: Seq Cell -> Bool
isNo0OnBoard cs = Seq.empty == Seq.filter ((0==) . unValue) cs

solve :: Seq Cell -> Seq Cell
solve cs
  | isNo0OnBoard cs = cs
  | isOver9Board cs = solve $ nextBoard $ trackBackBoard cs
  | otherwise = solve $ nextBoard $ moveCursorToNext cs

-- mutating
printBoard :: Seq Cell -> IO ()
printBoard = putStr . cellsToString

solveDo :: Seq Cell -> IO ()
solveDo cs 
  | isNo0OnBoard cs = do 
    printBoard cs
  | isOver9Board cs = solveDo $ nextBoard $ trackBackBoard cs
  | otherwise = do
    printBoard cs
    putStr "\n"
    solveDo $ nextBoard $ moveCursorToNext cs
