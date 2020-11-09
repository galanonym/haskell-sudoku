module Solver (solve, solveDo) where

import Data.List (nub)
import Data.Maybe (isNothing)
import Cell (Cell(..), intsToCells)
-- import Debug.Trace (trace)

ints = [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]

cells = intsToCells ints

cell = Cell {unCursor = False, unOrder = Just 0, unValue = 99999, unPositionX = 4, unPositionY = 7}

-- Reading cells
-- Version 1 (slowest):
-- getRow :: Cell -> [Cell] -> [Cell]
-- getRow needle = filter $ (unPositionY needle ==) . unPositionY

-- Version 2 (slow):
-- getRow :: Cell -> [Cell] -> [Cell]
-- getRow c cs = fst $ splitAt relativeEnd $ snd $ splitAt start cs
--   where y = unPositionY c
--         start = y * 9
--         relativeEnd = 9

getRow :: Cell -> [Cell] -> [Cell]
getRow c cs = take 9 endingPart 
  where y = unPositionY c
        startFrom = y * 9
        endingPart = drop startFrom cs

-- Version 1 (slow):
-- getColumn :: Cell -> [Cell] -> [Cell]
-- getColumn needle = filter $ (unPositionX needle ==) . unPositionX

getColumn :: Cell -> [Cell] -> [Cell]
getColumn c cs = c' : cs'' -- add first cell back to beginning
  where x = unPositionX c
        c' = cs !! x -- first cell
        cs' = drop (x + 1) cs -- drop until after first cell
        cs'' = every 9 cs' -- take every ninth

-- from StackOverflow 
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
  (y:ys) -> y : every n ys
  [] -> []

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

-- Version 1 (slower):
-- getKvadrant :: Cell -> [Cell] -> [Cell]
-- getKvadrant needle = filter $ (cellToKvadrant needle ==) . cellToKvadrant

-- Version 2 (without recursion):
getKvadrant :: Cell -> [Cell] -> [Cell]
getKvadrant c cs = (a1:a2:a3:a4:a5:a6:a7:a8:a9:[])
  where (x, y) = cellToKvadrant c -- kvadrant coords
        x0 = 3 * x -- starting cell in that kvadrant
        y0 = 3 * y -- starting cell in that kvadrant
        startFrom = y0 * 9 + x0 
        -- could maybe used a recursive function here instead
        (a1:a2:a3:endingPart) = drop startFrom cs -- extract first three
        (a4:a5:a6:endingPart') = drop 6 endingPart -- extract three below
        (a7:a8:a9:_) = drop 6 endingPart' -- extract three below

-- Version 3 (with recursion)
-- getKvadrant :: Cell -> [Cell] -> [Cell]
-- getKvadrant c cs = take 9 everyThirdsAfterSix 
--   where (x, y) = cellToKvadrant c -- kvadrant coords
--         x0 = 3 * x -- starting cell in that kvadrant
--         y0 = 3 * y -- starting cell in that kvadrant
--         startFrom = y0 * 9 + x0 
--         endingPart = drop startFrom cs
--         everyThirdsAfterSix = everyThirds 6 endingPart

-- everyThirds :: Int -> [a] -> [a]
-- everyThirds _ [] = []
-- everyThirds _ (a1:[]) = []
-- everyThirds _ (a1:a2:[]) = []
-- everyThirds n (a1:a2:a3:ys) = a1:a2:a3:(everyThirds n $ drop n ys)

moveCursorToNext :: [Cell] -> [Cell]
moveCursorToNext cs = cs'' 
  where cNext = setCursorTrue $ nextNotPrefilledCell cs
        c = if isCursor cs then setCursorFalse $ findCellWithCursor cs else cs !! 0 -- just return first cell dont replace it
        cs' = replaceCell cs cNext
        cs'' = replaceCell cs' c

isCursor :: [Cell] -> Bool
isCursor = not . null . filter (\c -> (unCursor c) == True)

-- Converting cells to other formats
cellsToInts :: [Cell] -> [Int]
cellsToInts = map unValue

intsToChars :: [Int] -> [Char]
intsToChars xs = map (head . show) xs -- show returns strings like "1" therefore head to convert to '1'

insertBetweenString :: Int -> Char -> String -> String 
insertBetweenString 0 _ xs = xs
insertBetweenString _ _ [] = []
insertBetweenString n char xs
 | length xs <= n = xs
 | otherwise = take n xs ++ [char] ++ insertBetweenString n char (drop n xs)

cellsToString :: [Cell] -> String
cellsToString = flip (++) "\n" . insertBetweenString 12 '\n' . insertBetweenString 3 ' ' . intsToChars . cellsToInts

findCellWithCursor :: [Cell] -> Cell
findCellWithCursor = head . filter ((True ==) . unCursor)

-- Operations
setCursorTrue :: Cell -> Cell
setCursorTrue c = c {unCursor = True}

setCursorFalse :: Cell -> Cell
setCursorFalse c = c {unCursor = False}

incrementCell :: Cell -> Cell
incrementCell c = c { unValue = 1 + unValue c }

replaceCell :: [Cell] -> Cell -> [Cell]
replaceCell cs c = cs'
  where x = unPositionX c
        y = unPositionY c
        index = x + (9 * y)
        (beginning, _ :ending) = splitAt index cs
        cs' = beginning ++ c : ending


validateField :: [Cell] -> Cell -> (Cell -> [Cell] -> [Cell]) -> Bool
validateField cs c getFunction = ints == nub ints
  where ints = filter (0 /=) $ cellsToInts $ getFunction c cs  

validateCell :: [Cell] -> Cell -> Bool
validateCell cs c = all (validateField cs c) [getRow, getColumn, getKvadrant]

isBoardValid :: [Cell] -> Bool
isBoardValid cs = and $ map (validateCell cs) cs

nextBoard :: [Cell] -> [Cell]
nextBoard cs
  | True == validateCell cs' c' = cs'
  | otherwise = nextBoard cs'
  where c = findCellWithCursor cs
        c' = incrementCell c
        cs' = replaceCell cs c'

isOver9Board :: [Cell] -> Bool
isOver9Board =  not . null . filter ((9<) . unValue)

previousNotPrefilledCell :: [Cell] -> Cell
previousNotPrefilledCell cs = last $ takeWhile ((False ==) . unCursor) $ filter (isNothing . unOrder) cs

nextNotPrefilledCell :: [Cell] -> Cell
nextNotPrefilledCell cs = last $ takeWhile ((False ==) . unCursor) $ filter (isNothing . unOrder) $ reverse cs

trackBackBoard :: [Cell] -> [Cell]
trackBackBoard cs = cs''
  where prev = previousNotPrefilledCell cs
        prev' = prev { unCursor = True }
        curr = findCellWithCursor cs
        curr' = curr { unValue = 0, unCursor = False }
        cs' = replaceCell cs curr'
        cs'' = replaceCell cs' prev'

isNo0OnBoard :: [Cell] -> Bool
isNo0OnBoard cs = [] == filter ((0==) . unValue) cs

solve :: [Cell] -> [Cell]
solve cs
  | isNo0OnBoard cs = cs
  | isOver9Board cs = solve $ nextBoard $ trackBackBoard cs
  | otherwise = solve $ nextBoard $ moveCursorToNext cs

-- mutating
printBoard :: [Cell] -> IO ()
printBoard = putStr . cellsToString

solveDo :: [Cell] -> IO ()
solveDo cs 
  | isNo0OnBoard cs = do 
    printBoard cs
  | isOver9Board cs = solveDo $ nextBoard $ trackBackBoard cs
  | otherwise = do
    printBoard cs
    putStr "\n"
    solveDo $ nextBoard $ moveCursorToNext cs
