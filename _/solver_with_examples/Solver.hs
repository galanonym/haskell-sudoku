module Solver (solveDo) where

import Data.List (nub)
import Cell (Cell(..), intsToCells)
import Debug.Trace (trace)

-- 530 070 000
-- 600 195 000
-- 098 000 060

-- 800 060 003
-- 400 803 001
-- 700 020 006

-- 060 000 280
-- 000 419 005
-- 000 080 079

intsExample = [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]

cellsExample = intsToCells intsExample
-- [Cell {unCursor = False, unPrefilled = True, unValue = 5, unPositionX = 1, unPositionY = 1},Cell {unCursor = False, unPrefilled = True, unValue = 3, unPositionX = 2, unPositionY = 1},Cell {unCursor = False, unPrefilled = False, unValue = 0, unPositionX = 3, unPositionY = 1}...]

cell_0x0_5 = cellsExample !! 0 
cell_1x0_3 = cellsExample !! 1 
cell_2x0_0 = cellsExample !! 2 
cell_4x0_7 = cellsExample !! 4 
cell_5x0_0 = cellsExample !! 5 
cell_6x0_0 = cellsExample !! 6 
cell_0x1_6 = cellsExample !! 9 
cell_3x1_1 = cellsExample !! 12
cell_3x4_8 = cellsExample !! 39
cell_7x8_7 = cellsExample !! 79
-- Cell {unCursor = False, unPrefilled = True, unValue = 5, unPositionX = 1, unPositionY = 1}

-- Reading cells
getRow :: Cell -> [Cell] -> [Cell]
getRow needle = filter $ (unPositionY needle ==) . unPositionY

getColumn :: Cell -> [Cell] -> [Cell]
getColumn needle = filter $ (unPositionX needle ==) . unPositionX

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

getKvadrant :: Cell -> [Cell] -> [Cell]
getKvadrant needle = filter $ (cellToKvadrant needle ==) . cellToKvadrant

next0Cell :: [Cell] -> Cell
next0Cell = head . filter ((0 ==) . unValue) . filter ((False ==) . unPrefilled)

moveCursorToNext :: [Cell] -> [Cell]
moveCursorToNext cells = cells2 
  where cellN = setCursorTrue $ nextNotPrefilledCell cells
        cellO = if isCursor cells then setCursorFalse $ findCellWithCursor cells else cells !! 0 -- just return first cell dont replace it
        cells1 = replaceCell cells cellN
        cells2 = replaceCell cells1 cellO

isCursor :: [Cell] -> Bool
isCursor = not . null . filter (\cell -> (unCursor cell) == True)

-- Converting cells to other formats
cellsToInts :: [Cell] -> [Int]
cellsToInts = map unValue

intsToChars :: [Int] -> [Char]
intsToChars xs = map (head . show) xs -- show returns strings like "1" therefore head to convert to '1'

insertBetweenString :: Int -> Char -> String -> String 
insertBetweenString 0 y xs = xs
insertBetweenString n y [] = []
insertBetweenString n y xs
 | length xs <= n = xs
 | otherwise = take n xs ++ [y] ++ insertBetweenString n y (drop n xs)

cellsToString :: [Cell] -> String
cellsToString = flip (++) "\n" . insertBetweenString 12 '\n' . insertBetweenString 3 ' ' . intsToChars . cellsToInts

findCellWithCursor :: [Cell] -> Cell
findCellWithCursor = head . filter ((True ==) . unCursor)

-- cellsToInfo :: [Cell] -> String
-- cellsToInfo

-- Operations
setCursorTrue :: Cell -> Cell
setCursorTrue cell = cell {unCursor = True}

setCursorFalse :: Cell -> Cell
setCursorFalse cell = cell {unCursor = False}

incrementCell :: Cell -> Cell
incrementCell cell = cell { unValue = 1 + unValue cell }

replaceCell :: [Cell] -> Cell -> [Cell]
replaceCell cells needle = map (\cell -> if 
                                     unPositionX cell == unPositionX needle &&
                                     unPositionY cell == unPositionY needle
                                   then
                                     needle 
                                   else 
                                     cell
                                   ) cells

cellWithCursor :: Cell
cellWithCursor = findCellWithCursor . replaceCell cellsExample $ setCursorTrue $ next0Cell cellsExample

cellToTest :: Cell
cellToTest = incrementCell cellWithCursor

cellsToTest :: [Cell]
cellsToTest = replaceCell cellsExample $ setCursorTrue cellWithCursor

validateField :: [Cell] -> Cell -> (Cell -> [Cell] -> [Cell]) -> Bool
validateField cells cell getFunction = ints == nub ints
  where ints = filter (0 /=) $ cellsToInts $ getFunction cell cells  

(...) = (.) . (.) -- blackbird operator

validateCell :: [Cell] -> Cell -> Bool
validateCell = flip all [getRow, getColumn, getKvadrant] ... validateField

-- validateCell :: Cell -> [Cell] -> Bool
-- validateCell cells cell = 
--   validateField cells cell getRow &&
--   validateField cells cell getColumn &&
--   validateField cells cell getKvadrant

validateBoard :: [Cell] -> Bool
validateBoard cells = and $ map (validateCell cells) cells

nextBoard :: [Cell] -> [Cell]
nextBoard cs1
  | validateBoard cs2 == True = cs2
  | otherwise = nextBoard cs2
  where c1 = findCellWithCursor cs1
        c2 = incrementCell c1
        cs2 = replaceCell cs1 c2

isOver9Board :: [Cell] -> Bool
isOver9Board =  not . null . filter ((9<) . unValue)

previousNotPrefilledCell :: [Cell] -> Cell
previousNotPrefilledCell cells = last $ takeWhile ((False ==) . unCursor) $ filter ((False ==) . unPrefilled) cells

nextNotPrefilledCell :: [Cell] -> Cell
nextNotPrefilledCell cells = last $ takeWhile ((False ==) . unCursor) $ filter ((False ==) . unPrefilled) $ reverse cells

trackBackBoard :: [Cell] -> [Cell]
trackBackBoard cs1 = cs3
  where prev1 = previousNotPrefilledCell cs1
        prev2 = prev1 { unCursor = True }
        curr1 = findCellWithCursor cs1
        curr2 = curr1 { unValue = 0, unCursor = False }
        cs2 = replaceCell cs1 curr2
        cs3 = replaceCell cs2 prev2

no0OnBoard :: [Cell] -> Bool
no0OnBoard cells = [] == filter ((0==) . unValue) cells

solve :: [Cell] -> [Cell]
solve cells
  | True == no0OnBoard cells = cells
  | True == isOver9Board cells = solve $ nextBoard $ trackBackBoard cells
  | otherwise = trace (cellsToString cells) $ solve $ nextBoard $ moveCursorToNext cells
  -- | otherwise = solve $ nextBoard $ moveCursorToNext $ cells

solveNext :: [Cell] -> [Cell]
solveNext cells
  | True == isOver9Board cells = solveNext $ nextBoard $ trackBackBoard cells
  | otherwise = nextBoard $ moveCursorToNext cells

-- mutating
printBoard :: [Cell] -> IO ()
printBoard = putStr . cellsToString

solveDo :: [Cell] -> IO ()
solveDo cells 
  | True == no0OnBoard cells = do 
    printBoard cells
  | otherwise = do
    printBoard cells
    putStr "\n"
    solveDo $ nextBoard $ moveCursorToNext cells
