module Order (boardOrderLeftRight, boardOrderOriginal) where

import Data.List (findIndex, sortBy)
import Data.Maybe (fromMaybe)

import Cell (Cell(..))
import Board (Board(..))

-- export

boardOrderOriginal :: Board -> Board
boardOrderOriginal Board{getPointer=p, getCells=cs} = Board{getPointer=p, getCells=cellsOrderOriginal cs}

boardOrderLeftRight :: Board -> Board
boardOrderLeftRight Board{getPointer=p, getCells=cs} = Board{getPointer=p, getCells=cellsOrderLeftRight [] cs}

-- internal

cellsOrderLeftRight :: [Cell] -> [Cell] -> [Cell]
cellsOrderLeftRight acc [] = acc
cellsOrderLeftRight acc (c:cs) = cellsOrderLeftRight acc' cs
  where index = fromMaybe 0 $ findIndex ((0 /=) . getValue) acc
        part1 = fst $ splitAt index acc
        part2 = snd $ splitAt index acc
        acc' = 
          if (getValue c /= 0) then acc ++ [c]
          else part1 ++ [c] ++ part2

cellsOrderOriginal :: [Cell] -> [Cell]
cellsOrderOriginal cs = map (\np -> snd np) $ numberedPairsSort $ cellsToNumberedPairs [] cs 

cellsToNumberedPairs :: [(Int, Cell)] -> [Cell] -> [(Int, Cell)]
cellsToNumberedPairs acc [] = acc
cellsToNumberedPairs acc (c:cs) = cellsToNumberedPairs acc' cs
  where n = getColumn c + getRow c * 9  
        acc' = acc ++ [(n, c)]

numberedPairsSort :: [(Int, Cell)] -> [(Int, Cell)]
numberedPairsSort = sortBy (\(a,_) (b,_) -> compare a b) 
