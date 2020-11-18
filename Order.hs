module Order (cellsOrderLeftRight) where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)

import Cell (Cell(..))

-- export
cellsOrderLeftRight :: [Cell] -> [Cell] -> [Cell]
cellsOrderLeftRight acc [] = acc
cellsOrderLeftRight acc (c:cs) = cellsOrderLeftRight acc' cs
  where index = fromMaybe 0 $ findIndex (\c -> getValue c /= 0) acc
        part1 = fst $ splitAt index acc
        part2 = snd $ splitAt index acc
        acc' = 
          if (getValue c /= 0) then acc ++ [c]
          else part1 ++ [c] ++ part2
