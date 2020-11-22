module Cell (Cell(..), intsToCells) where

data Cell = Cell {
  getRow :: Int,
  getColumn :: Int,
  getKvadrant :: Int,
  getIsFixed :: Bool, -- Flexible is opposite of Fixed
  getValue :: Int
} deriving (Show, Eq, Ord)
-- Cell { unCursor = False, unOrder = Nothing, unValue = 5, unPositionX = 0, unPositionY = 0 }

-- Testing
-- input :: [Int]
-- input = [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]

-- cells :: [Cell]
-- cells = intsToCells ints

checkLength :: [Int] -> [Int]
checkLength list 
  | 81 == (length list) = list
  | otherwise = error "Input not valid sudoku board"

type PositionX = Int
type PositionY = Int

-- @todo rewrite to list comprehension 
convertToTriples :: [Int] -> [(PositionX, PositionY, Int)]
convertToTriples vs = map (\i -> (xs !! i, ys !! i, vs !! i)) [0..80] 
  where xs = concat $ replicate 9 [0..8]
        -- [1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9,1,2...]
        ys = concat $ map (replicate 9) [0..8]
        -- [1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3...]
-- [(1,1,5),(2,1,3),(3,1,0),(4,1,0),(5,1,7),(6,1,0),(7,1,0)...]

cellToKvadrant :: PositionX -> PositionY -> Int
cellToKvadrant x y = (x `div` 3) + (y `div` 3) * 3

convertToCells :: [(PositionX, PositionY, Int)] -> [Cell]
convertToCells ts = map (\triple -> Cell {
    getRow = first triple,
    getColumn = second triple, 
    getKvadrant = cellToKvadrant (first triple) (second triple),
    getValue = third triple,
    getIsFixed = 0 /= third triple
  }) ts 
  where 
    first (x, _, _) = x
    second (_, x, _) = x
    third (_, _, x) = x

-- Exported
intsToCells :: [Int] -> [Cell]
intsToCells ints = convertToCells $ convertToTriples $ checkLength ints
