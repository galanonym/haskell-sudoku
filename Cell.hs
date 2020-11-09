module Cell (Cell(..), intsToCells) where

data Cell = Cell {
    unCursor :: Bool,
    unPrefilled :: Bool,
    unPositionX :: Int,
    unPositionY :: Int,
    unValue :: Int
  } deriving (Show, Eq, Ord)
-- Cell { unCursor = False, unPrefilled = False, unValue = 5, unPositionX = 0, unPositionY = 0 }

checkLength :: [Int] -> [Int]
checkLength list 
  | 81 == (length list) = list
  | otherwise = error "Input not valid sudoku board"

type PositionX = Int
type PositionY = Int

convertToTriples :: [Int] -> [(PositionX, PositionY, Int)]
convertToTriples vs = map (\i -> (xs !! i, ys !! i, vs !! i)) [0..80] 
  where xs = concat $ replicate 9 [0..8]
        -- [1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9,1,2...]
        ys = concat $ map (replicate 9) [0..8]
        -- [1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3...]
-- [(1,1,5),(2,1,3),(3,1,0),(4,1,0),(5,1,7),(6,1,0),(7,1,0)...]

convertToCells :: [(PositionX, PositionY, Int)] -> [Cell]
convertToCells ts = map (\triple -> Cell {
    unCursor = False,
    unPrefilled = (third triple) /= 0,
    unPositionX = first triple,
    unPositionY = second triple, 
    unValue = third triple
  }) ts 
  where 
    first (x, _, _) = x
    second (_, x, _) = x
    third (_, _, x) = x
-- [Cell {unCursor = False, unPrefilled = True, unValue = 5, unPositionX = 1, unPositionY = 1},Cell {unCursor = False, unPrefilled = True, unValue = 3, unPositionX = 2, unPositionY = 1},Cell {unCursor = False, unPrefilled = False, unValue = 0, unPositionX = 3, unPositionY = 1}...]

intsToCells :: [Int] -> [Cell]
intsToCells = convertToCells . convertToTriples . checkLength
